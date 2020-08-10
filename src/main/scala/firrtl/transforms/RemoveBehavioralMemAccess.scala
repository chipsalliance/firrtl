// See LICENSE for license details.

package firrtl
package transforms

import firrtl._
import firrtl.ir._
import firrtl.Mappers._
import firrtl.traversals.Foreachers._
import firrtl.passes.MemPortUtils._

import collection.mutable

object RemoveBehavioralMemAccess extends Transform with DependencyAPIMigration {
  override def prerequisites = Nil
  override def optionalPrerequisites = Nil
  override def optionalPrerequisiteOf = Nil
  override def invalidates(a: Transform) = false

  // TODO: how does explicit enable in accessor decl compose with condition on write op? on read op?
  // Proposal for writes: emit enable expr, not Utils.one, at point of write (lines up with expectations)
  // Proposal for reads: ban reads with explicit condition if separated from mem decl by when in Chisel

  // Checks: add checks for when-legality, mem/access always being Ref (not other exp)

  private case class PortMetadata(info: Info, name: String, isRead: Boolean, isWrite: Boolean, clock: Expression, addr: Expression, en: Expression) {
    def isReadOnly = isRead && !isWrite
    def isWriteOnly = !isRead && isWrite
    def isReadwrite = isRead && isWrite
  }

  private class MemAnalysis {
    private val memsByName = new mutable.HashMap[String, DefMemory]
    private val memNS = new mutable.HashMap[String, Namespace]

    private val accessesByName = new mutable.HashMap[String, DefMemAccess]
    private val accessesByMem = new mutable.HashMap[String, mutable.Map[DefMemAccess, PortMetadata]]

    def containsBehavioral(): Boolean = accessesByName.nonEmpty

    def isMem(ref: Reference): Boolean = memsByName.contains(ref.name)
    def getMem(mem: Reference): DefMemory = memsByName(mem.name)

    def defMem(mem: DefMemory): Unit = {
      memsByName(mem.name) = mem
      memNS(mem.name) = Namespace(mem.readers ++ mem.writers ++ mem.readwriters)
      accessesByMem(mem.name) = new mutable.LinkedHashMap[DefMemAccess, PortMetadata]
    }

    def defMemAccess(ma: DefMemAccess): Unit = accessesByName(ma.name) = ma

    private def associateOp(mem: String, ma: String, isWrite: Boolean): Unit = {
      val acc = accessesByName(ma)
      val port = accessesByMem(mem).get(acc)
      port match {
        case Some(p) if isWrite => accessesByMem(mem)(accessesByName(ma)) = p.copy(isWrite = true)
        case Some(p) => accessesByMem(mem)(accessesByName(ma)) = p.copy(isRead = true)
        case None =>
          val pName = memNS(mem).newName(ma)
          accessesByMem(mem)(acc) = PortMetadata(acc.info, pName, !isWrite, isWrite, acc.clock, acc.addr, acc.en)
      }
    }

    def associateRead(mem: String, ma: String): Unit = associateOp(mem, ma, false)
    def associateWrite(mem: String, ma: String): Unit = associateOp(mem, ma, true)

    def getPort(mem: String, access: String): PortMetadata = accessesByMem(mem)(accessesByName(access))

    def getInferredPorts(mem: String) = accessesByMem(mem).map(_._2)
  }

  private def analyzeExpr(memInfo: MemAnalysis)(expr: Expression): Unit = expr match {
    case ApplyMemAccess(mem: Reference, acc: Reference, _, _) if memInfo.isMem(mem) =>
      memInfo.associateRead(mem.name, acc.name)
    case e => e.foreach(analyzeExpr(memInfo))
  }

  private def analyzeStmt(memInfo: MemAnalysis)(stmt: Statement): Unit = stmt match {
    case mem: DefMemory =>
      memInfo.defMem(mem)
    case ma: DefMemAccess =>
      memInfo.defMemAccess(ma)
      ma.foreach(analyzeExpr(memInfo))
    case Connect(_, ApplyMemAccess(mem: Reference, acc: Reference, _, _), wdata) if memInfo.isMem(mem) =>
      memInfo.associateWrite(mem.name, acc.name)
      wdata.foreach(analyzeExpr(memInfo))
    case mw @ MemMaskedWrite(_, mem: Reference, acc: Reference, _, _) =>
      memInfo.associateWrite(mem.name, acc.name)
      mw.foreach(analyzeExpr(memInfo))
    case s =>
      s.foreach(analyzeExpr(memInfo))
      s.foreach(analyzeStmt(memInfo))
  }

  private def replaceExpr(memInfo: MemAnalysis)(expr: Expression): Expression = expr match {
    case ApplyMemAccess(mem: Reference, acc: Reference, _, _) if memInfo.isMem(mem) =>
      val rdataName = if (memInfo.getPort(mem.name, acc.name).isReadwrite) "rdata" else "data"
      memPortField(memInfo.getMem(mem), memInfo.getPort(mem.name, acc.name).name, rdataName)
    case e => e.map(replaceExpr(memInfo))
  }

  private def defaultConns(mem: DefMemory, port: PortMetadata): Seq[Statement] = {
    def conn(sf: String, rhs: Expression) = Connect(port.info, memPortField(mem, port.name, sf), rhs)
    def inv(sf: String) = IsInvalid(port.info, memPortField(mem, port.name, sf))
    val defaultEnVal = if (port.isWriteOnly) Utils.zero else port.en
    val base = Seq(conn("clk", port.clock), conn("en", defaultEnVal), conn("addr", port.addr))
    val wsignals = if (port.isReadwrite) Seq(inv("wmask"), inv("wdata"), conn("wmode", Utils.zero)) else Seq(inv("mask"), conn("data", Utils.zero))
    if (port.isReadOnly) base else base ++ wsignals
  }

  private def writeConns(info: Info, mem: DefMemory, port: PortMetadata, data: Expression, mask: Option[Expression] = None): Seq[Statement] = {
    def conn(sf: String, rhs: Expression) = Connect(port.info ++ info, memPortField(mem, port.name, sf), rhs)
    val (wen, wdata, wmask) = if (port.isReadwrite) ("wmode", "wdata", "wmask") else ("en", "data", "mask")
    val maskConns = mask match {
      case Some(m) => conn(wmask, m)
      case None => EmptyStmt // TODO: gen all-one mask
    }
    Seq(conn(wen, port.en), conn(wdata, data), maskConns)
  }

  private def replaceStmt(memInfo: MemAnalysis)(stmt: Statement): Statement = stmt match {
    case mem: DefMemory =>
      val inferredPorts = memInfo.getInferredPorts(mem.name)
      val inferredR = inferredPorts.filter(_.isReadOnly)
      val inferredW = inferredPorts.filter(_.isWriteOnly)
      val inferredRW = inferredPorts.filter(_.isReadwrite)
      val newMem = mem.copy(
        readers = mem.readers ++ inferredR.map(_.name),
        writers = mem.writers ++ inferredW.map(_.name),
        readwriters = mem.readwriters ++ inferredRW.map(_.name)
      )
      Block(newMem +: inferredPorts.flatMap(p => defaultConns(mem, p)).toSeq)
    case ma: DefMemAccess => EmptyStmt
    case Connect(info, SubAccess(mem: Reference, acc: Reference, _, _), wdata) if memInfo.isMem(mem) =>
      val wdataFinal = wdata.map(replaceExpr(memInfo))
      Block(writeConns(info, memInfo.getMem(mem), memInfo.getPort(mem.name, acc.name), wdataFinal))
    case MemMaskedWrite(info, mem: Reference, acc: Reference, wdata, mask) =>
      val wdataFinal = wdata.map(replaceExpr(memInfo))
      val maskFinal = mask.map(replaceExpr(memInfo))
      Block(writeConns(info, memInfo.getMem(mem), memInfo.getPort(mem.name, acc.name), wdataFinal, Some(maskFinal)))
    case s => s.map(replaceStmt(memInfo)).map(replaceExpr(memInfo))
  }

  private def lowerModule(m: DefModule): DefModule = {
    val memInfo = new MemAnalysis
    m.foreach(analyzeStmt(memInfo))
    if (memInfo.containsBehavioral) m.map(replaceStmt(memInfo)) else m
  }

  override def execute(state: CircuitState): CircuitState = {
    state.copy(circuit = state.circuit.map(lowerModule))
  }
}
