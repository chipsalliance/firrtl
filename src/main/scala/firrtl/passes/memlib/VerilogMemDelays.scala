// See LICENSE for license details.

package firrtl.passes
package memlib

import firrtl._
import firrtl.ir._
import firrtl.Utils._
import firrtl.Mappers._
import firrtl.traversals.Foreachers._

import MemPortUtils._
import WrappedExpression._

import collection.mutable

@deprecated("Internal helper objects from VerilogMemDelays will be removed in 1.3", "1.2.1")
object DelayPipe {
  private case class PipeState(ref: Expression, decl: Statement = EmptyStmt, connect: Statement = EmptyStmt, idx: Int = 0)
  def apply(ns: Namespace)(e: Expression, delay: Int, clock: Expression): (Expression, Seq[Statement]) = {
    def addStage(prev: PipeState): PipeState = {
      val idx = prev.idx + 1
      val name = ns.newName(s"${e.serialize}_r${idx}".replace('.', '_'))
      val regRef = WRef(name, e.tpe, RegKind)
      val regDecl = DefRegister(NoInfo, name, e.tpe, clock, zero, regRef)
      PipeState(regRef, regDecl, Connect(NoInfo, regRef, prev.ref), idx)
    }
    val pipeline = Seq.iterate(PipeState(e), delay+1)(addStage)
    (pipeline.last.ref, pipeline.map(_.decl) ++ pipeline.map(_.connect))
  }
}

object MemDelayAndReadwriteTransformer {
  // Representation of a group of signals and associated valid signals
  case class WithValid(valid: Expression, payload: Seq[Expression])

  // Grouped statements that are split into declarations and connects to ease ordering
  case class SplitStatements(decls: Seq[Statement], conns: Seq[Connect])

  // Utilities for generating hardware
  def NOT(e: Expression) = DoPrim(PrimOps.Not, Seq(e), Nil, BoolType)
  def AND(e1: Expression, e2: Expression) = DoPrim(PrimOps.And, Seq(e1, e2), Nil, BoolType)
  def connect(l: Expression, r: Expression): Connect = Connect(NoInfo, l, r)
  def condConnect(c: Expression)(l: Expression, r: Expression): Connect = connect(l, Mux(c, r, l, l.tpe))

  // Utilities for working with WithValid groups
  def connect(l: WithValid, r: WithValid): Seq[Connect] = {
    val paired = (l.valid +: l.payload) zip (r.valid +: r.payload)
    paired.map { case (le, re) => connect(le, re) }
  }

  def condConnect(l: WithValid, r: WithValid): Seq[Connect] = {
    connect(l.valid, r.valid) +: (l.payload zip r.payload).map { case (le, re) => condConnect(r.valid)(le, re) }
  }

  // Internal representation of a pipeline stage with an associated valid signal
  private case class PipeStageWithValid(idx: Int, ref: WithValid, stmts: SplitStatements = SplitStatements(Nil, Nil))

  // Utilities for creating legal names for registers
  private val metaChars = raw"[\[\]\.]".r
  private def flatName(e: Expression) = metaChars.replaceAllIn(e.serialize, "_")

  // Pipeline a group of signals with an associated valid signal. Gate registers when possible.
  def pipelineWithValid(ns: Namespace)(
    clock: Expression,
    depth: Int,
    src: WithValid,
    nameTemplate: Option[WithValid] = None): (WithValid, Seq[Statement], Seq[Connect]) = {

    def asReg(e: Expression) = DefRegister(NoInfo, e.serialize, e.tpe, clock, zero, e)
    val template = nameTemplate.getOrElse(src)

    val stages = Seq.iterate(PipeStageWithValid(0, src), depth + 1) { case prev =>
      def pipeRegRef(e: Expression) = WRef(ns.newName(s"${flatName(e)}_pipe_${prev.idx}"), e.tpe, RegKind)
      val ref = WithValid(pipeRegRef(template.valid), template.payload.map(pipeRegRef))
      val regs = (ref.valid +: ref.payload).map(asReg)
      PipeStageWithValid(prev.idx + 1, ref, SplitStatements(regs, condConnect(ref, prev.ref)))
    }
    (stages.last.ref, stages.flatMap(_.stmts.decls), stages.flatMap(_.stmts.conns))
  }
}

/**
  * This class performs the primary work of the transform: splitting readwrite ports into separate
  * read and write ports while simultaneously compiling memory latencies to combinational-read
  * memories with delay pipelines. It is represented as a class that takes a module as a constructor
  * argument, as it encapsulates the mutable state required to analyze and transform one module.
  * 
  * @note The final transformed module is found in the (sole public) field [[transformed]]
  */
class MemDelayAndReadwriteTransformer(m: DefModule) {
  import MemDelayAndReadwriteTransformer._

  private val ns = Namespace(m)
  private val netlist = new collection.mutable.HashMap[WrappedExpression, Expression]
  private val exprReplacements = new collection.mutable.HashMap[WrappedExpression, Expression]
  private val newConns = new mutable.ArrayBuffer[Connect]

  private def findMemConns(s: Statement): Unit = s match {
    case Connect(_, loc, expr) if (kind(loc) == MemKind) => netlist(we(loc)) = expr
    case _ => s.foreach(findMemConns)
  }

  private def swapMemRefs(e: Expression): Expression = e map swapMemRefs match {
    case sf: WSubField => exprReplacements.getOrElse(we(sf), sf)
    case ex => ex
  }

  private def transform(s: Statement): Statement = s.map(transform) match {
    case mem: DefMemory =>
      // Per-memory bookkeeping
      val portNS = Namespace(mem.readers ++ mem.writers)
      val rMap = mem.readwriters.map(rw => (rw -> portNS.newName(s"${rw}_r"))).toMap
      val wMap = mem.readwriters.map(rw => (rw -> portNS.newName(s"${rw}_w"))).toMap
      val newReaders = mem.readers ++ mem.readwriters.map(rMap(_))
      val newWriters = mem.writers ++ mem.readwriters.map(wMap(_))
      val newMem = DefMemory(mem.info, mem.name, mem.dataType, mem.depth, 1, 0, newReaders, newWriters, Nil)
      val rCmdDelay = if (mem.readUnderWrite == ReadUnderWrite.Old) 0 else mem.readLatency
      val rRespDelay = if (mem.readUnderWrite == ReadUnderWrite.Old) mem.readLatency else 0
      val wCmdDelay = mem.writeLatency - 1

      val readStmts = (mem.readers ++ mem.readwriters).map { case r =>
        def oldDriver(f: String) = swapMemRefs(netlist(we(memPortField(mem, r, f))))
        def newField(f: String) = memPortField(newMem, rMap.getOrElse(r, r), f)
        val clk = oldDriver("clk")

        // Pack sources of read command inputs into WithValid object -> different for readwriter
        val enSrc = if (rMap.contains(r)) AND(oldDriver("en"), NOT(oldDriver("wmode"))) else oldDriver("en")
        val cmdSrc = WithValid(enSrc, Seq(oldDriver("addr")))
        val cmdSink = WithValid(newField("en"), Seq(newField("addr")))
        val (cmdPiped, cmdDecls, cmdConns) = pipelineWithValid(ns)(clk, rCmdDelay, cmdSrc, nameTemplate = Some(cmdSink))
        val cmdPortConns = connect(cmdSink, cmdPiped) :+ connect(newField("clk"), clk)

        // Pipeline read response using *last* command pipe stage enable as the valid signal
        val resp = WithValid(cmdPiped.valid, Seq(newField("data")))
        val respPipeNameTemplate = Some(resp.copy(valid = cmdSink.valid)) // base pipeline register names off field names
        val (respPiped, respDecls, respConns) = pipelineWithValid(ns)(clk, rRespDelay, resp, nameTemplate = respPipeNameTemplate)

        // Make sure references to the read data get appropriately substituted
        val oldRDataName = if (rMap.contains(r)) "rdata" else "data"
        exprReplacements(we(memPortField(mem, r, oldRDataName))) = respPiped.payload.head

        // Return all statements; they're separated so connects can go after all declarations
        SplitStatements(cmdDecls ++ respDecls, cmdConns ++ cmdPortConns ++ respConns)
      }

      val writeStmts = (mem.writers ++ mem.readwriters).map { case w =>
        def oldDriver(f: String) = swapMemRefs(netlist(we(memPortField(mem, w, f))))
        def newField(f: String) = memPortField(newMem, wMap.getOrElse(w, w), f)
        val clk = oldDriver("clk")

        // Pack sources of write command inputs into WithValid object -> different for readwriter
        val cmdSrc = if (wMap.contains(w)) {
          val en = AND(oldDriver("en"), oldDriver("wmode"))
          WithValid(en, Seq(oldDriver("addr"), oldDriver("wmask"), oldDriver("wdata")))
        } else {
          WithValid(oldDriver("en"), Seq(oldDriver("addr"), oldDriver("mask"), oldDriver("data")))
        }

        // Pipeline write command, connect to memory
        val cmdSink = WithValid(newField("en"), Seq(newField("addr"), newField("mask"), newField("data")))
        val (cmdPiped, cmdDecls, cmdConns) = pipelineWithValid(ns)(clk, wCmdDelay, cmdSrc, nameTemplate = Some(cmdSink))
        val cmdPortConns = connect(cmdSink, cmdPiped) :+ connect(newField("clk"), clk)

        // Return all statements; they're separated so connects can go after all declarations
        SplitStatements(cmdDecls, cmdConns ++ cmdPortConns)
      }

      newConns ++= (readStmts ++ writeStmts).flatMap(_.conns)
      Block(newMem +: (readStmts ++ writeStmts).flatMap(_.decls))
    case sx: Connect if kind(sx.loc) == MemKind => EmptyStmt // Filter old mem connections
    case sx => sx.map(swapMemRefs)
  }

  val transformed = m match {
    case mod: Module =>
      findMemConns(mod.body)
      mod.copy(body = Block(transform(mod.body) +: newConns.toSeq))
    case mod => mod
  }
}

object VerilogMemDelays extends Pass {
  // Imported to support the backported methods
  import MemDelayAndReadwriteTransformer.{AND, NOT}

  // These public members are copied over for backporting binary compatibility
  @deprecated("Internal helper members from VerilogMemDelays will be removed in 1.3", "1.2.1")
  val ug = UnknownFlow

  @deprecated("Internal helper members from VerilogMemDelays will be removed in 1.3", "1.2.1")
  type Netlist = collection.mutable.HashMap[String, Expression]

  @deprecated("Internal helper members from VerilogMemDelays will be removed in 1.3", "1.2.1")
  implicit def expToString(e: Expression): String = e.serialize

  @deprecated("Internal helper members from VerilogMemDelays will be removed in 1.3", "1.2.1")
  def buildNetlist(netlist: Netlist)(s: Statement): Unit = s match {
   case Connect(_, loc, expr) if (kind(loc) == MemKind) => netlist(loc) = expr
   case _ =>
   s.foreach(buildNetlist(netlist))
  }

  @deprecated("Internal helper members from VerilogMemDelays will be removed in 1.3", "1.2.1")
  def replaceExp(repl: Netlist)(e: Expression): Expression = e match {
    case ex: WSubField => repl get ex match {
      case Some(exx) => exx
      case None => ex
    }
    case ex => ex map replaceExp(repl)
  }

  @deprecated("Internal helper members from VerilogMemDelays will be removed in 1.3", "1.2.1")
  def appendStmts(sx: Seq[Statement])(s: Statement): Statement = Block(s +: sx)

  @deprecated("Internal helper members from VerilogMemDelays will be removed in 1.3", "1.2.1")
  def memDelayMod(m: DefModule): DefModule = transform(m)

  @deprecated("Internal helper members from VerilogMemDelays will be removed in 1.3", "1.2.1")
  def memDelayStmt(
      netlist: Netlist,
      namespace: Namespace,
      repl: Netlist,
      stmts: mutable.ArrayBuffer[Statement])
      (s: Statement): Statement = s.map(memDelayStmt(netlist, namespace, repl, stmts)) match {
    case sx: DefMemory =>
      val ports = (sx.readers ++ sx.writers).toSet
      def newPortName(rw: String, p: String) = (for {
        idx <- Stream from 0
        newName = s"${rw}_${p}_$idx"
        if !ports(newName)
      } yield newName).head
      val rwMap = (sx.readwriters map (rw =>
        rw ->( (newPortName(rw, "r"), newPortName(rw, "w")) ))).toMap
      // 1. readwrite ports are split into read & write ports
      // 2. memories are transformed into combinational
      //    because latency pipes are added for longer latencies
      val mem = sx copy (
        readers = sx.readers ++ (sx.readwriters map (rw => rwMap(rw)._1)),
        writers = sx.writers ++ (sx.readwriters map (rw => rwMap(rw)._2)),
        readwriters = Nil, readLatency = 0, writeLatency = 1)
      def prependPipe(e: Expression, // Expression to be piped
               n: Int, // pipe depth
               clk: Expression, // clock expression
               cond: Expression // condition for pipes
              ): (Expression, Seq[Statement]) = {
        // returns
        // 1) reference to the last pipe register
        // 2) pipe registers and connects
        val node = DefNode(NoInfo, namespace.newTemp, netlist(e))
        val wref = WRef(node.name, e.tpe, NodeKind, SourceFlow)
        ((0 until n) foldLeft( (wref, Seq[Statement](node)) )){case ((ex, stmts), i) =>
          val name = namespace newName s"${LowerTypes.loweredName(e)}_pipe_$i"
          val exx = WRef(name, e.tpe, RegKind, ug)
          (exx, stmts ++ Seq(DefRegister(NoInfo, name, e.tpe, clk, zero, exx)) ++
            (if (i < n - 1 && WrappedExpression.weq(cond, one)) Seq(Connect(NoInfo, exx, ex)) else {
              val condn = namespace newName s"${LowerTypes.loweredName(e)}_en"
              val condx = WRef(condn, BoolType, NodeKind, SinkFlow)
              Seq(DefNode(NoInfo, condn, cond),
                  Connect(NoInfo, exx, Mux(condx, ex, exx, e.tpe)))
            })
          )
        }
      }
      def readPortConnects(reader: String,
                           clk: Expression,
                           en: Expression,
                           addr: Expression) = Seq(
        Connect(NoInfo, memPortField(mem, reader, "clk"), clk),
        // connect latency pipes to read ports
        Connect(NoInfo, memPortField(mem, reader, "en"), en),
        Connect(NoInfo, memPortField(mem, reader, "addr"), addr)
      )
      def writePortConnects(writer: String,
                            clk: Expression,
                            en: Expression,
                            mask: Expression,
                            addr: Expression,
                            data: Expression) = Seq(
        Connect(NoInfo, memPortField(mem, writer, "clk"), clk),
        // connect latency pipes to write ports
        Connect(NoInfo, memPortField(mem, writer, "en"), en),
        Connect(NoInfo, memPortField(mem, writer, "mask"), mask),
        Connect(NoInfo, memPortField(mem, writer, "addr"), addr),
        Connect(NoInfo, memPortField(mem, writer, "data"), data)
      )

      stmts ++= ((sx.readers flatMap {reader =>
        val clk = netlist(memPortField(sx, reader, "clk"))
        if (sx.readUnderWrite == ReadUnderWrite.Old) {
          // For a read-first ("old") mem, read data gets delayed, so don't delay read address/en
          val rdata = memPortField(sx, reader, "data")
          val enDriver = netlist(memPortField(sx, reader, "en"))
          val addrDriver = netlist(memPortField(sx, reader, "addr"))
          readPortConnects(reader, clk, enDriver, addrDriver)
        } else {
          // For a write-first ("new") or undefined mem, delay read control inputs
          val (en, ss1) = prependPipe(memPortField(sx, reader, "en"), sx.readLatency - 1, clk, one)
          val (addr, ss2) = prependPipe(memPortField(sx, reader, "addr"), sx.readLatency, clk, en)
          ss1 ++ ss2 ++ readPortConnects(reader, clk, en, addr)
        }
      }) ++ (sx.writers flatMap {writer =>
        // generate latency pipes for write ports (enable, mask, addr, data)
        val clk = netlist(memPortField(sx, writer, "clk"))
        val (en, ss1) = prependPipe(memPortField(sx, writer, "en"), sx.writeLatency - 1, clk, one)
        val (mask, ss2) = prependPipe(memPortField(sx, writer, "mask"), sx.writeLatency - 1, clk, one)
        val (addr, ss3) = prependPipe(memPortField(sx, writer, "addr"), sx.writeLatency - 1, clk, one)
        val (data, ss4) = prependPipe(memPortField(sx, writer, "data"), sx.writeLatency - 1, clk, one)
        ss1 ++ ss2 ++ ss3 ++ ss4 ++ writePortConnects(writer, clk, en, mask, addr, data)
      }) ++ (sx.readwriters flatMap {readwriter =>
        val (reader, writer) = rwMap(readwriter)
        val clk = netlist(memPortField(sx, readwriter, "clk"))
        // generate latency pipes for readwrite ports (enable, addr, wmode, wmask, wdata)
        val (en, ss1) = prependPipe(memPortField(sx, readwriter, "en"), sx.readLatency - 1, clk, one)
        val (wmode, ss2) = prependPipe(memPortField(sx, readwriter, "wmode"), sx.writeLatency - 1, clk, one)
        val (wmask, ss3) = prependPipe(memPortField(sx, readwriter, "wmask"), sx.writeLatency - 1, clk, one)
        val (wdata, ss4) = prependPipe(memPortField(sx, readwriter, "wdata"), sx.writeLatency - 1, clk, one)
        val (waddr, ss5) = prependPipe(memPortField(sx, readwriter, "addr"), sx.writeLatency - 1, clk, one)
        val stmts = ss1 ++ ss2 ++ ss3 ++ ss4 ++ ss5 ++ writePortConnects(writer, clk, AND(en, wmode), wmask, waddr, wdata)
        if (sx.readUnderWrite == ReadUnderWrite.Old) {
          // For a read-first ("old") mem, read data gets delayed, so don't delay read address/en
          val enDriver = netlist(memPortField(sx, readwriter, "en"))
          val addrDriver = netlist(memPortField(sx, readwriter, "addr"))
          val wmodeDriver = netlist(memPortField(sx, readwriter, "wmode"))
          stmts ++ readPortConnects(reader, clk, AND(enDriver, NOT(wmodeDriver)), addrDriver)
        } else {
          // For a write-first ("new") or undefined mem, delay read control inputs
          val (raddr, raddrPipeStmts) = prependPipe(memPortField(sx, readwriter, "addr"), sx.readLatency, clk, AND(en, NOT(wmode)))
          repl(memPortField(sx, readwriter, "rdata")) = memPortField(mem, reader, "data")
          stmts ++ raddrPipeStmts ++ readPortConnects(reader, clk, en, raddr)
        }
      }))

      def pipeReadData(p: String): Seq[Statement] = {
        val newName = rwMap.get(p).map(_._1).getOrElse(p) // Name of final read port, whether renamed (rw port) or not
        val rdataNew = memPortField(mem, newName, "data")
        val rdataOld = rwMap.get(p).map(rw => memPortField(sx, p, "rdata")).getOrElse(rdataNew)
        val clk = netlist(rdataOld.copy(name = "clk"))
        val (rdataPipe, rdataPipeStmts) = DelayPipe(namespace)(rdataNew, sx.readLatency, clk) // TODO: use enable
        repl(rdataOld) = rdataPipe
        rdataPipeStmts
      }

      // We actually pipe the read data here; this groups it with the mem declaration to keep declarations early
      if (sx.readUnderWrite == ReadUnderWrite.Old) {
        Block(mem +: (sx.readers ++ sx.readwriters).flatMap(pipeReadData(_)))
      } else {
        mem
      }
    case sx: Connect if kind(sx.loc) == MemKind => EmptyStmt
    case sx => sx map replaceExp(repl)
  }

  def transform(m: DefModule): DefModule = (new MemDelayAndReadwriteTransformer(m)).transformed
  def run(c: Circuit): Circuit = c.copy(modules = c.modules.map(transform))
}
