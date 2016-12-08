// See LICENSE for license details.

package firrtl.passes
package memlib

import firrtl._
import firrtl.ir._
import firrtl.Utils._
import firrtl.Mappers._
import firrtl.PrimOps._
import MemPortUtils._

/** This pass generates delay reigsters for memories for verilog */
object VerilogMemDelays extends Pass {
  def name = "Verilog Memory Delays"
  val ug = UNKNOWNGENDER
  type Netlist = collection.mutable.HashMap[String, Expression]
  implicit def expToString(e: Expression): String = e.serialize
  private def NOT(e: Expression) = DoPrim(Not, Seq(e), Nil, BoolType)
  private def AND(e1: Expression, e2: Expression) = DoPrim(And, Seq(e1, e2), Nil, BoolType)

  def buildNetlist(netlist: Netlist)(s: Statement): Statement = {
    s match {
      case Connect(_, loc, expr) => kind(loc) match {
        case MemKind => netlist(loc) = expr
        case _ =>
      }
      case _ =>
    }
    s map buildNetlist(netlist)
  }

  def memDelayStmt(
      netlist: Netlist,
      mems: Mems,
      namespace: Namespace,
      repl: Netlist)
      (s: Statement): Statement = s map memDelayStmt(netlist, mems, namespace, repl) match {
    case mem: DefMemory =>
      val ports = (mem.readers ++ mem.writers).toSet
      def newPortName(rw: String, p: String) = (for {
        idx <- Stream from 0
        newName = s"${rw}_${p}_$idx"
        if !ports(newName)
      } yield newName).head
      val ports = (sx.readers ++ sx.writers).toSet
      val rwMap = (sx.readwriters map (rw =>
        rw -> (newPortName(rw, "r"), newPortName(rw, "w")))).toMap
      mems(mem.name) = (mem, rwMap)
      // 1. readwrite ports are split into read & write ports
      // 2. memories are transformed into combinational
      //    because latency pipes are added for longer latencies
      mem copy (
        readers = mem.readers ++ (mem.readwriters map (rw => rwMap(rw)._1)),
        writers = mem.writers ++ (mem.readwriters map (rw => rwMap(rw)._2)),
        readwriters = Nil, readLatency = 0, writeLatency = 1)
    case Connect(i, WSubField(WSubField(WRef(memName, _, MemKind, _), port, _, _), field, _, _), exp) =>
      val (mem, rwMap) = mems(memName)
      def pipe(e: Expression, // Expression to be piped
               n: Int, // pipe depth
               clk: Expression, // clock expression
               cond: Expression // condition for pipes
              ): (Expression, Seq[Statement]) = {
        // returns
        // 1) reference to the last pipe register
        // 2) pipe registers and connects
        val node = DefNode(NoInfo, namespace.newTemp, netlist(e))
        val wref = WRef(node.name, e.tpe, NodeKind, MALE)
        println(s"NODE NAME: ${node.name}")
        ((0 until n) foldLeft (wref, Seq[Statement](node))){case ((ex, stmts), i) =>
          val name = namespace newName s"${LowerTypes.loweredName(e)}_pipe_$i"
          val exx = WRef(name, e.tpe, RegKind, ug)
          (exx, stmts ++ Seq(DefRegister(NoInfo, name, e.tpe, clk, zero, exx)) ++
            (if (i < n - 1 && WrappedExpression.weq(cond, one)) Seq(Connect(NoInfo, exx, ex)) else {
              val condn = namespace newName s"${LowerTypes.loweredName(e)}_en"
              val condx = WRef(condn, BoolType, NodeKind, FEMALE)
              Seq(DefNode(NoInfo, condn, cond),
                  Connect(NoInfo, exx, Mux(condx, ex, exx, e.tpe)))
            })
          )
        }
      }
      val clk = netlist(memPortField(mem, port, "clk"))
      val wFieldMatch = "(addr|en|mask|data)".r
      val rwWriterFieldMatch = "(wmask|wdata|wmode)".r
      val rwMap = (mem.readwriters map (rw =>
        rw -> (newPortName(rw, "r"), newPortName(rw, "w")))).toMap
      (field, mem.readers.contains(port), mem.writers.contains(port), mem.readwriters.contains(port))  match {
        case ("clk", _, _, _) => s
        case ("en", true, false, false) => 
          val (en, ss1) = pipe(memPortField(mem, port, "en"), mem.readLatency - 1, clk, one)
          Block(Seq(ss1) +: Connect(NoInfo, memPortField(mem, port, "en"), en))
        case ("addr", true, false, false) => 
          val (addr, ss1) = pipe(memPortField(mem, port, "addr"), mem.readLatency, clk, one)
          Block(Seq(ss1) +: Connect(NoInfo, memPortField(mem, port, "addr"), addr))
        case (wFieldMatch(wField), false, true, false) => 
          val (value, ss1) = pipe(memPortField(mem, port, wField), mem.writeLatency - 1, clk, one)
          Block(Seq(ss1) +: Connect(NoInfo, memPortField(mem, port, wField), value))
        case (rwWriterFieldMatch(wField), false, false, true) => 
          val (reader, writer) = rwMap(port)
          val (value, ss1) = pipe(memPortField(mem, port, wField), mem.writeLatency - 1, clk, one)
          Block(Seq(ss1) +: Connect(NoInfo, memPortField(mem, writer, wField), value))
        case ("addr", false, false, true) =>
          val (reader, writer) = rwMap(port)
          repl(memPortField(mem, port, "rdata")) = memPortField(mem, reader, "data") //replace all references to rw port's rdata
          val wmode = netlist(memPortField(mem, port, "wmode"))
          val en = netlist(memPortField(mem, port, "en"))
          val (raddr, ss5) = pipe(memPortField(mem, readwriter, "addr"), mem.readLatency, clk, AND(en, NOT(wmode)))
          val (waddr, ss6) = pipe(memPortField(mem, readwriter, "addr"), mem.writeLatency - 1, clk, one)
          Block(Seq(ss5, Connect(NoInfo, memPortField(mem, reader, "addr"), raddr), ss6, Connect(NoInfo, memPortField(mem, writer, "addr"), waddr)))
        case ("en", false, false, true) =>
          val (reader, writer) = rwMap(port)
          val (en, ss1) = pipe(memPortField(mem, port, "en"), mem.readLatency - 1, clk, one)
          Block(Seq(ss1) +: Connect(NoInfo, memPortField(mem, reader, "en"), en) +: Connect(NoInfo, memPortField(mem, writer, "en"), AND(en, wmode)))
      }
    case sx => sx
  }

  def replaceExp(repl: Netlist)(e: Expression): Expression = e match {
    case ex: WSubField => repl get ex match {
      case Some(exx) => exx
      case None => ex
    }
    case ex => ex map replaceExp(repl)
  }

  def replaceStmt(repl: Netlist)(s: Statement): Statement =
    s map replaceStmt(repl) map replaceExp(repl)

  def memDelayMod(m: DefModule): DefModule = {
    val netlist = new Netlist
    val namespace = Namespace(m)
    val repl = new Netlist
    type Mems = collection.mutable.HashMap[String, (Statement, Map[String, (String, String)])]
    val mems = new Mems
    (m map buildNetlist(netlist)
       map memDelayStmt(netlist, mems, namespace, repl)
       map replaceStmt(repl))
  }

  def run(c: Circuit): Circuit =
    c copy (modules = c.modules map memDelayMod)
}
