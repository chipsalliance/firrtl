// See LICENSE for license details.

package firrtl
package transforms

// Firrtl IR classes
import ir._
import PrimOps._

// Map functions
import firrtl.Mappers._
// Scala's mutable collections
import scala.collection.mutable
import firrtl.passes.CheckCombLoops.{LogicNode, getExprDeps}

class AggDeadCodeElimination extends Transform {
  def inputForm = LowForm
  def outputForm = LowForm

  case class LogicInstance(name: String, parent: Option[Instance])
  def execute(state: CircuitState): CircuitState = {
    val circuit = state.circuit
    val moduleMap = c.modules.map({m => (m.name,m) }).toMap
    val iGraph = new InstanceGraph(c)
    val topoModules = iGraph.topoSortedModules map { moduleMap(_) }
    val moduleGraphs = new mutable.HashMap[String,DiGraph[LogicNode]]
    val simplifiedModuleGraphs = new mutable.HashMap[String,DiGraph[LogicNode]]
    for (m <- topoSortedModules) {
      val internalDeps = new MutableDiGraph[LogicNode]
      m.ports foreach { p => internalDeps.addVertex(LogicNode(p.name)) }
      m map getStmtDeps(simplifiedModuleGraphs, internalDeps)
      moduleGraphs(m.name) = DiGraph(internalDeps)
      simplifiedModuleGraphs(m.name) = moduleGraphs(m.name).simplify((m.ports map { p => LogicNode(p.name) }).toSet)
    }

    /*
    === Mark ===
    for each module, create connected graph from outputs to inputs
    starting from top instance, record which ports of each child instance you use
    duplicate modules which have different instance uses, and rename them
    --- valid circuit, no two modules will dce differently ---
    === Sweep ===
    for each module, we know which output ports are used. deleted unused output ports
    for each module, follow back from all output ports and record used nodes
    Find all unused nodes by diffing graph with used nodes
    Delete unused nodes
    */

    
    circuit map walkModule(ledger)
    state
  }

  private def getStmtDeps(deps: MutableDiGraph[LogicNode])(s: Statement): Statement = {
    s match {
      case Connect(_,loc,expr) =>
        val lhs = toLogicNode(loc)
        if (deps.contains(lhs)) {
          getExprDeps(deps.getEdges(lhs))(expr)
        }
      case w: DefWire => deps.addVertex(LogicNode(w.name))
      case n: DefNode =>
        val lhs = LogicNode(n.name)
        deps.addVertex(lhs)
        getExprDeps(deps.getEdges(lhs))(n.value)
      case m: DefMemory if (m.readLatency == 0) =>
        for (rp <- m.readers) {
          val dataNode = deps.addVertex(LogicNode("data",Some(m.name),Some(rp)))
          deps.addEdge(dataNode, deps.addVertex(LogicNode("addr",Some(m.name),Some(rp))))
          deps.addEdge(dataNode, deps.addVertex(LogicNode("en",Some(m.name),Some(rp))))
        }
      case i: WDefInstance =>
        val iGraph = simplifiedModules(i.module).transformNodes(n => n.copy(inst = Some(i.name)))
        for (v <- iGraph.getVertices) {
          deps.addVertex(v)
          iGraph.getEdges(v).foreach { deps.addEdge(v,_) }
        }
      case _ =>
        s map getStmtDeps(simplifiedModules,deps)
    }
    s
  }

  def walkModule(ledger: DCELedger)(m: DefModule): DefModule = {
    ledger.setModuleName(m.name)
    val connects = getConnects(m)
    def toString 
    
    val queue = mutable.ArrayBuffer
    m.ports foreach {

    }
    m map walkStatement(connects)
  }

  def walkStatement(ledger: DCELedger)(s: Statement): Statement = {
    // Map the functions walkStatement(ledger) and walkExpression(ledger)
    s map walkStatement(ledger) map walkExpression(ledger) 
  }

  // Deeply visits every [[Expression]] in e.
  def walkExpression(ledger: DCELedger)(e: Expression): Expression = {
    e map walkExpression(ledger) match {
      case mux: Mux =>
        ledger.foundOp(mux)
        mux
      case primop: DoPrim =>
        ledger.foundOp(primop)
        primop
      case other => other
    }
  }
}
package firrtl.passes
package invalidlist

import firrtl._
import firrtl.ir._
import Annotations._
import Utils.error
import java.io.{File, CharArrayWriter, PrintWriter, Writer}
import wiring.WiringUtils.{getChildrenMap, countInstances, ChildrenMap, getLineage}
import wiring.Lineage
import firrtl.passes.clocklist.ClockListUtils._
import Utils._
import memlib.AnalysisUtils._
import memlib._
import Mappers._

/** Starting with a top module, determine the clock origins of each child instance.
 *  Write the result to writer.
 */
//class ClockList(top: String, writer: Writer) extends Pass {
class ClockList extends Pass {
  def run(c: Circuit): Circuit = {
    val top = c.main
    // Build useful datastructures
    val moduleMap = c.modules.foldLeft(Map[String, DefModule]())((map, m) => map + (m.name -> m))
    val outputBuffer = new CharArrayWriter

    // Inline the clock-only circuit up to the specified top module
    val modulesToInline = (c.modules.collect { case Module(_, n, _, _) if n != top => ModuleName(n, CircuitName(c.main)) }).toSet
    val inlineTransform = new InlineInstances
    val inlinedCircuit = inlineTransform.run(c, modulesToInline, Set()).circuit
    val topModule = inlinedCircuit.modules.find(_.name == top).getOrElse(throwInternalError)

    // Build a hashmap of connections to use for getOrigins
    val connects = getConnects(topModule)

    val invalids = collection.mutable.HashMap[String, Seq[String]]()
    def foundInvalid(seq: Seq[String], e: Expression): Unit = {
      val seqx = seq.+:(e.serialize)
      println("Found Invalid!")
      for(i <- 0 until seqx.size) {
        invalids(seqx(i)) = seqx.slice(0, i)
      }
    }

    val workset = collection.mutable.HashSet[Expression]()
    val worklist = collection.mutable.ArrayBuffer[(List[String], Set[String], Expression)]()
    val outputs = topModule.ports.flatMap { p =>
      val es = create_exps(WRef(p.name, p.tpe, PortKind, to_gender(p.direction)))
      es.flatMap {
        case e if gender(e) == FEMALE => Some(e)
        case e if gender(e) == MALE =>
          invalids(e.serialize) = List.empty
          None
      }
    }
    outputs.foreach(e => visit(List[String](), Set[String]())(e))

    def visit(seq: List[String], set: Set[String])(e: Expression): Expression = {
      if(workset.contains(e)) e
      else {
        worklist += ((seq, set, e))
        workset += e
        e
      }
    }
    var index = 0

    while(index < worklist.size) {
      if(index % 10000 == 0) println(s"index: $index, worklist: ${worklist.size}, diff: ${worklist.size - index}, invalids: ${invalids.size}")
      val (seq, set, e) = worklist(index)
      e match {
        case e if invalids.contains(e.serialize) =>
          //println(s"Found ${e.serialize}")
          e
        case WInvalid =>
          foundInvalid(seq, e)
          e
        case v: ValidIf =>
          foundInvalid(seq, e)
          v map visit(seq, set)
        case m: Mux => m map visit(seq, set)
        case d: DoPrim => d map visit(seq, set)
        case _: WRef| _: WSubField| _: WSubIndex if set.contains(e.serialize) => e
        case _: WRef| _: WSubField| _: WSubIndex if kind(e) == MemKind =>
          val (root, dc) = splitRef(e)
          val children = create_exps(root).collect {
            case e if gender(e) == FEMALE => e
          }
          children.map { e =>
            val n = e.serialize
            connects.get(n) match {
              case None => 
                println(s"ERROR!! $n")
                e
              case Some(ex) => visit(seq.+:(n), set + n)(ex)
            }
          }
          e
        case _: WRef| _: WSubField| _: WSubIndex =>
          val n = e.serialize
          connects.get(n) match {
            case None => 
              println(s"ERROR!! $n")
              e
            case Some(ex) => visit(seq.+:(n), set + n)(ex)
          }
        case e: Literal => e
        case x: WSubAccess => throwInternalError
      }
      index += 1
    }


    invalids.foreach{ case (signal, seq) =>
      if(outputs.map(_.serialize).contains(signal)) {
        outputBuffer.append(s"Visible: $signal -> $seq\n")
      } else {
        //outputBuffer.append(s"Invisible: $signal -> $seq\n")
      }
    }
    
    /*
    outputs.foreach{ e =>
      val n = e.serialize
      invalids.get(n) match {
        case None =>
        case Some(seq) => 
          outputBuffer.append(s"Visible: $seq\n")
      }
    }
    */
    //writer.write(outputBuffer.toString)
    println(outputBuffer.toString)
    //println(c.serialize)
    c
  }
}
