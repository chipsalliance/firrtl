// See license file for details

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
  def name = this.getClass.getSimpleName
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
