package firrtl.passes
package clocklist

import firrtl._
import firrtl.ir._
import Annotations._
import Utils.error
import java.io.{File, CharArrayWriter, PrintWriter, Writer}
import wiring.WiringUtils.{getChildrenMap, countInstances, ChildrenMap, getLineage}
import wiring.Lineage
import ClockListUtils._
import Utils._
import memlib.AnalysisUtils._
import memlib._
import Mappers._


class ClockListTransform(transID: TransID) extends Transform {
  def passSeq(top: String, writer: Writer): Seq[Pass] =
    Seq(new ClockList(top, writer))
  def execute(c: Circuit, map: AnnotationMap) = map get transID match {
    case Some(p) => (p.toSeq.collect { case (ModuleName(m, CircuitName(c.main)), cla) =>
      cla match {
        case x: ClockListAnnotation => (m, x.outputConfig)
        case _ => error(s"Found an unexpected annotation: $cla")
      }
    }) match {
      case Seq((top, out)) => 
        val outputFile = new PrintWriter(out)
        val newC = (new ClockList(top, outputFile)).run(c)
        outputFile.close()
        TransformResult(newC)
      case _ => error(s"Found too many (or too few) ClockListAnnotations!")
    }
    case None => TransformResult(c)
  }
}

case class ClockListAnnotation(t: String, tID: TransID)
    extends Annotation with Loose with Unstable {

  val usage = """
[Optional] ClockList
  List which signal drives each clock of every descendent of specified module

Usage: 
  --list-clocks -c:<circuit>:-m:<module>:-o:<filename>
  *** Note: sub-arguments to --list-clocks should be delimited by : and not white space!
"""    

  //Parse pass options
  val passOptions = PassConfigUtil.getPassOptions(t, usage)
  val outputConfig = passOptions.getOrElse(
    OutputConfigFileName, 
    error("No output config file provided for ClockList!" + usage)
  )
  val passCircuit = passOptions.getOrElse(
    PassCircuitName, 
    error("No circuit name specified for ClockList!" + usage)
  )
  val passModule = passOptions.getOrElse(
    PassModuleName, 
    error("No module name specified for ClockList!" + usage)
  )
  passOptions.get(InputConfigFileName) match {
    case Some(x) => error("Unneeded input config file name!" + usage)
    case None =>
  }
  val target = ModuleName(passModule, CircuitName(passCircuit))
  def duplicate(n: Named) = n match {
    case ModuleName(m, CircuitName(c)) => this copy (t = t.replace(s"-c:$passCircuit", s"-c:$c").replace(s"-m:$passModule", s"-m:$m"))
    case _ => error("Cannot move ClockListAnnotation to a non-module")
  }
}

object ClockListUtils {
  /** Returns a list of clock outputs from instances of external modules
   */
  def getSourceList(moduleMap: Map[String, DefModule])(lin: Lineage): Seq[String] = {
    val s = lin.foldLeft(Seq[String]()){case (sL, (i, l)) =>
      val sLx = getSourceList(moduleMap)(l)
      val sLxx = sLx map (i + "$" + _)
      sL ++ sLxx
    }
    val sourceList = moduleMap(lin.name) match {
      case ExtModule(i, n, ports, dn, p) =>
        val portExps = ports.flatMap{p => create_exps(WRef(p.name, p.tpe, PortKind, to_gender(p.direction)))}
        portExps.filter(e => (e.tpe == ClockType) && (gender(e) == FEMALE)).map(e => e.serialize)
      case _ => Nil
    }
    val sx = sourceList ++ s
    sx
  }
  /** Returns a map from instance name to its clock origin.
   *  Child instances are not included if they share the same clock as their parent
   */
  def getOrigins(connects: Connects, me: String, moduleMap: Map[String, DefModule])(lin: Lineage): Map[String, String] = {
    val sep = if(me == "") "" else "$"
    // Get origins from all children
    val childrenOrigins = lin.foldLeft(Map[String, String]()){case (o, (i, l)) =>
      o ++ getOrigins(connects, me + sep + i, moduleMap)(l)
    }
    // If I have a clock, get it
    val clockOpt = moduleMap(lin.name) match {
      case Module(i, n, ports, b) => ports.collectFirst { case p if p.name == "clock" => me + sep + "clock" }
      case ExtModule(i, n, ports, dn, p) => None
    }
    // Return new origins with children removed, if they match my clock
    clockOpt match {
      case Some(clock) =>
        val myOrigin = getOrigin(connects, clock).serialize
        childrenOrigins.foldLeft(Map(me -> myOrigin)) { case (o, (childInstance, childOrigin)) =>
          if(childOrigin == myOrigin) o else o + (childInstance -> childOrigin)
        }
      case None => childrenOrigins
    }
  }
}

/** Starting with a top module, determine the clock origins of each child instance.
 *  Write the result to writer.
 */
class ClockList(top: String, writer: Writer) extends Pass {
  def name = this.getClass.getSimpleName
  def run(c: Circuit): Circuit = {
    // Build useful datastructures
    val childrenMap = getChildrenMap(c)
    val moduleMap = c.modules.foldLeft(Map[String, DefModule]())((map, m) => map + (m.name -> m))
    val lineages = getLineage(childrenMap, top)
    val outputBuffer = new CharArrayWriter

    // === Checks ===
    // TODO(izraelevitz): Check all registers/memories use "clock" clock port
    // ==============
    
    // Clock sources must be blackbox outputs and top's clock
    val partialSourceList = getSourceList(moduleMap)(lineages)
    val sourceList = partialSourceList ++ moduleMap(top).ports.collect{ case Port(i, n, Input, ClockType) => n }
    writer.append(s"Sourcelist: $sourceList \n")

    // Remove everything from the circuit, unless it has a clock type
    // This simplifies the circuit drastically so InlineInstances doesn't take forever.
    val onlyClockCircuit = RemoveAllButClocks.run(c)
    
    // Inline the clock-only circuit up to the specified top module
    val modulesToInline = (c.modules.collect { case Module(_, n, _, _) if n != top => ModuleName(n, CircuitName(c.main)) }).toSet
    val inlineTransform = new InlineInstances(TransID(-10)) //TID doesn't matter
    val inlinedCircuit = inlineTransform.run(onlyClockCircuit, modulesToInline, Set()).circuit
    val topModule = inlinedCircuit.modules.collectFirst { case m if m.name == top => m }.get

    // Build a hashmap of connections to use for getOrigins
    val connects = getConnects(topModule)

    // Return a map from instance name to clock origin
    val origins = getOrigins(connects, "", moduleMap)(lineages)

    // If the clock origin is contained in the source list, label good (otherwise bad)
    origins.foreach { case (instance, origin) =>
      val sep = if(instance == "") "" else "."
      if(!sourceList.contains(origin.replace('.','$'))){
        outputBuffer.append(s"Bad Origin of $instance${sep}clock is $origin\n")
      } else {
        outputBuffer.append(s"Good Origin of $instance${sep}clock is $origin\n")
      }
    }

    // Write to output file
    writer.write(outputBuffer.toString)

    // Return unchanged circuit
    c
  }
}


/** Remove all statements and ports (except instances/whens/blocks) whose
 *  expressions do not relate to ground types.
 */
object RemoveAllButClocks extends Pass {
  def name = this.getClass.getSimpleName
  def onStmt(s: Statement): Statement = (s map onStmt) match {
    case DefWire(i, n, ClockType) => s
    case DefNode(i, n, value) if value.tpe == ClockType => s
    case Connect(i, l, r) if l.tpe == ClockType => s
    case sx: WDefInstance => sx
    case sx: DefInstance => sx
    case sx: Block => sx
    case sx: Conditionally => sx
    case _ => EmptyStmt
  }
  def onModule(m: DefModule): DefModule = m match {
    case Module(i, n, ps, b) => Module(i, n, ps.filter(_.tpe == ClockType), squashEmpty(onStmt(b)))
    case ExtModule(i, n, ps, dn, p) => ExtModule(i, n, ps.filter(_.tpe == ClockType), dn, p)
  }
  def run(c: Circuit): Circuit = c.copy(modules = c.modules map onModule)
}
