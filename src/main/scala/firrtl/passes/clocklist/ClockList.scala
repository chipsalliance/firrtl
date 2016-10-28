package firrtl.passes
package clocklist

import firrtl._
import firrtl.ir._
import Annotations._
import Utils.error
import java.io.{File, CharArrayWriter, PrintWriter, Writer}
import wiring.WiringUtils.{getChildrenMap, countInstances, ChildrenMap}
import ClockListUtils._
import Utils._
import memlib.AnalysisUtils._
import memlib._
import Mappers._

/** A lineage tree representing the instance hierarchy in a design
  */
case class Lineage(
    name: String,
    children: Seq[(String, Lineage)] = Seq.empty,
    values: Map[Any, Any] = Map.empty) {

  def map(f: Lineage => Lineage): Lineage =
    this.copy(children = children.map{ case (i, m) => (i, f(m)) })

  def foldLeft[B](z: B)(op: (B, (String, Lineage)) => B): B = 
    this.children.foldLeft(z)(op)

  override def toString: String = serialize("")

  def serialize(tab: String): String = s"""
    |$tab name: $name,
    |$tab map: $values,
    |$tab children: ${children.map(c => tab + "   " + c._2.serialize(tab + "    "))}
    |""".stripMargin
}

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
  /** Returns a module's lineage, containing all children lineages as well */
  def getLineage(childrenMap: ChildrenMap, module: String): Lineage =
    Lineage(module, childrenMap(module) map (c => (c._1, getLineage(childrenMap, c._2))))

  def getModules(set: Set[String], lin: Lineage): Set[String] = {
    val childSet = lin.foldLeft(set) { case (s, (i, l)) => getModules(s, l) }
    childSet + lin.name
  }

  def getLists(moduleMap: Map[String, DefModule])(lin: Lineage): (Seq[String], Seq[String]) = {
    //println(s"On ${lin.name}")
    val (c, s) = lin.foldLeft((Seq[String](), Seq[String]())){case ((cL, sL), (i, l)) =>
      //println(s"At child ${l.name}")
      val (cLx, sLx) = getLists(moduleMap)(l)
      val cLxx = cLx map (i + "$" + _)
      val sLxx = sLx map (i + "$" + _)
      (cL ++ cLxx, sL ++ sLxx)
    }
    val clockList = moduleMap(lin.name) match {
      case Module(i, n, ports, b) => ports.filter(p => p.name == "clock").map(_.name)
      case _ => Nil
    }
    val sourceList = moduleMap(lin.name) match {
      case ExtModule(i, n, ports, dn, p) =>
        val portExps = ports.flatMap{p => create_exps(WRef(p.name, p.tpe, PortKind, to_gender(p.direction)))}
        portExps.filter(e => (e.tpe == ClockType) && (gender(e) == FEMALE)).map(e => e.serialize)
      case _ => Nil
    }
    val (cx, sx) = (c ++ clockList, s ++ sourceList)
    //println(s"From ${lin.name}, returning clocklist $cx and sourcelist $sx")
    (cx, sx)
  }
}

class ClockList(top: String, writer: Writer) extends Pass {
  def name = this.getClass.getSimpleName
  def run(c: Circuit): Circuit = {
    // Assume blackbox output is the only clocks we care about
    // If a module has a single input clock, it is in that clock domain
    // All reg/mem's in a module must be connected to input "clock" or "clk" or whatever
    val childrenMap = getChildrenMap(c)
    val moduleMap = c.modules.foldLeft(Map[String, DefModule]())((map, m) => map + (m.name -> m))
    val outputBuffer = new CharArrayWriter

    // === Checks ===

    // Stuff
    val lineages = getLineage(childrenMap, top)
    val (clockList, partialSourceList) = getLists(moduleMap)(lineages)
    val sourceList = partialSourceList ++ moduleMap(top).ports.collect{ case Port(i, n, Input, ClockType) => n }
    writer.append(s"Clocklist: $clockList \n")
    writer.append(s"Sourcelist: $sourceList \n")

    // Inline all non-black-boxes
    val inline = new InlineInstances(TransID(-10)) //TID doesn't matter
    val modulesToInline = (c.modules.collect { case Module(_, n, _, _) if n != top => ModuleName(n, CircuitName(c.main)) }).toSet
    //val modulesToInline = (getModules(Set[String](), lineages) - top).map(m => ModuleName(m, CircuitName(c.main)))
    val cx = inline.run(RemoveAllButClocks.run(c), modulesToInline, Set()).circuit
    val topModule = cx.modules.collectFirst { case m if m.name == top => m }.get
    val connects = getConnects(topModule)
    clockList.foreach { clock =>
      val origin = getOrigin(connects, clock).serialize.replace('.', '$')
      if(!sourceList.contains(origin)){
        outputBuffer.append(s"Bad Origin of $clock is $origin\n")
      } else {
        outputBuffer.append(s"Good Origin of $clock is $origin\n")
      }
    }

    // Write to output file
    writer.write(outputBuffer.toString)

    // Return unchanged circuit
    c
  }
}

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
