// See LICENSE for license details.

package firrtl.passes
package wiring

import firrtl._
import firrtl.ir._
import firrtl.Utils._
import firrtl.Mappers._
import scala.collection.mutable
import firrtl.annotations._
import firrtl.analyses.InstanceGraph
import firrtl.graph.DiGraph
import WiringUtils._

/** Declaration kind in lineage (e.g. input port, output port, wire)
  */
sealed trait DecKind
case object DecInput extends DecKind
case object DecOutput extends DecKind
case object DecWire extends DecKind

case class Metadata(
  addPort: Option[(String, DecKind)] = None,
  cons: Seq[(String, String)] = Seq.empty) {

  override def toString: String = serialize("")

  def serialize(tab: String): String = s"""
$tab addPort: $addPort
$tab cons: $cons
"""
}

/** A lineage tree representing the instance hierarchy in a design
  */
case class Lineage(
    name: String,
    children: Seq[(String, Lineage)] = Seq.empty,
    source: Boolean = false,
    sink: Boolean = false,
    sourceParent: Boolean = false,
    sinkParent: Boolean = false,
    sharedParent: Boolean = false,
    addPort: Option[(String, DecKind)] = None,
    cons: Seq[(String, String)] = Seq.empty) {

  def map(f: Lineage => Lineage): Lineage =
    this.copy(children = children.map{ case (i, m) => (i, f(m)) })

  override def toString: String = shortSerialize("")

  def shortSerialize(tab: String): String = s"""
    |$tab name: $name,
    |$tab children: ${children.map(c => tab + "   " + c._2.shortSerialize(tab + "    "))}
    |""".stripMargin

  def foldLeft[B](z: B)(op: (B, (String, Lineage)) => B): B =
    this.children.foldLeft(z)(op)

  def serialize(tab: String): String = s"""
    |$tab name: $name,
    |$tab source: $source,
    |$tab sink: $sink,
    |$tab sourceParent: $sourceParent,
    |$tab sinkParent: $sinkParent,
    |$tab sharedParent: $sharedParent,
    |$tab addPort: $addPort
    |$tab cons: $cons
    |$tab children: ${children.map(c => tab + "   " + c._2.serialize(tab + "    "))}
    |""".stripMargin
}




object WiringUtils {
  type ChildrenMap = mutable.HashMap[String, Seq[(String, String)]]

  /** Given a circuit, returns a map from module name to children
    * instance/module names
    */
  def getChildrenMap(c: Circuit): ChildrenMap = {
    val childrenMap = new ChildrenMap()
    def getChildren(mname: String)(s: Statement): Statement = s match {
      case s: WDefInstance =>
        childrenMap(mname) = childrenMap(mname) :+ (s.name, s.module)
        s
      case s: DefInstance =>
        childrenMap(mname) = childrenMap(mname) :+ (s.name, s.module)
        s
      case s => s map getChildren(mname)
    }
    c.modules.foreach{ m =>
      childrenMap(m.name) = Nil
      m map getChildren(m.name)
    }
    childrenMap
  }

  /** Counts the number of instances of a module declared under a top module
    */
  def countInstances(childrenMap: ChildrenMap, top: String, module: String): Int = {
    if(top == module) 1
    else childrenMap(top).foldLeft(0) { case (count, (i, child)) =>
      count + countInstances(childrenMap, child, module)
    }
  }

  /** Returns a module's lineage, containing all children lineages as well
    */
  def getLineage(childrenMap: ChildrenMap, module: String): Lineage =
    Lineage(module, childrenMap(module) map { case (i, m) => (i, getLineage(childrenMap, m)) } )

  /** Sets the sink, sinkParent, source, and sourceParent fields of every
    *  Lineage in tree
    */
  def setFields(sinks: Set[String], source: String)(lin: Lineage): Lineage = lin map setFields(sinks, source) match {
    case l if sinks.contains(l.name) => l.copy(sink = true)
    case l =>
      val src = l.name == source
      val sinkParent = l.children.foldLeft(false) { case (b, (i, m)) => b || m.sink || m.sinkParent }
      val sourceParent = if(src) true else l.children.foldLeft(false) { case (b, (i, m)) => b || m.source || m.sourceParent }
      l.copy(sinkParent=sinkParent, sourceParent=sourceParent, source=src)
  }

  /** Return a map of sink instances to source instances that minimizes
    * distance.
    */
  def sinksToSources(sinks: Set[String], source: String, i: InstanceGraph): Map[Seq[WDefInstance], Seq[WDefInstance]] = {
    val indent = "  "

    val owners = new mutable.HashMap[Seq[WDefInstance], Seq[Seq[WDefInstance]]].withDefaultValue(Seq())
    val queue = new mutable.Queue[Seq[WDefInstance]]
    val visited = new mutable.HashMap[Seq[WDefInstance], Boolean].withDefaultValue(false)

    i.fullHierarchy.keys
      .filter { case WDefInstance(_, _, module, _) => module == source }
      .map    { k => i.fullHierarchy(k)
        .map  { l => queue.enqueue(l)
                     owners(l) = Seq(l)                                }}

    while (queue.nonEmpty) {
      val u = queue.dequeue
      visited(u) = true

      val allEdges = (i.graph.getEdges(u.last)
        .map(u :+ _)
        .toSeq :+ u.dropRight(1))
        .filter(e => !visited(e) && e.nonEmpty )

      for (v <- allEdges) {
        owners(v) = owners(v) ++ owners(u)
        queue.enqueue(v)
      }
    }

    val sinkInsts = i.fullHierarchy.keys
      .filter { case WDefInstance(_, _, module, _) => sinks.contains(module) }
      .map    { k => i.fullHierarchy(k)                                      }
      .toSeq.flatten

    val t = i.fullHierarchy.keys
      .filter { case WDefInstance(_, _, module, _) => sinks.contains(module) }
      .toSeq

    // Check that every sink has a unique owner. The only time that
    // this should fail is if a sink is equidistant to two sources.
    sinkInsts.map( s => {
      if (!owners.contains(s) || owners(s).size > 1) {
        throw new WiringException(s"Unable to determine source mapping for sink '${s.map(_.name)}'") }
    })

    owners
      .filter { case (k, v) => sinkInsts.contains(k) }
      .map    { case (k, v) => (k, v.flatten)        }
      .toMap
  }

  /** Finds the sharedParent (lowest common ancestor) of lineage top via
    * breadth first search
    */
  def findSharedParent(queue: mutable.Queue[Lineage]): Option[String] = {
    while (queue.nonEmpty) {
      val l = queue.dequeue
      if ((l.sinkParent && l.sourceParent) && (l.children.foldLeft(false) { case (b, (i, m)) => b || (m.sinkParent ^ m.sourceParent) || m.sink || m.source })) {
        return Some(l.name)
      }
      queue ++= l.children.map{ case (i, m) => m }
    }
    return None
  }

  /** Sets the sharedParent of lineage top via pre-order DFS
    */
  def setSharedParent(lca: String)(lin: Lineage): Lineage = lin match {
    case l if l.name == lca => l.copy(sharedParent = true)
    case l => l.copy(sourceParent=false, sinkParent=false) map setSharedParent(lca)
  }

  /** Sets the addPort and cons fields of the lineage tree
    */
  def setThings(portNames:Map[String, String], compName: String)(lin: Lineage): Lineage = {
    val funs = Seq(
      ((l: Lineage) => l map setThings(portNames, compName)),
      ((l: Lineage) => l match {
        case Lineage(name, _, _, _, _, _, true, _, _) => //SharedParent
          l.copy(addPort=Some((portNames(name), DecWire)))
        case Lineage(name, _, _, _, true, _, _, _, _) => //SourceParent
          l.copy(addPort=Some((portNames(name), DecOutput)))
        case Lineage(name, _, _, _, _, true, _, _, _) => //SinkParent
          l.copy(addPort=Some((portNames(name), DecInput)))
        case Lineage(name, _, _, true, _, _, _, _, _) => //Sink
          l.copy(addPort=Some((portNames(name), DecInput)))
        case l => l
      }),
      ((l: Lineage) => l match {
        case Lineage(name, _, true, _, _, _, _, _, _) => //Source
          val tos = Seq(s"${portNames(name)}")
          val from = compName
          l.copy(cons = l.cons ++ tos.map(t => (t, from)))
        case Lineage(name, _, _, _, true, _, _, _, _) => //SourceParent
          val tos = Seq(s"${portNames(name)}")
          val from = l.children.filter { case (i, c) => c.sourceParent }.map { case (i, c) => s"$i.${portNames(c.name)}" }.head
          l.copy(cons = l.cons ++ tos.map(t => (t, from)))
        case l => l
      }),
      ((l: Lineage) => l match {
        case Lineage(name, _, _, _, _, true, _, _, _) => //SinkParent
          val tos = l.children.filter { case (i, c) => (c.sinkParent || c.sink) && !c.sourceParent } map { case (i, c) => s"$i.${portNames(c.name)}" }
          val from = s"${portNames(name)}"
          l.copy(cons = l.cons ++ tos.map(t => (t, from)))
        case l => l
      })
    )
    funs.foldLeft(lin)((l, fun) => fun(l))
  }

  /** Return a map from module to its lineage in the tree
    */
  def pointToLineage(lin: Lineage): Map[String, Lineage] = {
    val map = mutable.HashMap[String, Lineage]()
    def onLineage(l: Lineage): Lineage = {
      map(l.name) = l
      l map onLineage
    }
    onLineage(lin)
    map.toMap
  }
}
