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

/** Store of pending wiring information for a Module */
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
    if (top == module) { 1 }
    else { childrenMap(top).foldLeft(0) { case (count, (i, child)) =>
      count + countInstances(childrenMap, child, module) }
    }
  }

  /** Returns a module's lineage, containing all children lineages as well
    */
  def getLineage(childrenMap: ChildrenMap, module: String): Lineage =
    Lineage(module, childrenMap(module) map { case (i, m) => (i, getLineage(childrenMap, m)) } )

  /** Return a map of sink instances to source instances that minimizes
    * distance
    *
    * @param sinks `Set[String]` of sink module names
    * @param source `String` of the source module name
    * @param i `InstanceGraph` of a `Circuit`
    * @return `Map` of sink instance names to source instance names
    * @throws WiringException if a sink is equidistant to two sources
    */
  def sinksToSources(sinks: Set[String], source: String, i: InstanceGraph):
      Map[Seq[WDefInstance], Seq[WDefInstance]] = {
    val indent = "  "

    val owners = new mutable.HashMap[Seq[WDefInstance], Seq[Seq[WDefInstance]]]
      .withDefaultValue(Seq())
    val queue = new mutable.Queue[Seq[WDefInstance]]
    val visited = new mutable.HashMap[Seq[WDefInstance], Boolean]
      .withDefaultValue(false)

    i.fullHierarchy.keys
      .filter { case WDefInstance(_, _, module, _) => module == source }
      .map    { k => i.fullHierarchy(k)
        .map  { l => queue.enqueue(l)
                     owners(l) = Seq(l)                                }}

    while (queue.nonEmpty) {
      val u = queue.dequeue
      visited(u) = true

      (i.graph.getEdges(u.last)
        .map(u :+ _)
        .toSeq :+ u.dropRight(1))
        .filter(e => !visited(e) && e.nonEmpty )
        .map (v => {
          owners(v) = owners(v) ++ owners(u)
          queue.enqueue(v)
        })
    }

    val sinkInsts = i.fullHierarchy.keys
      .filter { case WDefInstance(_, _, module, _) => sinks.contains(module) }
      .map    { k => i.fullHierarchy(k)                                      }
      .toSeq.flatten

    // Check that every sink has one unique owner. The only time that
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
}
