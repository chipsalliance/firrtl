// See LICENSE for license details.

package firrtl.analyses

import scala.collection.mutable
import firrtl._
import firrtl.ir._
import firrtl.graph._
import firrtl.Utils._
import firrtl.traversals.Foreachers._
import firrtl.annotations.TargetToken._


/** A class representing the instance hierarchy of a working IR Circuit
  *
  * @constructor constructs an instance graph from a Circuit
  * @param c the Circuit to analyze
  */
class InstanceGraph(c: Circuit) {

  val moduleMap = c.modules.map({m => (m.name,m) }).toMap
  private val instantiated = new mutable.LinkedHashSet[String]
  private val childInstances =
    new mutable.LinkedHashMap[String, mutable.LinkedHashSet[WDefInstance]]
  for (m <- c.modules) {
    childInstances(m.name) = new mutable.LinkedHashSet[WDefInstance]
    m.foreach(InstanceGraph.collectInstances(childInstances(m.name)))
    instantiated ++= childInstances(m.name).map(i => i.module)
  }

  private val instanceGraph = new MutableDiGraph[WDefInstance]
  private val instanceQueue = new mutable.Queue[WDefInstance]

  for (subTop <- c.modules.view.map(_.name).filterNot(instantiated)) {
    val topInstance = WDefInstance(subTop,subTop)
    instanceQueue.enqueue(topInstance)
    while (instanceQueue.nonEmpty) {
      val current = instanceQueue.dequeue
      instanceGraph.addVertex(current)
      for (child <- childInstances(current.module)) {
        if (!instanceGraph.contains(child)) {
          instanceQueue.enqueue(child)
          instanceGraph.addVertex(child)
        }
        instanceGraph.addEdge(current,child)
      }
    }
  }

  // The true top module (circuit main)
  private val trueTopInstance = WDefInstance(c.main, c.main)

  /** A directed graph showing the instance dependencies among modules
    * in the circuit. Every WDefInstance of a module has an edge to
    * every WDefInstance arising from every instance statement in
    * that module.
    */
  lazy val graph = DiGraph(instanceGraph)

  /** A list of absolute paths (each represented by a Seq of instances)
    * of all module instances in the Circuit.
    */
  lazy val fullHierarchy: mutable.LinkedHashMap[WDefInstance,Seq[Seq[WDefInstance]]] = graph.pathsInDAG(trueTopInstance)

  /** A count of the *static* number of instances of each module. For
    * any module other than the top module, this is equivalent to the
    * number of inst statements in the circuit instantiating each
    * module, irrespective of the number of times (if any) the
    * enclosing module appears in the hierarchy. Note that top module
    * of the circuit has an associated count of 1, even though it is
    * never directly instantiated.
    */
  lazy val staticInstanceCount: Map[OfModule, Int] = {
    val instModules = childInstances.flatMap(_._2.view.map(_.OfModule).toSeq)
    instModules.foldLeft(Map(c.main.OfModule -> 1)) { case (counts, mod) => counts.updated(mod, counts.getOrElse(mod, 0) + 1) }
  }

  /** Finds the absolute paths (each represented by a Seq of instances
    * representing the chain of hierarchy) of all instances of a
    * particular module.
    *
    * @param module the name of the selected module
    * @return a Seq[ Seq[WDefInstance] ] of absolute instance paths
    */
  def findInstancesInHierarchy(module: String): Seq[Seq[WDefInstance]] = {
    if (instantiated(module)) {
      val instances = graph.getVertices.filter(_.module == module).toSeq
      instances flatMap { i => fullHierarchy(i) }
    } else {
      Nil
    }
  }

  /** An [[firrtl.graph.EulerTour EulerTour]] representation of the [[firrtl.graph.DiGraph DiGraph]] */
  lazy val tour = EulerTour(graph, trueTopInstance)

  /** Finds the lowest common ancestor instances for two module names in
    * a design
    */
  def lowestCommonAncestor(moduleA: Seq[WDefInstance],
                           moduleB: Seq[WDefInstance]): Seq[WDefInstance] = {
    tour.rmq(moduleA, moduleB)
  }

  /**
    * Module order from highest module to leaf module
    * @return sequence of modules in order from top to leaf
    */
  def moduleOrder: Seq[DefModule] = {
    graph.transformNodes(_.module).linearize.map(moduleMap(_))
  }


  /** Given a circuit, returns a map from module name to children
     * instance/module definitions
     */
  def getChildrenInstances: mutable.LinkedHashMap[String, mutable.LinkedHashSet[WDefInstance]] = childInstances

  /** Given a circuit, returns a map from module name to children
    * instance/module [[firrtl.annotations.TargetToken]]s
    */
  def getChildrenInstanceOfModule: mutable.LinkedHashMap[String, mutable.LinkedHashSet[(Instance, OfModule)]] =
    childInstances.map(kv => kv._1 -> kv._2.map(_.toTokens))

  // Transforms a TraversableOnce input into an order-preserving map
  // Iterates only once, no intermediate collections
  // Can possibly be replaced using LinkedHashMap.from(..) or better immutable map in Scala 2.13
  private def asOrderedMap[K1, K2, V](it: TraversableOnce[K1], f: (K1) => (K2, V)): collection.Map[K2, V] = {
    val lhmap = new mutable.LinkedHashMap[K2, V]
    it.foreach { lhmap += f(_) }
    lhmap
  }

  /** Given a circuit, returns a map from module name to a map
    * in turn mapping instances names to corresponding module names
    */
  def getChildrenInstanceMap: collection.Map[OfModule, collection.Map[Instance, OfModule]] =
    childInstances.map(kv => kv._1.OfModule -> asOrderedMap(kv._2, (i: WDefInstance) => i.toTokens))

}

object InstanceGraph {

  /** Returns all WDefInstances in a Statement
    *
    * @param insts mutable datastructure to append to
    * @param s statement to descend
    * @return
    */
  def collectInstances(insts: mutable.Set[WDefInstance])
                      (s: Statement): Unit = s match {
    case i: WDefInstance => insts += i
    case i: DefInstance => throwInternalError("Expecting WDefInstance, found a DefInstance!")
    case i: WDefInstanceConnector => throwInternalError("Expecting WDefInstance, found a WDefInstanceConnector!")
    case _ => s.foreach(collectInstances(insts))
  }
}
