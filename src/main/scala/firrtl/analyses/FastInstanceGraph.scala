// See LICENSE for license details.

package firrtl.analyses

import firrtl.annotations._
import firrtl.annotations.TargetToken._
import firrtl.graph.{DiGraph, MutableDiGraph}
import firrtl.ir

import scala.collection.mutable

/** A class representing the instance hierarchy of firrtl Circuit
  * This is a faster version of the old `InstanceGraph` which only uses
  * pairs of InstanceName and Module name as vertex keys instead of using WDefInstance
  * which will hash the instance type causing some performance issues.
  */
class FastInstanceGraph(c: ir.Circuit) {
  import FastInstanceGraph._

  /** maps module names to the DefModule node */
  val moduleMap: Map[String, ir.DefModule] = c.modules.map({m => (m.name,m) }).toMap

  private val childInstances: Map[String, Seq[Key]] = c.modules.map { m =>
    m.name -> FastInstanceGraph.collectInstances(m)
  }.toMap
  private val instantiated = childInstances.flatMap(_._2).map(_.module).toSet
  private val roots = c.modules.map(_.name).filterNot(instantiated)
  val graph: DiGraph[Key] = buildGraph(childInstances, roots)
  private val circuitTopInstance = topKey(c.main)
  // cache vertices to speed up repeat calls to findInstancesInHierarchy
  private lazy val vertices = graph.getVertices


  /** A list of absolute paths (each represented by a Seq of instances)
    * of all module instances in the Circuit.
    */
  private lazy val fullHierarchy: mutable.LinkedHashMap[Key, Seq[Seq[Key]]] =
    graph.pathsInDAG(circuitTopInstance)

  /** Module order from highest module to leaf module */
  def moduleOrder: Seq[ir.DefModule] = graph.transformNodes(_.module).linearize.map(moduleMap(_))

  /** Returns a map from module name to instances defined in said module. */
  def getChildInstances: Map[String, Seq[Key]] = childInstances

  /** A count of the *static* number of instances of each module. For any module other than the top (main) module,
    * this is equivalent to the number of inst statements in the circuit instantiating each module,
    * irrespective of the number of times (if any) the enclosing module appears in the hierarchy.
    * Note that top module of the circuit has an associated count of one, even though it is never directly instantiated.
    * Any modules *not* instantiated at all will have a count of zero.
    */
  lazy val staticInstanceCount: Map[OfModule, Int] = {
    val foo = mutable.LinkedHashMap.empty[OfModule, Int]
    childInstances.keys.foreach {
      case main if main == c.main => foo += main.OfModule  -> 1
      case other                  => foo += other.OfModule -> 0
    }
    childInstances.values.flatten.map(_.OfModule).foreach {
      mod => foo += mod -> (foo(mod) + 1)
    }
    foo.toMap
  }

  /** Finds the absolute paths (each represented by a Seq of instances
    * representing the chain of hierarchy) of all instances of a particular
    * module. Note that this includes one implicit instance of the top (main)
    * module of the circuit. If the module is not instantiated within the
    * hierarchy of the top module of the circuit, it will return Nil.
    *
    * @param module the name of the selected module
    * @return a Seq[ Seq[WDefInstance] ] of absolute instance paths
    */
  def findInstancesInHierarchy(module: String): Seq[Seq[Key]] = {
    val instances = vertices.filter(_.module == module).toSeq
    instances.flatMap{ i => fullHierarchy.getOrElse(i, Nil) }
  }

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
    childInstances.map(kv => kv._1.OfModule -> asOrderedMap(kv._2, (i: Key) => i.toTokens))

  /** The set of all modules in the circuit */
  private def modules: collection.Set[OfModule] = graph.getVertices.map(_.OfModule)

  /** The set of all modules in the circuit reachable from the top module */
  private def reachableModules: collection.Set[OfModule] =
    mutable.LinkedHashSet(circuitTopInstance.OfModule) ++ graph.reachableFrom(circuitTopInstance).map(_.OfModule)

  /** The set of all modules *not* reachable in the circuit */
  lazy val unreachableModules: collection.Set[OfModule] = modules diff reachableModules
}


object FastInstanceGraph {
  /** We want to only use this untyped version as key because hashing bundle types is expensive
    * @param name the name of the instance
    * @param module the name of the module that is instantiated
    */
  case class Key(name: String, module: String) {
    def Instance: Instance = TargetToken.Instance(name)
    def OfModule: OfModule = TargetToken.OfModule(module)
    def toTokens: (Instance, OfModule) = (Instance, OfModule)
  }

  /** Finds all instance definitions in a firrtl Module. */
  def collectInstances(m: ir.DefModule): Seq[Key] = m match {
    case _ : ir.ExtModule => Seq()
    case ir.Module(_, _, _, body) => {
      val instances = mutable.ArrayBuffer[Key]()
      def onStmt(s: ir.Statement): Unit = s match {
        case firrtl.WDefInstance(_, name, module, _) => instances += Key(name, module)
        case ir.DefInstance(_, name, module, _)  => instances += Key(name, module)
        case _: firrtl.WDefInstanceConnector =>
          firrtl.Utils.throwInternalError("Expecting WDefInstance, found a WDefInstanceConnector!")
        case other => other.foreachStmt(onStmt)
      }
      onStmt(body)
      instances
    }
  }

  private def topKey(module: String): Key = Key(module, module)

  private def buildGraph(childInstances: Map[String, Seq[Key]], roots: Iterable[String]):
    DiGraph[Key] = {
    val instanceGraph = new MutableDiGraph[Key]

    // iterate over all modules that are not instantiated and thus act as a root
    roots.foreach { subTop =>
      // create a root node
      val topInstance = topKey(subTop)
      // graph traversal
      val instanceQueue = new mutable.Queue[Key]
      instanceQueue.enqueue(topInstance)
      while (instanceQueue.nonEmpty) {
        val current = instanceQueue.dequeue
        instanceGraph.addVertex(current)
        for (child <- childInstances(current.module)) {
          if (!instanceGraph.contains(child)) {
            instanceQueue.enqueue(child)
            instanceGraph.addVertex(child)
          }
          instanceGraph.addEdge(current, child)
        }
      }
    }
    instanceGraph
  }
}
