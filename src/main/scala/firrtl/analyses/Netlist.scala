package firrtl.analyses

import scala.collection.mutable

import firrtl._
import firrtl.ir._
import firrtl.graph._
import firrtl.Utils._
import firrtl.Mappers._


/** A class representing the instance hierarchy of a working IR Circuit
  * 
  * @constructor constructs an instance graph from a Circuit
  * @param c the Circuit to analyze
  */
class InstanceGraph(c: Circuit) {

  private def collectInstances(insts: mutable.Set[WDefInstance])(s: Statement): Statement = s match {
    case i: WDefInstance =>
      insts += i
      i
    case _ =>
      s map collectInstances(insts)
  }

  private val moduleMap = c.modules.map({m => (m.name,m) }).toMap
  private val instantiated = new mutable.HashSet[String]
  private val childInstances =
    new mutable.HashMap[String,mutable.Set[WDefInstance]]
  for (m <- c.modules) {
    childInstances(m.name) = new mutable.HashSet[WDefInstance]
    m map collectInstances(childInstances(m.name))
    instantiated ++= childInstances(m.name).map(i => i.module)
  }

  private val uninstantiated = moduleMap.keySet -- instantiated
  private val instanceGraph = new MutableDiGraph[WDefInstance]
  private val instanceQueue = new mutable.Queue[WDefInstance]

  uninstantiated.foreach({ subTop =>
    val topInstance = WDefInstance(subTop,subTop)
    instanceQueue.enqueue(topInstance)
    while (!instanceQueue.isEmpty) {
      val current = instanceQueue.dequeue
      instanceGraph.addVertex(current)
      for (child <- childInstances(current.module)) {
        if (!instanceGraph.contains(child)) {
          instanceQueue.enqueue(child)
        }
        instanceGraph.addEdge(current,child)
      }
    }
  })

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
  lazy val fullHierarchy = graph.pathsInDAG(trueTopInstance)

  /** Finds the absolute paths (each represented by a Seq of instances
    * representing the chain of hierarchy) of all instances of a
    * particular module.
    * 
    * @param module the name of the selected module
    * @return a Seq[Seq[WDefInstance]] of absolute instance paths
    */
  def findInstancesInHierarchy(module: String): Seq[Seq[WDefInstance]] = {
    val instances = graph.getVertices.filter(_.module == module).toSeq
    instances flatMap { i => fullHierarchy(i) }
  }

  lazy val tour = EulerTour(graph, trueTopInstance)

  /** Finds the lowest common ancestor instances for two module names in
    * a design
    */
  def lowestCommonAncestor(moduleA: Seq[WDefInstance],
    moduleB: Seq[WDefInstance]):
      Seq[WDefInstance] = {
    tour.rmq(moduleA, moduleB)
  }

  def test(): Unit = {
    println(s"[info] Euler Tour:")
    println(s"[info]   - map:")
    for (i <- tour.r) {
      println(s"[info]     - ${i._1 map (_.name)}: ${i._2}") }
    println(s"[info]   - tour:")
    for ((m, i) <- tour.e zip tour.h) {
      println(s"[info]     - ${m map (_.name)}: ${i}") }

    println(s"[info] e: ${tour.e.map(_.map(_.name))}")

    val instances = graph.pathsInDAG(trueTopInstance).values.flatten
    instances.toSeq.combinations(2).toList.map { case Seq(a, b) =>
      val n = tour.rmqNaive(a, b)
      val p = tour.rmqBV(a, b)
      println(s"[info]   - rmqNaive of ${a.map(_.name)}, ${b.map(_.name)}: ${n.map(_.name)}")
      println(s"[info]   - rmqBV of ${a.map(_.name)}, ${b.map(_.name)}: ${p.map(_.name)}")
      require(n == p, "Naive RMQ does not agree with BV implementation")
    }

    var t1 = System.currentTimeMillis
    val step = 1024 * 4
    instances.toSeq.combinations(2).toList.map { case Seq(a, b) =>
      for (i <- 1 to step) { tour.rmqNaive(a, b) }
    }
    var t2 = System.currentTimeMillis
    println(s"[info] Naive time: ${(t2 - t1)}ms")

    t1 = System.currentTimeMillis
    instances.toSeq.combinations(2).toList.map { case Seq(a, b) =>
      for (i <- 1 to step) { tour.rmqBV(a, b) }
    }
    t2 = System.currentTimeMillis
    println(s"[info] BV time: ${(t2 - t1)}ms")
 
    // val size = 1024 * 32
    // val (iters, increment) = (256, 1)
    // val synth_e = (0 to size).map(a => (0 to a).toList.toSeq).toList.toSeq
    // println(s"[info] Built data structures")
    // val synth_h = collection.mutable.ListBuffer(0)
    // (Seq.fill(size)(1) ++ Seq.fill(size)(-1))
    //   .map{ pm => synth_h.prepend(synth_h.head + pm) }
    // println(s"[info] Built data structures")
    // val synth_r = synth_e.map(n => n -> n.last).toMap
    // println(s"[info] Built data structures")
    // val synthetic = new EulerTour[Int](
    //   r = synth_r,
    //   e = synth_e,
    //   h = synth_h)
    // println(s"[info] Built Euler Tour of size $size")

    // t1 = System.currentTimeMillis
    // for (i <- 0 to iters * increment - 1 by increment) {
    //   synthetic.rmqNaive(0, i)
    // }
    // t2 = System.currentTimeMillis
    // println(s"[info] Naive time: ${t2 - t1}ms total, ${(t2 - t1) / iters}ms per query")

    // t1 = System.currentTimeMillis
    // synthetic.rmqBV(0, 0)
    // t2 = System.currentTimeMillis
    // for (i <- increment to iters * increment - 1 by increment) {
    //   synthetic.rmqBV(0, i)
    // }
    // var t3 = System.currentTimeMillis
    // println(s"[info] BV time: ${t3 - t1}ms total, ${(t3 - t1) / iters}ms per query")
    // println(s"[info] BV time: ${t3 - t2}ms post-setup, ${(t3 - t2) / iters}ms per query post-setup")

    // println(s"[info] Checking agreement...")
    // for (i <- 0 to iters * increment - 1 by increment) {
    //   require(synthetic.rmqNaive(0, i) ==  synthetic.rmqBV(0, i))
    // }
    // println(s"[info] Okay!")
  }
}
