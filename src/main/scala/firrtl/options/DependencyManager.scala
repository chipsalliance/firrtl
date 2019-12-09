// See LICENSE for license details.

package firrtl.options

import firrtl.AnnotationSeq
import firrtl.graph.{DiGraph, CyclicException}

import scala.collection.Set
import scala.collection.immutable.{Set => ISet}
import scala.collection.mutable.{ArrayBuffer, HashMap, LinkedHashMap, LinkedHashSet, Queue}

import scala.reflect.runtime

/** An exception arising from an in a [[DependencyManager]] */
case class DependencyManagerException(message: String, cause: Throwable = null) extends RuntimeException(message, cause)

/** A [[firrtl.options.TransformLike TransformLike]] that resolves a linear ordering of dependencies based on
  * requirements.
  * @tparam A the type over which this transforms
  * @tparam B the type of the [[firrtl.options.TransformLike TransformLike]]
  */
trait DependencyManager[A, B <: TransformLike[A] with DependencyAPI[B]] extends TransformLike[A] with DependencyAPI[B] {
  import DependencyManagerUtils.CharSet

  override lazy val prerequisites = currentState

  override lazy val dependents = Seq.empty

  override def invalidates(a: B): Boolean = (_currentState &~ _targets)(oToD(a))

  /** Requested [[firrtl.options.TransformLike TransformLike]]s that should be run. Internally, this will be converted to
    * a set based on the ordering defined here.
    */
  def targets: Seq[Dependency]
  private lazy val _targets: LinkedHashSet[Dependency] = targets
    .foldLeft(new LinkedHashSet[Dependency]()){ case (a, b) => a += b }

  /** A sequence of [[firrtl.Transform]]s that have been run. Internally, this will be converted to an ordered set.
    */
  def currentState: Seq[Dependency]
  private lazy val _currentState: LinkedHashSet[Dependency] = currentState
    .foldLeft(new LinkedHashSet[Dependency]()){ case (a, b) => a += b }

  /** Existing transform objects that have already been constructed */
  def knownObjects: Set[B]

  /** A sequence of wrappers to apply to the resulting [[firrtl.options.TransformLike TransformLike]] sequence. This can
    * be used to, e.g., add automated pre-processing and post-processing.
    */
  def wrappers: Seq[(B) => B] = Seq.empty

  /** Store of conversions between classes and objects. Objects that do not exist in the map will be lazily constructed.
    */
  protected lazy val dependencyToObject: LinkedHashMap[Dependency, B] = {
    val init = LinkedHashMap[Dependency, B](knownObjects.map(x => oToD(x) -> x).toSeq: _*)
    (_targets ++ _currentState)
      .filter(!init.contains(_))
      .map(x => init(x) = x.getObject())
    init
  }

  /** A method that will create a copy of this [[firrtl.options.DependencyManager DependencyManager]], but with different
    * requirements. This is used to solve sub-problems arising from invalidations.
    */
  protected def copy(
    targets: Seq[Dependency],
    currentState: Seq[Dependency],
    knownObjects: ISet[B] = dependencyToObject.values.toSet): B

  /** Implicit conversion from Dependency to B */
  private implicit def dToO(d: Dependency): B = dependencyToObject.getOrElseUpdate(d, d.getObject())

  /** Dynamically check if an object is a singleton. Works on AnyRef rather than B to avoid needing a ClassTag,
    * since DependencyManager is a trait.
    */
  private def isSingleton(o: AnyRef): Boolean = {
    runtime.currentMirror.reflect(o).symbol.isModuleClass
  }

  /** Implicit conversion from B to Dependency */
  private implicit def oToD(b: B): Dependency = {
    if (isSingleton(b)) {
      DependencyID(Right(b.asInstanceOf[B with Singleton]))
    } else {
      DependencyID(Left(b.getClass))
    }
  }

  /** Modified breadth-first search that supports multiple starting nodes and a custom extractor that can be used to
    * generate/filter the edges to explore. Additionally, this will include edges to previously discovered nodes.
    */
  private def bfs( start: LinkedHashSet[Dependency],
                   blacklist: LinkedHashSet[Dependency],
                   extractor: B => Set[Dependency] ): LinkedHashMap[B, LinkedHashSet[B]] = {

    val (queue, edges) = {
      val a: Queue[Dependency]                    = Queue(start.toSeq:_*)
      val b: LinkedHashMap[B, LinkedHashSet[B]] = LinkedHashMap[B, LinkedHashSet[B]](
        start.map((dToO(_) -> LinkedHashSet[B]())).toSeq:_*)
      (a, b)
    }

    while (queue.nonEmpty) {
      val u: Dependency = queue.dequeue
      for (v <- extractor(dependencyToObject(u))) {
        if (!blacklist.contains(v) && !edges.contains(v)) {
          queue.enqueue(v)
        }
        if (!edges.contains(v)) {
          val obj = dToO(v)
          edges(obj) = LinkedHashSet.empty
          dependencyToObject += (v -> obj)
        }
        edges(dependencyToObject(u)) = edges(dependencyToObject(u)) + dependencyToObject(v)
      }
    }

    edges
  }

  /** Pull in all registered [[firrtl.options.TransformLike TransformLike]] once [[firrtl.options.TransformLike
    * TransformLike]] registration is integrated
    * @todo implement this
    */
  private lazy val registeredTransforms: Set[B] = Set.empty

  /** A directed graph consisting of prerequisite edges */
  private lazy val prerequisiteGraph: DiGraph[B] = {
    val edges = bfs(
      start = _targets &~ _currentState,
      blacklist = _currentState,
      extractor = (p: B) => new LinkedHashSet[Dependency]() ++ p.prerequisites &~ _currentState)
    DiGraph(edges)
  }

  /** A directed graph consisting of prerequisites derived from only those transforms which are supposed to run. This
    * pulls in dependents for transforms which are not in the target set.
    */
  private lazy val dependentsGraph: DiGraph[B] = {
    val v = new LinkedHashSet() ++ prerequisiteGraph.getVertices
    DiGraph(new LinkedHashMap() ++ v.map(vv => vv -> (v & (vv.dependents.toSet).map(dToO)))).reverse
  }

  /** A directed graph of *optional* prerequisites. Each optional prerequisite is promoted to a full prerequisite if the
    * optional prerequisite is already a node in the prerequisite graph.
    */
  private lazy val optionalPrerequisitesGraph: DiGraph[B] = {
    val v = new LinkedHashSet() ++ prerequisiteGraph.getVertices
    DiGraph(new LinkedHashMap() ++ v.map(vv => vv -> (v & (vv.optionalPrerequisites.toSet).map(dToO))))
  }

  /** A directed graph consisting of prerequisites derived from ALL targets. This is necessary for defining targets for
    * [[DependencyManager]] sub-problems.
    */
  private lazy val otherDependents: DiGraph[B] = {
    val edges = {
      val x = new LinkedHashMap ++ _targets
        .map(dependencyToObject)
        .map{ a => a -> prerequisiteGraph.getVertices.filter(a._dependents(_)) }
      x
        .values
        .reduce(_ ++ _)
        .foldLeft(x){ case (xx, y) => if (xx.contains(y)) { xx } else { xx ++ Map(y -> Set.empty[B]) } }
    }
    DiGraph(edges).reverse
  }

  /** A directed graph consisting of all prerequisites, including prerequisites derived from dependents */
  lazy val dependencyGraph: DiGraph[B] = prerequisiteGraph + dependentsGraph + optionalPrerequisitesGraph
  // println("Dependency Graph -----------------------")
  // dependencyGraph.getEdgeMap.foreach{ case (a, b) =>
  //   println(s"""  - ${a.getClass.getSimpleName} -> ${b.map(_.getSimpleName).mkString(", ")}""")
  // }

  /** A directed graph consisting of invalidation edges */
  lazy val invalidateGraph: DiGraph[B] = {
    val v = new LinkedHashSet() ++ dependencyGraph.getVertices
    DiGraph(
      bfs(
        start = v.map(oToD(_)),
        blacklist = _currentState,

        /* Explore all invalidated transforms **EXCEPT** the current transform! */
        extractor = (p: B) => v.filter(p.invalidates).map(oToD(_)).toSet - oToD(p)))
      .reverse
  }
  // println("Invalidate Graph -----------------------")
  // invalidateGraph.getEdgeMap.foreach{ case (a, b) =>
  //   println(s"""  - ${a.getClass.getSimpleName} -> ${b.map(_.getSimpleName).mkString(", ")}""")
  // }

  /** Wrap a possible [[CyclicException]] thrown by a thunk in a [[DependencyManagerException]] */
  private def cyclePossible[A](a: String, diGraph: DiGraph[_])(thunk: => A): A = try { thunk } catch {
    case e: CyclicException =>
      throw new DependencyManagerException(
        s"""|No transform ordering possible due to cyclic dependency in $a with cycles:
            |${diGraph.findSCCs.filter(_.size > 1).mkString("    - ", "\n    - ", "")}""".stripMargin, e)
  }

  /** An ordering of [[firrtl.options.TransformLike TransformLike]]s that causes the requested [[DependencyManager.targets
    * targets]] to be executed starting from the [[DependencyManager.currentState currentState]]. This ordering respects
    * prerequisites, dependents, and invalidates of all constituent [[firrtl.options.TransformLike TransformLike]]s.
    * This uses an algorithm that attempts to reduce the number of re-lowerings due to invalidations. Re-lowerings are
    * implemented as new [[DependencyManager]]s.
    * @throws DependencyManagerException if a cycle exists in either the [[DependencyManager.dependencyGraph
    * dependencyGraph]] or the [[DependencyManager.invalidateGraph invalidateGraph]].
    */
  lazy val transformOrder: Seq[B] = {

    /* Topologically sort the dependency graph to determine a "good" initial ordering.
     */
    val sorted = {
      val edges = {
        val v = cyclePossible("invalidates", invalidateGraph){ invalidateGraph.linearize }.reverse
        /* A comparison function that will sort vertices based on the topological sort of the invalidation graph */
        val cmp =
          (l: B, r: B) => v.foldLeft((Map.empty[B, Dependency => Boolean], Set.empty[Dependency])){
            case ((m, s), r) => (m + (r -> ((a: Dependency) => !s(a))), s + r) }._1(l)(r)
        new LinkedHashMap() ++
          v.map(vv => vv -> (new LinkedHashSet() ++ (dependencyGraph.getEdges(vv).toSeq.sortWith(cmp))))
      }

      cyclePossible("prerequisites/dependents", dependencyGraph) {
        DiGraph(edges)
          .linearize
          .reverse
          .dropWhile(b => _currentState.contains(b))
      }
    }

    // println("Invalidate Sorted ---------------------------------")
    // println(s"""  - ${sorted.map(_.getClass.getSimpleName).mkString(", ")}""")

    // println("Dependencies Sorted ---------------------------------")
    // println(s"""  - ${sorted.map(_.getClass.getSimpleName).mkString(", ")}""")

    /* [todo] Seq is inefficient here, but Array has ClassTag problems. Use something else? */
    val (s, l) = sorted.foldLeft((_currentState, Seq[B]())){ case ((state, out), in) =>
      /* The prerequisites are both prerequisites AND dependents. */
      val prereqs = new LinkedHashSet() ++ in.prerequisites ++
        dependencyGraph.getEdges(in).toSeq.map(oToD) ++
        otherDependents.getEdges(in).toSeq.map(oToD)
      val preprocessing: Option[B] = {
        if ((prereqs -- state).nonEmpty) { Some(this.copy(prereqs.toSeq, state.toSeq)) }
        else                             { None                                        }
      }
      /* "in" is added *after* invalidation because a transform my not invalidate itself! */
      ((state ++ prereqs).map(dToO).filterNot(in.invalidates).map(oToD) + in, out ++ preprocessing :+ in)
    }
    val postprocessing: Option[B] = {
      if ((_targets -- s).nonEmpty) { Some(this.copy(_targets.toSeq, s.toSeq)) }
      else                          { None                                     }
    }
    l ++ postprocessing
  }

  /** A version of the [[DependencyManager.transformOrder transformOrder]] that flattens the transforms of any internal
    * [[DependencyManager]]s.
    */
  lazy val flattenedTransformOrder: Seq[B] = transformOrder.flatMap {
    case p: DependencyManager[A, B] => p.flattenedTransformOrder
    case p => Some(p)
  }

  final override def transform(annotations: A): A = {

    /* A local store of each wrapper to it's underlying class. */
    val wrapperToClass = new HashMap[B, Dependency]

    /* The determined, flat order of transforms is wrapped with surrounding transforms while populating wrapperToClass so
     * that each wrapped transform object can be dereferenced to its underlying class. Each wrapped transform is then
     * applied while tracking the state of the underlying A. If the state ever disagrees with a prerequisite, then this
     * throws an exception.
     */
    flattenedTransformOrder
      .map{ t =>
        val w = wrappers.foldLeft(t){ case (tx, wrapper) => wrapper(tx) }
        wrapperToClass += (w -> t)
        w
      }.foldLeft((annotations, _currentState)){ case ((a, state), t) =>
          if (!t.prerequisites.toSet.subsetOf(state)) {
            throw new DependencyManagerException(
              s"""|Tried to execute '$t' for which run-time prerequisites were not satisfied:
                  |  state: ${state.mkString("\n    -", "\n    -", "")}
                  |  prerequisites: ${prerequisites.mkString("\n    -", "\n    -", "")}""".stripMargin)
          }
          (t.transform(a), ((state + wrapperToClass(t)).map(dToO).filterNot(t.invalidates).map(oToD)))
      }._1
  }

  /** This colormap uses Colorbrewer's 4-class OrRd color scheme */
  protected val colormap = Seq("#fef0d9", "#fdcc8a", "#fc8d59", "#d7301f")

  /** Get a name of some [[firrtl.options.TransformLike TransformLike]] */
  private def transformName(transform: B, suffix: String = ""): String = s""""${transform.name}$suffix""""

  /** Convert all prerequisites, dependents, and invalidates to a Graphviz representation.
    * @param file the name of the output file
    */
  def dependenciesToGraphviz: String = {

    def toGraphviz(digraph: DiGraph[B], attributes: String = "", tab: String = "    "): Option[String] = {
      val edges =
        digraph
          .getEdgeMap
          .collect{ case (v, edges) if edges.nonEmpty => (v -> edges) }
          .map{ case (v, edges) =>
            s"""${transformName(v)} -> ${edges.map(e => transformName(e)).mkString("{ ", " ", " }")}""" }

      if (edges.isEmpty) { None } else {
        Some(
          s"""|  { $attributes
              |${edges.mkString(tab, "\n" + tab, "")}
              |  }""".stripMargin
        )
      }
    }

    val connections =
      Seq( (prerequisiteGraph, "edge []"),
           (dependentsGraph,   """edge [style=bold color="#4292c6"]"""),
           (invalidateGraph,   """edge [minlen=2 style=dashed constraint=false color="#fb6a4a"]"""),
           (optionalPrerequisitesGraph, """edge [style=dotted color="#a1d99b"]""") )
        .flatMap{ case (a, b) => toGraphviz(a, b) }
        .mkString("\n")

    val nodes =
      (prerequisiteGraph + dependentsGraph + invalidateGraph + otherDependents)
        .getVertices
        .map(v => s"""${transformName(v)} [label="${v.name}"]""")

    s"""|digraph DependencyManager {
        |  graph [rankdir=BT]
        |  node [fillcolor="${colormap(0)}",style=filled,shape=box]
        |${nodes.mkString("  ", "\n" + "  ", "")}
        |$connections
        |}
        |""".stripMargin
  }

  def transformOrderToGraphviz(colormap: Seq[String] = colormap): String = {

    def rotate[A](a: Seq[A]): Seq[A] = a match {
      case Nil => Nil
      case car :: cdr => cdr :+ car
      case car => car
    }

    val sorted = ArrayBuffer.empty[String]

    def rec(pm: DependencyManager[A, B], cm: Seq[String], tab: String = "", id: Int = 0): (String, Int) = {
      var offset = id

      val targets = pm._targets.toSeq.map(_.name).mkString(", ")
      val state = pm._currentState.toSeq.map(_.name).mkString(", ")

      val header = s"""|${tab}subgraph cluster_$id {
                       |$tab  label="targets: $targets\\nstate: $state"
                       |$tab  labeljust=l
                       |$tab  node [fillcolor="${cm.head}"]""".stripMargin

      val body = pm.transformOrder.map{
        case a: DependencyManager[A, B] =>
          val (str, d) = rec(a, rotate(cm), tab + "  ", offset + 1)
          offset = d
          str
        case a =>
          val name = s"""${transformName(a, "_" + id)}"""
          sorted += name
          s"""$tab  $name [label="${a.name}"]"""
      }.mkString("\n")

      (Seq(header, body, s"$tab}").mkString("\n"), offset)
    }

    s"""|digraph DependencyManagerTransformOrder {
        |  graph [rankdir=TB]
        |  node [style=filled,shape=box]
        |${rec(this, colormap, "  ")._1}
        |  ${sorted.mkString(" -> ")}
        |}""".stripMargin
  }

  /** A method that can be overridden to define custom print handling. This is useful if you would like to make some
    * transform print additional information.
    * @param tab the current tab setting
    * @param charSet the character set in use
    * @param size the number of nodes at the current level of the tree
    */
  def customPrintHandling(
    tab: String,
    charSet: CharSet,
    size: Int): Option[PartialFunction[(B, Int), Seq[String]]] = None

  /** Helper utility when recursing during pretty printing
    * @param tab an indentation string to use for every line of output
    * @param charSet a collection of characters to use when printing
    * @param preprocess a partial function that will be used before any other printing logic
    */
  def prettyPrintRec(tab: String, charSet: CharSet): Seq[String] = {

    val (l, n, c) = (charSet.lastNode, charSet.notLastNode, charSet.continuation)
    val last = transformOrder.size - 1

    val defaultHandling: PartialFunction[(B, Int), Seq[String]] = {
      case (a: DependencyManager[_, _], `last`) =>
        Seq(s"$tab$l ${a.name}") ++ a.prettyPrintRec(s"""$tab${" " * c.size} """, charSet)
      case (a: DependencyManager[_, _], _)      => Seq(s"$tab$n ${a.name}") ++ a.prettyPrintRec(s"$tab$c ", charSet)
      case (a, `last`)                          => Seq(s"$tab$l ${a.name}")
      case (a, _)                               => Seq(s"$tab$n ${a.name}")
    }

    val handling = customPrintHandling(tab, charSet, transformOrder.size) match {
      case Some(a) => a.orElse(defaultHandling)
      case None    => defaultHandling
    }

    transformOrder
      .zipWithIndex
      .flatMap(handling)
  }

  /** Textually show the determined transform order
    * @param tab an indentation string to use for every line of output
    * @param charSet a collection of characters to use when printing
    */
  def prettyPrint(
    tab: String = "",
    charSet: DependencyManagerUtils.CharSet = DependencyManagerUtils.PrettyCharSet): String = {

    (Seq(s"$tab$name") ++ prettyPrintRec(tab, charSet)).mkString("\n")

  }

}

/** A [[Phase]] that will ensure that some other [[Phase]]s and their prerequisites are executed.
  *
  * This tries to determine a phase ordering such that an [[AnnotationSeq]] ''output'' is produced that has had all of
  * the requested [[Phase]] target transforms run without having them be invalidated.
  * @param targets the [[Phase]]s you want to run
  */
class PhaseManager(
  val targets: Seq[PhaseManager.PhaseDependency],
  val currentState: Seq[PhaseManager.PhaseDependency] = Seq.empty,
  val knownObjects: Set[Phase] = Set.empty) extends DependencyManager[AnnotationSeq, Phase] with Phase {

  // TODO: are these adapter constructors needed?
  def this(targets: Seq[Class[_ <: Phase]], currentState: Seq[Class[_ <: Phase]]) {
    this(targets.map(c => DependencyID[Phase](Left(c))), currentState.map(c => DependencyID[Phase](Left(c))))
  }

  def this(targets: Seq[Class[_ <: Phase]]) {
    this(targets, Seq.empty)
  }

  protected def copy(a: Seq[Dependency], b: Seq[Dependency], c: ISet[Phase]) = new PhaseManager(a, b, c)
}

object PhaseManager {

  /** The type used to represent dependencies between [[Phase]]s */
  type PhaseDependency = DependencyID[Phase]

}

object DependencyManagerUtils {

  /** A character set used for pretty printing
    * @see [[PrettyCharSet]]
    * @see [[ASCIICharSet]]
    */
  trait CharSet {
    /** Used when printing the last node */
    val lastNode: String

    /** Used when printing a node that is NOT the last */
    val notLastNode: String

    /** Used while recursing into a node that is NOT the last */
    val continuation: String
  }

  /** Uses prettier characters, but possibly not supported by all fonts */
  object PrettyCharSet extends CharSet {
    val lastNode     = "└──"
    val notLastNode  = "├──"
    val continuation = "│  "
  }

  /** Basic ASCII output */
  object ASCIICharSet extends CharSet {
    val lastNode     = "\\--"
    val notLastNode  = "|--"
    val continuation = "|  "
  }

}
