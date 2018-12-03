// See LICENSE for license details.

package firrtl.transforms

import firrtl.analyses._
import firrtl.annotations.TargetToken._
import firrtl.annotations._
import firrtl.{CircuitForm, CircuitState, FEMALE, MALE, MemKind, MidForm, PortKind, RegKind, RenameMap, Transform}
import firrtl.ir._

import scala.collection.mutable

/** Finds clock sources of specific signals
  *
  * For all targets contained within a [[GetClockSources]] annotation, calculate their clock source
  * Produces a [[ClockSources]] annotation containing their mapping
  *
  * If target is:
  *   - ReferenceTarget; calculate clock source
  *   - InstanceTarget; calculate clock source of all input ports to that instance
  *   - ModuleTarget; calculate clock source of all output ports
  *   - CircuitTarget; calculate clock source of all output ports of all modules
  */
class FindClockSources() extends Transform {

  override def inputForm: CircuitForm = MidForm
  override def outputForm: CircuitForm = MidForm

  override def execute(state: CircuitState): CircuitState = {

    val targets = state.annotations.flatMap {
      case GetClockSources(targets) => targets
    }

    if(targets.nonEmpty) {
      val finder = new ClockSourceFinder(CircuitGraph(state.circuit))
      val signalToClockSources = finder.getClockSource(targets).map { case (signal, sources) =>
        signal -> sources.map { c =>
          (c, c.tokens.last) match {
            case (c: ReferenceTarget, _) => (c, None)
            case (c: GenericTarget, op@OpToken("asClock", _)) =>
              (c.remove(1).complete.asInstanceOf[IsMember], Some(op.value))
          }
        }
      }
      state.copy(annotations = ClockSources(signalToClockSources) +: state.annotations)
    } else {
      state
    }
  }
}

/** Consumed by [[FindClockSources]], records a given set of signals for which to find their clock sources
  * @param targets
  */
case class GetClockSources(targets: Seq[CompleteTarget]) extends Annotation {
  override def update(renames: RenameMap): Seq[Annotation] = {
    val newTargets = targets.flatMap(renames(_))
    Seq(GetClockSources(newTargets))
  }
}

/** Generated by [[FindClockSources]], maps a given clock source to signals that are synchronized with it
  */
case class ClockSources(signalToClocks: Map[ReferenceTarget, Set[(IsMember, Option[String])]]) extends Annotation {
  override def update(renames: RenameMap): Seq[Annotation] = {
    val newSignalToClocks = signalToClocks.flatMap { case (signal, sources) =>
      val newSignals = renames(signal).collect {
        case x: ReferenceTarget => x
        case other => sys.error("Don't make my signals not a ReferenceTarget!")
      }

      val newClockSources = sources.map { case (clockSource, isAsClock) =>
        renames.get(clockSource) match {
          case Some(Seq(x: IsMember)) => (x, isAsClock)
          case None => (clockSource, isAsClock)
          case other => sys.error("Don't split or delete my clock source!!")
        }
      }
      newSignals.map(s => s -> newClockSources)
    }
    Seq(ClockSources(newSignalToClocks))
  }
}


/** Instance-Viewed Graph to find clock sources of signals */
class ClockSourceFinder(graph: CircuitGraph) extends CircuitGraph(graph.circuit, graph.digraph.reverse, graph.irLookup) {

  /** Finds clock sources of specific signals
    *
    * If target is:
    *   - ReferenceTarget; calculate clock source, must be ground type
    *   - InstanceTarget; calculate clock source of all input ports to that instance
    *   - ModuleTarget; calculate clock source of all output ports
    *   - CircuitTarget; calculate clock source of all output ports of all modules
    *
    * @param targets
    * @return
    */
  def getClockSource(targets: Seq[CompleteTarget]): Map[ReferenceTarget, Set[Target]] = {
    val memberTargets: Seq[IsMember] = targets.flatMap {
      case ct: CircuitTarget => graph.circuit.modules.map(m => ct.module(m.name))
      case other: IsMember => Seq(other)
    }.distinct

    val moduleOrder = new InstanceGraph(circuit).moduleOrder.zipWithIndex.map {
      case (m, i) => m.name -> i
    }.toMap

    val topoTargets = memberTargets.sortWith { (t0, t1) =>
      (t0, t1) match {
        case (x: CircuitTarget, _) => false
        case (_, x: CircuitTarget) => true
        case (x: IsMember, y: IsMember) => moduleOrder(x.module) > moduleOrder(y.module)
      }
    }

    topoTargets.foldLeft(Map.empty[ReferenceTarget, Set[Target]]) { (map, t) =>
      t match {
        case it: InstanceTarget =>
          val lit = it.asReference.pathlessTarget
          val inputTargets = irLookup.leafTargets(lit).collect {
            case r if irLookup.gender(r) == FEMALE => r
          }
          inputTargets.foldLeft(map){ (m, inTarget) =>
            val x = it.addReference(inTarget)
            m ++ Map(x -> getClockSource(x))
          }
        case rt: ReferenceTarget => map ++ Map(rt -> getClockSource(rt))
        case mt: ModuleTarget =>
          val outputTargets = irLookup.ports(mt).flatMap { irLookup.leafTargets }.collect {
            case r if irLookup.gender(r) == FEMALE => r
          }
          outputTargets.foldLeft(map) { (m, ot) => m ++ Map(ot -> getClockSource(ot)) }
      }
    }
  }

  /** Returns the clock sources that are synchronized with given signal target
    * @param t
    * @return
    */
  def getClockSource(t: Target): Set[Target] = {
    require(
      irLookup.contains(t),
      s"Cannot find\n${t.prettyPrint()}\nin circuit, when computing its clock source!"
    )

    val tpe = irLookup.tpe(t)
    //require(
    //  tpe.isInstanceOf[GroundType],
    //  s"Cannot compute clock source of\n${t.prettyPrint()}\nwith type ${tpe.serialize}!"
    //)

    val finalSources = mutable.HashSet.empty[Target]
    IRLookup.leafTargets(t, tpe).foreach { x =>
      BFS(x)
      finalSources ++= clockMap.getOrElse(x, mutable.HashSet.empty[Target])
    }

    //val finalSources = clockMap.getOrElse(t, mutable.HashSet.empty[Target])
    finalSources.toSet
  }

  private val extModuleNames = circuit.modules.collect { case e: ExtModule => e.name }.toSet

  // Maps signal to set of clock sources it is synchronized with
  private val clockMap = mutable.HashMap[Target, mutable.HashSet[Target]]()

  // Utility function to determine if a target is a register
  private def isReg(t: Target): Boolean = {
    t.tryToComplete match {
      case rt: ReferenceTarget if !rt.isClock && !rt.isInit && !rt.isReset && irLookup.kind(t) == RegKind => true
      case other => false
    }
  }

  /** Returns instance-viewed combinational-edges or reg-to-clock edges
    * Ends early if visiting a node that was previously visited in another BFS
    * @param v the specified node
    * @param prevOpt
    * @return a Set[T] of all vertices that v has edges to
    */
  override def getEdges(v: Target, prevOpt: Option[collection.Map[Target, Target]]): collection.Set[Target] = {
    def recordClock(clock: Target): Unit = recordClocks(Set(clock))
    def recordClocks(clocks: collection.Set[Target]): Unit = {
      val nodePath = new mutable.ArrayBuffer[Target]()
      nodePath += v
      val prev = prevOpt.get
      while (prev.contains(nodePath.last)) {
        clockMap.getOrElseUpdate(nodePath.last, mutable.HashSet.empty[Target]) ++= clocks
        nodePath += prev(nodePath.last)
      }
      clockMap.getOrElseUpdate(nodePath.last, mutable.HashSet.empty[Target]) ++= clocks
    }

    val instanceEdges = super.getEdges(v)

    val filteredEdges = instanceEdges.flatMap {
      case rt: ReferenceTarget
        if irLookup.kind(v) == MemKind && irLookup.gender(v) == MALE =>
          (irLookup.declaration(v).asInstanceOf[DefMemory].readLatency, rt.component.last.value) match {
            case (0, "clk") => Seq()
            case (0, _) => Seq(rt)
            case (_, "clk") => Seq(rt)
            case (_, _) => Seq()
          }
      // Is a Register, filter non-clock outgoing edges
      case rt: ReferenceTarget
        if isReg(v) && (rt.tokens.last != Clock) =>
        Seq()
      case other =>
        Seq(other)
    }

    val shortCutEdges = filteredEdges.flatMap {
      // If seen node before, record clock and end
      case x if clockMap.contains(x) =>
        recordClocks(clockMap(x))
        Seq()
      // If seen node before but with child root, return clock sources with added hierarchy
      case x: ReferenceTarget if x.path.nonEmpty && clockMap.contains(x.stripHierarchy(1)) =>
        clockMap(x.stripHierarchy(1)).map(_.addHierarchy(x.module, x.path.head._1.value))
      case other => Seq(other)
    }

    val ret = shortCutEdges.flatMap {
      // Top-level Input Clock Port
      // Must check if not isClock because expression that is in the clock port of reg could be a port
      case rt@ ReferenceTarget(c, m, Nil, _, _)
        if irLookup.kind(rt) == PortKind && irLookup.gender(rt) == MALE && !rt.isClock =>
        recordClock(rt)
        Seq()
      // Black-box Output Clock Port
      case rt: ReferenceTarget
        if extModuleNames.contains(rt.encapsulatingModule) && irLookup.tpe(rt) == ClockType && irLookup.gender(rt) == FEMALE =>
        recordClock(rt)
        Seq()
      // AsClock Expression
      case ct@GenericTarget(_, _, tokens)
        if tokens.last.isInstanceOf[OpToken] && tokens.last.asInstanceOf[OpToken].op == "asClock" =>
        recordClock(ct)
        Seq()
      case other =>
        Seq(other)
    }
    ret
  }
}
