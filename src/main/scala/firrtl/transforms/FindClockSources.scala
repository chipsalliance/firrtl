// See LICENSE for license details.

package firrtl.transforms

import firrtl.PrimOps.AsClock
import firrtl.analyses.{CircuitGraph, IRLookup, InstanceViewedGraph, OpToken}
import firrtl.annotations.TargetToken._
import firrtl.annotations._
import firrtl.graph.{DiGraph, DiGraphLike}
import firrtl.{CircuitForm, CircuitState, FEMALE, MALE, MidForm, PortKind, RegKind, RenameMap, Transform, Utils, WRef}
import firrtl.ir._

import scala.collection.mutable

class ClockSourceFinder(digraph: DiGraph[Target],
                        circuit: Circuit,
                        irLookup: IRLookup) extends InstanceViewedGraph(digraph) {
  override val prev = new mutable.LinkedHashMap[Target, Target]()

  val circuitTarget = CircuitTarget(circuit.main)
  val extModuleNames = circuit.modules.collect { case e: ExtModule => e.name }.toSet
  val astClockOutputs = circuit.modules.flatMap {
    case e: ExtModule =>
      val (inputs, outputs) = CircuitGraph.modulePortTargets(circuitTarget.module(e.name), e)
      outputs.collect {
        case (rt, ClockType) => rt
      }
    case _ => Seq()
  }.toSet

  private val clockSources = mutable.ArrayBuffer[Target]()

  def isReg(t: Target): Boolean = {
    t.tryToComplete match {
      case rt: ReferenceTarget if !rt.isClock && !rt.isInit && !rt.isReset && irLookup.kind(t) == RegKind => true
      case other => false
    }
  }

  override def getEdges(v: Target): collection.Set[Target] = {
    val instanceEdges = super.getEdges(v)
    val ret = instanceEdges.flatMap {
      case rt@ ReferenceTarget(c, m, Nil, _, _)
        if irLookup.kind(rt) == PortKind && irLookup.gender(rt) == MALE && irLookup.tpe(rt) == ClockType && !rt.isClock =>
        clockSources += rt
        Seq()
      case rt: ReferenceTarget
        if extModuleNames.contains(rt.encapsulatingModule) && irLookup.tpe(rt) == ClockType && irLookup.gender(rt) == FEMALE =>
        clockSources += rt
        Seq()
      case ct@GenericTarget(_, _, tokens)
        if tokens.last.isInstanceOf[OpToken] && tokens.last.asInstanceOf[OpToken].op == "asClock" =>
        clockSources += ct
        Seq()
      case rt: ReferenceTarget
        if isReg(v) && (rt.tokens.last != Clock) =>
        Seq()
      case other =>
        Seq(other)
    }
    ret
    /*
      ((i, irLookup(i)), vIR) match {
        case ((rt@ ReferenceTarget(c, m, Nil, _, _), e: Expression), _)
          if(Utils.kind(e) == PortKind && Utils.gender(e) == MALE && e.tpe == ClockType && !rt.isClock) =>
            clockSources += rt
            Seq()
        case ((rt: ReferenceTarget, _: Port), _)
          if extModuleNames.contains(rt.encapsulatingModule) && astClockOutputs.contains(rt.pathlessTarget) =>
            clockSources += rt
            Seq()
        case ((t, DoPrim(AsClock, _, _, _)), _) =>
          clockSources += t
          Seq()
        case ((rt: ReferenceTarget, _), e: Expression)
          if Utils.kind(e) == RegKind && rt.tokens.last != Clock =>
            Seq()
        case other => Seq(i)
      }
    }
      */
  }

  def getClockSource(t: Target): Seq[Target] = {
    clockSources.clear()
    BFS(t)
    val finalSources = clockSources.toList
    clockSources.clear()
    finalSources
  }
}

case class ClockSource(signals: Seq[ReferenceTarget],
                       clockSource: IsMember,
                       isAsClock: Option[String]) extends Annotation {
  override def update(renames: RenameMap): Seq[Annotation] = {
    val newSignals = signals.flatMap{ s => renames.get(s).getOrElse(Seq(s)) }.collect {
      case x: ReferenceTarget => x
      case other => sys.error("Don't make my signals not a ReferenceTarget!")
    }

    val newClockSource = renames.get(clockSource) match {
      case Some(Seq(x: IsMember)) => x
      case None => clockSource
      case other => sys.error("Don't split or delete my clock source!!")
    }

    Seq(ClockSource(newSignals, newClockSource, isAsClock))
  }
}

class FindClockSources() extends Transform {

  override def inputForm: CircuitForm = MidForm
  override def outputForm: CircuitForm = MidForm

  override def execute(state: CircuitState): CircuitState = {
    val clockSourceMap = findTopClockSources(state.circuit)

    val newAnnos = clockSourceMap.map { case (c, signals) =>
      (c, c.tokens.last) match {
        case (c: ReferenceTarget, _) => ClockSource(signals, c, None)
        case (c: GenericTarget, op@OpToken("asClock", _)) =>
          ClockSource(signals, c.remove(1).complete.asInstanceOf[IsMember], Some(op.value))
      }
    }

    state.copy(annotations = state.annotations ++ newAnnos)

  }

  def findTopClockSources(circuit: Circuit): collection.Map[Target, Seq[ReferenceTarget]] = {
    val (circuitGraph, irLookup) = firrtl.analyses.CircuitGraph.buildCircuitGraph(circuit)
    val clockSourceFinder = new ClockSourceFinder(circuitGraph.reverse, circuit, irLookup)

    val topModule = circuit.modules.collectFirst { case m if m.name == circuit.main => m }.get

    val (inputs, outputs) = CircuitGraph.modulePortTargets(CircuitTarget(circuit.main).module(circuit.main), topModule)
    val outputTargets = outputs.map(_._1)

    val clockSourceMap = mutable.HashMap[Target, Vector[ReferenceTarget]]()
    outputTargets.foreach { o =>
      val osClockSources = clockSourceFinder.getClockSource(o)
      osClockSources.foreach { c =>
        clockSourceMap(c) = clockSourceMap.getOrElse(c, Vector.empty[ReferenceTarget]) :+ o
      }
    }

    clockSourceMap


  }
}
