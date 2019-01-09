// See LICENSE for license details.

package firrtl.transforms.clockfinder

import firrtl.{FEMALE, MALE, MemKind, PortKind, RegKind}
import firrtl.analyses.{CircuitGraph, ConnectionGraph, InstanceGraph}
import firrtl.annotations.TargetToken.Clock
import firrtl.annotations._
import firrtl.ir.{ClockType, DefMemory, ExtModule, UIntLiteral}

import scala.collection.mutable


object ClockFinder {

  case class IllegalClockCrossing(reg: ReferenceTarget, clockPaths: Seq[Seq[ReferenceTarget]]) {
    def prettyPrint(): String = {
      s"Illegal Clock Crossing Detected at register $reg!\n" +
        clockPaths.map { path =>
          s"From Clock ${path.head}:\n" +
          CircuitGraph.prettyToString(path, "\t")
        }.mkString("\n")
    }
  }

  implicit class ClockFinderPML(circuitGraph: CircuitGraph) {
    def getClockSource(targets: Seq[CompleteTarget],
                       signalToClocks: Map[ReferenceTarget, Set[ReferenceTarget]] = Map.empty
                      ): Map[ReferenceTarget, Set[ReferenceTarget]] = {
      val finder = new ClockFinder(circuitGraph.reverseConnectionGraph, signalToClocks)
      finder.getClockSource(targets)
    }

    def findClockCrossings(whitelistedRegs: Set[ReferenceTarget] = Set.empty,
                           signalToClocks: Map[ReferenceTarget, Set[ReferenceTarget]] = Map.empty
                          ): Seq[IllegalClockCrossing] = {

      val finder = new ClockFinder(circuitGraph.reverseConnectionGraph, signalToClocks)

      circuitGraph.deepReferences(circuitGraph.main, RegKind).flatMap { reg =>
        if(!whitelistedRegs.contains(reg)) {
          val sinkClocks = finder.getClockSource(reg)

          val fanIns = if(circuitGraph.irLookup.expr(reg.reset) == UIntLiteral(0)) {
            circuitGraph.fanInSignals(reg).filter(r => !(r.isClock || r.isReset || r.isInit))
          } else {
            circuitGraph.fanInSignals(reg).filter(r => !r.isClock)
          }

          val sourceClocks = fanIns.flatMap(finder.getClockSource)

          if(sinkClocks != sourceClocks) {
            Set(IllegalClockCrossing(reg, (sinkClocks.toSeq ++ sourceClocks).map { c => circuitGraph.path(c, reg)}))
          } else {
            Set.empty[IllegalClockCrossing]
          }
        } else Set.empty[IllegalClockCrossing]
      }
    }.toList
  }
}

/** Instance-Viewed Graph to find clock sources of signals */
class ClockFinder(reverseGraph: ConnectionGraph,
                  signalToClocks: Map[ReferenceTarget, Set[ReferenceTarget]] = Map.empty
                 ) extends ConnectionGraph(reverseGraph.circuit, reverseGraph.digraph, reverseGraph.irLookup) {

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
  def getClockSource(targets: Seq[CompleteTarget]): Map[ReferenceTarget, Set[ReferenceTarget]] = {
    val memberTargets: Seq[IsMember] = targets.map {
      case ct: CircuitTarget => ct.module(ct.circuit)
      case other: IsMember => other
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

    val ret = memberTargets.foldLeft(Map.empty[ReferenceTarget, Set[ReferenceTarget]]) { (map, t) =>
      t match {
        case it: InstanceTarget =>
          val lit = it.asReference.pathlessTarget

          val inputTargets = lit.leafSubTargets(irLookup.tpe(lit)).collect {
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
    ret
  }

  /** Returns the clock sources that are synchronized with given signal target
    * @param t
    * @return
    */
  def getClockSource(t: ReferenceTarget): Set[ReferenceTarget] = {
    require(
      irLookup.contains(t),
      s"Cannot find\n${t.prettyPrint()}\nin circuit, when computing its clock source!"
    )

    val tpe = irLookup.tpe(t)

    val finalSources = mutable.HashSet.empty[ReferenceTarget]
    t.leafSubTargets(tpe).foreach { x =>
      BFS(x, Set.empty[ReferenceTarget])

      finalSources ++= getTag(x, clockMap).getOrElse(mutable.HashSet.empty[ReferenceTarget])
    }
    finalSources.toSet
  }

  private val extModuleNames = circuit.modules.collect { case e: ExtModule => e.name }.toSet

  // Maps signal to set of clock sources it is synchronized with
  private val clockMap = mutable.LinkedHashMap[(String, ReferenceTarget), mutable.HashSet[ReferenceTarget]]()

  // Utility function to determine if a target is a register
  private def isReg(t: ReferenceTarget): Boolean = {
    t.tryToComplete match {
      case rt: ReferenceTarget if !rt.isClock && !rt.isInit && !rt.isReset && irLookup.kind(t) == RegKind => true
      case other => false
    }
  }


  /** Returns instance-viewed combinational-edges or reg-to-clock edges
    * Ends early if visiting a node that was previously visited in another BFS
    * @param node the specified node
    * @param prevOpt
    * @return a Set[T] of all vertices that source has edges to
    */
  override def getEdges(node: ReferenceTarget,
                        prevOpt: Option[collection.Map[ReferenceTarget, ReferenceTarget]]
                       ): collection.Set[ReferenceTarget] = {
    val prev = prevOpt.get
    node match {
      // If known clock relationship, tagPath
      case rt if signalToClocks.contains(rt) =>
        tagPath(rt, prev, signalToClocks(rt), clockMap)
        Set()

      // If cached result, record clock and end. Exclude cached top-level signals as input port could be a new result
      case rt if getTag(rt, clockMap).nonEmpty =>
        tagPath(rt, prev, getTag(rt, clockMap).get, clockMap)
        Set()

      // Top-level Input Port
      // Must check if not isClock because expression that is in the clock port of reg could be a port
      case rt@ ReferenceTarget(c, m, Nil, _, _)
        if irLookup.kind(rt) == PortKind && irLookup.gender(rt) == MALE && !rt.isClock =>
        tagPath(rt, prev, Set(rt), clockMap)
        Set()

      // Black-box Output Clock Port
      case rt: ReferenceTarget
        if extModuleNames.contains(rt.encapsulatingModule) && irLookup.tpe(rt) == ClockType && irLookup.gender(rt) == FEMALE =>
        tagPath(rt, prev, Set(rt), clockMap)
        Set()

      // AsClock Expression
      case rt if ConnectionGraph.isAsClock(rt) =>
        tagPath(rt, prev, Set(rt), clockMap)
        Set()

      case nonClockSource =>

        val superEdges = super.getEdges(nonClockSource)

        val filteredEdges = superEdges.flatMap {

          // If is is a combinational read-memory, return non-clock signals, otherwise return only clock signals
          case rt: ReferenceTarget if irLookup.kind(nonClockSource) == MemKind && irLookup.gender(nonClockSource) == MALE =>
            (irLookup.declaration(nonClockSource).asInstanceOf[DefMemory].readLatency, rt.component.last.value) match {
              case (0, "clk") => Seq()
              case (0, _) => Seq(rt)
              case (_, "clk") => Seq(rt)
              case (_, _) => Seq()
            }

          // Is a Register, filter non-clock outgoing edges
          case rt: ReferenceTarget if isReg(nonClockSource) && (rt.tokens.last != Clock) => Seq()

          case other => Seq(other)
        }

        filteredEdges.toSet

    }
  }
}
