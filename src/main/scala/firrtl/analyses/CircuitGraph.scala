package firrtl.analyses

import firrtl.annotations.TargetToken.{Instance, OfModule}
import firrtl.{Kind, WDefInstance}
import firrtl.annotations._
import firrtl.ir.Circuit

object CircuitGraph {
  def apply(circuit: Circuit): CircuitGraph = new CircuitGraph(ConnectionGraph(circuit))
  def prettyPrintPath(path: Seq[ReferenceTarget], tab: String = ""): String = {
    tab + path.mkString(s"\n$tab")
  }

}

class CircuitGraph private[analyses] (val connectionGraph: ConnectionGraph) {

  lazy val reverseConnectionGraph = connectionGraph.reverseConnectionGraph

  val circuit = connectionGraph.circuit

  val irLookup = connectionGraph.irLookup

  lazy val instanceGraph = new InstanceGraph(circuit)

  lazy val moduleChildren = instanceGraph.getChildrenInstanceOfModule

  val main = ModuleTarget(circuit.main, circuit.main)

  def fanOutSignals(source: ReferenceTarget): Set[ReferenceTarget] = connectionGraph.getEdges(source, None).toSet

  def fanInSignals(sink: ReferenceTarget): Set[ReferenceTarget] = reverseConnectionGraph.getEdges(sink, None).toSet

  def absolutePaths(mt: ModuleTarget): Seq[IsModule] = instanceGraph.findInstancesInHierarchy(mt.module).map {
    case seq if seq.nonEmpty => seq.foldLeft(CircuitTarget(circuit.main).module(circuit.main): IsModule) {
      case (it, WDefInstance(_, instance, ofModule, _)) => it.instOf(instance, ofModule)
    }
  }

  def path(source: ReferenceTarget, sink: ReferenceTarget): Seq[ReferenceTarget] =
    connectionGraph.path(source, sink)

  def localReferences(path: IsModule, kind: Kind): Seq[ReferenceTarget] = {
    val leafModule = path.leafModule
    irLookup.references(ModuleTarget(circuit.main, leafModule), kind).map(_.setPathTarget(path))
  }

  def deepReferences(path: IsModule, kind: Kind): Seq[ReferenceTarget] = {
    val leafModule = path.leafModule
    val children = moduleChildren(leafModule)
    val localRefs = localReferences(path, kind)
    localRefs ++ children.flatMap {
      case (Instance(inst), OfModule(ofModule)) => deepReferences(path.instOf(inst, ofModule), kind)
    }
  }

  def absoluteReferences(moduleTarget: ModuleTarget, kind: Kind): Seq[ReferenceTarget] = {
    localReferences(moduleTarget, kind).flatMap(makeAbsolute)
  }

  def makeAbsolute(reference: ReferenceTarget): Seq[ReferenceTarget] = {
    absolutePaths(reference.moduleTarget).map(abs => reference.setPathTarget(abs))
  }
}
