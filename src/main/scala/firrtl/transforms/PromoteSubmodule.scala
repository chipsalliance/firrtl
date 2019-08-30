// See LICENSE for license details.

package firrtl.transforms

import firrtl._
import ir._
import Utils._
import Mappers._
import annotations.{CircuitTarget, InstanceTarget, ReferenceTarget, Annotation, SingleTargetAnnotation}
import analyses.InstanceGraph
import graph.DiGraph
import scala.collection.mutable
import firrtl.passes.{InlineInstances,PassException}

class TestPromoteSubmodule extends Transform {
  def inputForm = HighForm
  def outputForm = HighForm
  def execute(state: CircuitState) = state.copy(annotations = state.annotations :+ new PromoteSubmoduleAnnotation(InstanceTarget("top", "middle", Nil, "bot", "bottom")))
}

case class PromoteSubmoduleAnnotation(target: InstanceTarget) extends SingleTargetAnnotation[InstanceTarget] {
  def targets = Seq(target)
  def duplicate(n: InstanceTarget) = this.copy(n)
}


-> 

object PromoteSubmodule {
  import collection._

  private def setRef(newRefName: String)(e: Expression): Expression = e match {
    case wr: WRef => wr.copy(name = newRefName)
    case e => e map setRef(newRefName)
  }

  private def getRef(e: Expression) = splitRef(e)._1


}



/**
  * Step 1: Find parentToChildConns, childToParentConns, parentChildAttaches
  * Step 2: Find otherIO (child IOs not related to special cases)
  * Step 3: Transform parent
  *   Step 3.1: Add passThruPort to parent ports; bundle type containing all otherIO (and flipped)
  *   Step 3.2: Delete all special-case statements
  *   Step 3.3: Replace all WRefs to child with refs to passThruPort (and change type)
  * Step 4: Transform parent instances
  *   Step 4.1: Replaces all WRefs, WDefInstaces to parent with new type (adding passThruPort)
  *   Step 4.2: Add promotedChild instance
  *   Step 4.3: parentToChildConns.foreach { connect driver of parent input to promotedChild input }
  *   Step 4.4: childToParentConns.foreach { replace all refs to parent output with promotedChild output }
  *   Step 4.5: parentChildAttaches.foreach { add attach(promotedChild inout, attacher of parent inout) }
  *   Step 4.6: connect/attach other IOs (one at a time)
  * Step 3: Find parentChildAttaches
  * Step 4: 
  */

class PromoteSubmodule extends Transform {
  import PromoteSubmodule._
  def inputForm = LowForm
  def outputForm = MidForm

  class TransformedParent(anno: PromoteSubmoduleAnnotation, parent: Module, child: Module) {
    val parentToChildConns = new mutable.ArrayBuffer[(WSubField, WRef)]
    val childToParentConns = new mutable.ArrayBuffer[(WRef, WSubField)]
    val parentChildAttaches = new mutable.ArrayBuffer[(WRef, WSubField)]
    val retainedAnalogs = new mutable.HashSet[String]

    def isDirect(r: WRef, sf: WSubField) = kind(r) == PortKind && kind(sf) == InstanceKind && getRef(sf).name == anno.target.instance

    def onStmt(s: Statement): Statement = {
      case Attach((sf: WSubField) :: (r: WRef)) if isDirect(r, sf) =>
        parentChildAttaches += (r, sf)
        EmptyStmt
      case Attach((r: WRef) :: (sf: WSubField)) if isDirect(r, sf) =>
        parentChildAttaches += (r, sf)
        EmptyStmt
      case a: Attach =>
        retainedAnalogs ++= a.exprs.map(e => getRef(e).name)
        a
      case c @ Connect(_, sf: WSubField, r: WRef) if isDirect(r, sf) =>
        parentToChildConns += c
        EmptyStmt
      case c @ Connect(_, r: WRef, sf: WSubField) if isDirect(r, sf) =>
        childToParentConns += (r, sf)
        EmptyStmt
      case WDefInstance(_, name, _, _) if (name == anno.target.instance) => EmptyStmt
      case s => s map onStmt
    }

    val trimmedPorts = (parentToChildConns.map(_._1.name) ++ childToParentConns.map(_._2.name)).toSet
    def punchOut(p: Port) = p.tpe match {
      case a: AnalogType => retainedAnalogs(p.name)
      case p => !trimmedPorts(p.name)
    }

    val newPortType = BundleType(m.ports.filter(punchOut).map(p => Field(p.name, to_flip(p.direction), p.tpe)))
    val newPort = Port(NoInfo, anno.target.instance, Input, newPortType)
    val result = parent.copy(ports = parent.ports :+ promotedPort, body = transformedBody)
  }

  /*
  def redirectPortRefs(refMap: Map[WrappedExpression, WSubField])(e: Expression): Expression = e match {
    case wsf @ WSubField(wr: WRef, _, _, _) => refMap.getOrElse(WrappedExpression(wsf), wsf)
    case e => e map redirectPortRefs(refMap)
  }

  def promo

  def removeSubmoduleFromParent(anno: PromoteSubmoduleAnnotation, parent: Module, grandparent: Module): (Module, Module) = {
    val childTemplate = WDefInstance(NoInfo, anno.target.instance, anno.target.ofModule, UnknownType)




    val inputDrivers = new mutable.LinkedHashMap[WrappedExpression, Expression]

    def promotedChild(p: WDefInstance)(c: ChildInst): Seq[Statement] = {
      def parentPortWE(pPort: ParentPort) = WrappedExpression(mergeRef(WRef(pDefInst), pPort))
      val ptcConns = parentToChildConns(cInst)
      val redirects = childToParentConns(cInst) ++ parentChildUniqueAttaches(cInst)
      val promotedInst = childTemplates(cInst).copy(name = ns.newName(s"${pInst.name}_${cInst.name}"))
      val promotedConns = ptcConns.map(c => Connect(c.info, c.loc.map(setRef(promotedInst.name)), inputDrivers(parentPortWE(c.expr))))
      val defaultConn = PartialConnect(cInst.info, WSubField(WRef(pDefInst), cInst.name), WRef(promotedInst))
      redirectedPortRefs ++= redirects.map((pPort, cPort) => (parentPortWE(pPort), cPort.map(setRef(promotedInstance.name))))
      Seq(promotedInst, defaultConn) ++ promotedConns
    }

    def transformParentInstances(s: Statement): Statement = s match {
      case pDefInst: WDefInstance if childrenToPromote.contains(pDefInst.name) =>
        Block(Seq(pDefInst) ++ childrenToPromote(pDefInst.name).flatMap(promotedChild(pDefInst)))
      case Block(
      case s => s map transformParentInstances map redirectPortRefs(redirectedPortRefs)
    }
      parentToChildConns(
      val promotedChildRenames = promotedChildTemplates(parentInst.module)
        val promotedChildInstances = promotedChildren(parentInst.module).map(name =>
          val retypedParentInst = wi.copy(tpe = instanceType(
      Block(wi +: 
      case x =>

  }
  def onModule(m: Module): Module = {
    // Transform instances that are parents of promoted modules (add promoted children, conns)
    val ns = Namespace(m)
    m.map(transformParentInstances(ns))

    // Analyze connections
    va
    m.map(analyzeConns)

    // Transform promoted child instances
    m.map(transformChildInstances(
   // 
 

  def execute(state: CircuitState): CircuitState = {
    val c = state.circuit
    val order = getVisitOrder(cs)
    val transformedModules = order.map(onModule)
    val outputCircuit = c.copy(modules = matchModuleOrder(transformedModules, c.modules))
    val filteredAnnos = state.annotations.filterNot(_.isInstanceOf[PromoteSubmoduleAnnotation])
    CircuitState(outputCircuit, outputForm, filteredAnnos, Some(renames))
  }
}

/**
 * Takes PromoteSubmodule annotations for instantiations and causes
 * each corresponding instance to be removed; ports are added to the
 * parent module and the submodule is added as a peer instance to all
 * modules instantiating the parent module.

 * Module nomenclature:
 * Grandparent = instantiator of parent.
 *     Transformed to instantiate child alongside parent, connect.
 * Parent = instantiator of child.
 *     Transformed to get port of child IO instead of instantiating child.
 * Child = submodule to be promoted.
 *     Does not get transformed.
 */

class PromoteSubmodule extends Transform {
  def inputForm = LowForm
  def outputForm = LowForm

  private def findPromotedInstances(iGraph: InstanceGraph, anns: Iterable[Annotation]): Set[WDefInstance] = {
    val annotatedInstances = anns.collect {
      case PromoteSubmoduleAnnotation(instTarget) =>
        /* For now, parent must be a module, not an instance.
         * Otherwise, the parent (and possibly grandparent) would potentially
         * need to be replicated into versions with and without the promoted child.
         */
        assert(instTarget.isLocal)
        (instTarget.module, instTarget.instance)
    }
    val annotatedInstanceSet = annotatedInstances.toSet
    val instancesToPromote = iGraph.getChildrenInstances.flatMap {
      case (modName, instanceSet) => instanceSet.flatMap { wi => Some(wi).filter(wi => annotatedInstanceSet((modName, wi.name))) }
    }
    instancesToPromote.toSet
  }

  private def instanceRef(inst: WDefInstance): WRef = WRef(inst.name, inst.tpe, InstanceKind, FEMALE)
  private def instanceField(inst: WDefInstance, field: String): WSubField = {
    val wref = instanceRef(inst)
    WSubField(wref, field, field_type(wref.tpe, field), FEMALE)
  }

  private def deleteSubStatement(subStmt: Statement)(stmt: Statement): Statement = stmt match {
    case `subStmt` => EmptyStmt
    case s => s map deleteSubStatement(subStmt)
  }

  private def getRef(e: Expression): WRef = splitRef(e)._1

  private def getAssignment(s: Statement): (WRef, WRef) = s match {
    case Connect(_, loc, expr) => (getRef(loc), getRef(expr))
    case PartialConnect(_, loc, expr) => (getRef(loc), getRef(expr))
    case Attach(_, exprs) if (exprs.length == 2) => (getRef(exprs(0)), getRef(exprs(1)))
    case _ => (WRef(""), WRef(""))
  }

  private def stripDirectConns(child: WDefInstance, directConns: mutable.ArrayBuffer[Statement])(stmt: Statement): Statement = {
    val (lRef, rRef) = getAssignment(stmt)
    val isDirectConn = (lRef.kind == PortKind && rRef.name == child.name) || (lRef.name == child.name && rRef.kind == PortKind)

    stmt match {
      case c: Conditionally => c
      case s if isDirectConn =>
        directConns += s
        EmptyStmt
      case s => s map stripDirectConns(child, directConns)
    }
  }

  private def promotePortRefs(parent: WDefInstance, child: WDefInstance, promotedChild: WDefInstance)(expr: Expression): Expression = expr match {
    case wr @ WRef(_, _, PortKind, _) => mergeRef(WRef(parent), wr)
    case wr: WRef if wr.name == child.name => wr.copy(name = promotedChild.name)
    case e => e map promotePortRefs(parent, child, promotedChild)
  }

  private def instanceToPort(parent: Module, childInstance: WDefInstance, childModule: DefModule): Module = {
    val promotedPort = Port(childInstance.info, childInstance.name, Input, portBundle(childModule))
    parent.copy(ports = parent.ports :+ promotedPort, body = deleteSubStatement(childInstance)(parent.body))
  }

  private def transformParentInstances(
    parentTemplate: WDefInstance,
    childTemplate: WDefInstance,
    namespace: Namespace,
    promotedNames: mutable.ArrayBuffer[String],
    directConns: Seq[Statement])(stmt: Statement): Statement = stmt match {
    case oldParentInstance @ WDefInstance(_, _, parentTemplate.module, _) =>
      val retypedParentInst = oldParentInstance.copy(tpe = parentTemplate.tpe)
      val childPeerInst = childTemplate.copy(name = namespace.newName(oldParentInstance.name + "_" + childTemplate.name))
      promotedNames += childPeerInst.name
      val connection = PartialConnect(childTemplate.info, instanceField(retypedParentInst, childTemplate.name), instanceRef(childPeerInst))
      val promotedConns = directConns.map(_ map promotePortRefs(retypedParentInst, childTemplate, childPeerInst))
      Block(Seq(retypedParentInst, childPeerInst, connection) ++ promotedConns)
    case s => s map transformParentInstances(parentTemplate, childTemplate, namespace, promotedNames, directConns)
  }

  override def execute(state: CircuitState): CircuitState = {
    val iGraph = new InstanceGraph(state.circuit)
    val allExtModules = state.circuit.modules.collect({ case e: ExtModule => e })
    val updatedModules = new mutable.LinkedHashMap[String, Module]
    iGraph.moduleMap.foreach { case (k, v: Module) => updatedModules += (k -> v); case (k, v) => }
    val reversedIGraph = iGraph.graph.reverse
    val promoted = findPromotedInstances(iGraph, state.annotations)
    val order = reversedIGraph.linearize.filter(reversedIGraph.getEdges(_).size > 0).filter(promoted)
    val renames = RenameMap()
    for (childInstance <- order) {
      val childModule = iGraph.moduleMap(childInstance.module)
      val parentInstances = reversedIGraph.getEdges(childInstance)
      val parentModule = updatedModules(parentInstances.head.module)
      val originalTarget = CircuitTarget(state.circuit.main).module(parentModule.name).instOf(childInstance.name, childInstance.module)
      if (parentModule.name == state.circuit.main) {
        throw new PassException("Cannot promote child instance ${childInstance.name} from top module ${parentModule.name}")
      }
      val directConns = new mutable.ArrayBuffer[Statement]
      val directConnsRemoved = (parentModule map stripDirectConns(childInstance, directConns)).asInstanceOf[Module]
      updatedModules(parentModule.name) = instanceToPort(directConnsRemoved, childInstance, childModule)
      val grandparentInstances = parentInstances.flatMap(reversedIGraph.getEdges(_))
      val grandparentModules = grandparentInstances.map(i => updatedModules(i.module)).toSet
      for (grandparent <- grandparentModules) {
        val parentTemplate = WDefInstance(NoInfo, "", parentModule.name, portBundle(parentModule))
        val namespace = Namespace(grandparent)
        val promotedNames = new mutable.ArrayBuffer[String]
        updatedModules(grandparent.name) = grandparent.map(transformParentInstances(parentTemplate, childInstance, namespace, promotedNames, directConns)).asInstanceOf[Module]
        // Record renames
        promotedNames.map(s => renames.record(originalTarget, originalTarget.copy(module = grandparent.name, instance = s)))
      }
    }
    state.copy(
      circuit = state.circuit.copy(modules = updatedModules.map({ case (k, v) => v }).toSeq ++ allExtModules),
      renames = Some(renames),
      annotations = state.annotations.filterNot(_.isInstanceOf[PromoteSubmoduleAnnotation])
    )
  }
}

   */
