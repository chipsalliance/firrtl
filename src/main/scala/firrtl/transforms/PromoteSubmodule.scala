// See LICENSE for licenseA details.

package firrtl.transforms

import firrtl._
import ir._
import Utils._
import Mappers._
import traversals.Foreachers._
import annotations.{CircuitTarget, InstanceTarget, ReferenceTarget, Annotation, SingleTargetAnnotation}
import analyses.InstanceGraph
import graph.DiGraph
import scala.collection.mutable
import firrtl.passes.{InlineInstances,PassException}


case class PromoteSubmoduleAnnotation(target: InstanceTarget) extends SingleTargetAnnotation[InstanceTarget] {
  def targets = Seq(target)
  def duplicate(n: InstanceTarget) = this.copy(n)
}

object PromoteSubmodule {
  import collection._

  private def setRef(newRefName: String)(e: Expression): Expression = e match {
    case wr: WRef => wr.copy(name = newRefName)
    case e => e map setRef(newRefName)
  }

  private def setRef(newRefName: String, sf: WSubField): WSubField = sf.expr match {
    case wr: WRef => sf.copy(expr = wr.copy(name = newRefName))
  }

  private def getRef(e: Expression) = splitRef(e)._1

  private def containsModule(m: Module, ofModule: String): Boolean = {
    var result = false
    def onStmt(s: Statement): Unit = s match {
      case wi: WDefInstance if (wi.module == ofModule) => result = true
      case s => s.foreach(onStmt)
    }
    m.foreach(onStmt)
    result
  }
}

class PromoteSubmodule extends Transform {
  import PromoteSubmodule._
  def inputForm = LowForm
  def outputForm = HighForm

  class TransformedParent(anno: PromoteSubmoduleAnnotation, parent: Module, child: DefModule, renames: RenameMap) {
    val ciName = anno.target.instance
    val parentToChildConns = new mutable.ArrayBuffer[(WRef, WSubField)]
    val childToParentConns = new mutable.ArrayBuffer[(WRef, WSubField)]
    val parentChildAttaches = new mutable.ArrayBuffer[(WRef, WSubField)]
    val parentInputPorts = (parent.ports.collect { case Port(_, name, Input, _) => name }).toSet
    val parentPortRefs = new mutable.HashMap[String, Int] ++= parent.ports.map(p => p.name -> 0)
    val childPortRefs = new mutable.HashMap[String, Int] ++= child.ports.map(p => p.name -> 0)

    // Right now, hoisting of output connections is coupled to deleting parent and child ports
    // Could separate by checking fallback condition similar to simplePTC to hoist only
    // Don't rely on kind -> no need to run ResolveAndCheck between iterations of transform
    def simpleConn(r: WRef, sf: WSubField): Boolean = {
      val checkR = parentPortRefs.getOrElse(r.name, 0) == 1
      val checkSF = getRef(sf).name == ciName && childPortRefs.getOrElse(sf.name, 0) == 1
      checkR && checkSF
    }

    // Flagging as a PTC (parentToChild) does not cause deletion of parent port (relies on constProp)
    // Therefore, no need to check for other references to RHS
    def simplePTC(r: WRef, sf: WSubField): Boolean = {
      getRef(sf).name == ciName && parentInputPorts(r.name) && childPortRefs.getOrElse(sf.name, 0) == 1
    }

    def analyzeExpr(e: Expression): Unit = e match {
      case WRef(name, _, _, _) if (parentPortRefs.contains(name)) =>
        parentPortRefs(name) = parentPortRefs(name) + 1
      case sf @ WSubField(r: WRef, name, _, _) if (r.name == ciName && childPortRefs.contains(name)) =>
        childPortRefs(name) = childPortRefs(name) + 1
      case e => e foreach analyzeExpr
    }

    def analyzeStmt(s: Statement): Unit = s match {
      case s: Block => s foreach analyzeStmt
      case s => s foreach analyzeExpr
    }

    // TODO: How do casts play into this logic?
    def onStmt(s: Statement): Statement = s match {
      case a @ Attach(_, exprs) if (exprs.length == 2) => (exprs(0), exprs(1)) match {
        case (sf: WSubField, r: WRef) if simpleConn(r, sf) =>
          parentChildAttaches += ((r, sf))
          EmptyStmt
        case (r: WRef, sf: WSubField) if simpleConn(r, sf) =>
          parentChildAttaches += ((r, sf))
          EmptyStmt
        case _ => a
      }
      case c @ Connect(_, sf: WSubField, r: WRef) if simplePTC(r, sf) =>
        parentToChildConns += ((r, sf))
        EmptyStmt
      case c @ Connect(_, r: WRef, sf: WSubField) if simpleConn(r, sf) =>
        childToParentConns += ((r, sf))
        EmptyStmt
      case WDefInstance(_, name, _, _) if (name == ciName) => EmptyStmt
      case s => s map onStmt
    }

    parent.body.foreach(analyzeStmt)
    val transformedBody = parent.body.map(onStmt)

    val optimizedParentPorts = (parentChildAttaches ++ childToParentConns).map(_._1.name).toSet
    val optimizedChildPorts = (parentChildAttaches ++ parentToChildConns ++ childToParentConns).map(_._2.name).toSet

    val punchoutPorts = child.ports.filterNot(p => optimizedChildPorts(p.name) || childPortRefs(p.name) == 0)
    val newPortType = BundleType(punchoutPorts.map(p => Field(p.name, to_flip(p.direction), p.tpe)))
    val newPort = Port(NoInfo, ciName, Input, newPortType)

    val updatedParentPorts = parent.ports.filterNot(p => optimizedParentPorts(p.name)) :+ newPort
    val result = parent.copy(ports = updatedParentPorts, body = transformedBody)
  }


  def transformGrandparent(anno: PromoteSubmoduleAnnotation, parentAnalysis: TransformedParent, grandparent: Module, renames: RenameMap): Module = {
    val ciName = anno.target.instance
    val ns = Namespace(grandparent)
    val abstractRedirects = (parentAnalysis.childToParentConns ++ parentAnalysis.parentChildAttaches).toMap
    val concreteRedirects = new mutable.HashMap[WrappedExpression, WSubField]
    val inputDrivers = new mutable.HashMap[WrappedExpression, Expression]
    val promotedConns = new mutable.ArrayBuffer[Statement]

    // TODO: remove reliance on Kind here? Might make it more robust to not running ResolveandCheck
    def analyze(s: Statement): Unit = s match {
      case Connect(_, loc, expr) if (kind(loc) == InstanceKind) =>
        inputDrivers(WrappedExpression(loc)) = expr
      case s => s foreach analyze
    }

    def onExpr(e: Expression): Expression = e match {
      case wsf @ WSubField(wr: WRef, _, _, _) => concreteRedirects.getOrElse(WrappedExpression(wsf), wsf)
      case e => e map onExpr
    }

    def onStmt(s: Statement): Statement = s match {
      case pInst: WDefInstance if (pInst.module == anno.target.module) =>
        val newName = ns.newName(s"${pInst.name}_${ciName}")
        renames.record(anno.target, anno.target.copy(module = grandparent.name, instance = newName))
        val promotedInst = WDefInstance(NoInfo, newName, anno.target.ofModule, UnknownType)
        def parentPortWE(pPort: WRef) = WrappedExpression(mergeRef(WRef(pInst), pPort))
        def replacePTC(ptc: (WRef, WSubField)) = Connect(NoInfo, setRef(promotedInst.name, ptc._2), inputDrivers(parentPortWE(ptc._1)))
        promotedConns ++= parentAnalysis.parentToChildConns.map(replacePTC(_))
        val defaultConn = PartialConnect(NoInfo, WSubField(WRef(pInst), ciName), WRef(promotedInst))
        concreteRedirects ++= abstractRedirects.map { case (pPort, cPort) => (parentPortWE(pPort), setRef(promotedInst.name, cPort)) }
        Block(Seq(pInst, promotedInst, defaultConn))
       case s => s map onStmt map onExpr
    }

    grandparent foreach analyze
    grandparent.copy(body = Block(Seq(grandparent.body map onStmt) ++ promotedConns))
  }

  def execute(cs: CircuitState): CircuitState = {
    // TODO: what about renames of trimmed ports?
    val renames = RenameMap()
    val myAnnos = cs.annotations.collect { case psa: PromoteSubmoduleAnnotation => psa }
    val moduleMap = (cs.circuit.modules.collect { case m: DefModule => m.name -> m }).toMap
    assert(myAnnos.size <= 1)
    val updatedModules = myAnnos.headOption.map({
      case anno =>
        // Update parent and memoize analyses in TransformedParent object
        assert(anno.target.isLocal)
        val parent = moduleMap(anno.target.module).asInstanceOf[Module]
        val child = moduleMap(anno.target.ofModule)
        val transformedParent = new TransformedParent(anno, parent, child, renames)
        cs.circuit.modules.map {
          case m: Module if (m == parent) => transformedParent.result
          case m: Module if (containsModule(m, parent.name)) => transformGrandparent(anno, transformedParent, m, renames)
          case m => m
        }
    }).getOrElse(cs.circuit.modules)

    val updatedAnnos = cs.annotations.filterNot(a => a.isInstanceOf[PromoteSubmoduleAnnotation])
    CircuitState(cs.circuit.copy(modules = updatedModules), outputForm, updatedAnnos, Some(renames))
  }
}
