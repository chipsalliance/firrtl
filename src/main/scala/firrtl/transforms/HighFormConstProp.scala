// See LICENSE for license details.

package firrtl.transforms

import firrtl.Mappers._
import firrtl.Utils._
import firrtl._
import firrtl.annotations._
import firrtl.ir._
import ConstantPropagation._

import scala.collection.mutable

class HighFormConstProp extends Transform {
  private def constPropModule(m: Module, dontTouches: Set[String]): Module = {
    val nodeMap = new NodeMap()
    // For cases where we are trying to constprop a bad name over a good one, we swap their names
    // during the second pass
    val swapMap = mutable.HashMap.empty[String, String]

    def swapNamesExpr(e: Expression): Expression = e map swapNamesExpr match {
      case ref@WRef(rname, _, _, _) if swapMap.contains(rname) => ref.copy(name = swapMap(rname))
      case other => other
    }

    def swapNamesStmt(s: Statement): Statement = s map swapNamesExpr match {
      case decl: IsDeclaration if swapMap.contains(decl.name) =>
        val newName = swapMap(decl.name)
        decl match {
          case node: DefNode => node.copy(name = newName)
          case wire: DefWire => wire.copy(name = newName)
          case reg: DefRegister => reg.copy(name = newName)
          case other => throwInternalError()
        }
      case other => other map swapNamesStmt
    }

    def constPropExpression(nodeMap: NodeMap)(e: Expression): Expression = {
      val old = e map constPropExpression(nodeMap)
      val propagated = old match {
        case p: DoPrim => constPropPrim(p)
        case m: Mux => constPropMux(m)
        case ref@WRef(rname, _, _, _) if nodeMap.contains(rname) => constPropNodeRef(ref, nodeMap(rname))
        case x => x
      }
      propagated
    }

    def constPropStmt(s: Statement): Statement = {
      s.map(constPropStmt).map(constPropExpression(nodeMap)) match {
        case node: DefNode if dontTouches.contains(node.name) => node
        // if the rhs is a ref with a worse name then delete this node and rename the rhs to this node's name
        case x@DefNode(_, lname, ref@WRef(rname, _, kind, _)) if
        betterName(lname, rname) && kind != PortKind && !swapMap.contains(rname) =>
          swapMap += (rname -> lname)
          nodeMap(lname) = nodeMap.getOrElse(rname, ref.copy(name = lname))
          EmptyStmt
        // if the rhs is a ref with a better name then delete this node and propagate the rhs
        case x@DefNode(_, lname, ref@WRef(rname, _, _, _)) if betterName(rname, lname) =>
          swapMap += (lname -> rname)
          nodeMap(lname) = nodeMap.getOrElse(rname, ref.copy(name = rname))
          EmptyStmt
        // if this node bound to a constant or a ref then delete it and propagate the constant
        case x@DefNode(_, lname, value) =>
          nodeMap(lname) = value
          value match {
            case _: UIntLiteral | _: SIntLiteral | _: WRef => EmptyStmt
            case _ => x
          }
        case other => other
      }
    }

    m.copy(body = swapNamesStmt(constPropStmt(m.body)))
  }

  def inputForm: CircuitForm = HighForm
  def outputForm: CircuitForm = HighForm

  def execute(state: CircuitState): CircuitState = {
    val dontTouches: Seq[(String, String)] = state.annotations.collect {
      case DontTouchAnnotation(ComponentName(c, ModuleName(m, _))) => m -> c
    }
    // Map from module name to component names
    val dontTouchMap: Map[String, Set[String]] = dontTouches.groupBy(_._1).mapValues(_.map(_._2).toSet)

    val modulesx = state.circuit.modules.map {
      case m: Module => constPropModule(m, dontTouchMap.getOrElse(m.name, Set.empty))
      case e: ExtModule => e
    }

    state.copy(circuit = state.circuit.copy(modules = modulesx))
  }
}
