// See LICENSE for license details.

package firrtl.transforms

import firrtl.Mappers._
import firrtl.Utils._
import firrtl._
import firrtl.annotations._
import firrtl.ir._
import firrtl.transforms.ConstantPropagation._

import scala.collection.mutable

class HighFormConstProp extends Transform {
  private def constPropModule(m: Module, dontTouches: Set[String], renames: RenameMap, noDCE: Boolean): Module = {
    renames.setModule(m.name)
    val nodeMap = new NodeMap()
    // For cases where we are trying to constprop a bad name over a good one, we swap their names
    // during the second pass
    val swapMap = mutable.HashMap.empty[String, String]
    // Keep track of references so we know which nodes can be removed
    val keepSet = mutable.HashSet.empty[String]

    def deleteNode(name: String, tpe: Type): Unit = tpe match {
      case b: BundleType => b.fields.foreach(f => deleteNode(name + "." + f.name, f.tpe))
      case v: VectorType => (0 until v.size).foreach(i => deleteNode(s"$name[$i]", v.tpe))
      case g: GroundType => renames.delete(name)
    }

    def swapNamesExpr(e: Expression): Expression = e map swapNamesExpr match {
      case ref@WRef(rname, _, _, _) if swapMap.contains(rname) => ref.copy(name = swapMap(rname))
      case other => other
    }

    def swapNamesStmt(s: Statement): Statement = s map swapNamesExpr match {
      case DefNode(_, name, value) if !keepSet.contains(name) && !noDCE && !dontTouches.contains(name) =>
        deleteNode(name, value.tpe)
        renames.delete(name)
        EmptyStmt
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

      propagated match {
        case WRef(name, _, _, _) => keepSet.add(name)
        case _=>
      }

      propagated
    }

    def constPropStmt(s: Statement): Statement = {
      s.map(constPropStmt).map(constPropExpression(nodeMap)) match {
        case node: DefNode if dontTouches.contains(node.name) => node
        // if the rhs is a ref with a worse name then delete this node and rename the rhs to this node's name
        case x@DefNode(_, lname, ref@WRef(rname, _, kind, _)) if
        betterName(lname, rname) && kind != PortKind && !swapMap.contains(rname) && !noDCE =>
          swapMap += (rname -> lname)
          nodeMap(lname) = nodeMap.getOrElse(rname, ref.copy(name = lname))
          deleteNode(rname, ref.tpe)
          EmptyStmt
        // if the rhs is a ref with a better name then delete this node and propagate the rhs
        case x@DefNode(_, lname, ref@WRef(rname, _, _, _)) if betterName(rname, lname) && !noDCE =>
          assert(!swapMap.contains(lname))
          swapMap += (lname -> rname)
          nodeMap(lname) = nodeMap.getOrElse(rname, ref.copy(name = rname))
          deleteNode(lname, ref.tpe)
          EmptyStmt
        // if this node is bound to a constant or a ref then delete it and propagate the constant
        case x@DefNode(_, lname, _: UIntLiteral | _: SIntLiteral | _: WRef) if !noDCE =>
          nodeMap(lname) = x.value
          deleteNode(lname, x.value.tpe)
          EmptyStmt
        case x@DefNode(_, lname, value) =>
          nodeMap(lname) = value
          x
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
    val dontTouchMap: Map[String, Set[String]] = dontTouches.groupBy(_._1)
      .mapValues(_.map(_._2.takeWhile(c => c != '.' && c != '[')).toSet)
    val renamesx = RenameMap()
    renamesx.setCircuit(state.circuit.main)

    val noDCE = state.annotations.contains(NoDCEAnnotation)
    val modulesx = state.circuit.modules.map {
      case m: Module =>
        constPropModule(m, dontTouchMap.getOrElse(m.name, Set.empty), renamesx, noDCE)
      case e: ExtModule => e
    }

    state.copy(circuit = state.circuit.copy(modules = modulesx), renames = Some(renamesx))
  }
}
