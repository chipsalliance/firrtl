package firrtl
package transforms

import firrtl.ir._
import firrtl.Mappers._
import firrtl.Utils.{isNot, NodeMap}

object InlineNotsTransform {

  /** Recursively replace [[WRef]]s with new [[Expression]]s
    *
    * @param replace a '''mutable''' HashMap mapping [[WRef]]s to values with which the [[WRef]]
    * will be replaced. It is '''not''' mutated in this function
    * @param expr the Expression being transformed
    * @return Returns expr with [[WRef]]s replaced by values found in replace
    */
  def onExpr(replace: NodeMap)(expr: Expression): Expression = {
    expr.map(onExpr(replace)) match {
      case e @ WRef(name, _,_,_) =>
        replace.get(name)
               .filter(isNot)
               .getOrElse(e)
      case other => other // Not a candidate
    }
  }

  /** Inline nots in a Statement
    *
    * @param netlist a '''mutable''' HashMap mapping references to [[firrtl.ir.DefNode DefNode]]s to their connected
    * [[firrtl.ir.Expression Expression]]s. This function '''will''' mutate it if stmt is a [[firrtl.ir.DefNode
    * DefNode]] with a value that is a [[PrimOp]] Not
    * @param stmt the Statement being searched for nodes and transformed
    * @return Returns stmt with nots inlined
    */
  def onStmt(netlist: NodeMap)(stmt: Statement): Statement =
    stmt.map(onStmt(netlist)).map(onExpr(netlist)) match {
      case node @ DefNode(_, name, value) =>
        netlist(name) = value
        node
      case other => other
    }

  /** Inline nots in a Module */
  def onMod(mod: DefModule): DefModule = mod.map(onStmt(new NodeMap))
}

/** Inline nodes that are simple nots */
class InlineNotsTransform extends Transform {
  def inputForm = LowForm
  def outputForm = LowForm

  def execute(state: CircuitState): CircuitState = {
    val modulesx = state.circuit.modules.map(InlineNotsTransform.onMod(_))
    state.copy(circuit = state.circuit.copy(modules = modulesx))
  }
}
