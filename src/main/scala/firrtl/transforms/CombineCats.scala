
package firrtl
package transforms

import firrtl.ir._
import firrtl.Mappers._
import firrtl.PrimOps._
import firrtl.WrappedExpression._

import scala.collection.mutable

object CombineCats {
  /** Mapping from references to the [[Expression]]s that drive them */
  type Netlist = mutable.HashMap[WrappedExpression, Expression]

  private val SeqBIOne = Seq(BigInt(1))

  def onExpr(netlist: Netlist)(expr: Expression): Expression =
    expr.map(onExpr(netlist)) match {
      case orig => netlist.getOrElse(we(orig), orig) match {
        case cat @ DoPrim(Cat, _, _, _) => cat
        case _ => orig
      }
      case other => other // Not a candidate
    }

  def onStmt(netlist: Netlist)(stmt: Statement): Statement = {
    stmt.map(onStmt(netlist)) match {
      case node @ DefNode(_, name, value) =>
        val newValue = value match {
          case cat @ DoPrim(Cat, _, _, _) => onExpr(netlist)(cat)
          case other => other
        }
        netlist(we(WRef(name))) = newValue
        node.copy(value = newValue)
      case other => other
    }
  }

  /** Replaces truncating arithmetic in a Module */
  def onMod(mod: DefModule): DefModule = mod.map(onStmt(new Netlist))
}

/** Recombine Cat DoPrims that have been split by SplitExpressions
  *
  * Expands the arguments of any Cat DoPrims if they are references to other Cat DoPrims.
  * Operates only on Cat DoPrims that are node values.
  * */
class CombineCats extends Transform {
  def inputForm = LowForm
  def outputForm = LowForm

  def execute(state: CircuitState): CircuitState = {
    val modulesx = state.circuit.modules.map(CombineCats.onMod)
    val newState = state.copy(circuit = state.circuit.copy(modules = modulesx))
    newState
  }
}

