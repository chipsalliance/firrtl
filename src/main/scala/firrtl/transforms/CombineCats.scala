
package firrtl
package transforms

import firrtl.ir._
import firrtl.Mappers._
import firrtl.PrimOps._
import firrtl.WrappedExpression._

import scala.collection.mutable

object CombineCats {
  /** Mapping from references to the [[Expression]]s that drive them */
  type Netlist = mutable.HashMap[WrappedExpression, (Int, Expression)]

  def expandCatArgs(maxCatLen: Int, netlist: Netlist)(expr: Expression): (Int, Expression) = expr match {
    case cat@DoPrim(Cat, args, _, _) =>
      val (a0Len, a0Expanded) = expandCatArgs(maxCatLen - 1, netlist)(args.head)
      val (a1Len, a1Expanded) = expandCatArgs(maxCatLen - a0Len, netlist)(args(1))
      (a0Len + a1Len, cat.copy(args = Seq(a0Expanded, a1Expanded)).asInstanceOf[Expression])
    case other =>
      netlist.get(we(expr)).collect {
        case (len, cat@DoPrim(Cat, _, _, _)) if maxCatLen >= len => expandCatArgs(maxCatLen, netlist)(cat)
      }.getOrElse((1, other))
  }

  def onStmt(maxCatLen: Int, netlist: Netlist)(stmt: Statement): Statement = {
    stmt.map(onStmt(maxCatLen, netlist)) match {
      case node@DefNode(_, name, value) =>
        val catLenAndVal = value match {
          case cat@DoPrim(Cat, _, _, _) => expandCatArgs(maxCatLen, netlist)(cat)
          case other => (1, other)
        }
        netlist(we(WRef(name))) = catLenAndVal
        node.copy(value = catLenAndVal._2)
      case other => other
    }
  }

  def onMod(maxCatLen: Int)(mod: DefModule): DefModule = mod.map(onStmt(maxCatLen, new Netlist))
}

/** Recombine Cat DoPrims that have been split by SplitExpressions
  *
  * Expands the arguments of any Cat DoPrims if they are references to other Cat DoPrims.
  * Operates only on Cat DoPrims that are node values.
  */
class CombineCats(maxCatLen: Int) extends Transform {
  def inputForm = LowForm
  def outputForm = LowForm

  def execute(state: CircuitState): CircuitState = {
    val modulesx = state.circuit.modules.map(CombineCats.onMod(maxCatLen))
    val newState = state.copy(circuit = state.circuit.copy(modules = modulesx))
    newState
  }
}

