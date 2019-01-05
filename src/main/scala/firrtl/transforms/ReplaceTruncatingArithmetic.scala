package firrtl
package transforms

import firrtl.ir._
import firrtl.analyses._
import firrtl.Mappers._
import firrtl.PrimOps._
import firrtl.WrappedExpression._

import scala.collection.mutable

object ReplaceTruncatingArithmetic {

  private val SeqBIOne = Seq(BigInt(1))

  /** Replaces truncating arithmetic in an Expression
    *
    * @param expr the Expression being transformed
    * @return Returns expr with truncating arithmetic replaced
    */
  def onExpr(main: String, mod: String, lookup: IRLookup)(expr: Expression): Expression =
    expr.map(onExpr(main, mod, lookup)) match {
      case orig @ DoPrim(Tail, Seq(e), SeqBIOne, tailtpe) =>
        e match {
          case ref: WRef =>
            val target = Utils.toTarget(main, mod)(ref)
            lookup.declaration(target) match {
              case DefNode(_,_, DoPrim(Add, args, cs, _)) => DoPrim(Addw, args, cs, tailtpe)
              case DefNode(_,_, DoPrim(Sub, args, cs, _)) => DoPrim(Subw, args, cs, tailtpe)
              case _ => orig // Not a candidate
            }
          case other => other
        }
      case other => other
    }

  /** Replaces truncating arithmetic in a Statement
    *
    * @param stmt the Statement being searched for nodes and transformed
    * @return Returns stmt with truncating arithmetic replaced
    */
  def onStmt(main: String, mod: String, lookup: IRLookup)(stmt: Statement): Statement =
    stmt.map(onStmt(main, mod, lookup)).map(onExpr(main, mod, lookup))

  /** Replaces truncating arithmetic in a Module */
  def onMod(main: String, lookup: IRLookup)(mod: DefModule): DefModule =
    mod.map(onStmt(main, mod.name, lookup))
}

/** Replaces non-expanding arithmetic
  *
  * In the case where the result of `add` or `sub` immediately throws away the expanded msb, this
  * transform will replace the operation with a non-expanding operator `addw` or `subw`
  * respectively.
  *
  * @note This replaces some FIRRTL primops with ops that are not actually legal FIRRTL. They are
  * useful for emission to languages that support non-expanding arithmetic (like Verilog)
  */
class ReplaceTruncatingArithmetic extends Transform {
  def inputForm = LowForm
  def outputForm = LowForm

  def execute(state: CircuitState): CircuitState = {
    val lookup = IRLookup(state.circuit)
    val modulesx = state.circuit.modules.map(ReplaceTruncatingArithmetic.onMod(state.circuit.main, lookup))
    state.copy(circuit = state.circuit.copy(modules = modulesx))
  }
}

