// See LICENSE for license details.

package firrtl.transforms

import firrtl.{CircuitState, DependencyAPIMigration, Transform}
import firrtl.ir._
import firrtl.options.Dependency
import firrtl.PrimOps.Neg

/** Constant propagates negation of negative literals
  *
  * Negation of negative literals leads to illegal verilog. For example,
  * neg(SInt<2>(-1)) would emit '--2'sh1' if it were not constant propagated.
  */
class FixNegatingNegativeLiterals extends Transform with DependencyAPIMigration {

  override def prerequisites = Seq(
    Dependency[firrtl.passes.InferWidths],
    Dependency(firrtl.passes.InferTypes)
  )

  override def optionalPrerequisites = Seq.empty

  override def optionalPrerequisiteOf = Seq.empty

  override def invalidates(a: Transform) = false

  private def foldNeg(e: DoPrim) = e.args.head match {
    case UIntLiteral(v, IntWidth(w)) => SIntLiteral(-v, IntWidth(w + 1))
    case SIntLiteral(v, IntWidth(w)) => SIntLiteral(-v, IntWidth(w + 1))
    case _ => e
  }

  private def fixupExpr(e: Expression): Expression = {
    e.mapExpr(fixupExpr) match {
      case neg@ DoPrim(Neg, _, _, _) => foldNeg(neg)
      case _ => e
    }
  }
  private def fixupStmt(stmt: Statement): Statement = {
    stmt.mapStmt(fixupStmt).mapExpr(fixupExpr)
  }

  def execute(state: CircuitState): CircuitState = {
    val modulesx = state.circuit.modules.map(_.mapStmt(fixupStmt))
    state.copy(circuit = state.circuit.copy(modules = modulesx))
  }
}
