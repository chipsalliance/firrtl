// See LICENSE for license details.

package firrtl
package transforms

import firrtl.ir._
import firrtl.Mappers._
import firrtl.WrappedExpression._
import firrtl.Utils._

import scala.collection.mutable

object RestructureMuxes {

  private def AND(e1: Expression, e2: Expression): DoPrim = DoPrim(PrimOps.And, Seq(e1, e2), Nil, BoolType)
  private def NOT(e1: Expression): DoPrim = DoPrim(PrimOps.Not, Seq(e1), Nil, BoolType)

  /** Mapping from references to the [[Expression]]s that drive them */
  type Netlist = mutable.HashMap[WrappedExpression, Expression]

  /** Restructures nested mux expressions where the two muxes have a common input
    *
    * This function is recursive
    *
    * @param netlist A netlist lookup to get expressions refered to with a reference, NOT mutated in
    *   this function
    * @param expr The expression to transform
    * @return The transformed expression
    */
  def onExpr(netlist: Netlist)(expr: Expression): Expression = expr match {
    case orig @ Mux(c1, tval1, fval1, mtpe) =>
      (netlist.getOrElse(tval1, tval1) match {
        // r <= mux(a, mux(b, s, r), r) ->
        // r <= mux(and(a, b), s, r)
        case Mux(c2, tval2, fval2, _) if we(fval1) == we(fval2) =>
          Some(Mux(AND(c1, c2), tval2, fval2, mtpe))
        // r <= mux(a, mux(b, r, s), r)
        // r <= mux(and(a, not(b)), s, r)
        case Mux(c2, tval2, fval2, _) if we(fval1) == we(tval2) =>
          Some(Mux(AND(c1, NOT(c2)), fval2, tval2, mtpe))
        case _ => None
      }).orElse(netlist.getOrElse(fval1, fval1) match {
        // r <= mux(a, r, mux(b, s, r))
        // r <= mux(and(not(a), b), s, r)
        case Mux(c2, tval2, fval2, _) if we(tval1) == we(fval2) =>
          Some(Mux(AND(NOT(c1), c2), tval2, fval2, mtpe))
        // r <= mux(a, r, mux(b, r, s))
        // r <= mux(or(a, b), s, r)
        case Mux(c2, tval2, fval2, _) if we(tval1) == we(tval2) =>
          Some(Mux(AND(NOT(c1), NOT(c2)), fval2, tval2, mtpe))
        case _ => None
      }).getOrElse(orig)
    case other => other
  }

  /** Restructures nested mux expressions where the two muxes have a common input
    *
    * This function is recursive
    *
    * @param netlist A netlist lookup to get expressions refered to with a reference, entries are
    *   added in this function!
    * @param stmt The statement to transform
    * @return The transformed statement
    */
  def onStmt(netlist: Netlist)(stmt: Statement): Statement =
    stmt.map(onStmt(netlist)).map(onExpr(netlist)) match {
      case node @ DefNode(_, name, value) =>
        netlist(WRef(name)) = value
        node
      case other => other
    }

  /** Restructures nested mux expressions where the two muxes have a common input */
  def onModule(mod: DefModule): DefModule = {
    mod.map(onStmt(new Netlist))
  }
}

/** Restructure Muxes
  *
  * Combines conditions of nested muxes that have an input in common into a single mux
  */
class RestructureMuxes extends Transform {
  def inputForm = LowForm
  def outputForm = LowForm

  def execute(state: CircuitState): CircuitState = {
    val circuitx = state.circuit.map(RestructureMuxes.onModule)
    state.copy(circuit = circuitx)
  }
}
