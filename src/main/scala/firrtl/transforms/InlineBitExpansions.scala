package firrtl
package transforms

import firrtl.ir._
import firrtl.Mappers._
import firrtl.PrimOps.{AsUInt, Pad, Not}
import firrtl.Utils.{isBitExpand, isTemp}
import firrtl.WrappedExpression._

import scala.collection.mutable

object InlineBitExpansionsTransform {

  // Checks if an Expression is made up of only bit expansions terminated by a Literal or Reference.
  // private because it's not clear if this definition of "Simple Expression" would be useful elsewhere.
  // Note that this can have false negatives but MUST NOT have false positives.
  private def isSimpleExpr(expr: Expression): Boolean = expr match {
    case _: WRef | _: Literal | _: WSubField => true
    case DoPrim(_, args, _,_) if isBitExpand(expr) => args.forall(isSimpleExpr)
    case _ => false
  }

  /** Mapping from references to the [[firrtl.ir.Expression Expression]]s that drive them */
  type Netlist = mutable.HashMap[WrappedExpression, Expression]

  /** Recursively replace [[WRef]]s with new [[Expression]]s
    *
    * @param netlist a '''mutable''' HashMap mapping references to [[firrtl.ir.DefNode DefNode]]s to their connected
    * [[firrtl.ir.Expression Expression]]s. It is '''not''' mutated in this function
    * @param expr the Expression being transformed
    * @return Returns expr with Andr/Orr/Xorr inlined
    */
  def onExpr(netlist: Netlist)(expr: Expression): Expression = {
    expr.map(onExpr(netlist)) match {
      case e @ WRef(name, _,_,_) =>
        netlist.get(we(e))
               .filter(isBitExpand)
               .getOrElse(e)
      case lhs @ Mux(cond, UIntLiteral(tval,IntWidth(tw)), UIntLiteral(fval,IntWidth(fw)), tpe) if (bitWidth(cond.tpe) == 1) && (tw == fw) => cond match {
        case _: WRef | _: Literal | _: WSubField => cond.tpe match {
          case (_: UIntType) if (tval == (BigInt(1) << tw.toInt) - 1) && (fval == BigInt(0)) =>
            DoPrim(AsUInt, Seq(DoPrim(Pad, Seq(cond), Seq(bitWidth(tpe)), SIntType(IntWidth(1)))), Seq(), tpe)
          case (_: UIntType) if (tval == BigInt(0)) && (fval == (BigInt(1) << fw.toInt) - 1) =>
            DoPrim(Not, Seq(DoPrim(Pad, Seq(cond), Seq(bitWidth(tpe)), SIntType(IntWidth(1)))), Seq(), tpe)
          case _ => lhs // Not a candidate
        }
        case _ => lhs // Not a candidate
      }
      case other => other // Not a candidate
    }
  }

  /** Inline bit expansions in a Statement
    *
    * @param netlist a '''mutable''' HashMap mapping references to [[firrtl.ir.DefNode DefNode]]s to their connected
    * [[firrtl.ir.Expression Expression]]s. This function '''will''' mutate it if stmt is a [[firrtl.ir.DefNode
    * DefNode]] with a Temporary name and a value that is a [[PrimOp]] bit expansion
    * @param stmt the Statement being searched for nodes and transformed
    * @return Returns stmt with bit expansions inlined
    */
  def onStmt(netlist: Netlist)(stmt: Statement): Statement =
    stmt.map(onStmt(netlist)).map(onExpr(netlist)) match {
      case node @ DefNode(_, name, value) if isTemp(name) =>
        netlist(we(WRef(name))) = value
        node
      case other => other
    }

  /** Replaces bit expansions in a Module */
  def onMod(mod: DefModule): DefModule = mod.map(onStmt(new Netlist))
}

/** Inline nodes that are simple bit expansions */
class InlineBitExpansionsTransform extends Transform {
  def inputForm = UnknownForm
  def outputForm = UnknownForm

  def execute(state: CircuitState): CircuitState = {
    val modulesx = state.circuit.modules.map(InlineBitExpansionsTransform.onMod(_))
    state.copy(circuit = state.circuit.copy(modules = modulesx))
  }
}
