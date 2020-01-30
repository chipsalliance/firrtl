package firrtl
package transforms

import firrtl.ir._
import firrtl.Mappers._
import firrtl.PrimOps.{Andr, Not, Orr, Xorr}
import firrtl.Utils.{isBitReduce, isTemp}
import firrtl.WrappedExpression._

import scala.collection.mutable

object InlineBitReductionsTransform {

  // Checks if an Expression is made up of only bit reductions terminated by a Literal or Reference.
  // private because it's not clear if this definition of "Simple Expression" would be useful elsewhere.
  // Note that this can have false negatives but MUST NOT have false positives.
  private def isSimpleExpr(expr: Expression): Boolean = expr match {
    case _: WRef | _: Literal | _: WSubField => true
    case DoPrim(op, args, _,_) if isBitReduce(op) => args.forall(isSimpleExpr)
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
               .filter(isBitReduce)
               .getOrElse(e)
      // De Morgan's: replace &~ with ~|
      case lhs @ DoPrim(Andr, Seq(inv), _, ltpe) =>
        netlist.getOrElse(we(inv), inv) match {
          case DoPrim(Not, Seq(rhs), _, rtpe) =>
            DoPrim(Not, Seq(DoPrim(Orr, Seq(rhs), Seq(), ltpe)), Seq(), ltpe)
          case _ => lhs  // Not a candiate
        }
      // De Morgan's: replace |~ with ~&
      case lhs @ DoPrim(Orr, Seq(inv), _, ltpe) =>
        netlist.getOrElse(we(inv), inv) match {
          case DoPrim(Not, Seq(rhs), _, rtpe) =>
            DoPrim(Not, Seq(DoPrim(Andr, Seq(rhs), Seq(), ltpe)), Seq(), ltpe)
          case _ => lhs  // Not a candiate
        }
      // commutative: replace ^~ with ~^
      case lhs @ DoPrim(Xorr, Seq(inv), _, ltpe) =>
        netlist.getOrElse(we(inv), inv) match {
          case DoPrim(Not, Seq(rhs), _, rtpe) =>
            DoPrim(Not, Seq(DoPrim(Xorr, Seq(rhs), Seq(), ltpe)), Seq(), ltpe)
          case _ => lhs  // Not a candiate
        }
      case other => other // Not a candidate
    }
  }

  /** Inline bit reductions in a Statement
    *
    * @param netlist a '''mutable''' HashMap mapping references to [[firrtl.ir.DefNode DefNode]]s to their connected
    * [[firrtl.ir.Expression Expression]]s. This function '''will''' mutate it if stmt is a [[firrtl.ir.DefNode
    * DefNode]] with a Temporary name and a value that is a [[PrimOp]] bit reduction
    * @param stmt the Statement being searched for nodes and transformed
    * @return Returns stmt with bit reductions inlined
    */
  def onStmt(netlist: Netlist)(stmt: Statement): Statement =
    stmt.map(onStmt(netlist)).map(onExpr(netlist)) match {
      case node @ DefNode(_, name, value) if isTemp(name) =>
        netlist(we(WRef(name))) = value
        node
      case other => other
    }

  /** Replaces bit reductions in a Module */
  def onMod(mod: DefModule): DefModule = mod.map(onStmt(new Netlist))
}

/** Inline nodes that are simple bit reductions */
class InlineBitReductionsTransform extends Transform {
  def inputForm = UnknownForm
  def outputForm = UnknownForm

  def execute(state: CircuitState): CircuitState = {
    val modulesx = state.circuit.modules.map(InlineBitReductionsTransform.onMod(_))
    state.copy(circuit = state.circuit.copy(modules = modulesx))
  }
}
