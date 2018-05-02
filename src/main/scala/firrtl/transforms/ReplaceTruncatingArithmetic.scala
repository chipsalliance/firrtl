package firrtl
package transforms

import firrtl.ir._
import firrtl.Mappers._
import firrtl.PrimOps._
import firrtl.WrappedExpression._

import scala.collection.mutable

/** Replaces non-expanding arithmetic
  *
  * This replaces FIRRTL primops with ops that are not legal FIRRTL. They are useful for emission to
  * languages that support non-expanding arithmetic (like Verilog)
  */
object ReplaceTruncatingArithmetic {

  /** Mapping from references to the [[Expression]]s that drive them */
  type Netlist = mutable.HashMap[WrappedExpression, Expression]

  val SeqBIOne = Seq(BigInt(1))

  def onExpr(netlist: Netlist)(expr: Expression): Expression = 
    expr.map(onExpr(netlist)) match {
      case orig @ DoPrim(Tail, Seq(e), SeqBIOne, tailtpe) =>
        netlist.getOrElse(we(e), e) match {
          case DoPrim(Add, args, cs, _) => DoPrim(Addw, args, cs, tailtpe)
          case DoPrim(Sub, args, cs, _) => DoPrim(Subw, args, cs, tailtpe)
          case _ => orig // Not a candidate
        }
      case other => other // Not a candidate
    }

  def onStmt(netlist: Netlist)(stmt: Statement): Statement =
    stmt.map(onStmt(netlist)).map(onExpr(netlist)) match {
      case node @ DefNode(_, name, value) =>
        netlist(we(WRef(name))) = value
        node
      case other => other
    }
  def onMod(mod: DefModule): DefModule = mod.map(onStmt(new Netlist))
}

class ReplaceTruncatingArithmetic extends Transform {
  def inputForm = LowForm
  def outputForm = LowForm

  def execute(state: CircuitState): CircuitState = {
    val modulesx = state.circuit.modules.map(ReplaceTruncatingArithmetic.onMod(_))
    state.copy(circuit = state.circuit.copy(modules = modulesx))
  }
}

