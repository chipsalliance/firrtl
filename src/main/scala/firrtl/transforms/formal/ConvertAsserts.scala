// See LICENSE for license details.

package firrtl.transforms.formal

import firrtl._
import firrtl.ir._
import firrtl.options.Dependency

/**
  * Convert Asserts
  *
  * Replaces all Assert nodes with a conditional print-and-stop. This effectively
  * emulates the assert for IEEE 1364 Verilog.
  */
object ConvertAsserts extends Transform with DependencyAPIMigration {
  override def prerequisites = Nil
  override def optionalPrerequisites = Nil
  override def optionalPrerequisiteOf = Seq(
    Dependency[VerilogEmitter],
    Dependency[MinimumVerilogEmitter],
    Dependency[RemoveVerificationStatements])

  override def invalidates(a: Transform): Boolean = false

  def convertAsserts(stmt: Statement): Statement = stmt match {
    case Verification(Formal.Assert, i, clk, pred, en, msg) =>
      val nPred = DoPrim(PrimOps.Not, Seq(pred), Nil, pred.tpe)
      Conditionally(i, nPred, Block(Print(i, msg, Nil, clk, en), Stop(i, 1, clk, en)), EmptyStmt)
      case s => s.mapStmt(convertAsserts)
  }

  def execute(state: CircuitState): CircuitState = {
    state.copy(circuit = state.circuit.mapModule(m => m.mapStmt(convertAsserts)))
  }
}
