
package firrtl.transforms.formal

import firrtl.ir.{Circuit, EmptyStmt, Statement, Verification}
import firrtl.{CircuitState, DependencyAPIMigration, MinimumVerilogEmitter, Transform, VerilogEmitter}
import firrtl.options.{Dependency, PreservesAll}
import firrtl.stage.TransformManager.TransformDependency


/**
  * Remove Verification Statements
  *
  * Replaces all verification statements in all modules with the empty statement.
  * This is intended to be required by the Verilog emitter to ensure compatibility
  * with the Verilog 2001 standard.
  */
class RemoveVerificationStatements extends Transform
  with DependencyAPIMigration
  with PreservesAll[Transform] {

  override def prerequisites: Seq[TransformDependency] = Seq.empty
  override def optionalPrerequisites: Seq[TransformDependency] = Seq.empty
  override def optionalPrerequisiteOf: Seq[TransformDependency] =
    Seq( Dependency[VerilogEmitter],
      Dependency[MinimumVerilogEmitter])

  def removeVerification(s: Statement): Statement = s match {
    case _: Verification => EmptyStmt
    case t => t.mapStmt(removeVerification)
  }

  def run(c: Circuit): Circuit = {
    c.mapModule(mod => {
      mod.mapStmt(removeVerification)
    })
  }

  def execute(state: CircuitState): CircuitState = {
    state.copy(circuit = run(state.circuit))
  }
}
