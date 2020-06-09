
package firrtl.passes.formal

import firrtl.passes.Pass
import firrtl.ir.{Verification, Formal, Circuit, Statement}
import firrtl.stage.TransformManager.TransformDependency
import firrtl.Transform
import firrtl.annotations.NoTargetAnnotation


object AssertSubmoduleAssumptions extends Pass {
  override def prerequisites: Seq[TransformDependency] =
    firrtl.stage.Forms.Deduped
  override def invalidates(a: Transform): Boolean = a match {
    case _ => false
  }
  override def optionalPrerequisites = Seq.empty
  override def optionalPrerequisiteOf = firrtl.stage.Forms.MidEmitters

  def assertAssumption(s: Statement): Statement = s match {
    case Verification(Formal.Assume, info, clk, cond, en, msg) =>
      Verification(Formal.Assert, info, clk, cond, en, msg)
    case t => t.mapStmt(assertAssumption)
  }

  def run(c: Circuit): Circuit = {
    c.mapModule(mod => {
      if (mod.name != c.main) {
        mod.mapStmt(assertAssumption)
      } else {
        mod
      }
    })
  }
}


case object DontAssertSubmoduleAssumptionsAnnotation extends NoTargetAnnotation
