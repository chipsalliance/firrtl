
package firrtl.passes.formal

import firrtl.passes.Pass
import firrtl.ir.{Assert, Assume, Circuit, Statement}
import firrtl.stage.TransformManager.TransformDependency
import firrtl.Transform


object AssertSubmoduleAssumptions extends Pass {
  override def prerequisites: Seq[TransformDependency] =
    firrtl.stage.Forms.Deduped

  override def invalidates(a: Transform): Boolean = a match {
    case _ => false
  }

  def assertAssumption(s: Statement): Statement = s match {
    case Assume(info, clk, cond, en, msg) => Assert(info, clk, cond, en, msg)
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
