// See LICENSE for license details.

package firrtl.passes

import firrtl.Transform
import firrtl.ir._
import firrtl.options.{Dependency, PreservesAll}
import firrtl.stage.CircuitPhase

object CheckChirrtl extends Pass with CheckHighFormLike with PreservesAll[CircuitPhase] {

  override val dependents = firrtl.stage.Forms.ChirrtlForm ++
    Seq( Dependency(CInferTypes),
         Dependency(CInferMDir),
         Dependency(RemoveCHIRRTL) )

  def errorOnChirrtl(info: Info, mname: String, s: Statement): Option[PassException] = None
}
