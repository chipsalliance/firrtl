// See LICENSE for license details.

package firrtl.passes

import firrtl.{CircuitState, Transform}
import firrtl.ir._
import firrtl.options.{Dependency, IdentityLike, PreservesAll}

object CheckChirrtl extends Pass
    with CheckHighFormLike
    with PreservesAll[Transform]
    with IdentityLike[CircuitState] {

  override val optionalPrerequisiteOf = firrtl.stage.Forms.ChirrtlForm ++
    Seq( Dependency(CInferTypes),
         Dependency(CInferMDir),
         Dependency(RemoveCHIRRTL) )

  def errorOnChirrtl(info: Info, mname: String, s: Statement): Option[PassException] = None
}
