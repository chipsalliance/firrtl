// See LICENSE for license details.

package firrtl.passes

import firrtl.Transform
import firrtl.ir._
import firrtl.options.{DependencyID, PreservesAll}

object CheckChirrtl extends Pass with CheckHighFormLike with DeprecatedPassObject {

  override protected lazy val underlying = new CheckChirrtl

  def errorOnChirrtl(info: Info, mname: String, s: Statement): Option[PassException] =
    underlying.errorOnChirrtl(info, mname, s)
}

class CheckChirrtl extends Pass with CheckHighFormLike with PreservesAll[Transform] {

  override val dependents = firrtl.stage.Forms.ChirrtlForm ++
    Seq( DependencyID[CInferTypes],
         DependencyID[CInferMDir],
         DependencyID[RemoveCHIRRTL] )

  def errorOnChirrtl(info: Info, mname: String, s: Statement): Option[PassException] = None

}
