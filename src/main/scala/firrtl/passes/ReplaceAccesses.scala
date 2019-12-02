// See LICENSE for license details.

package firrtl.passes

import firrtl.Transform
import firrtl.ir._
import firrtl.{WSubAccess, WSubIndex}
import firrtl.Mappers._
import firrtl.options.{DependencyID, PreservesAll}

/** Replaces constant [[firrtl.WSubAccess]] with [[firrtl.WSubIndex]]
  * TODO Fold in to High Firrtl Const Prop
  */
class ReplaceAccesses extends Pass with PreservesAll[Transform] {

  override val prerequisites = firrtl.stage.Forms.Deduped :+ DependencyID[PullMuxes]

  def run(c: Circuit): Circuit = {
    def onStmt(s: Statement): Statement = s map onStmt map onExp
    def onExp(e: Expression): Expression = e match {
      case WSubAccess(ex, UIntLiteral(value, width), t, g) => WSubIndex(onExp(ex), value.toInt, t, g)
      case _ => e map onExp
    }

    c copy (modules = c.modules map (_ map onStmt))
  }
}

object ReplaceAccesses extends Pass with DeprecatedPassObject {

  override protected lazy val underlying = new ReplaceAccesses

}
