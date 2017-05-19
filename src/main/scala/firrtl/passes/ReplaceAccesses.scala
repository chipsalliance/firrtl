// See LICENSE for license details.

package firrtl.passes

import firrtl.ir.Mappers._
import firrtl.ir._
import firrtl.transforms.core.{WSubAccess, WSubIndex}



/** Replaces constant [[WSubAccess]] with [[WSubIndex]]
  * TODO Fold in to High Firrtl Const Prop
  */
object ReplaceAccesses extends Pass {
  def run(c: Circuit): Circuit = {
    def onStmt(s: Statement): Statement = s map onStmt map onExp
    def onExp(e: Expression): Expression = e match {
      case WSubAccess(ex, UIntLiteral(value, width), t, g) => WSubIndex(ex, value.toInt, t, g)
      case _ => e map onExp
    }
  
    c copy (modules = c.modules map (_ map onStmt))
  }
}
