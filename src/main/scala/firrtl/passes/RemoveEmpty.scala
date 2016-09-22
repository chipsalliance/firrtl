package firrtl
package passes

import scala.collection.mutable
import firrtl.Mappers._
import firrtl.ir._

object RemoveEmpty extends Pass {
  def name = "Remove Empty Statements"
  private def onModule(m: DefModule): DefModule = m map Utils.squashEmpty
  def run(c: Circuit): Circuit = c copy(modules=c.modules.map(onModule _))
}
