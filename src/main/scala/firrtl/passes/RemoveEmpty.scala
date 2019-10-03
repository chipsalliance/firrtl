// See LICENSE for license details.

package firrtl
package passes

import firrtl.ir._

@deprecated("No longer necessary, no-op", "1.3")
object RemoveEmpty extends Pass {
  def run(c: Circuit): Circuit = c
}
