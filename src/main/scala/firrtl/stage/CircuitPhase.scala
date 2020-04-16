// See LICENSE for license details.

package firrtl.stage

import firrtl.CircuitState
import firrtl.options.{DependencyAPI, TransformLike}

abstract class CircuitPhase extends TransformLike[CircuitState] with DependencyAPI[CircuitPhase] {

  override def name: String = this.getClass.getName

}
