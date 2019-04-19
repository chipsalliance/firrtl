// See LICENSE for license details.

package firrtl.stage

import firrtl.{CircuitForm, CircuitState, Transform, UnknownForm}
import firrtl.options.DependencyManager

class TransformManager(
  val targets: Seq[Class[Transform]],
  val currentState: Seq[Class[Transform]] = Seq.empty,
  val knownObjects: Set[Transform] = Set.empty) extends Transform with DependencyManager[CircuitState, Transform] {

  override def inputForm: CircuitForm = UnknownForm

  override def outputForm: CircuitForm = UnknownForm

  override def execute(state: CircuitState): CircuitState = transform(state)

  override protected def copy(a: Seq[Class[Transform]], b: Seq[Class[Transform]], c: Set[Transform]) =
    new TransformManager(a, b, c)

}
