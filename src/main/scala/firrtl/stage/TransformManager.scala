// See LICENSE for license details.

package firrtl.stage

import firrtl.{CircuitForm, CircuitState, Transform, UnknownForm}
import firrtl.options.{Dependency, DependencyManager}
import TransformManager._

object TransformManager {

  /** The type used to represent dependencies between [[Transform]]s */
  type TransformDependency = Dependency[CircuitPhase]

}

/** A [[Transform]] that ensures some other [[Transform]]s and their prerequisites are executed.
  *
  * @param targets the transforms you want to run
  * @param currentState the transforms that have already run
  * @param knownObjects existing transform objects that have already been constructed
  */
class TransformManager(
  val targets: Seq[TransformDependency],
  val currentState: Seq[TransformDependency] = Seq.empty,
  val knownObjects: Set[CircuitPhase] = Set.empty) extends CircuitPhase with DependencyManager[CircuitState, CircuitPhase] {

  override protected def copy(
    a: Seq[TransformDependency],
    b: Seq[TransformDependency],
    c: Set[CircuitPhase]) = new TransformManager(a, b, c)

}
