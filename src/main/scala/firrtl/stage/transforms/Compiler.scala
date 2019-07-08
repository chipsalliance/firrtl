// See LICENSE for license details.

package firrtl.stage.transforms

import firrtl.{CircuitState, Transform}
import firrtl.stage.TransformManager

class Compiler(
  targets: Seq[TransformManager.TransformDependency],
  currentState: Seq[TransformManager.TransformDependency] = Seq.empty,
  knownObjects: Set[Transform] = Set.empty) extends TransformManager(targets, currentState, knownObjects) {

  override val wrappers = Seq(
    (a: Transform) => CatchCustomTransformExceptions(a),
    (a: Transform) => TrackTransforms(a),
    (a: Transform) => UpdateAnnotations(a)
  )

}
