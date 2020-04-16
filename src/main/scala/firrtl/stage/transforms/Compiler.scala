// See LICENSE for license details.

package firrtl.stage.transforms

import firrtl.options.DependencyManagerUtils.CharSet
import firrtl.stage.{CircuitPhase, TransformManager}
import firrtl.{Transform, VerilogEmitter}

class Compiler(
  targets: Seq[TransformManager.TransformDependency],
  currentState: Seq[TransformManager.TransformDependency] = Seq.empty,
  knownObjects: Set[CircuitPhase] = Set.empty) extends TransformManager(targets, currentState, knownObjects) {

  override val wrappers = Seq(
    (a: CircuitPhase) => CatchCustomTransformExceptions(a),
    (a: CircuitPhase) => UpdateAnnotations(a)
  )

}
