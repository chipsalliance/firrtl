// See LICENSE for license details.

package firrtl.stage.transforms

import firrtl.stage.CircuitPhase

/** A [[firrtl.stage.Circuitphase]] that "wraps" a second [[firrtl.Transform Transform]] to do some work before and after the
  * second [[firrtl.stage.CircuitPhase CircuitPhase]].
  *
  * This is intended to synergize with the [[firrtl.options.DependencyManager.wrappers]] method.
  * @see [[firrtl.stage.transforms.CatchCustomTransformExceptions]]
  * @see [[firrtl.stage.transforms.TrackTransforms]]
  * @see [[firrtl.stage.transforms.UpdateAnnotations]]
  */
trait WrappedTransform { this: CircuitPhase =>

  /** The underlying [[firrtl.Transform]] */
  val underlying: CircuitPhase

  /** Return the original [[firrtl.Transform]] if this wrapper is wrapping other wrappers. */
  lazy final val trueUnderlying: CircuitPhase = underlying match {
    case a: WrappedTransform => a.trueUnderlying
    case _ => underlying
  }

  override final val prerequisites = underlying.prerequisites
  override final val dependents = underlying.dependents
  override final def invalidates(b: CircuitPhase): Boolean = underlying.invalidates(b)
  override final lazy val name = s"${underlying.name}.wrappedWith(${this.getClass.getName})"

}
