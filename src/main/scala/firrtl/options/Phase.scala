// See LICENSE for license details.

package firrtl.options

import firrtl.AnnotationSeq

import logger.LazyLogging

import scala.collection.mutable.LinkedHashSet

/** A polymorphic mathematical transform
  * @tparam A the transformed type
  */
trait TransformLike[A] extends LazyLogging {

  /** An identifier of this [[TransformLike]] that can be used for logging and informational printing */
  def name: String

  /** A mathematical transform on some type
    * @param a an input object
    * @return an output object of the same type
    */
  def transform(a: A): A

}

/** Mixin that defines dependencies between [[firrtl.options.TransformLike TransformLike]]s (hereafter referred to as
  * "transforms")
  *
  * This trait forms the basis of the Dependency API of the Chisel/FIRRTL Hardware Compiler Framework. Dependencies are
  * defined in terms of prerequisistes, dependents, and invalidates. A prerequisite is a transform that must run before
  * this transform. A dependent is a transform that must run ''after'' this transform. (This can be viewed as a means of
  * injecting a prerequisite into some other transform.) Finally, invalidates define the set of transforms whose effects
  * this transform undos/invalidates. (Invalidation then implies that a transform that is invalidated by this transform
  * and needed by another transform will need to be re-run.)
  *
  * This Dependency API only defines dependencies. A concrete [[DependencyManager]] is expected to be used to statically
  * resolve a linear ordering of transforms that satisfies dependency requirements.
  * @tparam A some transform
  * @define seqNote @note The use of a Seq here is to preserve input order. Internally, this will be converted to a private,
  * ordered Set.
  */
trait DependencyAPI[A <: DependencyAPI[A]] { this: TransformLike[_] =>

  /** The type used to express dependencies: a class which itself has dependencies. */
  type Dependency = Class[_ <: A]

  /** All transform that must run before this transform
    * $seqNote
    */
  def prerequisites: Seq[Dependency] = Seq.empty
  private[options] lazy val _prerequisites: LinkedHashSet[Dependency] = new LinkedHashSet() ++ prerequisites.toSet

  /** All transforms that must run ''after'' this transform
    *
    * ''This is a means of prerequisite injection into some other transform.'' Normally a transform will define its own
    * prerequisites. Dependents exist for two main situations:
    *
    * First, they improve the composition of optional transforms. If some first transform is optional (e.g., an
    * expensive validation check), you would like to be able to conditionally cause it to run. If it is listed as a
    * prerequisite on some other, second transform then it must always run before that second transform. There's no way
    * to turn it off. However, by listing the second transform as a dependent of the first transform, the first
    * transform will only run (and be treated as a prerequisite of the second transform) if included in a list of target
    * transforms that should be run.
    *
    * Second, an external library would like to inject some first transform before a second transform inside FIRRTL. In
    * this situation, the second transform cannot have any knowledge of external libraries. The use of a dependent here
    * allows for prerequisite injection into FIRRTL proper.
    *
    * @see [[firrtl.passes.CheckTypes]] for an example of an optional checking [[firrtl.Transform]]
    * $seqNote
    */
  def dependents: Seq[Dependency] = Seq.empty
  private[options] lazy val _dependents: LinkedHashSet[Dependency] = new LinkedHashSet() ++ dependents.toSet

  /** A function that, given a transform will return true if this transform invalidates/undos the effects of the input
    * transform
    * @note Can a [[firrtl.options.Phase Phase]] ever invalidate itself?
    */
  def invalidates(a: A): Boolean = true

}

/** A trait indicating that no invalidations occur, i.e., all previous transforms are preserved
  * @tparam A some [[TransformLike]]
  */
trait PreservesAll[A <: DependencyAPI[A]] { this: DependencyAPI[A] =>

  override def invalidates(a: A): Boolean = false

}

/** A mathematical transformation of an [[AnnotationSeq]].
  *
  * A [[firrtl.options.Phase Phase]] forms one unit in the Chisel/FIRRTL Hardware Compiler Framework (HCF). The HCF is
  * built from a sequence of [[firrtl.options.Phase Phase]]s applied to an [[AnnotationSeq]]. Note that a
  * [[firrtl.options.Phase Phase]] may consist of multiple phases internally.
  */
trait Phase extends TransformLike[AnnotationSeq] with DependencyAPI[Phase] {

  /** The name of this [[firrtl.options.Phase Phase]]. This will be used to generate debug/error messages or when deleting
    * annotations. This will default to the `simpleName` of the class.
    * @return this phase's name
    * @note Override this with your own implementation for different naming behavior.
    */
  lazy val name: String = this.getClass.getName

}

/** A [[firrtl.options.TransformLike TransformLike]] that internally ''translates'' the input type to some other type,
  * transforms the internal type, and converts back to the original type.
  *
  * This is intended to be used to insert a [[firrtl.options.TransformLike TransformLike]] parameterized by type `B`
  * into a sequence of [[firrtl.options.TransformLike TransformLike]]s parameterized by type `A`.
  * @tparam A the type of the [[firrtl.options.TransformLike TransformLike]]
  * @tparam B the internal type
  */
trait Translator[A, B] extends TransformLike[A] {

  /** A method converting type `A` into type `B`
    * @param an object of type `A`
    * @return an object of type `B`
    */
  protected implicit def aToB(a: A): B

  /** A method converting type `B` back into type `A`
    * @param an object of type `B`
    * @return an object of type `A`
    */
  protected implicit def bToA(b: B): A

  /** A transform on an internal type
    * @param b an object of type `B`
    * @return an object of type `B`
    */
  protected def internalTransform(b: B): B

  /** Convert the input object to the internal type, transform the internal type, and convert back to the original type
    */
  override final def transform(a: A): A = bToA(internalTransform(aToB(a)))

}
