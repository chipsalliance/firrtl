// See LICENSE for license details.

package firrtl.options

import firrtl.AnnotationSeq
import firrtl.annotations.DeletedAnnotation

import logger.LazyLogging

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

/** Additional methods that define dependencies between [[TransformLike]]s
  * @tparam A some [[TransformLike]]
  */
trait DependencyAPI[A <: DependencyAPI[A]] {

  /** All `A` that must run before this `A` */
  def prerequisites: Set[Class[A]] = Set.empty

  /** All [[Phase]]s that must run ''after'' this [[Phase]].
    *
    * ''This is a means of prerequisite injection into some other [[Phase]].'' Normally a [[Phase]] will define its own
    * prerequisites. This is a fallback approach for the narrow situation of an external library [[Phase]] needing to
    * run before some other [[Phase]] where that other [[Phase]] does not know about the [[Phase]] added by the library.
    * As dependents and prerequisites are two ways of expressing the same thing, a user should always use a prerequisite
    * first and fallback to specifying dependents if needed.
    */
  def dependents: Set[Class[A]] = Set.empty

  /** A function that, given some other [[Phase]], will return true if this [[Phase]] invalidates the other [[Phase]]. By
    * default, this invalidates everything.
    * @note Can a [[Phase]] ever invalidate itself?
    */
  def invalidates(a: A): Boolean = true

  /** Helper method to return the underlying class */
  final def asClass: Class[A] = this.getClass.asInstanceOf[Class[A]]

  /** Implicit conversion that allows for terser specification of [[DependencyAPI.prerequisites prerequisites]] and
    * [[DependencyAPI.dependents dependents]].
    */
  implicit def classHelper(a: Class[_ <: A]): Class[A] = a.asInstanceOf[Class[A]]

}

/** A trait indicating that no invalidations occur, i.e., all previous transforms are preserved
  * @tparam A some [[TransformLike]]
  */
trait PreservesAll[A <: DependencyAPI[A]] extends DependencyAPI[A] {

  override def invalidates(a: A): Boolean = false

}

/** A mathematical transformation of an [[AnnotationSeq]].
  *
  * A [[Phase]] forms one unit in the Chisel/FIRRTL Hardware Compiler Framework (HCF). The HCF is built from a sequence
  * of [[Phase]]s applied to an [[AnnotationSeq]]. Note that a [[Phase]] may consist of multiple phases internally.
  */
trait Phase extends TransformLike[AnnotationSeq] with DependencyAPI[Phase] {

  /** The name of this [[Phase]]. This will be used to generate debug/error messages or when deleting annotations. This
    * will default to the `simpleName` of the class.
    * @return this phase's name
    * @note Override this with your own implementation for different naming behavior.
    */
  lazy val name: String = this.getClass.getName

}

/** A [[TransformLike]] that internally ''translates'' the input type to some other type, transforms the internal type,
  * and converts back to the original type.
  *
  * This is intended to be used to insert a [[TransformLike]] parameterized by type `B` into a sequence of
  * [[TransformLike]]s parameterized by type `A`.
  * @tparam A the type of the [[TransformLike]]
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
