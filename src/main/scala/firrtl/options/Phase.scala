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

trait DependencyAPI { this: Phase =>

  /** All [[Phase]]s that must run before this [[Phase]] */
  def prerequisites: Set[Phase] = Set.empty

  /** All [[Phase]]s that must run ''after'' this [[Phase]].
    *
    * ''This is a means of prerequisite injection into some other [[Phase]].'' Normally a [[Phase]] will define its own
    * prerequisites. This is a fallback approach for the narrow situation of an external library [[Phase]] needing to
    * run before some other [[Phase]] where that other [[Phase]] does not know about the [[Phase]] added by the library.
    * As dependents and prerequisites are two ways of expressing the same thing, a user should always use a prerequisite
    * first and fallback to specifying dependents if needed.
    */
  def dependents: Set[Phase] = Set.empty

  /** A function that, given some other [[Phase]], will return [[true]] if this [[Phase]] invalidates the other [[Phase]].
    * By default, this invalidates everything except itself, i.e., this is a set that does not contain this [[Phase]].
    * @note Can a [[Phase]] ever invalidate itself?
    */
  def invalidates(phase: Phase): Boolean = phase match {
    case _: this.type => false
    case _ => true
  }

}

/** A mathematical transformation of an [[AnnotationSeq]].
  *
  * A [[Phase]] forms one unit in the Chisel/FIRRTL Hardware Compiler Framework (HCF). The HCF is built from a sequence
  * of [[Phase]]s applied to an [[AnnotationSeq]]. Note that a [[Phase]] may consist of multiple phases internally.
  */
abstract class Phase extends TransformLike[AnnotationSeq] with DependencyAPI {

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
trait Translator[A, B] { this: TransformLike[A] =>

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
  final def transform(a: A): A = internalTransform(a)

}
