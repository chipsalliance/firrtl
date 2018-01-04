// See LICENSE for license details.

package firrtl
package annotations

import net.jcazevedo.moultingyaml._
import firrtl.annotations.AnnotationYamlProtocol._

case class AnnotationException(message: String) extends Exception(message)

/** Base type of auxiliary information */
trait Annotation {
  /** Update the target based on how signals are renamed */
  def update(renames: RenameMap): Seq[Annotation]

  /** Pretty Print
    *
    * @note In [[logger.LogLevel.Debug]] this is called on every Annotation after every Transform
    */
  def serialize: String = this.toString
}

/** If an Annotation does not target any [[Named]] thing in the circuit, then all updates just
  * return the Annotation itself
  */
trait NoTargetAnnotation extends Annotation {
  def update(renames: RenameMap) = Seq(this)
}

/** An Annotation that targets a single [[Named]] thing */
trait SingleTargetAnnotation[T <: Named] extends Annotation {
  val target: T

  /** Create another instance of this Annotation */
  def duplicate(n: T): Annotation

  // This mess of @unchecked and try-catch is working around the fact that T is unknown due to type
  // erasure. We cannot that newTarget is of type T, but a CastClassException will be thrown upon
  // invoking duplicate if newTarget cannot be cast to T (only possible in the concrete subclass)
  def update(renames: RenameMap): Seq[Annotation] =
    renames.get(target).map(_.map(newT => (newT: @unchecked) match {
      case newTarget: T @unchecked =>
        try {
          duplicate(newTarget)
        } catch {
          case _: java.lang.ClassCastException =>
          val msg = s"${this.getClass.getName} target ${target.getClass.getName} " +
            s"cannot be renamed to ${newTarget.getClass}"
          throw AnnotationException(msg)
        }
    })).getOrElse(List(this))
}

trait SingleStringAnnotation extends NoTargetAnnotation {
  def value: String
}

object Annotation {
  @deprecated("This returns a LegacyAnnotation, use an explicit Annotation type", "1.1")
  def apply(target: Named, transform: Class[_ <: Transform], value: String) =
    new LegacyAnnotation(target, transform, value)
  @deprecated("This uses LegacyAnnotation, use an explicit Annotation type", "1.1")
  def unapply(a: LegacyAnnotation): Option[(Named, Class[_ <: Transform], String)] =
    Some((a.target, a.transform, a.value))
}

// Constructor is private so that we can still construct these internally without deprecation
// warnings
final case class LegacyAnnotation private[firrtl] (
    target: Named,
    transform: Class[_ <: Transform],
    value: String) extends SingleTargetAnnotation[Named] {
  val targetString: String = target.serialize
  val transformClass: String = transform.getName

  def targets(named: Named): Boolean = named == target
  def targets(transform: Class[_ <: Transform]): Boolean = transform == this.transform

  /**
    * This serialize is basically a pretty printer, actual serialization is handled by
    * AnnotationYamlProtocol
    * @return a nicer string than the raw case class default
    */
  override def serialize: String = {
    s"Annotation(${target.serialize},${transform.getCanonicalName},$value)"
  }

  def update(tos: Seq[Named]): Seq[Annotation] = {
    check(target, tos, this)
    propagate(target, tos, duplicate)
  }
  def propagate(from: Named, tos: Seq[Named], dup: Named=>Annotation): Seq[Annotation] = tos.map(dup(_))
  def check(from: Named, tos: Seq[Named], which: Annotation): Unit = {}
  def duplicate(n: Named) = new LegacyAnnotation(n, transform, value)
}

// Private so that LegacyAnnotation can only be constructed via deprecated Annotation.apply
private object LegacyAnnotation

case class DeletedAnnotation(xFormName: String, anno: Annotation) extends NoTargetAnnotation {
  override def serialize: String = s"""DELETED by $xFormName\n${anno.serialize}"""
}

