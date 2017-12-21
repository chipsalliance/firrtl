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

trait SingleTargetAnnotation extends Annotation {
  def target: Named
  def update(renames: RenameMap): Seq[Annotation] =
    renames.get(target).map(_.map(duplicate)).getOrElse(List(this))
  def duplicate(n: Named): Annotation
}

object Annotation {
  def apply(target: Named, transform: Class[_ <: Transform], value: String) =
    LegacyAnnotation(target, transform, value)
  def unapply(a: LegacyAnnotation): Option[(Named, Class[_ <: Transform], String)] =
    Some((a.target, a.transform, a.value))
}

final case class LegacyAnnotation(
    target: Named,
    transform: Class[_ <: Transform],
    value: String) extends SingleTargetAnnotation {
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
  def duplicate(n: Named) = Annotation(n, transform, value)
}

object DeletedAnnotation {
  def apply(xFormName: String, anno: LegacyAnnotation): Annotation =
    Annotation(anno.target, classOf[Transform], s"""DELETED by $xFormName\n${AnnotationUtils.toYaml(anno)}""")

  private val deletedRegex = """(?s)DELETED by ([^\n]*)\n(.*)""".r
  def unapply(a: Annotation): Option[(String, LegacyAnnotation)] = a match {
    case Annotation(named, t, deletedRegex(xFormName, annoString)) if t == classOf[Transform] =>
      Some((xFormName, AnnotationUtils.fromYaml(annoString)))
    case _ => None
  }
}

/** Parent class to create global annotations
  *
  * These annotations are Circuit-level and available to every transform
  */
abstract class GlobalCircuitAnnotation {
  private lazy val marker = this.getClass.getName
  def apply(value: String): Annotation =
    Annotation(CircuitTopName, classOf[Transform], s"$marker:$value")
  def unapply(a: Annotation): Option[String] = a match {
    // Assumes transform is already filtered appropriately
    case Annotation(CircuitTopName, _, str) if str.startsWith(marker) =>
      Some(str.stripPrefix(s"$marker:"))
    case _ => None
  }
}

