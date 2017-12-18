// See LICENSE for license details.

package firrtl
package annotations

import net.jcazevedo.moultingyaml._
import firrtl.annotations.AnnotationYamlProtocol._

case class AnnotationException(message: String) extends Exception(message)

// TODO
//   - Should Annotations be able to "target" multiple named things?
//   - Should Annotations be able to "target" multiple transforms?
trait Annotation {
  def update(renames: RenameMap): Seq[Annotation]
  // TODO This is probably not the right API
  def targets(named: Named): Boolean
  def targets(transform: Class[_ <: Transform]): Boolean

  /**
    * This serialize is basically a pretty printer, actual serialization is handled by
    * AnnotationYamlProtocol
    * @return a nicer string than the raw case class default
    */
  def serialize: String
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
    value: String) extends Annotation {
  val targetString: String = target.serialize
  val transformClass: String = transform.getName

  def targets(named: Named): Boolean = named == target
  def targets(transform: Class[_ <: Transform]): Boolean = transform == this.transform

  /**
    * This serialize is basically a pretty printer, actual serialization is handled by
    * AnnotationYamlProtocol
    * @return a nicer string than the raw case class default
    */
  def serialize: String = {
    s"Annotation(${target.serialize},${transform.getCanonicalName},$value)"
  }

  def update(renames: RenameMap): Seq[Annotation] =
    renames.renameMap.get(target).map(update).getOrElse(Seq(this))

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

