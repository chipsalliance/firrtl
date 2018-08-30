// See LICENSE for license details.

package firrtl.annotations

import firrtl.RenameMap

import scala.collection.mutable


trait BrittleAnnotation extends Annotation {
  def duplicate(targets: Seq[Target]): BrittleAnnotation

  override def update(renames: RenameMap): Seq[Annotation] = {
    val errors = mutable.ArrayBuffer[String]()
    def rename(y: Target): Target = {
      renames.get(y) match {
        case Some(Seq(x)) => x
        case None => y
        case other =>
          val msg = s"${this.getClass.getName} target ${y.getClass.getName} " +
            s"cannot be renamed to $other"
          errors += msg
          y
      }
    }
    val newTargets = getTargets.map(rename)
    if(errors.nonEmpty) throw AnnotationException(errors.mkString("\n"))
    Seq(duplicate(newTargets))
  }
}


