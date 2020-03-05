package firrtl.transforms

import firrtl.RenameMap
import firrtl.annotations.{Annotation, CompleteTarget, DeletedAnnotation, MultiTargetAnnotation, Target}


/*
case class Rename[T <: CompleteTarget](from: T, to: T) extends MultiTargetAnnotation {
  override val targets: Seq[Seq[Target]] = Seq(Seq(from), Seq(to))

  override def update(renames: RenameMap): Seq[Annotation] = {
    super.update(renames) match {
      case _: DeletedAnnotation => Nil
      case other => other
    }
  }

  override def duplicate(n: Seq[Seq[Target]]): Annotation = n match {
    case Seq(Seq(newFrom), Seq(newTo)) => Rename(newFrom, newTo)
    case _ => DeletedAnnotation("", this)
  }
}

/** Renames signals, modules, ports, signal fields
  */
class RenamerTransform {

}

 */
