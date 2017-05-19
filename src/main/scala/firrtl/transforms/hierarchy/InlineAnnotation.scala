package firrtl.transforms.hierarchy

import firrtl.annotations.{Annotation, Named}

/**
  * Created by adamiz on 5/19/17.
  */
// Tags an annotation to be consumed by this pass
object InlineAnnotation {
  def apply(target: Named): Annotation = Annotation(target, classOf[InlineInstances], "")

  def unapply(a: Annotation): Option[Named] = a match {
    case Annotation(named, t, _) if t == classOf[InlineInstances] => Some(named)
    case _ => None
  }
}
