// See LICENSE for license details.

import firrtl.AnnotationSeq
import firrtl.annotations.Annotation

package object firrtl {
  implicit def seqToAnnoSeq(xs: Seq[Annotation]) = AnnotationSeq(xs)
  implicit def annoSeqToSeq(as: AnnotationSeq): Seq[Annotation] = as.underlying

  /* Options as annotations compatibility items */
  @deprecated("Use firrtl.options.TargetDirAnnotation", "1.2")
  type TargetDirAnnotation = firrtl.options.TargetDirAnnotation

  @deprecated("Use firrtl.options.TargetDirAnnotation", "1.2")
  val TargetDirAnnotation = firrtl.options.TargetDirAnnotation
}
