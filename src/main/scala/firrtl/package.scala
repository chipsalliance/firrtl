// See LICENSE for license details.

import firrtl.annotations.Annotation

package object firrtl {
  // Force initialization of the Forms object - https://github.com/freechipsproject/firrtl/issues/1462
  private val _dummyForms = firrtl.stage.Forms

  implicit def seqToAnnoSeq(xs: Seq[Annotation]) = AnnotationSeq(xs)
  implicit def annoSeqToSeq(as: AnnotationSeq): Seq[Annotation] = as.underlying

  /* Options as annotations compatibility items */
<<<<<<< HEAD
  @deprecated("Use firrtl.stage.TargetDirAnnotation", "1.2")
  type TargetDirAnnotation = firrtl.options.TargetDirAnnotation

  @deprecated("Use firrtl.stage.TargetDirAnnotation", "1.2")
=======
  @deprecated("Use firrtl.options.TargetDirAnnotation", "FIRRTL 1.2")
  type TargetDirAnnotation = firrtl.options.TargetDirAnnotation

  @deprecated("Use firrtl.options.TargetDirAnnotation", "FIRRTL 1.2")
>>>>>>> 06ccf51a (Fix incorrect deprecation warning for TargetDirAnnotation (#2511))
  val TargetDirAnnotation = firrtl.options.TargetDirAnnotation
}
