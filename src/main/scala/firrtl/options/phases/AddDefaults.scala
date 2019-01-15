// See LICENSE for license details.

package firrtl.options.phases

import firrtl.AnnotationSeq
import firrtl.options.{Phase, StageOption, TargetDirAnnotation}

/** Add default annotations for a [[Stage]]
  *
  * This currently only adds a [[TargetDirAnnotation]]. This isn't necessary for a [[StageOptionsView]], but downstream
  * tools may expect a [[TargetDirAnnotation]] to exist.
  */
object AddDefaults extends Phase {

  def transform(annotations: AnnotationSeq): AnnotationSeq = {
    val td = annotations.collectFirst{ case a: TargetDirAnnotation => a}.isEmpty

    (if (td) Seq(TargetDirAnnotation()) else Seq()) ++
      annotations
  }

}
