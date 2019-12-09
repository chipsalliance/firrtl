// See LICENSE for license details.

package firrtl.options.phases

import firrtl.AnnotationSeq
import firrtl.options.{DependencyID, Phase, PreservesAll, TargetDirAnnotation}

/** Add default annotations for a [[Stage]]
  *
  * This currently only adds a [[TargetDirAnnotation]]. This isn't necessary for a [[StageOptionsView]], but downstream
  * tools may expect a [[TargetDirAnnotation]] to exist.
  */
class AddDefaults extends Phase with PreservesAll[Phase] {

  override val prerequisites = Seq(DependencyID[GetIncludes], DependencyID[ConvertLegacyAnnotations])

  override val dependents = Seq.empty

  def transform(annotations: AnnotationSeq): AnnotationSeq = {
    val td = annotations.collectFirst{ case a: TargetDirAnnotation => a}.isEmpty

    (if (td) Seq(TargetDirAnnotation()) else Seq()) ++
      annotations
  }

}
