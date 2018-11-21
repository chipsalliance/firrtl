// See LICENSE for license details.

package firrtl.stage.phases

import firrtl.{AnnotationSeq, EmittedAnnotation}
import firrtl.annotations.DeletedAnnotation
import firrtl.options.Phase
import firrtl.stage.{CircuitOption, DontStripAnnotation, FirrtlOption}

/** Remove [[firrtl.annotations.Annotation Annotation]]s that are of no use byeond [[FirrtlStage]]. Disable this with a
  * [[DontStripAnnotation]].
  *
  * All of the following [[firrtl.annotations.Annotation Annotation]] types will be stripped:
  *   - Annotations that mixin [[FirrtlOption]] (indicating that they are options specific to FIRRTL)
  *   - Annotations that mixin [[CircuitOption]] (indicating that they provide a circuit)
  *   - Any [[firrtl.annotations.DeletedAnnotation DeletedAnnotation]]s
  *   - Any [[EmittedAnnotation]]s if '''not''' running in [[Driver]] compatibility mode. (In [[Driver]] compatibility
  *     mode, the emitted circuit has to be used to construct the return type that the [[Driver]] expected.)
  */
object Strip extends Phase {

  /** Remove unneeded/unecessary [[firrtl.annotations.Annotation Annotation]]s from an [[AnnotationSeq]] */
  def transform(annotations: AnnotationSeq): AnnotationSeq = {
    lazy val compat = annotations
      .collectFirst { case a: DriverCompatibility.DriverCompatibilityAnnotation.type => a }
      .isDefined

    annotations
      .collectFirst { case a: DontStripAnnotation.type => a } match {
        case Some(_) => annotations
        case None => annotations.filterNot {
          case _: FirrtlOption | _: CircuitOption | _: DeletedAnnotation => true
          case _: EmittedAnnotation[_] if (!compat)                      => true
          case _                                                         => false
        }
      }
  }

}
