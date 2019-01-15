// See LICENSE for license details.

package firrtl.stage.phases

import firrtl.{AnnotationSeq, EmittedModuleAnnotation, EmittedCircuitAnnotation}
import firrtl.options.{Phase, StageOptions, Viewer}
import firrtl.stage.FirrtlOptions

import java.io.PrintWriter

/** [[firrtl.options.Phase Phase]] that writes any [[EmittedAnnotation]]s in an input [[AnnotationSeq]] to one or more
  * files. The input [[AnnotationSeq]] is viewed as both [[FirrtlOptions]] and [[firrtl.options.StageOptions
  * StageOptions]] to determine the output filenames in the following way:
  *   - [[EmittedModuleAnnotation]]s are written to a file in [[firrtl.options.StageOptions.targetDir
  *     StageOptions.targetDir]] with the same name as the module and the [[EmittedComponent.outputSuffix outputSuffix]]
  *     that the [[EmittedComponent]] specified
  *   - [[EmittedCircuitAnnotation]]s are written to a file in [[firrtl.options.StageOptions.targetDir
  *     StageOptions.targetDir]] using the [[FirrtlOptions.outputFileName]] viewed from the [[AnnotationSeq]]. If no
  *     [[FirrtlOptions.outputFileName]] exists, then the top module/main name will be used. The
  *     [[EmittedComponent.outputSuffix outputSuffix]] will be appended as needed.
  *
  * This does no sanity checking of the input [[AnnotationSeq]]. This simply writes any modules or circuits it sees to
  * files. If you need additional checking, then you should stack an appropriate checking phase before this.
  */
object WriteEmitted extends Phase {

  /** Write any [[EmittedAnnotation]]s in an [[AnnotationSeq]] to files. */
  def transform(annotations: AnnotationSeq): AnnotationSeq = {
    val fopts = Viewer.view[FirrtlOptions](annotations)
    val sopts = Viewer.view[StageOptions](annotations)

    annotations.foreach {
      case a: EmittedModuleAnnotation[_] =>
        val pw = new PrintWriter(sopts.getBuildFileName(a.value.name, Some(a.value.outputSuffix)))
        pw.write(a.value.value)
        pw.close()
      case a: EmittedCircuitAnnotation[_] =>
        val pw = new PrintWriter(
          sopts.getBuildFileName(fopts.outputFileName.getOrElse(a.value.name), Some(a.value.outputSuffix)))
        pw.write(a.value.value)
        pw.close()
      case _ =>
    }

    annotations
  }
}
