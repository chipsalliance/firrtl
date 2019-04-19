// See LICENSE for license details.

package firrtl.stage

import firrtl.{AnnotationSeq, CustomTransformException, FIRRTLException, Utils}
import firrtl.options.{Phase, PhaseException, PhaseManager, PreservesAll, Shell, Stage, OptionsException, StageMain}
import firrtl.options.phases.DeletedWrapper
import firrtl.passes.{PassException, PassExceptions}

import scala.util.control.ControlThrowable

import java.io.PrintWriter

class FirrtlStage extends Stage with PreservesAll[Phase] {

  val shell: Shell = new Shell("firrtl") with FirrtlCli

  val phases: Seq[Phase] = new PhaseManager(Set(classOf[firrtl.stage.phases.WriteEmitted]))
      .transformOrder
      .map(DeletedWrapper(_))

  def run(annotations: AnnotationSeq): AnnotationSeq = try {
    phases.foldLeft(annotations)((a, f) => f.transform(a))
  } catch {
    /* Rethrow the exceptions which are expected or due to the runtime environment (out of memory, stack overflow, etc.).
     * Any UNEXPECTED exceptions should be treated as internal errors. */
    case p @ (_: ControlThrowable | _: PassException | _: PassExceptions | _: FIRRTLException | _: OptionsException
                | _: PhaseException) => throw p
    case CustomTransformException(cause) => throw cause
    case e: Exception => Utils.throwInternalError(exception = Some(e))
  }

}

object FirrtlMain extends StageMain(new FirrtlStage)
