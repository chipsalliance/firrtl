// See LICENSE for license details.

package firrtl.stage

import firrtl.{AnnotationSeq, CustomTransformException, FirrtlInternalException,
               FirrtlUserException, FIRRTLException, Utils}
import firrtl.options.{Stage, Phase, PhaseException, Shell, OptionsException, StageMain, StageUtils}
import firrtl.options.phases.DeletedWrapper
import firrtl.passes.{PassException, PassExceptions}

import scala.util.control.ControlThrowable


class FirrtlStage extends Stage {
  val shell: Shell = new Shell("firrtl") with FirrtlCli

  private val phases: Seq[Phase] =
    Seq( new firrtl.stage.phases.AddDefaults,
         new firrtl.stage.phases.AddImplicitEmitter,
         new firrtl.stage.phases.Checks,
         new firrtl.stage.phases.AddCircuit,
         new firrtl.stage.phases.AddImplicitOutputFile,
         new firrtl.stage.phases.Compiler,
         new firrtl.stage.phases.WriteEmitted )
      .map(DeletedWrapper(_))

  def run(annotations: AnnotationSeq): AnnotationSeq = try {
    phases.foldLeft(annotations)((a, f) => f.transform(a))
  } catch {
    /* Rethrow the exceptions which are expected or due to the runtime environment (out of memory, stack overflow, etc.).
     * Any UNEXPECTED exceptions should be treated as internal errors. */
    case p @ (_: ControlThrowable | _: FIRRTLException | _: OptionsException | _: FirrtlUserException
                | _: FirrtlInternalException | _: PhaseException) => throw p
    case CustomTransformException(cause) => throw cause
    case e: Exception => Utils.throwInternalError(exception = Some(e))
  }

}

object FirrtlMain extends StageMain(new FirrtlStage)
