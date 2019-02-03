// See LICENSE for license details.

package firrtl.stage

import firrtl.{AnnotationSeq, FIRRTLException, Utils}
import firrtl.options.{Stage, Phase, PhaseException, Shell, OptionsException}
import firrtl.options.phases.DeletedWrapper
import firrtl.passes.{PassException, PassExceptions}

import scala.util.control.ControlThrowable

import java.io.PrintWriter

object FirrtlStage extends Stage {
  val shell: Shell = new Shell("firrtl") with FirrtlCli

  private val phases: Seq[Phase] =
    Seq( firrtl.stage.phases.AddDefaults,
         firrtl.stage.phases.Checks,
         firrtl.stage.phases.AddCircuit,
         firrtl.stage.phases.AddImplicitOutputFile,
         firrtl.stage.phases.Compiler,
         firrtl.stage.phases.WriteEmitted )
      .map(DeletedWrapper(_))

  def run(annotations: AnnotationSeq): AnnotationSeq = try {
    phases.foldLeft(annotations)((a, f) => f.transform(a))
  } catch {
    /* Rethrow the exceptions which are expected or due to the runtime environment (out of memory, stack overflow, etc.).
     * Any UNEXPECTED exceptions should be treated as internal errors. */
    case p @ (_: ControlThrowable | _: PassException | _: PassExceptions | _: FIRRTLException | _: OptionsException
                | _: PhaseException) => throw p
    case e: Exception => Utils.throwInternalError(exception = Some(e))
  }

}
