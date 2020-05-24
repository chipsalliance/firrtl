// See LICENSE for license details

package firrtl.checks

import firrtl._
import firrtl.passes.Pass
import firrtl.annotations.NoTargetAnnotation
import firrtl.options.{ShellOption, HasShellOptions}

case object SkipChecksAnnotation extends NoTargetAnnotation with HasShellOptions {
  val options = Seq(
    new ShellOption[Unit](
      longOption      = "skip-checks",
      toAnnotationSeq = _ => Seq(SkipChecksAnnotation),
      helpText        = "skip all check passes for compiler runtime emergencies"
    ))
}

trait Check extends Pass {
  final override def execute(state: CircuitState): CircuitState = {
    val skipChecksOption = state.annotations.find(_ == SkipChecksAnnotation)
    skipChecksOption match {
      case Some(_) =>
        state
      case None =>
        run(state.circuit)
        state
    }
  }
}
