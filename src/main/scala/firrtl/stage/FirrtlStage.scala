// See LICENSE for license details.

package firrtl.stage

import firrtl.AnnotationSeq
import firrtl.options.{Phase, PhaseManager, PreservesAll, Shell, Stage, StageMain}
import firrtl.options.phases.DeletedWrapper
import firrtl.stage.phases.CatchExceptions

class FirrtlPhase
    extends PhaseManager(targets=Seq(classOf[firrtl.stage.phases.WriteEmitted]))
    with PreservesAll[Phase] {

  override val wrappers = Seq(CatchExceptions(_: Phase), DeletedWrapper(_: Phase))

}

class FirrtlStage extends Stage with PreservesAll[Phase] {

  val shell: Shell = new Shell("firrtl") with FirrtlCli

  def run(annotations: AnnotationSeq): AnnotationSeq = (new FirrtlPhase).transform(annotations)

}

object FirrtlMain extends StageMain(new FirrtlStage)
