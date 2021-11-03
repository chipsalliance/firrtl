// SPDX-License-Identifier: Apache-2.0

package firrtl.elk

import firrtl.annotations.{Annotation, NoTargetAnnotation}
import firrtl.elk.transforms.MakeCircuit
import firrtl.options.{HasShellOptions, RegisteredLibrary, ShellOption, Unserializable}
import firrtl.stage.RunFirrtlTransformAnnotation

case class ElkTopModuleAnnotation(name: String) extends Annotation with NoTargetAnnotation with Unserializable

object ElkTopModuleAnnotation extends HasShellOptions {
  val options = Seq(
    new ShellOption[String](
      longOption = "top",
      toAnnotationSeq = (a: String) => Seq(ElkTopModuleAnnotation(a)),
      helpText = "The start module in the hierarchy graph (default: The top module)"
    )
  )
}

case class ElkFlattenLevelAnnotation(depth: Int) extends Annotation with NoTargetAnnotation with Unserializable

object ElkFlattenLevelAnnotation extends HasShellOptions {
  val options = Seq(
    new ShellOption[Int](
      longOption = "flatten",
      toAnnotationSeq = (a: Int) => Seq(ElkFlattenLevelAnnotation(a)),
      helpText = "The maximum depth of the flatten levels of the Graph",
      helpValueName = Some("depth")
    )
  )
}

object ElkAnnotation extends HasShellOptions {

  val options = Seq(
    new ShellOption[Unit](
      longOption = "elk",
      toAnnotationSeq = _ => Seq(RunFirrtlTransformAnnotation(new MakeCircuit)),
      helpText = "Generate ELK Graph to represent Firrtl circuit"
    )
  )
}

class ElkOptions extends RegisteredLibrary {
  val name: String = "Elk Graph Options"

  val options: Seq[ShellOption[_]] = Seq(
    ElkAnnotation,
    ElkTopModuleAnnotation,
    ElkFlattenLevelAnnotation
  ).flatMap(_.options)

}
