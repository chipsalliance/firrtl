// See LICENSE for license details.

import sbt._
import sbt.Keys._
import sbt.internal.util.FeedbackProvidedException

import sbt.complete.DefaultParsers.spaceDelimited

case class JQFException(message: String) extends Exception(message)
  with FeedbackProvidedException

object JQFPlugin extends AutoPlugin {
  object autoImport {
    val jqfOutputDirectory = settingKey[File](
      "The the output directory where fuzzing results will be stored. " +
        "Defaults to ${compileTargetDir}/JQF")

    val jqfInputDirectory = settingKey[Option[File]](
      "The name of the input directory containing seed files. " +
        "If not provided, then fuzzing starts with randomly generated initial inputs.")

    val jqfProject = settingKey[Project](
      "Project containing the JQF main class. JQF tasks will use the runMain task of this project")

    val jqfFuzzMainClass = settingKey[String](
      "The main class to run for the JQF Fuzz task. " +
        "Must accept --inputDirectory, --outputDirectory, and --classpathElements arguments")

    val jqfFuzzExtraArguments = settingKey[Seq[String]](
      "Additional command-line arguments to pass on every jqfFuzz invocation. "
    )

    val jqfFuzz = inputKey[Unit](
      "Runs the jqfFuzzMainClass in the jqfProject. " +
        "Supplies the '--classpathElements' argument with the current project's classpath")
  }

  import autoImport._

  override val trigger: PluginTrigger = noTrigger

  override val requires: Plugins = plugins.JvmPlugin

  override def projectSettings: Seq[Def.Setting[_]] = Seq(
    jqfOutputDirectory := target.in(Compile).value / "JQF",

    jqfInputDirectory := None,

    jqfFuzzExtraArguments := Seq.empty,

    jqfFuzz := (Def.inputTaskDyn {
      val extraArgs = jqfFuzzExtraArguments.value
      val parsed = spaceDelimited("<arg>").parsed
      val defaultArgs = toCmdlineArgs.value
      val args = (defaultArgs ++ parsed).mkString(" ")
      val project = jqfProject.value
      val main = jqfFuzzMainClass.value
      Def.taskDyn {
        (project/runMain).in(Compile).toTask(s" $main $args")
      }
    }).evaluated,
  )


  private def toCmdlineArgs: Def.Initialize[Task[Seq[String]]] = Def.task {
    Seq(
      "--classpathElements", (Compile / fullClasspathAsJars).toTask.value.files.mkString(":"),
      "--outputDirectory", jqfOutputDirectory.value.toString
    ) ++
    jqfInputDirectory.value.map(f => Seq("--inputDirectory", f.toString)).getOrElse(Seq.empty)
  }
}
