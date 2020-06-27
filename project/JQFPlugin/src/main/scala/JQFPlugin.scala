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

    val jqfFuzzInputDirectory = settingKey[Option[File]](
      "The name of the input directory containing seed files. " +
        "If not provided, then fuzzing starts with randomly generated initial inputs.")

    val jqfProject = settingKey[Project](
      "Project containing the JQF main class. JQF tasks will use the runMain task of this project")

    val jqfFuzzMainClass = settingKey[String](
      "The main class to run for the jqfFuzz task. " +
        "Must accept --inputDirectory, --outputDirectory, and --classpathElements arguments")

    val jqfFuzz = inputKey[Unit](
      "Runs the jqfFuzzMainClass in the jqfProject. " +
        "Supplies the '--classpathElements' argument with the current project's classpath")


    val jqfReproMainClass = settingKey[String](
      "The main class to run for the jqfRepro task. " +
        "Must accept --input, and --classpathElements arguments")

    val jqfReproInput = settingKey[Option[File]](
      "Input file or directory to reproduce test case(s).")

    val jqfRepro = inputKey[Unit](
      "Runs the jqfReproMainClass in the jqfProject. " +
        "Supplies the '--classpathElements' argument with the current project's classpath")
  }

  import autoImport._

  override val trigger: PluginTrigger = noTrigger

  override val requires: Plugins = plugins.JvmPlugin

  override def projectSettings: Seq[Def.Setting[_]] = Seq(
    jqfOutputDirectory := target.in(Compile).value / "JQF",

    jqfFuzzInputDirectory := None,

    jqfFuzz := (Def.inputTaskDyn {
      val parsed = spaceDelimited("<arg>").parsed
      val defaultArgs = fuzzCmdlineArgs.value
      val args = (defaultArgs ++ parsed).mkString(" ")
      val project = jqfProject.value
      val main = jqfFuzzMainClass.value
      Def.taskDyn {
        (project/runMain).in(Compile).toTask(s" $main $args")
      }
    }).evaluated,


    jqfReproInput := None,

    jqfRepro := (Def.inputTaskDyn {
      val parsed = spaceDelimited("<arg>").parsed
      val defaultArgs = reproCmdlineArgs.value
      val args = (defaultArgs ++ parsed).mkString(" ")
      val project = jqfProject.value
      val main = jqfReproMainClass.value
      Def.taskDyn {
        (project/runMain).in(Compile).toTask(s" $main $args")
      }
    }).evaluated,
  )


  private def fuzzCmdlineArgs: Def.Initialize[Task[Seq[String]]] = Def.task {
    Seq(
      "--classpathElements", (Compile / fullClasspathAsJars).toTask.value.files.mkString(":"),
      "--outputDirectory", jqfOutputDirectory.value.toString
    ) ++
    jqfFuzzInputDirectory.value.map(f => Seq("--inputDirectory", f.toString)).getOrElse(Seq.empty)
  }

  private def reproCmdlineArgs: Def.Initialize[Task[Seq[String]]] = Def.task {
    Seq(
      "--classpathElements", (Compile / fullClasspathAsJars).toTask.value.files.mkString(":"),
    ) ++
    jqfReproInput.value.map(f => Seq("--input", f.toString)).getOrElse(Seq.empty)
  }
}
