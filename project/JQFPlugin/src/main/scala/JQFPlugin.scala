// See LICENSE for license details.

import sbt._
import sbt.Keys._
import sbt.internal.util.FeedbackProvidedException

import sbt.complete.DefaultParsers.spaceDelimited

case class JQFException(message: String) extends Exception(message)
  with FeedbackProvidedException

object JQFPlugin extends AutoPlugin {
  object autoImport {
    val jqfOutputDirectory = settingKey[File]("")

    val jqfInputDirectory = settingKey[Option[File]]("")

    val jqfProject = settingKey[Project](
      "Project containing the JQF main class, JQF tasks will use the runMain task of this project")

    val jqfFuzzMainClass = settingKey[String](
      "The main class to run for the JQF Fuzz task")

    val jqfExtraArguments = settingKey[Seq[String]](
      "Additional command-line arguments to pass on every mdoc invocation. " +
        "For example, add '--no-link-hygiene' to disable link hygiene."
    )

    val jqfFuzz = inputKey[Unit]("")
  }

  import autoImport._


  override val trigger: PluginTrigger = noTrigger

  override val requires: Plugins = plugins.JvmPlugin
  
  override def projectSettings: Seq[Def.Setting[_]] = Seq(
    jqfOutputDirectory := target.in(Compile).value / "JQF",

    jqfInputDirectory := None,

    jqfExtraArguments := Seq.empty,
    
    jqfFuzz := (Def.inputTaskDyn {
      val extraArgs = jqfExtraArguments.value
      val parsed = spaceDelimited("<arg>").parsed
      val defaultArgs = toCmdlineArgs.value
      val args = (defaultArgs ++ parsed).mkString(" ")
      Def.taskDyn {
        val project = jqfProject.value
        val main = jqfFuzzMainClass.value
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
