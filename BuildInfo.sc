import ammonite.ops._
import mill._
import mill.scalalib._

// Define our own BuildInfo since mill doesn't currently have one.
trait BuildInfo extends ScalaModule { outer =>

  def buildInfoObjectName: String = "BuildInfo"

  def buildInfoMembers: T[Map[String, String]] = T {
    Map.empty[String, String]
  }

  private def generateBuildInfo(outputPath: Path, members: Map[String, String]) = {
    val outputFile = outputPath / "BuildInfo.scala"
    val packageName = members.getOrElse("buildInfoPackage", "")
    val packageDef = if (packageName != "") {
      s"package ${packageName}"
    } else {
      ""
    }
    val internalMembers =
      members
        .map {
          case (name, value) => s"""  val ${name}: String = "${value}""""
        }
        .mkString("\n")
    write(outputFile,
      s"""
         |${packageDef}
         |case object ${buildInfoObjectName}{
         |$internalMembers
         |  override val toString: String = {
         |    "buildInfoPackage: %s, version: %s, scalaVersion: %s" format (
         |        buildInfoPackage, version, scalaVersion
         |    )
         |  }
         |}
       """.stripMargin)
    outputPath
  }

  override def generatedSources = T {
    super.generatedSources() :+ PathRef(generateBuildInfo(T.ctx().dest, buildInfoMembers()))
  }
}
