import ammonite.ops._
import ammonite.ops.ImplicitWd._
import mill._
import mill.scalalib._
import mill.scalalib.publish._
import mill.eval.Evaluator

import $file.BuildInfo
import $file.CommonBuild
import $file.Protobuf

// An sbt layout with src in the top directory.
trait CrossUnRootedSbtModule extends CrossSbtModule {
  override def millSourcePath = super.millSourcePath / ammonite.ops.up
}

trait CommonModule extends CrossUnRootedSbtModule with PublishModule {
  def publishVersion = "1.2-SNAPSHOT"

  def pomSettings = PomSettings(
    description = artifactName(),
    organization = "edu.berkeley.cs",
    url = "https://github.com/freechipsproject/firrtl.git",
    licenses = Seq(License.`BSD-3-Clause`),
    versionControl = VersionControl.github("freechipsproject", "firrtl"),
    developers = Seq(
      Developer("jackbackrack",    "Jonathan Bachrach",      "https://eecs.berkeley.edu/~jrb/")
    )
  )

  override def scalacOptions = Seq(
    "-deprecation",
    "-explaintypes",
    "-feature", "-language:reflectiveCalls",
    "-unchecked",
    "-Xcheckinit",
    "-Xlint:infer-any",
    "-Xlint:missing-interpolator"
  ) ++ CommonBuild.scalacOptionsVersion(crossScalaVersion)

  override def javacOptions = CommonBuild.javacOptionsVersion(crossScalaVersion)
}

// Generic antlr4 configuration.
// This could be simpler, but I'm trying to keep some compatibility with the sbt plugin.
case class Antlr4Config(val sourcePath: Path) {
  val antlr4Version: String = "4.7.1"
  val ANTLR4_JAR = (pwd / s"antlr-${antlr4Version}-complete.jar").toString
  if (!new java.io.File(ANTLR4_JAR).exists) {
    println("Downloading ANTLR4 .jar")
    %%("wget", s"https://www.antlr.org/download/antlr-${antlr4Version}-complete.jar", "-O", ANTLR4_JAR)
  }
  val antlr4GenVisitor: Boolean = true
  val antlr4GenListener: Boolean = false
  val antlr4PackageName: Option[String] = Some("firrtl.antlr")

  val listenerArg: String = if (antlr4GenListener) "-listener" else "-no-listener"
  val visitorArg: String = if (antlr4GenVisitor) "-visitor" else "-no-visitor"
  val packageArg: Seq[String] = antlr4PackageName match {
    case Some(p) => Seq("-package", p)
    case None => Seq.empty
  }
  def runAntlr(outputPath: Path) = {
    val cmd = Seq[String]("java", "-jar", ANTLR4_JAR, "-o", outputPath.toString, "-lib", sourcePath.toString, listenerArg, visitorArg) ++ packageArg :+ (sourcePath / "FIRRTL.g4").toString
    val result = %%(cmd)
  }
}

val crossVersions = Seq("2.12.4", "2.11.12")

// Make this available to external tools.
object firrtl extends Cross[FirrtlModule](crossVersions: _*) {
  def defaultVersion(ev: Evaluator) = T.command{
    println(crossVersions.head)
  }

  def compile = T{
    firrtl(crossVersions.head).compile()
  }

  def jar = T{
    firrtl(crossVersions.head).jar()
  }

  def test = T{
    firrtl(crossVersions.head).test.test()
  }

  def publishLocal = T{
    firrtl(crossVersions.head).publishLocal()
  }

  def docJar = T{
    firrtl(crossVersions.head).docJar()
  }
}

class FirrtlModule(val crossScalaVersion: String) extends CommonModule with BuildInfo.BuildInfo {
  override def artifactName = "firrtl"

  override def ivyDeps = Agg(
    ivy"com.typesafe.scala-logging::scala-logging:3.9.0",
    ivy"ch.qos.logback:logback-classic:1.2.3",
    ivy"com.github.scopt::scopt:3.7.0",
    ivy"net.jcazevedo::moultingyaml:0.4.0",
    ivy"org.json4s::json4s-native:3.6.1",
    ivy"org.apache.commons:commons-text:1.6",
    ivy"org.antlr:antlr4-runtime:4.7.1",
    ivy"${Protobuf.ProtobufConfig.ivyDep}"
  )

  object test extends Tests {
    override def ivyDeps = Agg(
      ivy"org.scalatest::scalatest:3.0.5",
      ivy"org.scalacheck::scalacheck:1.13.4"
    )
    def testFrameworks = Seq("org.scalatest.tools.Framework")
  }

  def antlrSourceRoot = T.sources{ millSourcePath / 'src / 'main / 'antlr4 }

  def generateAntlrSources(p: Path, sourcePath: Path) = {
    val antlr = new Antlr4Config(sourcePath)
    mkdir! p
    antlr.runAntlr(p)
    p
  }

  def protobufSourceRoot = T.sources{ millSourcePath / 'src / 'main / 'proto }

  def generateProtobufSources(p: Path, sourcePath: Path) = {
    val protobuf = new Protobuf.ProtobufConfig(sourcePath)
    mkdir! p
    protobuf.runProtoc(p)
    p
  }

  override def generatedSources = T {
    val antlrSourcePath: Path = antlrSourceRoot().head.path
    val antlrSources = Seq(PathRef(generateAntlrSources(T.ctx().dest/'antlr, antlrSourcePath)))
    val protobufSourcePath: Path = protobufSourceRoot().head.path
    val protobufSources = Seq(PathRef(generateProtobufSources(T.ctx().dest/'proto, protobufSourcePath)))
    protobufSources ++ antlrSources
  }

  override def buildInfoMembers = T {
    Map[String, String](
      "buildInfoPackage" -> artifactName(),
      "version" -> publishVersion(),
      "scalaVersion" -> scalaVersion()
    )
  }
}
