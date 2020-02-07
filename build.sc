import mill._
import mill.scalalib._
import mill.scalalib.publish._
import mill.modules.Util

object firrtl extends ScalaModule with SbtModule with PublishModule {
  override def millSourcePath = super.millSourcePath / os.up

  def scalaVersion = "2.12.10"

  def publishVersion = "1.3-SNAPSHOT"

  def antlr4Version = "4.7.1"

  def protocVersion = "3.5.1"

  def mainClass = Some("firrtl.stage.FirrtlMain")

  override def scalacOptions = Seq(
    "-deprecation",
    "-unchecked",
    "-Yrangepos", // required by SemanticDB compiler plugin
    "-Xsource:2.11"
  )

  override def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"${scalaOrganization()}:scala-reflect:${scalaVersion()}",
    ivy"com.github.scopt::scopt:3.7.1",
    ivy"net.jcazevedo::moultingyaml:0.4.1",
    ivy"org.json4s::json4s-native:3.6.7",
    ivy"org.apache.commons:commons-text:1.7",
    ivy"org.antlr:antlr4-runtime:4.7.1",
    ivy"com.google.protobuf:protobuf-java:3.5.1"
  )

  override def generatedSources = T {
    generatedAntlr4Source() ++ generatedProtoSources()
  }

  /** antlr4 */

  def antlrSource = T.source {
    millSourcePath / 'src / 'main / 'antlr4 / "FIRRTL.g4"
  }

  def downloadAntlr4Jar = T {
    Util.download(s"https://www.antlr.org/download/antlr-$antlr4Version-complete.jar")
  }

  def generatedAntlr4Source = T.sources {
    os.proc("java",
      "-jar", downloadAntlr4Jar().path.toString,
      "-o", T.ctx.dest.toString,
      "-lib", antlrSource().path.toString,
      "-package", "firrtl.antlr",
      "-no-listener", "-visitor",
      antlrSource().path.toString
    ).call()
    T.ctx.dest
  }

  /** protoc */

  def protobufSource = T.source {
    millSourcePath / 'src / 'main / 'proto / "firrtl.proto"
  }

  def downloadProtocJar = T {
    Util.download(s"https://repo.maven.apache.org/maven2/com/github/os72/protoc-jar/$protocVersion/protoc-jar-$protocVersion.jar")
  }

  def generatedProtoSources = T.sources {
    os.proc("java",
      "-jar", downloadProtocJar().path.toString,
      "-I", protobufSource().path / os.up,
      s"--java_out=${T.ctx.dest.toString}",
      protobufSource().path.toString()
    ).call()
    T.ctx.dest / "firrtl"
  }

  def pomSettings = PomSettings(
    description = artifactName(),
    organization = "edu.berkeley.cs",
    url = "https://github.com/freechipsproject/firrtl",
    licenses = Seq(License.`BSD-3-Clause`),
    versionControl = VersionControl.github("freechipsproject", "firrtl"),
    developers = Seq(
      Developer("jackbackrack", "Jonathan Bachrach", "https://eecs.berkeley.edu/~jrb/")
    )
  )
}