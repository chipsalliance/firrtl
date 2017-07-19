// See LICENSE for license details.

// sbt-site - sbt-ghpages

site.settings

site.includeScaladoc()

ghpages.settings

git.remoteRepo := "git@github.com:ucb-bar/firrtl.git"

// Firrtl code

organization := "edu.berkeley.cs"

name := "firrtl"

version := "1.0-SNAPSHOT_2017-07-19"

scalaVersion := "2.11.11"

javacOptions ++= Seq("-source", "1.7", "-target", "1.7")

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.7"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.5.0"

libraryDependencies += "net.jcazevedo" %% "moultingyaml" % "0.4.0"

// Assembly

assemblyJarName in assembly := "firrtl.jar"

test in assembly := {} // Should there be tests?

assemblyOutputPath in assembly := file("./utils/bin/firrtl.jar")

// ANTLRv4

antlr4Settings

antlr4GenVisitor in Antlr4 := true // default = false

antlr4GenListener in Antlr4 := false // default = true

antlr4PackageName in Antlr4 := Option("firrtl.antlr")

  publishMavenStyle := true
  publishArtifact in Test := false
  pomIncludeRepository := { x => false }
  pomExtra := <url>http://chisel.eecs.berkeley.edu/</url>
    <licenses>
      <license>
        <name>BSD-style</name>
        <url>http://www.opensource.org/licenses/bsd-license.php</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>https://github.com/ucb-bar/firrtl.git</url>
      <connection>scm:git:github.com/ucb-bar/firrtl.git</connection>
    </scm>
    <developers>
      <developer>
        <id>jackbackrack</id>
        <name>Jonathan Bachrach</name>
        <url>http://www.eecs.berkeley.edu/~jrb/</url>
      </developer>
    </developers>

  publishTo <<= version { v: String =>
    val nexus = "https://oss.sonatype.org/"
    if (v.trim.endsWith("SNAPSHOT")) {
      Some("snapshots" at nexus + "content/repositories/snapshots")
    }
    else {
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
    }
  }

  resolvers ++= Seq(
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases")
  )

// ScalaDoc

import UnidocKeys._

lazy val customUnidocSettings = unidocSettings ++ Seq (
  doc in Compile := (doc in ScalaUnidoc).value,
  target in unidoc in ScalaUnidoc := crossTarget.value / "api"
)

autoAPIMappings := true

scalacOptions in Compile in doc ++= Seq(
  "-diagrams",
  "-diagrams-max-classes", "25",
  "-doc-version", version.value,
  "-doc-title", name.value,
  "-doc-root-content", baseDirectory.value+"/root-doc.txt"
)

