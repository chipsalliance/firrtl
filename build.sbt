// See LICENSE for license details.

enablePlugins(SiteScaladocPlugin)

lazy val commonSettings = Seq(
  organization := "edu.berkeley.cs",
  name := "firrtl",
  version := "1.4-SNAPSHOT",
  scalaVersion := "2.13.3",
  crossScalaVersions := Seq("2.11.12", "2.12.11", "2.13.3"),
  scalacOptions := Seq(
    "-deprecation",
    "-unchecked",
    "-language:reflectiveCalls",
    "-language:implicitConversions",
  ),
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "org.scalatest" %% "scalatest" % "3.1.2" % "test",
    "org.scalatestplus" %% "scalacheck-1-14" % "3.1.1.1" % "test",
    "com.github.scopt" %% "scopt" % "4.0.0-RC2",
    "net.jcazevedo" %% "moultingyaml" % "0.4.2",
    "org.json4s" %% "json4s-native" % "3.6.8",
    "org.apache.commons" % "commons-text" % "1.8"
  ),
  resolvers ++= Seq(
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases")
  )
)

lazy val protobufSettings = Seq(
  sourceDirectory in ProtobufConfig := baseDirectory.value / "src" / "main" / "proto",
  protobufRunProtoc in ProtobufConfig := (args =>
    com.github.os72.protocjar.Protoc.runProtoc("-v351" +: args.toArray)
  ),
  javaSource in ProtobufConfig := (sourceManaged in Compile).value
)

lazy val assemblySettings = Seq(
  assemblyJarName in assembly := "firrtl.jar",
  test in assembly := {},
  assemblyOutputPath in assembly := file("./utils/bin/firrtl.jar")
)


lazy val testAssemblySettings = Seq(
  test in (Test, assembly) := {}, // Ditto above
  assemblyMergeStrategy in (Test, assembly) := {
    case PathList("firrtlTests", xs @ _*) => MergeStrategy.discard
    case x =>
      val oldStrategy = (assemblyMergeStrategy in (Test, assembly)).value
      oldStrategy(x)
  },
  assemblyJarName in (Test, assembly) := s"firrtl-test.jar",
  assemblyOutputPath in (Test, assembly) := file("./utils/bin/" + (Test / assembly / assemblyJarName).value)
)

lazy val antlrSettings = Seq(
  antlr4GenVisitor in Antlr4 := true,
  antlr4GenListener in Antlr4 := false,
  antlr4PackageName in Antlr4 := Option("firrtl.antlr"),
  antlr4Version in Antlr4 := "4.7.1",
  javaSource in Antlr4 := (sourceManaged in Compile).value
)

lazy val publishSettings = Seq(
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := { x => false },
  // Don't add 'scm' elements if we have a git.remoteRepo definition,
  //  but since we don't (with the removal of ghpages), add them in below.
  pomExtra := <url>http://chisel.eecs.berkeley.edu/</url>
    <licenses>
      <license>
        <name>BSD-style</name>
        <url>http://www.opensource.org/licenses/bsd-license.php</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>https://github.com/freechipsproject/firrtl.git</url>
      <connection>scm:git:github.com/freechipsproject/firrtl.git</connection>
    </scm>
    <developers>
      <developer>
        <id>jackbackrack</id>
        <name>Jonathan Bachrach</name>
        <url>http://www.eecs.berkeley.edu/~jrb/</url>
      </developer>
    </developers>,
  publishTo := {
    val v = version.value
    val nexus = "https://oss.sonatype.org/"
    if (v.trim.endsWith("SNAPSHOT")) {
      Some("snapshots" at nexus + "content/repositories/snapshots")
    } else {
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
    }
  }
)


def scalacDocOptionsVersion(scalaVersion: String): Seq[String] = {
  Seq() ++ {
    // If we're building with Scala > 2.11, enable the compile option
    //  to flag warnings as errors. This must be disabled for 2.11 since
    //  references to the Java class library from Java 9 on generate warnings.
    //  https://github.com/scala/bug/issues/10675
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, scalaMajor: Long)) if scalaMajor < 12 => Seq()
      case _ => Seq("-Xfatal-warnings")
    }
  }
}
lazy val docSettings = Seq(
  doc in Compile := (doc in ScalaUnidoc).value,
  autoAPIMappings := true,
  scalacOptions in Compile in doc ++= Seq(
    "-feature",
    "-diagrams",
    "-diagrams-max-classes", "25",
    "-doc-version", version.value,
    "-doc-title", name.value,
    "-doc-root-content", baseDirectory.value+"/root-doc.txt",
    "-sourcepath", (baseDirectory in ThisBuild).value.toString,
    "-doc-source-url",
    {
      val branch =
        if (version.value.endsWith("-SNAPSHOT")) {
          "master"
        } else {
          s"v${version.value}"
        }
      s"https://github.com/freechipsproject/firrtl/tree/$branch€{FILE_PATH}.scala"
    }
  ) ++ scalacDocOptionsVersion(scalaVersion.value)
)

lazy val firrtl = (project in file("."))
  .enablePlugins(ProtobufPlugin)
  .enablePlugins(ScalaUnidocPlugin)
  .enablePlugins(Antlr4Plugin)
  .settings(
    fork := true,
    Test / testForkedParallel := true
  )
  .settings(commonSettings)
  .settings(protobufSettings)
  .settings(antlrSettings)
  .settings(assemblySettings)
  .settings(inConfig(Test)(baseAssemblySettings))
  .settings(testAssemblySettings)
  .settings(publishSettings)
  .settings(docSettings)

lazy val benchmark = (project in file("benchmark"))
  .dependsOn(firrtl)
  .settings(
    assemblyJarName in assembly := "firrtl-benchmark.jar",
    test in assembly := {},
    assemblyOutputPath in assembly := file("./utils/bin/firrtl-benchmark.jar")
  )

// Aliases
addCommandAlias("rel", "reload")
addCommandAlias("com", "all compile test:compile it:compile")
addCommandAlias("fix", "all compile:scalafix test:scalafix")
addCommandAlias("fmt", "all scalafmtSbt scalafmtAll")
