resolvers += Resolver.url("scalasbt", new URL("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases"))(
  Resolver.ivyStylePatterns
)
resolvers += Classpaths.sbtPluginReleases
resolvers += "jgit-repo" at "http://download.eclipse.org/jgit/maven"

addSbtPlugin("com.typesafe.sbt"  % "sbt-site"      % "1.4.0")
addSbtPlugin("org.scoverage"     % "sbt-scoverage" % "1.6.0")
addSbtPlugin("com.github.gseitz" % "sbt-protobuf"  % "0.6.5")

// Protobuf
addSbtPlugin("com.thesamet" % "sbt-protoc" % "0.99.20")
libraryDependencies += "com.thesamet.scalapb" %% "compilerplugin" % "0.9.0-M1"
