import ammonite.ops._
import ammonite.ops.ImplicitWd._
import mill._
import mill.scalalib._
import $ivy.`com.github.os72:protoc-jar:3.5.1`

case object ProtobufConfig {
  val version: String = "3.5.0"
  val ivyDep: String = s"com.google.protobuf:protobuf-java:${version}"
  def apply(sourcePath: Path): ProtobufConfig = new ProtobufConfig(sourcePath)
}

case class ProtobufConfig(val sourcePath: Path) {
  // Regex for protobuf source files.
  val protobufIncludeFilter = """.+\.proto""".r
  val protobufIncludePaths = Seq[Path](sourcePath)

  private[this] def executeProtoc(schemas: Set[Path], includePaths: Seq[Path], protocOptions: Seq[String]) : Int =
    try {
      val incPath = includePaths.map("-I" + _)
      val args = Seq[String]("-v351") ++ incPath ++ protocOptions ++ schemas.map(_.toString)
      com.github.os72.protocjar.Protoc.runProtoc(args.toArray)
    } catch { case e: Exception =>
      throw new RuntimeException("error occurred while compiling protobuf files: %s" format(e.getMessage), e)
    }

  def runProtoc(outputPath: Path): Unit = {
    // Find the protobuf source files we need to compile.
    val schemas = ls! sourcePath |? (f => protobufIncludeFilter.pattern.matcher(f.last).matches())
    if (!schemas.isEmpty) {
      executeProtoc(
        schemas = schemas.toSet,
        includePaths = protobufIncludePaths,
        protocOptions = Seq[String](s"--java_out=${outputPath.toString}")
      )
    } else {
      throw new RuntimeException(s"""mill.Protobuf: No schemas in ${sourcePath} matching "${protobufIncludeFilter.toString()}".r""")
    }
  }

}
