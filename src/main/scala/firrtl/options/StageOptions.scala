// SPDX-License-Identifier: Apache-2.0

package firrtl.options

import java.io.File

/** Options that every stage shares
  * @param targetDir a target (build) directory
  * @param annotationFilesIn input annotation file
  * @param programArgs explicit program arguments
  * @param annotationFileOut an output annotation filename
  * @param dumpOnCrash on error, dump raw AnnotationSeq to a file
  */
class StageOptions private[firrtl] (
  val targetDir:         String = TargetDirAnnotation().directory,
  val annotationFilesIn: Seq[String] = Seq.empty,
  val annotationFileOut: Option[String] = None,
  val programArgs:       Seq[String] = Seq.empty,
  val writeDeleted:      Boolean = false,
  val dumpOnCrash:       Boolean = false) {

  private[options] def copy(
    targetDir:         String = targetDir,
    annotationFilesIn: Seq[String] = annotationFilesIn,
    annotationFileOut: Option[String] = annotationFileOut,
    programArgs:       Seq[String] = programArgs,
    writeDeleted:      Boolean = writeDeleted,
    dumpOnCrash:       Boolean = dumpOnCrash
  ): StageOptions = {

    new StageOptions(
      targetDir = targetDir,
      annotationFilesIn = annotationFilesIn,
      annotationFileOut = annotationFileOut,
      programArgs = programArgs,
      writeDeleted = writeDeleted,
      dumpOnCrash = dumpOnCrash
    )

  }

  /** Generate a filename (with an optional suffix) and create any parent directories. Suffix is only added if it is not
    * already there.
    * @param filename the name of the file
    * @param suffix an optional suffix that the file must end in
    * @return the name of the file
    * @note the filename may include a path
    */
  def getBuildFileName(filename: String, suffix: Option[String] = None): String = {
    require(filename.nonEmpty, "requested filename must not be empty")
    require(suffix.isEmpty || suffix.get.startsWith("."), s"suffix must start with '.', but got ${suffix.get}")

    /* Mangle the file in the following ways:
     *   1. Ensure that the file ends in the requested suffix
     *   2. Prepend the target directory if this is not an absolute path
     */
    val file = {
      val f = if (suffix.nonEmpty && !filename.endsWith(suffix.get)) {
        new File(filename + suffix.get)
      } else {
        new File(filename)
      }
      if (f.isAbsolute) {
        f
      } else {
        new File(targetDir + "/" + f)
      }
    }.toPath.normalize.toFile

    file.getParentFile match {
      case null                       =>
      case parent if (!parent.exists) => parent.mkdirs()
      case _                          =>
    }

    file.toString
  }

}
