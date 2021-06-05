// SPDX-License-Identifier: Apache-2.0

package firrtl.transforms

import firrtl._
import firrtl.annotations._

import scala.collection.immutable.ListSet

sealed trait BlackBoxHelperAnno extends Annotation

case class BlackBoxTargetDirAnno(targetDir: String) extends BlackBoxHelperAnno with NoTargetAnnotation {
  override def serialize: String = s"targetDir\n$targetDir"
}

case class BlackBoxResourceAnno(target: ModuleName, resourceId: String)
    extends BlackBoxHelperAnno
    with SingleTargetAnnotation[ModuleName] {
  def duplicate(n: ModuleName) = this.copy(target = n)
  override def serialize: String = s"resource\n$resourceId"
}

case class BlackBoxInlineAnno(target: ModuleName, name: String, text: String)
    extends BlackBoxHelperAnno
    with SingleTargetAnnotation[ModuleName] {
  def duplicate(n: ModuleName) = this.copy(target = n)
  override def serialize: String = s"inline\n$name\n$text"
}

case class BlackBoxPathAnno(target: ModuleName, path: String)
    extends BlackBoxHelperAnno
    with SingleTargetAnnotation[ModuleName] {
  def duplicate(n: ModuleName) = this.copy(target = n)
  override def serialize: String = s"path\n$path"
}

case class BlackBoxResourceFileNameAnno(resourceFileName: String) extends BlackBoxHelperAnno with NoTargetAnnotation {
  override def serialize: String = s"resourceFileName\n$resourceFileName"
}

/** Exception indicating that a blackbox wasn't found
  * @param fileName the name of the BlackBox file (only used for error message generation)
  * @param e an underlying exception that generated this
  */
class BlackBoxNotFoundException(fileName: String, message: String)
    extends FirrtlUserException(
      s"BlackBox '$fileName' not found. Did you misspell it? Is it in src/{main,test}/resources?\n$message"
    )

/** Handle source for Verilog ExtModules (BlackBoxes)
  *
  * This transform handles the moving of Verilog source for black boxes into the
  * target directory so that it can be accessed by verilator or other backend compilers
  * While parsing it's annotations it looks for a BlackBoxTargetDir annotation that
  * will set the directory where the Verilog will be written.  This annotation is typically be
  * set by the execution harness, or directly in the tests
  */
class BlackBoxSourceHelper extends Transform with DependencyAPIMigration {
  import BlackBoxSourceHelper._
  // @todo remove java.io
  private val DefaultTargetDir = new java.io.File(".")

  override def prerequisites = Seq.empty

  override def optionalPrerequisites = Seq.empty

  override def optionalPrerequisiteOf = Seq.empty

  override def invalidates(a: Transform) = false

  /** Collect BlackBoxHelperAnnos and and find the target dir if specified
    * @param annos a list of generic annotations for this transform
    * @return BlackBoxHelperAnnos and target directory
    * @todo deprecate java.io.File
    */
  def collectAnnos(annos: Seq[Annotation]): (ListSet[BlackBoxHelperAnno], java.io.File, java.io.File) =
    // @todo remove java.io.File
    annos.foldLeft((ListSet.empty[BlackBoxHelperAnno], DefaultTargetDir, new java.io.File(defaultFileListName))) {
      case ((acc, tdir, flistName), anno) =>
        anno match {
          case BlackBoxTargetDirAnno(dir) =>
            // @todo remove java.io.File
            val targetDir = new java.io.File(dir)
            if (!targetDir.exists()) { FileUtils.makeDirectory(targetDir.getAbsolutePath) }
            (acc, targetDir, flistName)
          // @todo remove java.io.File
          case BlackBoxResourceFileNameAnno(fileName) => (acc, tdir, new java.io.File(fileName))
          case a: BlackBoxHelperAnno => (acc + a, tdir, flistName)
          case _ => (acc, tdir, flistName)
        }
    }

  /**
    * write the verilog source for each annotation to the target directory
    * @note the state is not changed by this transform
    * @param state Input Firrtl AST
    * @return A transformed Firrtl AST
    * @throws BlackBoxNotFoundException if a Verilog source cannot be found
    */
  override def execute(state: CircuitState): CircuitState = {
    val (annos, targetDir, flistName) = collectAnnos(state.annotations)

    // @todo remove java.io.File
    val resourceFiles: ListSet[java.io.File] = annos.collect {
      case BlackBoxResourceAnno(_, resourceId) =>
        writeResourceToDirectory(resourceId, targetDir)
      case BlackBoxPathAnno(_, path) =>
        val fileName = path.split("/").last
        // @todo remove java.io.File
        val fromFile = new java.io.File(path)
        // @todo remove java.io.File
        val toFile = new java.io.File(targetDir, fileName)

        // @todo remove java.io.File
        val inputStream = safeFile(fromFile.toString)(new java.io.FileInputStream(fromFile).getChannel)
        // @todo remove java.io.File
        val outputStream = new java.io.FileOutputStream(toFile).getChannel
        outputStream.transferFrom(inputStream, 0, Long.MaxValue)

        toFile
    }

    // @todo remove java.io.File
    val inlineFiles: ListSet[java.io.File] = annos.collect {
      case BlackBoxInlineAnno(_, name, text) =>
        // @todo remove java.io.File
        val outFile = new java.io.File(targetDir, name)
        (text, outFile)
    }.map {
      case (text, file) =>
        writeTextToFile(text, file)
        file
    }

    // Issue #917 - We don't want to list Verilog header files ("*.vh") in our file list - they will automatically be included by reference.
    def isHeader(name: String) = name.endsWith(".h") || name.endsWith(".vh") || name.endsWith(".svh")
    val verilogSourcesOnly = (resourceFiles ++ inlineFiles).filterNot { f => isHeader(f.getName()) }
    // @todo remove java.io.File
    val filelistFile = if (flistName.isAbsolute()) flistName else new java.io.File(targetDir, flistName.getName())

    // We need the canonical path here, so verilator will create a path to the file that works from the targetDir,
    //  and, so we can compare the list of files automatically included, with an explicit list provided by the client
    //  and reject duplicates.
    // If the path isn't canonical, when make tries to determine dependencies based on the *__ver.d file, we end up with errors like:
    //  make[1]: *** No rule to make target `test_run_dir/examples.AccumBlackBox_PeekPokeTest_Verilator345491158/AccumBlackBox.v', needed by `.../chisel-testers/test_run_dir/examples.AccumBlackBox_PeekPokeTest_Verilator345491158/VAccumBlackBoxWrapper.h'.  Stop.
    //  or we end up including the same file multiple times.
    if (verilogSourcesOnly.nonEmpty) {
      writeTextToFile(verilogSourcesOnly.map(_.getCanonicalPath).mkString("\n"), filelistFile)
    }

    state
  }
}

object BlackBoxSourceHelper {

  /** Safely access a file converting [[FileNotFoundException]]s and [[NullPointerException]]s into
    * [[BlackBoxNotFoundException]]s
    * @param fileName the name of the file to be accessed (only used for error message generation)
    * @param code some code to run
    */
  private def safeFile[A](fileName: String)(code: => A) = try { code }
  catch {
    // @todo remove java.io.File
    case e @ (_: java.io.FileNotFoundException | _: NullPointerException) =>
      throw new BlackBoxNotFoundException(fileName, e.getMessage)
  }

  /**
    * finds the named resource and writes into the directory
    * @param name the name of the resource
    * @param dir the directory in which to write the file
    * @return the closed File object
    * @todo deprecate java.io.File
    */
  def writeResourceToDirectory(name: String, dir: java.io.File): java.io.File = {
    val fileName = name.split("/").last
    // @todo remove java.io.File
    val outFile = new java.io.File(dir, fileName)
    copyResourceToFile(name, outFile)
    outFile
  }

  /**
    * finds the named resource and writes into the directory
    * @param name the name of the resource
    * @param file the file to write it into
    * @throws BlackBoxNotFoundException if the requested resource does not exist
    * @todo deprecate java.io.File
    */
  def copyResourceToFile(name: String, file: java.io.File): Unit = {
    val in = getClass.getResourceAsStream(name)
    // @todo remove java.io
    val out = new java.io.FileOutputStream(file)
    safeFile(name)(Iterator.continually(in.read).takeWhile(-1 != _).foreach(out.write))
    out.close()
  }

  val defaultFileListName = "firrtl_black_box_resource_files.f"

  @deprecated(
    "Renamed to defaultFileListName, as the file list name may now be changed with an annotation",
    "FIRRTL 1.2"
  )
  def fileListName = defaultFileListName

  // @todo deprecate java.io
  def writeTextToFile(text: String, file: java.io.File): Unit = {
    val out = new java.io.PrintWriter(file)
    out.write(text)
    out.close()
  }
}
