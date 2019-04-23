package firrtl.util

import java.io.{File, IOException}
import java.nio.file.{Files, Path, Paths}
import java.util.{Collections, Comparator}

import scala.collection.JavaConverters._
import scala.collection.Seq
import scala.sys.process.{BasicIO, ProcessLogger, stringSeqToProcess}

object FileUtils {
  /**
    * recursive create directory and all parents
    *
    * @param directoryPath a directory Path with one or more levels
    * @return either an exception or the directory path
    */
  def makeDirectoryOrNot(directoryPath: Path): Either[Exception, Path] = {
    try {
      Right(Files.createDirectories(directoryPath))
    } catch {
      case ioe: IOException => Left(ioe)
    }
  }
  /**
    * recursive create directory and all parents
    *
    * @param directoryName a directory string with one or more levels
    * @return Boolean success or failure
    */
  def makeDirectory(directoryName: String): Boolean = {
    makeDirectoryOrNot(Paths.get(directoryName)) match {
      case Left(ex) =>
        System.err.println(ex)
        false
      case Right(path) => true
    }
  }

  /**
    * recursively delete all directories in a relative path
    * DO NOT DELETE absolute paths
    *
    * @param directoryPath a directory hierarchy to delete
    */
  def deleteDirectoryHierarchyOrNot(directoryPath: Path): Either[Exception, Path] = {
    if(directoryPath.isAbsolute) {
      Left(new IOException(s"delete directory ${directoryPath.toString} will not delete absolute paths"))
    } else {
      try {
        // Once we upgrade to Java 8 minimal support, we can use:
//        Files.walk(directoryPath).sorted(Comparator.reverseOrder()).iterator().asScala.foreach(Files.deleteIfExists)
        Files.walk(directoryPath).sorted(Collections.reverseOrder[Path]).iterator().asScala.foreach(Files.deleteIfExists)
        Right(directoryPath)
      } catch {
        case ioe: IOException => Left(ioe)
      }
    }
  }
  /**
    * recursively delete all directories in a relative path
    * DO NOT DELETE absolute paths
    *
    * @param file: a directory hierarchy to delete
    */
  def deleteDirectoryHierarchy(file: File): Boolean = {
    deleteDirectoryHierarchyOrNot(file.toPath) match {
      case Left(ex) =>
        System.err.println(ex)
        false
      case Right(path) => true
    }
  }
  /**
    * recursively delete all directories in a relative path
    * DO NOT DELETE absolute paths
    *
    * @param file: a directory hierarchy to delete
    */
  def deleteDirectoryHierarchy(fileName: String): Boolean = {
    deleteDirectoryHierarchyOrNot(Paths.get(fileName)) match {
      case Left(ex) =>
        System.err.println(ex)
        false
      case Right(path) => true
    }
  }

  /** Indicate if an external command (executable) is available (from the current PATH).
    *
    * @param cmd the command/executable plus any arguments to the command as a Seq().
    * @return true if ```cmd <args>``` returns a 0 exit status.
    */
  def isCommandAvailable(cmd: Seq[String]): Boolean = {
    // Eat any output.
    val sb = new StringBuffer
    val ioToDevNull = BasicIO(withIn = false, ProcessLogger(line => sb.append(line)))

    try {
      cmd.run(ioToDevNull).exitValue == 0
    } catch {
      case e: Throwable => false
    }
  }

  /** Indicate if an external command (executable) is available (from the current PATH).
    *
    * @param cmd the command/executable (without any arguments).
    * @return true if ```cmd``` returns a 0 exit status.
    */
  def isCommandAvailable(cmd:String): Boolean = {
    isCommandAvailable(Seq(cmd))
  }

  /** Flag indicating if vcs is available (for Verilog compilation and testing).
    * We used to use a bash command (`which ...`) to determine this, but this is problematic on Windows (issue #807).
    * Instead we try to run the executable itself (with innocuous arguments) and interpret any errors/exceptions
    *  as an indication that the executable is unavailable.
    */
  lazy val isVCSAvailable: Boolean = isCommandAvailable(Seq("vcs",  "-platform"))
}
