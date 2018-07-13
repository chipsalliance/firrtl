// See LICENSE for license details.

package firrtl

import scala.collection._
import scala.io.Source
import scala.sys.process.{BasicIO, ProcessLogger, stringSeqToProcess}
import scala.util.{Failure, Success, Try}
import scala.util.control.ControlThrowable
import java.io.File

import net.jcazevedo.moultingyaml._
import logger.Logger
import annotations._
import firrtl.annotations.AnnotationYamlProtocol._
import firrtl.passes.{PassException, PassExceptions}
import firrtl.transforms._
import firrtl.Utils.throwInternalError
import firrtl.options.{ExecutionOptionsManager, OptionsException}
import firrtl.options.Viewer._
import firrtl.FirrtlViewer._

/**
  * The Driver enables invocation of the FIRRTL compiler using command
  * line arguments (an [[Array[String]]]) or an
  * [[options.ExecutionOptionsManager]]. Both approaches are equivalent.
  *
  * Invocation using explicit command line arguments:
  * {{{
  * val args = Array(
  *   "--top-name",  "MyTopModule", // The name of the top module
  *   " --compiler", "verilog" )    // The compiler to use
  * Driver.execute(args)
  * }}}
  *
  * Invocation using an [[options.ExecutionOptionsManager]]:
  * {{{
  * val args = Array(
  *   "--top-name",  "MyTopModule", // The name of the top module
  *   " --compiler", "verilog" )    // The compiler to use
  * val optionsManager = new ExecutionOptionsManager("firrtl", args)
  * firrtl.Driver.execute(optionsManager)
  * }}}
  *
  * @see firrtlTests/DriverSpec.scala in the test directory for a lot more examples
  * @see [[CompilerUtils.mergeTransforms]] to see how customTransformations are inserted
  */

object Driver {
  val optionsManager = new ExecutionOptionsManager("firrtl") with HasFirrtlExecutionOptions

  /**
    * Print a warning message (in yellow)
    *
    * @param message error message
    */
  //scalastyle:off regex
  def dramaticWarning(message: String): Unit = {
    println(Console.YELLOW + "-"*78)
    println(s"Warning: $message")
    println("-"*78 + Console.RESET)
  }

  /**
    * Print an error message (in red)
    *
    * @param message error message
    * @note This does not stop the Driver.
    */
  //scalastyle:off regex
  def dramaticError(message: String): Unit = {
    println(Console.RED + "-"*78)
    println(s"Error: $message")
    println("-"*78 + Console.RESET)
  }

  private def execute(annotations: AnnotationSeq): FirrtlExecutionResult = {
    val firrtlOptions = view[FirrtlExecutionOptions](annotations).getOrElse{
      throw new FIRRTLException("Unable to determine FIRRTL options for provided command line options and annotations") }

    Logger.makeScope(firrtlOptions) {
      // Wrap compilation in a try/catch to present Scala MatchErrors in a more user-friendly format.
      val finalState = try {
        val circuit = firrtlOptions.getCircuit match {
          case Success(c) => c
          case Failure(OptionsException(msg)) =>
            dramaticError(msg)
            return FirrtlExecutionFailure(msg)
          case Failure(e) => throw e
        }

        // Does this need to be before calling compiler?
        firrtlOptions.makeTargetDir

        firrtlOptions.compiler.compile(
          CircuitState(circuit, ChirrtlForm, firrtlOptions.annotations),
          firrtlOptions.customTransforms
        )
      }
      catch {
        // Rethrow the exceptions which are expected or due to the runtime environment (out of memory, stack overflow)
        case p @ (_: ControlThrowable | _: PassException | _: PassExceptions | _: FIRRTLException) =>
          throw p
        // Treat remaining exceptions as internal errors.
        case e: Exception => throwInternalError(exception = Some(e))
      }

      // Do emission
      // Note: Single emission target assumption is baked in here
      // Note: FirrtlExecutionSuccess emitted is only used if we're emitting the whole Circuit
      val emittedRes = firrtlOptions.getOutputConfig match {
        case SingleFile(filename) =>
          val emitted = finalState.getEmittedCircuit
          val outputFile = new java.io.PrintWriter(filename)
          outputFile.write(emitted.value)
          outputFile.close()
          emitted.value
        case OneFilePerModule(dirName) =>
          val emittedModules = finalState.emittedComponents collect { case x: EmittedModule => x }
          if (emittedModules.isEmpty) throwInternalError() // There should be something
          emittedModules.foreach { module =>
            val filename = firrtlOptions.getBuildFileName(firrtlOptions.outputSuffix, Some(s"$dirName/${module.name}"))
            val outputFile = new java.io.PrintWriter(filename)
            outputFile.write(module.value)
            outputFile.close()
          }
          "" // Should we return something different here?
      }

      // If set, emit final annotations to a file
      firrtlOptions.outputAnnotationFileName match {
        case None =>
        case file =>
          val filename = firrtlOptions.getBuildFileName("anno.json", file)
          val outputFile = new java.io.PrintWriter(filename)
          outputFile.write(JsonProtocol.serialize(finalState.annotations))
          outputFile.close()
      }

      FirrtlExecutionSuccess(firrtlOptions.compilerName, emittedRes, finalState)
    }
  }

  /**
    * Run the FIRRTL compiler using provided command line arguments
    *
    * @param args command line arguments
    * @return the result of running the FIRRTL compiler
    */
  def execute(args: Array[String], initAnnos: AnnotationSeq = Seq.empty): FirrtlExecutionResult = {
    try {
      execute(optionsManager.parse(args, initAnnos)) match {
        case success: FirrtlExecutionSuccess => success
        case failure: FirrtlExecutionFailure =>
          optionsManager.showUsageAsError()
          failure
      }
    } catch {
      case e: FIRRTLException => throw e
    }
  }

  def main(args: Array[String]): Unit = {
    execute(args)
  }
}

object FileUtils {
  /**
    * recursive create directory and all parents
    *
    * @param directoryName a directory string with one or more levels
    * @return
    */
  def makeDirectory(directoryName: String): Boolean = {
    val dirFile = new java.io.File(directoryName)
    if(dirFile.exists()) {
      if(dirFile.isDirectory) {
        true
      }
      else {
        false
      }
    }
    else {
      dirFile.mkdirs()
    }
  }

  /**
    * recursively delete all directories in a relative path
    * DO NOT DELETE absolute paths
    *
    * @param directoryPathName a directory hierarchy to delete
    */
  def deleteDirectoryHierarchy(directoryPathName: String): Boolean = {
    deleteDirectoryHierarchy(new File(directoryPathName))
  }
  /**
    * recursively delete all directories in a relative path
    * DO NOT DELETE absolute paths
    *
    * @param file: a directory hierarchy to delete
    */
  def deleteDirectoryHierarchy(file: File, atTop: Boolean = true): Boolean = {
    if(file.getPath.split("/").last.isEmpty ||
      file.getAbsolutePath == "/" ||
      file.getPath.startsWith("/")) {
      Driver.dramaticError(s"delete directory ${file.getPath} will not delete absolute paths")
      false
    }
    else {
      val result = {
        if(file.isDirectory) {
          file.listFiles().forall( f => deleteDirectoryHierarchy(f)) && file.delete()
        }
        else {
          file.delete()
        }
      }
      result
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
