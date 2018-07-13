// See LICENSE for license details.

package firrtl

import scala.collection._
import scala.io.Source
import scala.sys.process.{BasicIO, ProcessLogger, stringSeqToProcess}
import scala.util.{Failure, Success, Try}
import scala.util.control.ControlThrowable
import java.io.{File, FileNotFoundException}

import net.jcazevedo.moultingyaml._
import logger.Logger
import Parser.{IgnoreInfo, InfoMode}
import annotations._
import firrtl.annotations.AnnotationYamlProtocol._
import firrtl.passes.{PassException, PassExceptions}
import firrtl.transforms._
import firrtl.Utils.throwInternalError
import firrtl.options.ExecutionOptionsManager
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

  /**
    * Load annotations from an [[options.ExecutionOptionsManager]]
    *
    * @param optionsManager use optionsManager config to load annotation file if it exists
    *                       update the firrtlOptions with new annotations if it does
    */
  @deprecated("This has no effect and is unnecessary due to ExecutionOptionsManager immutability", "1.1.0")
  def loadAnnotations(optionsManager: ExecutionOptionsManager with HasFirrtlExecutionOptions): Unit =
    Driver.dramaticWarning("Driver.loadAnnotations doesn't do anything, use Driver.getAnnotations instead")

  /**
    * Extract the annotations from an [[ExecutionOptionsManager]]
    *
    * @param optionsManager use optionsManager config to load annotation files
    * @return Annotations read from files
    */
  def getAnnotations(optionsManager: ExecutionOptionsManager with HasFirrtlExecutionOptions): Seq[Annotation] =
    optionsManager.firrtlOptions.annotations

  // Useful for handling erros in the options
  case class OptionsException(msg: String) extends Exception(msg)

  /** Get the Circuit from the compile options
    *
    * Handles the myriad of ways it can be specified
    */
  def getCircuit(implicit optionsManager: ExecutionOptionsManager with HasFirrtlExecutionOptions): Try[ir.Circuit] = {
    val firrtlOptions = view[FirrtlExecutionOptions].getOrElse{
      throw new FIRRTLException("Unable to determine FIRRTL options for provided command line options and annotations") }
    Try {
      // Check that only one "override" is used
      firrtlOptions.firrtlCircuit.getOrElse {
        firrtlOptions.firrtlSource.map(x => Parser.parseString(x, firrtlOptions.infoMode)).getOrElse {
          val inputFileName = firrtlOptions.getInputFileName(optionsManager)
          try {
            // TODO What does InfoMode mean to ProtoBuf?
            FirrtlExecutionUtils.getFileExtension(inputFileName) match {
              case ProtoBufFile => proto.FromProto.fromFile(inputFileName)
              case FirrtlFile => Parser.parseFile(inputFileName, firrtlOptions.infoMode)
            }
          }
          catch {
            case _: FileNotFoundException =>
              val message = s"Input file $inputFileName not found"
              throw new OptionsException(message)
          }
        }
      }
    }
  }

  /**
    * Run the FIRRTL compiler using the provided [[ExecutionOptionsManager]]
    *
    * @param optionsManager the desired flags to the compiler
    * @return the result of running the FIRRTL compiler
    */
  //scalastyle:off cyclomatic.complexity method.length
  def execute(implicit optionsManager: ExecutionOptionsManager with HasFirrtlExecutionOptions): FirrtlExecutionResult = {
    val firrtlOptions = view[FirrtlExecutionOptions].getOrElse{
      throw new FIRRTLException("Unable to determine FIRRTL options for provided command line options and annotations") }

    Logger.makeScope(optionsManager) {
      // Wrap compilation in a try/catch to present Scala MatchErrors in a more user-friendly format.
      val finalState = try {
        val circuit = getCircuit(optionsManager) match {
          case Success(c) => c
          case Failure(OptionsException(msg)) =>
            dramaticError(msg)
            return FirrtlExecutionFailure(msg)
          case Failure(e) => throw e
        }

        // Does this need to be before calling compiler?
        optionsManager.makeTargetDir()

        firrtlOptions.compiler.compile(
          CircuitState(circuit, ChirrtlForm, firrtlOptions.annotations),
          firrtlOptions.customTransforms
        )
      }
      catch {
        // Rethrow the exceptions which are expected or due to the runtime environment (out of memory, stack overflow)
        case p @ (_: ControlThrowable | _: PassException | _: PassExceptions | _: FIRRTLException)  => throw p
        // Treat remaining exceptions as internal errors.
        case e: Exception => throwInternalError(exception = Some(e))
      }

      // Do emission
      // Note: Single emission target assumption is baked in here
      // Note: FirrtlExecutionSuccess emitted is only used if we're emitting the whole Circuit
      val emittedRes = optionsManager.getOutputConfig match {
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
            val filename = optionsManager.getBuildFileName(firrtlOptions.outputSuffix, Some(s"$dirName/${module.name}"))
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
          val filename = optionsManager.getBuildFileName("anno.json", file)
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
  def execute(args: Array[String]): FirrtlExecutionResult = {
    try {
      val optionsManager = new ExecutionOptionsManager("firrtl", args) with HasFirrtlExecutionOptions
      execute(optionsManager) match {
        case success: FirrtlExecutionSuccess =>
          success
        case failure: FirrtlExecutionFailure =>
          optionsManager.showUsageAsError()
          failure
      }
    } catch {
      case e: FIRRTLException => FirrtlExecutionFailure("Failed to parse command line arguments")
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
