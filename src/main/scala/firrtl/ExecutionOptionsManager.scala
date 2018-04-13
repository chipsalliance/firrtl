// See LICENSE for license details.

package firrtl

import firrtl.annotations.{
  Annotation,
  LegacyAnnotation,
  AnnotationFileNotFoundException,
  JsonProtocol }

import scopt.{OptionParser, OptionDef, Zero}
import scala.util.{Try, Failure}
import java.io.File
import net.jcazevedo.moultingyaml._
import firrtl.annotations.AnnotationYamlProtocol._

/** A modified [[scopt.OptionParser]] with mutable termination and additional checks
  *
  * @param name the name of the application
  */
sealed class AugmentedOptionParser(name: String) extends OptionParser[AnnotationSeq](name) {
  var terminateOnExit = true
  override def terminate(exitState: Either[String, Unit]): Unit =
    if (terminateOnExit) sys.exit(0)

  /* Check that no long or short option declarations are duplicated */
  override def parse(args: Seq[String], init: AnnotationSeq): Option[AnnotationSeq] = {
    val longDups = options.map(_.name).groupBy(identity).collect{ case (k, v) if v.size > 1 && k != "" => k }
    val shortDups = options.map(_.shortOpt).flatten.groupBy(identity).collect{ case (k, v) if v.size > 1 => k }
    def msg(x: String, y: String) = s"""Duplicate $x "$y" (did your custom Transform or OptionsManager add this?)"""
    if (longDups.nonEmpty)  { throw new FIRRTLException(msg("long option", shortDups.map("--" + _).mkString(","))) }
    if (shortDups.nonEmpty) { throw new FIRRTLException(msg("short option", shortDups.map("-" + _).mkString(","))) }
    super.parse(args, init)
  }
}

/** A store of command line options as an [[AnnotationSeq]]
  *
  * @param applicationName  The name shown in the usage
  * @param args Command line arguments to process
  * @param annotations Initial options to start with
  */
class ExecutionOptionsManager(
  val applicationName: String,
  args: Array[String],
  annotations: AnnotationSeq = AnnotationSeq(Seq.empty)) {

  /** Command line argument parser ([[scopt.OptionParser]]) with modifications */
  final val parser = new AugmentedOptionParser(applicationName)

  /** By default scopt calls sys.exit when --help is in options, this defeats that */
  def doNotExitOnHelp(): Unit = {
    parser.terminateOnExit = false
  }
  /** By default scopt calls sys.exit when --help is in options, this un-defeats doNotExitOnHelp */
  def exitOnHelp(): Unit = {
    parser.terminateOnExit = true
  }

  /** Show usage and exit */
  def showUsageAsError(): Unit = parser.showUsageAsError()

  /** The [[AnnotationSeq]] generated from command line arguments */
  lazy val options: AnnotationSeq = parser
    .parse(args, annotations)
    .getOrElse(throw new FIRRTLException("Failed to parse command line options"))
}

object ExecutionUtils {
  /** [todo] Add scaladoc */
  def readAnnotationsFromFile(fileNames: List[String]): List[Annotation] = fileNames
    .flatMap{ filename =>
      val file = new File(filename).getCanonicalFile
      // [todo] Check for implicit annotation file usage
      if (!file.exists) { throw new AnnotationFileNotFoundException(file) }
      JsonProtocol.deserializeTry(file).recoverWith { case jsonException =>
        // Try old protocol if new one fails
        Try {
          val yaml = io.Source.fromFile(file).getLines().mkString("\n").parseYaml
          val result = yaml.convertTo[List[LegacyAnnotation]]
          val msg = s"$file is a YAML file!\n" + (" "*9) + "YAML Annotation files are deprecated! Use JSON"
          Driver.dramaticWarning(msg)
          result
        }.orElse { // Propagate original JsonProtocol exception if YAML also fails
          Failure(jsonException)
        }
      }.get
    }

  /** todo] Add scaladoc */
  def readAnnotationsFromFile(filename: String): List[Annotation] = readAnnotationsFromFile(List(filename))

  /** Utilities that collect "early" information about options before
    * [[FirrtlExecutionOptions]] are available. These operate directly on
    * annotations and are safe to use while constructing
    * [[FirrtlExecutionOptions]]. */
  object Early {

    /** Determine the target directory with the following precedence:
      *   1) From --target-dir
      *   2) From the default supplied by [[FirrtlExecutionOptions]]
      *
      *  @param annotations input annotations to extract targetDir from
      *  @return the target directory
      */
    def targetDir(annotations: Seq[Annotation]): String = annotations
      .reverse
      .collectFirst{ case TargetDirAnnotation(dir) => dir }
      .getOrElse(new FirrtlExecutionOptions().targetDirName)

    /** Determine the top name using the following precedence:
      *   1) --top-name
      *   2) From the circuit "main" of --input-file
      *   3) From the circuit "main" of --firrtl-source
      *
      * @param annotations annotations to extract topName from
      * @return top module
      * @note --input-file and --firrtl-source are mutually exclusive
      */
    def topName(annotations: Seq[Annotation]): String = {
      var Seq(topName, inputFileName, firrtlSource) = Seq.fill(3)(None: Option[String]) //scalastyle:ignore
      annotations.foreach{
        case TopNameAnnotation(name)         => topName       = Some(name)
        case InputFileAnnotation(file)       => inputFileName = Some(file)
        case FirrtlSourceAnnotation(circuit) => firrtlSource  = Some(circuit)
        case _ => }
      topName.getOrElse {
        val circuit: String = if (inputFileName.nonEmpty) {
          io.Source.fromFile(inputFileName.get).getLines().mkString("\n")
        } else {
          firrtlSource.get
        }
        Parser.parse(circuit).main
      }
    }
  }
}
