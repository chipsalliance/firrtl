// See LICENSE for license details.

package firrtl.options

import firrtl.{AnnotationSeq, FIRRTLException}
import scopt.OptionParser

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
  implicit final val parser = new OptionParser[AnnotationSeq](applicationName)
      with TerminateOnExit[AnnotationSeq]
      with DuplicateHandling[AnnotationSeq]

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

  /** The [[AnnotationSeq]] generated from command line arguments
    *
    * This requires lazy evaluation as subclasses will mixin new command
    * line options via methods of [[ExecutionOptionsManager.parser]]
    */
  lazy implicit final val options: AnnotationSeq = parser
    .parse(args, annotations)
    .getOrElse(throw new FIRRTLException("Failed to parse command line options"))
}
