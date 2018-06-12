// See LICENSE for license details.

package firrtl

import scopt.OptionParser

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

  /** The [[AnnotationSeq]] generated from command line arguments
    *
    * This requires lazy evaluation as subclasses will mixin new command
    * line options via methods of [[ExecutionOptionsManager.parser]]
    */
  lazy implicit final val options: AnnotationSeq = parser
    .parse(args, annotations)
    .getOrElse(throw new FIRRTLException("Failed to parse command line options"))
}

trait MoreOptions { this: ExecutionOptionsManager =>
  def newOptions(p: OptionParser[AnnotationSeq]): Unit
  newOptions(parser)
}

trait OptionsView[T] {
  def view(implicit options: AnnotationSeq): Option[T]
}

object Viewer {
  implicit def extractAnnotations(implicit m: ExecutionOptionsManager): AnnotationSeq = m.options

  def view[T](implicit optionsView: OptionsView[T], options: AnnotationSeq): Option[T] = optionsView.view
}
