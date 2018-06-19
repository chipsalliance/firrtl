// See LICENSE for license details

package firrtl.options

import firrtl.FIRRTLException
import scopt.OptionParser

trait TerminateOnExit[T] { this: OptionParser[T] =>
  var terminateOnExit = true
  override def terminate(exitState: Either[String, Unit]): Unit =
    if (terminateOnExit) sys.exit(0)
}

/** A modified [[scopt.OptionParser]] with mutable termination and additional checks
  *
  * @param name the name of the application
  */
trait DuplicateHandling[T] extends OptionParser[T] {
  /* Check that no long or short option declarations are duplicated */
  override def parse(args: Seq[String], init: T): Option[T] = {
    val longDups = options.map(_.name).groupBy(identity).collect{ case (k, v) if v.size > 1 && k != "" => k }
    val shortDups = options.map(_.shortOpt).flatten.groupBy(identity).collect{ case (k, v) if v.size > 1 => k }
    def msg(x: String, y: String) = s"""Duplicate $x "$y" (did your custom Transform or OptionsManager add this?)"""
    if (longDups.nonEmpty)  { throw new FIRRTLException(msg("long option", shortDups.map("--" + _).mkString(","))) }
    if (shortDups.nonEmpty) { throw new FIRRTLException(msg("short option", shortDups.map("-" + _).mkString(","))) }
    super.parse(args, init)
  }
}
