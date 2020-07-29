// See LICENSE for license details.

package firrtl.options

import firrtl.AnnotationSeq

import scopt.OptionParser

case object OptionsHelpException extends Exception("Usage help invoked")

/** OptionParser mixin that causes the OptionParser to not call exit (call `sys.exit`) if the `--help` option is
  * passed */
trait DoNotTerminateOnExit { this: OptionParser[_] =>
  override def terminate(exitState: Either[String, Unit]): Unit = ()
}

/** OptionParser mixin that converts to [[OptionsException]]
  *
  * Scopt, by default, will print errors to stderr, e.g., invalid arguments will do this. However, a [[Stage]] uses
  * [[StageUtils.dramaticError]]. By converting this to an [[OptionsException]], a [[Stage]] can then catch the error an
  * convert it to an [[OptionsException]] that a [[Stage]] can get at.
  */
trait ExceptOnError { this: OptionParser[_] =>
  override def reportError(msg: String): Unit = throw new OptionsException(msg)
}
