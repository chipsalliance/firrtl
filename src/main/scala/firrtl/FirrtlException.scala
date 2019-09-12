// See LICENSE for license details.

package firrtl

import scala.util.control.NoStackTrace

@deprecated("External users should use either FirrtlUserException or their own hierarchy", "1.2")
object FIRRTLException {
  def defaultMessage(message: String, cause: Throwable) = {
    if (message != null) {
      message
    } else if (cause != null) {
      cause.toString
    } else {
      null
    }
  }
}
@deprecated("External users should use either FirrtlUserException or their own hierarchy", "1.2")
class FIRRTLException(val str: String, cause: Throwable = null)
  extends RuntimeException(FIRRTLException.defaultMessage(str, cause), cause)

/** Exception indicating user error
  *
  * These exceptions indicate a problem due to bad input and thus do not include a stack trace.
  * This can be extended by custom transform writers.
  */
class FirrtlUserException(message: String, cause: Throwable = null)
  extends RuntimeException(message, cause) with NoStackTrace

/** Wraps exceptions from CustomTransforms so they can be reported appropriately */
case class CustomTransformException(cause: Throwable) extends Exception("", cause)

/** Exception indicating something went wrong *within* Firrtl itself
  *
  * These exceptions indicate a problem inside the compiler and include a stack trace to help
  * developers debug the issue.
  *
  * This class is private because these are issues within Firrtl itself. Exceptions thrown in custom
  * transforms are treated differently and should thus have their own structure
  */
private[firrtl] class FirrtlInternalException(message: String, cause: Throwable = null)
  extends Exception(message, cause)
