// See LICENSE for license details.

package firrtl.options

import firrtl.AnnotationSeq

trait DriverExecutionResult

abstract class Driver {
  val optionsManager: ExecutionOptionsManager

  def execute(args: Array[String], initialAnnotations: AnnotationSeq): DriverExecutionResult

  def main(args: Array[String]): Unit = execute(args, Seq.empty)

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
}
