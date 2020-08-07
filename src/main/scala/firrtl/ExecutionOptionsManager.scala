// See LICENSE for license details.

package firrtl

/** Firrtl output configuration specified by [[FirrtlExecutionOptions]]
  *
  * Derived from the fields of the execution options
  */
@deprecated("Will be removed in Firrtl 1.5", "FIRRTL 1.4")
sealed abstract class OutputConfig
@deprecated("Will be removed in Firrtl 1.5", "FIRRTL 1.4")
final case class SingleFile(targetFile: String) extends OutputConfig
@deprecated("Will be removed in Firrtl 1.5", "FIRRTL 1.4")
final case class OneFilePerModule(targetDir: String) extends OutputConfig