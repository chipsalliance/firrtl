// SPDX-License-Identifier: Apache-2.0

package firrtl

import logger.LogLevel
import logger.{ClassLogLevelAnnotation, LogClassNamesAnnotation, LogFileAnnotation, LogLevelAnnotation}
import firrtl.annotations._
import firrtl.Parser.{AppendInfo, GenInfo, IgnoreInfo, InfoMode, UseInfo}
import firrtl.ir.Circuit
import firrtl.passes.memlib.{InferReadWriteAnnotation, ReplSeqMemAnnotation}
import firrtl.passes.clocklist.ClockListAnnotation
import firrtl.transforms.NoCircuitDedupAnnotation
import scopt.OptionParser
import firrtl.stage.{
  CompilerAnnotation,
  FirrtlCircuitAnnotation,
  FirrtlFileAnnotation,
  FirrtlSourceAnnotation,
  InfoModeAnnotation,
  OutputFileAnnotation,
  RunFirrtlTransformAnnotation
}
import firrtl.stage.phases.DriverCompatibility.{EmitOneFilePerModuleAnnotation, TopNameAnnotation}
import firrtl.options.{InputAnnotationFileAnnotation, OutputAnnotationFileAnnotation, ProgramArgsAnnotation, StageUtils}
import firrtl.transforms.{DontCheckCombLoopsAnnotation, NoDCEAnnotation}

import scala.collection.Seq

/** Firrtl output configuration specified by [[FirrtlExecutionOptions]]
  *
  * Derived from the fields of the execution options
  * @see [[FirrtlExecutionOptions.getOutputConfig]]
  */
sealed abstract class OutputConfig
final case class SingleFile(targetFile: String) extends OutputConfig
final case class OneFilePerModule(targetDir: String) extends OutputConfig
