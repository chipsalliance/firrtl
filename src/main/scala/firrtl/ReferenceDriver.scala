// See LICENSE for details.

package firrtl

import scala.io.Source
import Annotations._
import Parser.{InfoMode, IgnoreInfo, UseInfo, GenInfo, AppendInfo}
import ir._

// It's expected that other drivers that will be subclassing ReferenceDriver
// will fill out this structure from command-line arguments.
abstract class ReferenceArgs(
  val input: Option[String],
  val output: Option[String],
  val infoMode: Option[String]
)

// A simple driver that will run a FIRRTL compilation.  Users should extend
// this to handle the command-line argument parsing that's relative to their
// FIRRTL drivers and then call "toVerilog" to compile a FIRRTL circuit to
// Verilog.
abstract class ReferenceDriver {
  // A helper method, in case the driver wants to open the circuit before
  // compiling (to check the validity of command-line arguments, for example)
  def readInput(args: ReferenceArgs) = {
    val input = args.input match {
      case Some(str) => str
      case None => error("--input is a required argument")
    }

    val infoMode = args.infoMode match {
      case (Some("use") | None) => UseInfo
      case Some("ignore") => IgnoreInfo
      case Some("gen") => GenInfo(input)
      case Some("append") => AppendInfo(input)
      case Some(mode) => error("Unknown info mode " + mode)
    }

    Parser.parse(Source.fromFile(input).getLines, infoMode)
  }

  // The meat of this driver: produces a Verilog file from a FIRRTL file.
  def toVerilog(args: ReferenceArgs,
                pm: ReferencePassManager,
                input: Option[Circuit] = None): Unit = {
    val output = args.output match {
      case Some(str) => str
      case None => error("--output is a required argument")
    }

    val parsedInput = input match {
      case Some(input) => input
      case None => readInput(args)
    }

    val outputBuffer = new java.io.CharArrayWriter
    pm.compile(parsedInput, pm.annotations, outputBuffer)

    val outputFile = new java.io.PrintWriter(output)
    outputFile.write(outputBuffer.toString)
    outputFile.close()
  }
}
