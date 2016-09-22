// See LICENSE for license details.

package firrtl

import Annotations._
import Parser.{InfoMode, IgnoreInfo, UseInfo, GenInfo, AppendInfo}

// There are two pass managers for Rocket Chip: one that's run in the "vsim"
// directory and one that's run in the "emulator" directory.  The command-line
// argument code (RocketChipDriver) figures out which one should be used.
class RocketChipVsimPassManager(inlineArgs: Option[String], inferRWArgs: Option[String], replSeqMemArgs: Option[String]) extends ReferencePassManager {
  private val inlineInstancesID = TransID(0)
  private val inferReadWriteID = TransID(-1)
  private val replSeqMemID = TransID(-2)

  override def operateMiddle = Seq(
    new passes.InferReadWrite(inferReadWriteID),
    new passes.ReplSeqMem(replSeqMemID)
  )

  override def operateLow = Seq(
    new passes.InlineInstances(TransID(0))
  )

  override def annotations =  {
    AnnotationMap(
      Seq(
        inlineArgs match {
          case Some(str) => str.split('.') match {
            case Array(circuit) =>
              Some(passes.InlineAnnotation(CircuitName(circuit), inlineInstancesID))
            case Array(circuit, module) =>
              Some(passes.InlineAnnotation(ModuleName(module, CircuitName(circuit)), inlineInstancesID))
            case Array(circuit, module, inst) =>
              Some(passes.InlineAnnotation((ComponentName(inst,ModuleName(module,CircuitName(circuit)))), inlineInstancesID))
          }
          case None => None
        },
        inferRWArgs match {
          case Some(str) => Some(passes.InferReadWriteAnnotation(str, inferReadWriteID))
          case None => None
        },
        replSeqMemArgs match {
          case Some(str) => Some(passes.ReplSeqMemAnnotation(str, replSeqMemID))
          case lNone => None
        }
      ).flatMap{ x => x }
    )
  }
}

// There's nothing special about the emulator's pass manager
class RocketChipEmulatorPassManager extends ReferencePassManager

case class RocketChipArgs(
  override val input: Option[String] = None,
  override val output: Option[String] = None,
  override val infoMode: Option[String] = None,
  replSeqMem: Option[String] = None,
  inferRW: Option[String] = None,
  inline: Option[String] = None,
  targetFormat: Option[String] = None
) extends ReferenceArgs(
  input = input,
  output = output,
  infoMode = infoMode
)

class RocketChipDriver extends ReferenceDriver with App {
  val parser = new scopt.OptionParser[RocketChipArgs]("firrtl") {
    head("firrtl", "1.0")

    opt[String]('i', "input")
      .required
      .action( (arg, args) => args.copy(input = Some(arg)) )
      .text("input FIRRTL file name")

    opt[String]('o', "output")
      .required
      .action( (arg, args) => args.copy(output = Some(arg)) )
      .text("output Verilog file name")

    opt[String]("info-mode")
      .optional
      .action( (arg, args) => args.copy(infoMode = Some(arg)) )
      .text("Supported modes: ignore, use, gen, append")

    opt[String]("replSeqMem")
      .optional
      .action( (arg, args) => args.copy(replSeqMem = Some(arg)) )
      .text("Replaces SeqMem with ExtModule, args of format \"-c:<circuit>:-i:<filename>:-o:<filename>\"")

    opt[String]("inferRW")
      .optional
      .action( (arg, args) => args.copy(inferRW = Some(arg)) )
      .text("Infers RW ports on SeqMems, args of format \"<circuit>\"")

    opt[String]("inline")
      .optional
      .action( (arg, args) => args.copy(inline = Some(arg)) )
      .text("Inline a module (e.g. \"MyModule\") or instance (e.g. \"MyModule.myinstance\")")

    opt[String]('X', "targetCompiler")
      .required
      .action( (arg, args) => args.copy(targetFormat = Some(arg)) )
      .text("Must pass \"verilog\"")

    help("help")
      .text("prints this usage text")
  }

  val parsedArgs = parser.parse(args, new RocketChipArgs) match {
    case Some(config) => config
    case None => error("Unable to parse command-line arguments")
  }

  // Validates some of the command-line arguments
  parsedArgs.targetFormat match {
    case Some("verilog") | None => Unit
    case _ => error("RocketChipDriver only supports Verilog compilation")
  }

  // One pass manager runs the SRAM stuff used by the "vsim" folder, and the
  // other just runs FIRRTL as used by the "emulator" folder.
  val passManager = (parsedArgs.replSeqMem, parsedArgs.inferRW, parsedArgs.inline) match {
    case (None, None, None) => new RocketChipEmulatorPassManager
    case (seqmem, rw, inline) => new RocketChipVsimPassManager(inline, rw, seqmem)
  }

  toVerilog(parsedArgs, passManager)
}

// This is here for backwards-compatibility, so I don't have to change any of
// the scripts that call FIRRTL.  Otherwise RocketChipDriver could just be the
// main object.
object Driver extends RocketChipDriver
