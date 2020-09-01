// See LICENSE for license details.

package firrtl.passes
package memlib

import firrtl._
import firrtl.annotations._
import firrtl.options.{HasShellOptions, ShellOption}
import Utils.error
import java.io.{CharArrayWriter, File, PrintWriter}
import wiring._
import firrtl.stage.{Forms, RunFirrtlTransformAnnotation}

sealed trait PassOption
case object InputConfigFileName extends PassOption
case object OutputConfigFileName extends PassOption
case object PassCircuitName extends PassOption
case object PassModuleName extends PassOption

object PassConfigUtil {
  type PassOptionMap = Map[PassOption, String]

  def getPassOptions(t: String, usage: String = "") = {
    // can't use space to delimit sub arguments (otherwise, Driver.scala will throw error)
    val passArgList = t.split(":").toList

    def nextPassOption(map: PassOptionMap, list: List[String]): PassOptionMap = {
      list match {
        case Nil => map
        case "-i" :: value :: tail =>
          nextPassOption(map + (InputConfigFileName -> value), tail)
        case "-o" :: value :: tail =>
          nextPassOption(map + (OutputConfigFileName -> value), tail)
        case "-c" :: value :: tail =>
          nextPassOption(map + (PassCircuitName -> value), tail)
        case "-m" :: value :: tail =>
          nextPassOption(map + (PassModuleName -> value), tail)
        case option :: tail =>
          error("Unknown option " + option + usage)
      }
    }
    nextPassOption(Map[PassOption, String](), passArgList)
  }
}

class ConfWriter(filename: String) {
  val outputBuffer = new CharArrayWriter
  def append(m: DefAnnotatedMemory) = {
    // legacy
    // assert that we don't overflow going from BigInt to Int conversion
    require(bitWidth(m.dataType) <= Int.MaxValue)
    m.maskGran.foreach { case x => require(x <= Int.MaxValue) }
    val conf = MemConf(
      m.name,
      m.depth,
      bitWidth(m.dataType).toInt,
      m.readers.length,
      m.writers.length,
      m.readwriters.length,
      m.maskGran.map(_.toInt)
    )
    outputBuffer.append(conf.toString)
  }
  def serialize() = {
    val outputFile = new PrintWriter(filename)
    outputFile.write(outputBuffer.toString)
    outputFile.close()
  }
}

case class ReplSeqMemAnnotation(inputFileName: String, outputConfig: String) extends NoTargetAnnotation

object ReplSeqMemAnnotation {
  def parse(t: String): ReplSeqMemAnnotation = {
    val usage = """
[Optional] ReplSeqMem
  Pass to replace sequential memories with blackboxes + configuration file

Usage:
  --replSeqMem -c:<circuit>:-i:<filename>:-o:<filename>
  *** Note: sub-arguments to --replSeqMem should be delimited by : and not white space!

Required Arguments:
  -o<filename>         Specify the output configuration file
  -c<circuit>          Specify the target circuit

Optional Arguments:
  -i<filename>         Specify the input configuration file (for additional optimizations)
"""

    val passOptions = PassConfigUtil.getPassOptions(t, usage)
    val outputConfig = passOptions.getOrElse(
      OutputConfigFileName,
      error("No output config file provided for ReplSeqMem!" + usage)
    )
    val inputFileName = PassConfigUtil.getPassOptions(t).getOrElse(InputConfigFileName, "")
    ReplSeqMemAnnotation(inputFileName, outputConfig)
  }
}

@deprecated(
  "Migrate to a transform that does not take arguments. This will be removed in 1.4.",
  "FIRRTL 1.3"
)
class SimpleTransform(p: Pass, form: CircuitForm) extends Transform {
  def inputForm = form
  def outputForm = form
  def execute(state: CircuitState): CircuitState = CircuitState(p.run(state.circuit), state.form, state.annotations)
}

class SimpleMidTransform(p: Pass) extends SimpleTransform(p, MidForm)

// SimpleRun instead of PassBased because of the arguments to passSeq
class ReplSeqMem extends Transform with HasShellOptions with DependencyAPIMigration {

  override def prerequisites = Forms.MidForm
  override def optionalPrerequisites = Seq.empty
  override def optionalPrerequisiteOf = Forms.MidEmitters
  override def invalidates(a: Transform) = a match {
    case InferTypes | ResolveKinds | ResolveFlows | LowerTypes => true
    case _                                                     => false
  }

  val options = Seq(
    new ShellOption[String](
      longOption = "repl-seq-mem",
      toAnnotationSeq =
        (a: String) => Seq(passes.memlib.ReplSeqMemAnnotation.parse(a), RunFirrtlTransformAnnotation(new ReplSeqMem)),
      helpText = "Blackbox and emit a configuration file for each sequential memory",
      shortOption = Some("frsq"),
      helpValueName = Some("-c:<circuit>:-i:<file>:-o:<file>")
    )
  )

  def transforms(inConfigFile: Option[YamlFileReader], outConfigFile: ConfWriter): Seq[Transform] =
    Seq(
      new SimpleMidTransform(Legalize),
      new SimpleMidTransform(ToMemIR),
      new SimpleMidTransform(ResolveMaskGranularity),
      new SimpleMidTransform(RenameAnnotatedMemoryPorts),
      new ResolveMemoryReference,
      new CreateMemoryAnnotations(inConfigFile),
      new ReplaceMemMacros(outConfigFile),
      new WiringTransform
    )

  def execute(state: CircuitState): CircuitState = {
    val annos = state.annotations.collect { case a: ReplSeqMemAnnotation => a }
    annos match {
      case Nil => state // Do nothing if there are no annotations
      case Seq(ReplSeqMemAnnotation(inputFileName, outputConfig)) =>
        val inConfigFile = {
          if (inputFileName.isEmpty) None
          else if (new File(inputFileName).exists) Some(new YamlFileReader(inputFileName))
          else error("Input configuration file does not exist!")
        }
        val outConfigFile = new ConfWriter(outputConfig)
        transforms(inConfigFile, outConfigFile).foldLeft(state) { (in, xform) => xform.runTransform(in) }
      case _ => error("Unexpected transform annotation")
    }
  }
}
