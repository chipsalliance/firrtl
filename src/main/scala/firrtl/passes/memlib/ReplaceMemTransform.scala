// See LICENSE for license details.

package firrtl.passes
package memlib

import firrtl._
import firrtl.ir._
import Annotations._
import AnalysisUtils._
import Utils.error
import java.io.{File, CharArrayWriter, PrintWriter}
import wiring._

sealed trait PassOption
case object InputConfigFileName extends PassOption
case object OutputConfigFileName extends PassOption
case object PassCircuitName extends PassOption

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
    val maskGran = m.maskGran
    val readers = List.fill(m.readers.length)("read")
    val writers = List.fill(m.writers.length)(if (maskGran.isEmpty) "write" else "mwrite")
    val readwriters = List.fill(m.readwriters.length)(if (maskGran.isEmpty) "rw" else "mrw")
    val ports = (writers ++ readers ++ readwriters) mkString ","
    val maskGranConf = maskGran match { case None => "" case Some(p) => s"mask_gran $p" }
    val width = bitWidth(m.dataType)
    val conf = s"name ${m.name} depth ${m.depth} width $width ports $ports $maskGranConf \n"
    outputBuffer.append(conf)
  }
  def serialize() = {
    val outputFile = new PrintWriter(filename)
    outputFile.write(outputBuffer.toString)
    outputFile.close()
  }
}

case class ReplSeqMemAnnotation(t: String, tID: TransID)
    extends Annotation with Loose with Unstable {

  val usage = """
[Optional] ReplSeqMem
  Pass to replace sequential memories with blackboxes + configuration file

Usage: 
  --replSeqMem -c:<circuit>:-i:<filename>:-o:<filename>
  *** Note: sub-arguments to --replSeqMem should be delimited by : and not white space!

Required Arguments:
  -o<filename>         Specify the output configuration file
  -c<compiler>         Specify the target circuit

Optional Arguments:
  -i<filename>         Specify the input configuration file (for additional optimizations)
"""    

  val passOptions = PassConfigUtil.getPassOptions(t, usage)
  val outputConfig = passOptions.getOrElse(
    OutputConfigFileName, 
    error("No output config file provided for ReplSeqMem!" + usage)
  )
  val passCircuit = passOptions.getOrElse(
    PassCircuitName, 
    error("No circuit name specified for ReplSeqMem!" + usage)
  )
  val target = CircuitName(passCircuit)
  def duplicate(n: Named) = this copy (t = t.replace(s"-c:$passCircuit", s"-c:${n.name}"))
}

case class SimpleTransform(p: Pass) extends Transform {
  def execute(c: Circuit, map: AnnotationMap): TransformResult =
    TransformResult(p.run(c))
}
class ReplSeqMem(transID: TransID) extends Transform with SimpleRun {
  def passSeq(inConfigFile: Option[YamlFileReader], outConfigFile: ConfWriter): Seq[Transform] =
    Seq(SimpleTransform(Legalize),
        SimpleTransform(ToMemIR),
        SimpleTransform(ResolveMaskGranularity),
        SimpleTransform(RenameAnnotatedMemoryPorts),
        SimpleTransform(ResolveMemoryReference),
        new CreateMemoryAnnotations(inConfigFile, TransID(-7), TransID(-8)),
        new ReplaceMemMacros(outConfigFile, TransID(-7), TransID(-8)),
        new WiringTransform(TransID(-8)),
        SimpleTransform(RemoveEmpty),
        SimpleTransform(CheckInitialization),
        SimpleTransform(InferTypes),
        SimpleTransform(Uniquify),
        SimpleTransform(ResolveKinds),
        SimpleTransform(ResolveGenders))
  def run(circuit: Circuit, map: AnnotationMap, xForms: Seq[Transform]): TransformResult = {
    (xForms foldLeft TransformResult(circuit, None, Some(map))){ case (tr: TransformResult, xForm: Transform) =>
      //val name = xForm.name
      //val x = Utils.time(name)(xForm.execute(tr.circuit, tr.annotation.get))
      val x = xForm.execute(tr.circuit, tr.annotation.get)
      //logger.debug(x.circuit.serialize)
      x.annotation match {
        case None => TransformResult(x.circuit, None, Some(map))
        case Some(ann) => TransformResult(x.circuit, None, Some(
          AnnotationMap(ann.annotations ++ tr.annotation.get.annotations)))
      }
    }
  }

  def execute(c: Circuit, map: AnnotationMap) = map get transID match {
    case Some(p) => p get CircuitName(c.main) match {
      case Some(ReplSeqMemAnnotation(t, _)) =>
        val inputFileName = PassConfigUtil.getPassOptions(t).getOrElse(InputConfigFileName, "")
        val inConfigFile = {
          if (inputFileName.isEmpty) None 
          else if (new File(inputFileName).exists) Some(new YamlFileReader(inputFileName))
          else error("Input configuration file does not exist!")
        }
        val outConfigFile = new ConfWriter(PassConfigUtil.getPassOptions(t)(OutputConfigFileName))
        run(c, map, passSeq(inConfigFile, outConfigFile))
      case _ => error("Unexpected transform annotation")
    }
    case _ => TransformResult(c)
  }
}
