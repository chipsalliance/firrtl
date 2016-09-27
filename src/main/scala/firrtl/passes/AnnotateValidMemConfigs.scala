// See LICENSE for license details.

package firrtl.passes

import firrtl._
import firrtl.ir._
import firrtl.Mappers._
import Utils.error
import AnalysisUtils._

import net.jcazevedo.moultingyaml._
import net.jcazevedo.moultingyaml.DefaultYamlProtocol._
import scala.collection.mutable
import java.io.{File, CharArrayWriter, PrintWriter}

object CustomYAMLProtocol extends DefaultYamlProtocol {
  // bottom depends on top
  implicit val dr = yamlFormat4(DimensionRules)
  implicit val md = yamlFormat2(MemDimension)
  implicit val sr = yamlFormat4(SRAMRules)
  implicit val wm = yamlFormat2(WMaskArg)
  implicit val sc = yamlFormat11(SRAMCompiler)
}

case class DimensionRules(
    min: Int,
    // step size
    inc: Int,
    max: Int,
    // these values should not be used, regardless of min,inc,max
    illegal: Option[List[Int]]) {
  def getValid = {
    val range = (min to max by inc).toList
    range.filterNot(illegal.getOrElse(List[Int]()).toSet)
  }
}

case class MemDimension(
    rules: Option[DimensionRules],
    set: Option[List[Int]]) {
  require (
    if (rules.isEmpty) set.isDefined else set.isEmpty,
    "Should specify either rules or a list of valid options, but not both"
  )
  def getValid = set.getOrElse(rules.get.getValid).sorted
}

case class SRAMConfig(
    ymux: String = "",
    ybank: String = "",
    width: Int,
    depth: Int,
    xsplit: Int = 1,
    ysplit: Int = 1) {
  // how many duplicate copies of this SRAM are needed
  def num = xsplit * ysplit
  def serialize(pattern: String): String = {
    val fieldMap = getClass.getDeclaredFields.map { f => 
      f.setAccessible(true)
      f.getName -> f.get(this)
    } toMap

    val fieldDelimiter = """\[.*?\]""".r
    val configOptions = fieldDelimiter.findAllIn(pattern).toList

    configOptions.foldLeft(pattern)((b, a) => {
      // Expects the contents of [] are valid configuration fields (otherwise key match error)
      val fieldVal = {
        try fieldMap(a.substring(1, a.length-1)) 
        catch { case e: Exception => error("**SRAM config field incorrect**") }
      }
      b.replace(a, fieldVal.toString)
    } )
  }
}

// Ex: https://www.ece.cmu.edu/~ece548/hw/hw5/meml80.pdf
case class SRAMRules(
    // column mux parameter (for adjusting aspect ratio)
    ymux: (Int, String),
    // vertical segmentation (banking -- tradeoff performance / area)
    ybank: (Int, String),
    width: MemDimension,
    depth: MemDimension) {
  def getValidWidths = width.getValid
  def getValidDepths = depth.getValid
  def getValidConfig(width: Int, depth: Int): Option[SRAMConfig] = {
    if (getValidWidths.contains(width) && getValidDepths.contains(depth)) 
      Some(SRAMConfig(ymux = ymux._2, ybank = ybank._2, width = width, depth = depth))
    else
      None
  }
  def getValidConfig(m: DefMemory): Option[SRAMConfig] = getValidConfig(bitWidth(m.dataType).intValue, m.depth) 
} 

case class WMaskArg(
    t: String,
    f: String)

// vendor-specific compilers
case class SRAMCompiler(
    vendor: String,
    node: String,
    // i.e. RF, SRAM, etc.
    memType: String,
    portType: String,
    wMaskArg: Option[WMaskArg],
    // rules for valid SRAM flavors
    rules: Seq[SRAMRules],
    // path to executable 
    path: Option[String],
    // (output) config file path
    configFile: Option[String],
    // config pattern
    configPattern: Option[String],
    // read documentation for details 
    defaultArgs: Option[String],
    // default behavior (if not used) is to have wmask port width = datawidth/maskgran
    // if true: wmask port width pre-filled to datawidth
    fillWMask: Boolean) {
  require(portType == "RW" || portType == "R,W", "Memory must be single port RW or dual port R,W")
  require(
    (configFile.isDefined && configPattern.isDefined && wMaskArg.isDefined) || configFile.isEmpty,
    "Config pattern must be provided with config file"
  ) 
  def ymuxVals = rules.map(_.ymux._1).sortWith(_ < _)
  def ybankVals = rules.map(_.ybank._1).sortWith(_ > _)
  // TODO: verify this default ordering works out
  // optimize search for better FoM (area,power,clk); ymux has more effect
  def defaultSearchOrdering = for (x <- ymuxVals; y <- ybankVals) yield {
    rules.find(r => r.ymux._1 == x && r.ybank._1 == y).get
  }

  private val maskConfigOutputBuffer = new CharArrayWriter
  private val noMaskConfigOutputBuffer = new CharArrayWriter

  def append(m: DefMemory) : DefMemory = {
    val validCombos = (defaultSearchOrdering map (_ getValidConfig m)
      collect { case Some(config) => config })
    // non empty if successfully found compiler option that supports depth/width
    // TODO: don't just take first option
    val usedConfig = {
      if (validCombos.nonEmpty) validCombos.head
      else getBestAlternative(m)
    }
    val usesMaskGran = containsInfo(m.info, "maskGran")
    configPattern match {
      case None =>
      case Some(p) =>
        val newConfig = usedConfig.serialize(p) + "\n"
        val currentBuff = {
          if (usesMaskGran) maskConfigOutputBuffer 
          else noMaskConfigOutputBuffer
        }
        if (!currentBuff.toString.contains(newConfig)) currentBuff append newConfig
    }
    val temp = appendInfo(m.info, "sramConfig" -> usedConfig)
    val newInfo = if (usesMaskGran && fillWMask) appendInfo(temp, "maskGran" -> 1) else temp
    m copy (info = newInfo)
  }

  // TODO: Should you really be splitting in 2 if, say, depth is 1 more than allowed? should be thresholded and
  // handled w/ a separate set of registers ?
  // split memory until width, depth achievable via given memory compiler
  private def getInRange(m: SRAMConfig): Seq[SRAMConfig] = {
    val validXRange = mutable.ArrayBuffer[SRAMRules]()
    val validYRange = mutable.ArrayBuffer[SRAMRules]()
    defaultSearchOrdering foreach { r =>
      if (m.width <= r.getValidWidths.max) validXRange += r
      if (m.depth <= r.getValidDepths.max) validYRange += r
    }
    (validXRange.isEmpty, validYRange.isEmpty) match {
      case (true, true) =>
        getInRange(SRAMConfig(xsplit = 2*m.xsplit, ysplit = 2*m.ysplit, width = m.width/2, depth = m.depth/2))
      case (true, false) =>
        getInRange(SRAMConfig(xsplit = 2*m.xsplit, ysplit = m.ysplit, width = m.width/2, depth = m.depth))
      case (false, true) =>
        getInRange(SRAMConfig(xsplit = m.xsplit, ysplit = 2*m.ysplit, width = m.width, depth = m.depth/2))
      case (false, false) if validXRange.intersect(validYRange).nonEmpty =>
        Seq(m)
      case (false, false) =>
        getInRange(SRAMConfig(xsplit = m.xsplit, ysplit = 2*m.ysplit, width = m.width, depth = m.depth/2)) ++ 
        getInRange(SRAMConfig(xsplit = 2*m.xsplit, ysplit = m.ysplit, width = m.width/2, depth = m.depth))    
    }
  }

  private def getBestAlternative(m: DefMemory): SRAMConfig = {
    val validConfigs = getInRange(SRAMConfig(width = bitWidth(m.dataType).intValue, depth = m.depth))
    val minNum = validConfigs.map(_.num).min
    val validMinConfigs = validConfigs.filter(_.num == minNum)
    val validMinConfigsSquareness = validMinConfigs.map(
      x => math.abs(x.width.toDouble / x.depth - 1) -> x).toMap
    val squarestAspectRatio = validMinConfigsSquareness.unzip._1.min
    val validConfig = validMinConfigsSquareness(squarestAspectRatio)
    val validRules = defaultSearchOrdering filter (r =>
      (validConfig.width <= r.getValidWidths.max && validConfig.depth <= r.getValidDepths.max))
    // TODO: don't just take first option
    // TODO: More optimal split if particular value is in range but not supported
    // TODO: Support up to 2 read ports, 2 write ports; should be power of 2?
    val bestRule = validRules.head
    val memWidth = bestRule.getValidWidths.find(validConfig.width <= _).get
    val memDepth = bestRule.getValidDepths.find(validConfig.depth <= _).get
    (bestRule.getValidConfig(width = memWidth, depth = memDepth).get
      copy (xsplit = validConfig.xsplit, ysplit = validConfig.ysplit))
  }

  // TODO
  def serialize = ???
}  

// TODO: assumption that you would stick to just SRAMs or just RFs in a design -- is that true?
// Or is this where module-level transforms (rather than circuit-level) make sense?
class YamlFileReader(file: String) {
  import CustomYAMLProtocol._
  def parse[A](implicit reader: YamlReader[A]) : Seq[A] = {
    if (new File(file).exists) {
      val yamlString = scala.io.Source.fromFile(file).getLines.mkString("\n")
      yamlString.parseYamls flatMap (x =>
        try Some(reader read x)
        catch { case e: Exception => None }
      )
    }
    else error("Yaml file doesn't exist!")
  }
}

class YamlFileWriter(file: String) {
  import CustomYAMLProtocol._
  val outputBuffer = new CharArrayWriter
  val separator = "--- \n"
  def append(in: YamlValue) {
    outputBuffer append s"$separator${in.prettyPrint}"
  }
  def dump() {
    val outputFile = new PrintWriter(file)
    outputFile write outputBuffer.toString
    outputFile.close()
  }
}

class AnnotateValidMemConfigs(reader: Option[YamlFileReader]) extends Pass {
  import CustomYAMLProtocol._
  def name = "Annotate memories with valid split depths, widths, #\'s"

  // TODO: Consider splitting InferRW to analysis + actual optimization pass, in case sp doesn't exist
  // TODO: Don't get first available? 
  case class SRAMCompilerSet(
      sp: Option[SRAMCompiler] = None, 
      dp: Option[SRAMCompiler] = None) {
    def serialize() = {
      sp match {
        case None =>
        case Some(p) => p.serialize
      }
      dp match {
        case None =>
        case Some(p) => p.serialize
      }
    }
  }

  val sramCompilers = reader match {
    case None => None
    case Some(r) =>
      val compilers = r.parse[SRAMCompiler]
      val sp = compilers find (_.portType == "RW")
      val dp = compilers find (_.portType == "R,W")
      Some(SRAMCompilerSet(sp = sp, dp = dp))
  }

  def updateStmts(s: Statement): Statement = s match {
    case m: DefMemory if containsInfo(m.info, "useMacro") => sramCompilers match {
      case None => m
      case Some(compiler) if (m.readwriters.length == 1) =>
        compiler.sp match {
          case None => error("Design needs RW port memory compiler!")
          case Some(p) => p append m
        }
      case Some(compiler) =>
        compiler.dp match {
          case None => error("Design needs R,W port memory compiler!")
          case Some(p) => p append m
        }
    }
    case s => s map updateStmts
  }

  def run(c: Circuit) = c copy (modules = (c.modules map (_ map updateStmts)))
}
