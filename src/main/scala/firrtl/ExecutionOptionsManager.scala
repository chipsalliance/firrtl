// See License

package firrtl

import firrtl.Annotations._
import firrtl.Parser._
import scopt.OptionParser

import scala.collection.Seq

/**
  * Use this trait to define an options class that can add its private command line options to a externally
  * declared parser
  */
trait ComposableOptions

/**
  * Most of the chisel toolchain components require a topName which defines a circuit or a device under test.
  * Much of the work that is done takes place in a directory.
  * It would be simplest to require topName to be defined but in practice it is preferred to defer this.
  * For example, in chisel, by deferring this it is possible for the execute there to first elaborate the
  * circuit and then set the topName from that if it has not already been set.
  */
case class CommonOptions(
                          topName:       String = "",
                          targetDirName: String = "test_run_dir"
                        ) extends ComposableOptions {

  val key = "CommonOptions"

}

abstract class HasParser(applicationName: String) {
  val parser: OptionParser[Unit] = new OptionParser[Unit](applicationName) {}
}

trait HasCommonOptions {
  self: ExecutionOptionsManager =>
  var commonOptions = CommonOptions()
  addToParser()

  def addToParser(): Unit = {
    parser.note("common options")

    parser.opt[String]("top-name").abbr("tn").valueName("<top-level-circuit-name>").foreach { x =>
      commonOptions = commonOptions.copy(topName = x)
    }.text("This options defines the top level circuit, defaults to dut when possible")
    parser.opt[String]("target-dir").abbr("td").valueName("<target-directory>").foreach { x =>
      commonOptions = commonOptions.copy(targetDirName = x)
    }.text("This options defines a work directory for intermediate files")

    parser.help("help").text("prints this usage text")
  }
}

/**
  * The options that firrtl supports in callable component sense
  *
  * @param inputFileNameOverride  default is targetDir/topName.fir
  * @param outputFileNameOverride default is targetDir/topName.v  the .v is based on the compilerName parameter
  * @param compilerName           which compiler to use
  * @param annotations            annotations to pass to compiler
  */

case class FirrtlExecutionOptions(
    inputFileNameOverride:  String = "",
    outputFileNameOverride: String = "",
    compilerName:           String = "verilog",
    infoModeName:           String = "use",
    inferRW:                Seq[String] = Seq.empty,
    firrtlSource:           Option[String] = None,
    annotations:            List[Annotation] = List.empty)
  extends ComposableOptions {


  def infoMode: InfoMode = {
    infoModeName match {
      case "use" => UseInfo
      case "ignore" => IgnoreInfo
      case "gen" => GenInfo(inputFileNameOverride)
      case "append" => AppendInfo(inputFileNameOverride)
      case other => UseInfo
    }
  }

  def compiler: Compiler = {
    compilerName match {
      case "high"      => new HighFirrtlCompiler()
      case "low"       => new LowFirrtlCompiler()
      case "verilog"   => new VerilogCompiler()
    }
  }

  def outputSuffix: String = {
    compilerName match {
      case "verilog"   => "v"
      case "low"       => "lo.fir"
      case "high"      => "hi.fir"
      case _ =>
        throw new Exception(s"Illegal compiler name $compilerName")
    }
  }

  /**
    * build the input file name, taking overriding parameters
    *
    * @param optionsManager this is needed to access build function and its common options
    * @return a properly constructed input file name
    */
  def getInputFileName(optionsManager: ExecutionOptionsManager): String = {
    optionsManager.getBuildFileName("fir", inputFileNameOverride)
  }
  /**
    * build the output file name, taking overriding parameters
    *
    * @param optionsManager this is needed to access build function and its common options
    * @return
    */
  def getOutputFileName(optionsManager: ExecutionOptionsManager): String = {
    optionsManager.getBuildFileName(outputSuffix, outputFileNameOverride)
  }
}

trait HasFirrtlOptions {
  self: ExecutionOptionsManager =>
  var firrtlOptions = FirrtlExecutionOptions()

  parser.note("firrtl options")

    parser.opt[String]("input-file")
    .abbr("fif")
    .valueName ("<firrtl-source>")
    .foreach { x =>
      firrtlOptions = firrtlOptions.copy(inputFileNameOverride = x)
    }.text {
      "use this to override the top name default"
    }

    parser.opt[String]("output-file")
    .abbr("fof")
    .valueName ("<output>").
    foreach { x =>
      firrtlOptions = firrtlOptions.copy(outputFileNameOverride = x)
    }.text {
      "use this to override the default name"
    }

    parser.opt[String]("compiler")
    .abbr("fc")
    .valueName ("<high|low|verilog>")
    .foreach { x =>
      firrtlOptions = firrtlOptions.copy(compilerName = x)
    }.validate { x =>
    if (Array("high", "low", "verilog").contains(x.toLowerCase)) parser.success
    else parser.failure(s"$x not a legal compiler")
  }.text {
    "compiler to use, default is verilog"
  }

    parser.opt[String]("info-mode")
    .valueName ("<ignore|use|gen|append>")
    .foreach { x =>
      firrtlOptions = firrtlOptions.copy(infoModeName = x.toLowerCase)
    }.validate { x =>
    if (Array("ignore", "use", "gen", "append").contains(x.toLowerCase)) parser.success
    else parser.failure(s"$x not a legal compiler")
  }.text {
    "specifies the source info handling"
  }

    parser.opt[Seq[String]]("in-line")
    .abbr("fil")
    .valueName ("<circuit>[.<module>[.<instance>]][,..],")
    .foreach { x =>
      val newAnnotations = x.map { value =>
        value.split('.') match {
          case Array(circuit) =>
            passes.InlineAnnotation(CircuitName(circuit), TransID(0))
          case Array(circuit, module) =>
            passes.InlineAnnotation(ModuleName(module, CircuitName(circuit)), TransID(0))
          case Array(circuit, module, inst) =>
            passes.InlineAnnotation(ComponentName(inst, ModuleName(module, CircuitName(circuit))), TransID(0))
        }
      }
      firrtlOptions = firrtlOptions.copy(annotations = firrtlOptions.annotations ++ newAnnotations)
    }.text {
    """Inline one or more module (comma separated, no spaces) module looks like "MyModule" or "MyModule.myinstance"""
  }

    parser.opt[Seq[String]]("infer-rw")
    .abbr("firw")
    .valueName ("<circuit[,...]>")
    .foreach { x =>
      firrtlOptions = firrtlOptions.copy(
        annotations = firrtlOptions.annotations ++
          x.map { value => passes.InferReadWriteAnnotation(value, TransID(-1)) }
      )
    }.text {
    "Enable readwrite port inference for the target circuit, for multiples separate with commas and no spaces"
  }

    parser.opt[Seq[String]]("repl-seq-mem")
    .abbr("frsq")
    .valueName ("-c:<circuit>:-i:<filename>:-o:<filename>[,...]")
    .foreach { x =>
      firrtlOptions = firrtlOptions.copy(
        annotations = firrtlOptions.annotations ++ x.map { value => passes.ReplSeqMemAnnotation(value, TransID(-2))}
      )
    }
    .text {
      "Replace sequential memories with blackboxes + configuration file"
    }

  parser.note("Input configuration file optional")
  parser.note("Note: sub-arguments to --replSeqMem should be delimited by : and not white space!")
  parser.note("for multiples separate with commas and no spaces")
  parser.note("")

}

trait FirrtlExecutionResult

/**
  * Indicates a successful execution of the firrtl compiler, returning the compiled result and
  * the type of compile
  *
  * @param emitType  The name of the compiler used, currently "high", "low", or "verilog"
  * @param emitted   The text result of the compilation, could be verilog or firrtl text.
  */
case class FirrtlExecutionSuccess(
                                   emitType: String,
                                   emitted:  String
                                 ) extends FirrtlExecutionResult

/**
  * The firrtl compilation failed.
  *
  * @param message  Some kind of hint as to what went wrong.
  */
case class FirrtlExecutionFailure(message: String) extends FirrtlExecutionResult

/**
  *
  * @param applicationName  The name shown in the usage
  */
class ExecutionOptionsManager(val applicationName: String) extends HasParser(applicationName) with HasCommonOptions {

  def parse(args: Array[String]): Boolean = {
    parser.parse(args)
  }

  def showUsageAsError(): Unit = parser.showUsageAsError()

  /**
    * make sure that all levels of targetDirName exist
    *
    * @return true if directory exists
    */
  def makeTargetDir(): Boolean = {
    FileUtils.makeDirectory(commonOptions.targetDirName)
  }

  def targetDirName: String = commonOptions.targetDirName

  /**
    * this function sets the topName in the commonOptions.
    * It would be nicer to not need this but many chisel tools cannot determine
    * the name of the device under test until other options have been parsed.
    * Havin this function allows the code to set the TopName after it has been
    * determined
    *
    * @param newTopName  the topName to be used
    */
  def setTopName(newTopName: String): Unit = {
    commonOptions = commonOptions.copy(topName = newTopName)
  }
  def setTopNameIfNotSet(newTopName: String): Unit = {
    if(commonOptions.topName.isEmpty) {
      setTopName(newTopName)
    }
  }
  def topName: String = commonOptions.topName
  def setTargetDirName(newTargetDirName: String): Unit = {
    commonOptions = commonOptions.copy(targetDirName = newTargetDirName)
  }

  /**
    * return a file based on targetDir, topName and suffix
    * Will not add the suffix if the topName already ends with that suffix
    *
    * @param suffix suffix to add, removes . if present
    * @param fileNameOverride this will override the topName if nonEmpty
    * @return
    */
  def getBuildFileName(suffix: String, fileNameOverride: String = ""): String = {
    makeTargetDir()

    val baseName = if(fileNameOverride.nonEmpty) fileNameOverride else topName
    val directoryName = {
      if(baseName.startsWith("./") || baseName.startsWith("/")) {
        ""
      }
      else {
        if(targetDirName.endsWith("/")) targetDirName else targetDirName + "/"
      }
    }
    val normalizedSuffix = {
      val dottedSuffix = if(suffix.startsWith(".")) suffix else s".$suffix"
      if(baseName.endsWith(dottedSuffix)) "" else dottedSuffix
    }

    s"$directoryName$baseName$normalizedSuffix"
  }
}

