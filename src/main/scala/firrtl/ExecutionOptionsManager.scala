// See LICENSE for license details.

package firrtl

import firrtl.annotations._
import firrtl.Parser._
import firrtl.ir.Circuit
import firrtl.passes.memlib.{InferReadWriteAnnotation, ReplSeqMemAnnotation}
import firrtl.passes.clocklist.ClockListAnnotation
import logger.LogLevel
import scopt.OptionParser

import scala.collection.Seq

import org.clapper.classutil.ClassFinder
import java.net.URLClassLoader
import java.io.File

final case class ComposableAnnotationOptions( annotations: AnnotationSeq = List.empty,
                                              customTransforms: Seq[Transform] = List.empty )

/**
  * Use this trait to define an options class that can add its private command line options to a externally
  * declared parser.
  * '''NOTE''' In all derived trait/classes, if you intend on maintaining backwards compatibility,
  *  be sure to add new options at the end of the current ones and don't remove any existing ones.
  */
trait ComposableOptions

class HasParser(applicationName: String) {
  final val parser = new OptionParser[ComposableAnnotationOptions](applicationName) {
    var terminateOnExit = true
    override def terminate(exitState: Either[String, Unit]): Unit = {
      if(terminateOnExit) sys.exit(0)
    }
  }

  /**
    * By default scopt calls sys.exit when --help is in options, this defeats that
    */
  def doNotExitOnHelp(): Unit = {
    parser.terminateOnExit = false
  }
  /**
    * By default scopt calls sys.exit when --help is in options, this un-defeats doNotExitOnHelp
    */
  def exitOnHelp(): Unit = {
    parser.terminateOnExit = true
  }
}

/** Annotation that contains the [[CommonOptions]] target directory */
sealed trait CommonOption
case class TopNameAnnotation(value: String) extends SingleStringAnnotation with CommonOption
case class TargetDirAnnotation(value: String) extends SingleStringAnnotation with CommonOption
case class LogLevelAnnotation(level: LogLevel.Value) extends SingleStringAnnotation with CommonOption {
  val value = level.toString }
case class ClassLogLevelAnnotation(name: String, level: LogLevel.Value) extends NoTargetAnnotation with CommonOption
case class LogToFileAnnotation() extends NoTargetAnnotation with CommonOption
case class LogClassNamesAnnotation() extends NoTargetAnnotation with CommonOption
case class ProgramArgsAnnotation(value: String) extends SingleStringAnnotation with CommonOption

/**
  * Most of the chisel toolchain components require a topName which defines a circuit or a device under test.
  * Much of the work that is done takes place in a directory.
  * It would be simplest to require topName to be defined but in practice it is preferred to defer this.
  * For example, in chisel, by deferring this it is possible for the execute there to first elaborate the
  * circuit and then set the topName from that if it has not already been set.
  */
case class CommonOptions(
    topName:           String         = "",
    targetDirName:     String         = ".",
    globalLogLevel:    LogLevel.Value = LogLevel.None,
    logToFile:         Boolean        = false,
    logClassNames:     Boolean        = false,
    classLogLevels: Map[String, LogLevel.Value] = Map.empty,
    programArgs:    Seq[String]                 = Seq.empty
) extends ComposableOptions {

  def getLogFileName(optionsManager: ExecutionOptionsManager): String = {
    if(topName.isEmpty) {
      optionsManager.getBuildFileName("log", "firrtl")
    }
    else {
      optionsManager.getBuildFileName("log")
    }
  }
}

trait HasCommonOptions {
  self: ExecutionOptionsManager =>

  lazy val commonOptions: CommonOptions = options.annotations.collect{ case opt: CommonOption => opt }
    .foldLeft(CommonOptions()){
      case (old, x) => x match {
        case TopNameAnnotation(name) => old.copy(topName = name)
        case TargetDirAnnotation(dir) => old.copy(targetDirName = dir)
        case LogLevelAnnotation(level) => old.copy(globalLogLevel = level)
        case ClassLogLevelAnnotation(name, level) => old.copy(classLogLevels = old.classLogLevels ++ Map(name -> level))
        case _: LogToFileAnnotation => old.copy(logToFile = true)
        case _: LogClassNamesAnnotation => old.copy(logClassNames = true)
        case ProgramArgsAnnotation(s) => old.copy(programArgs = old.programArgs :+ s)
      }
    }

  parser.note("common options")

  parser.opt[String]("top-name")
    .abbr("tn")
    .valueName("<top-level-circuit-name>")
    .action( (x, c) => c.copy(annotations = c.annotations :+ TopNameAnnotation(x)) )
    .maxOccurs(1)
    .text("This options defines the top level circuit, defaults to dut when possible")

  parser.opt[String]("target-dir")
    .abbr("td")
    .valueName("<target-directory>")
    .action( (x, c) => c.copy(annotations = c.annotations :+ TargetDirAnnotation(x)) )
    .maxOccurs(1)
    .text(s"This options defines a work directory for intermediate files, default is ${CommonOptions().targetDirName}")

  parser.opt[String]("log-level")
    .abbr("ll")
    .valueName("<Error|Warn|Info|Debug|Trace>")
    .action( (x, c) => c.copy(annotations = c.annotations :+ LogLevelAnnotation(LogLevel(x))) )
    .validate( x =>
      if (Array("error", "warn", "info", "debug", "trace").contains(x.toLowerCase)) parser.success
      else parser.failure(s"$x bad value must be one of error|warn|info|debug|trace") )
    .maxOccurs(1)
    .text(s"This options defines a work directory for intermediate files, default is ${CommonOptions().targetDirName}")

  parser.opt[Seq[String]]("class-log-level")
    .abbr("cll")
    .valueName("<FullClassName:[Error|Warn|Info|Debug|Trace]>[,...]")
    .action( (x, c) => {
              val annosx = x.map { y =>
                val className :: levelName :: _ = y.split(":").toList
                val level = LogLevel(levelName)
                ClassLogLevelAnnotation(className, level) }
              c.copy(annotations = c.annotations ++ annosx) } )
    .maxOccurs(1)
    .text(s"This options defines a work directory for intermediate files, default is ${CommonOptions().targetDirName}")

  parser.opt[Unit]("log-to-file")
    .abbr("ltf")
    .action( (x, c) => c.copy(annotations = c.annotations :+ LogToFileAnnotation()) )
    .maxOccurs(1)
    .text(s"default logs to stdout, this flags writes to topName.log or firrtl.log if no topName")

  parser.opt[Unit]("log-class-names")
    .abbr("lcn")
    .action( (x, c) => c.copy(annotations = c.annotations :+ LogClassNamesAnnotation()) )
    .maxOccurs(1)
    .text(s"shows class names and log level in logging output, useful for target --class-log-level")

  parser.help("help").text("prints this usage text")

  parser.arg[String]("<arg>...")
    .unbounded()
    .optional()
    .action( (x, c) => c.copy(annotations = c.annotations :+ ProgramArgsAnnotation(x)) )
    .text("optional unbounded args")
}

/** Firrtl output configuration specified by [[FirrtlExecutionOptions]]
  *
  * Derived from the fields of the execution options
  * @see [[FirrtlExecutionOptions.getOutputConfig]]
  */
sealed abstract class OutputConfig
final case class SingleFile(targetFile: String) extends OutputConfig
final case class OneFilePerModule(targetDir: String) extends OutputConfig

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
    annotationFileNameOverride: String = "",
    outputAnnotationFileName: String = "",
    compilerName:           String = "verilog",
    infoModeName:           String = "append",
    customTransforms:       Seq[Transform] = List.empty,
    firrtlSource:           Option[String] = None,
    annotations:            List[Annotation] = List.empty,
    emitOneFilePerModule:   Boolean = false,
    dontCheckCombLoops:     Boolean = false,
    noDCE:                  Boolean = false,
    annotationFileNames:    List[String] = List.empty,
    firrtlCircuit:          Option[Circuit] = None
)
extends ComposableOptions {

  require(!(emitOneFilePerModule && outputFileNameOverride.nonEmpty),
    "Cannot both specify the output filename and emit one file per module!!!")

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
      case "middle"    => new MiddleFirrtlCompiler()
      case "verilog"   => new VerilogCompiler()
      case "sverilog"  => new VerilogCompiler()
    }
  }

  def outputSuffix: String = {
    compilerName match {
      case "verilog"   => "v"
      case "sverilog"  => "sv"
      case "low"       => "lo.fir"
      case "high"      => "hi.fir"
      case "middle"    => "mid.fir"
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
  def getInputFileName(optionsManager: ExecutionOptionsManager ): String = {
    optionsManager.getBuildFileName("fir", inputFileNameOverride)
  }
  /** Get the user-specified [[OutputConfig]]
    *
    * @param optionsManager this is needed to access build function and its common options
    * @return the output configuration
    */
  def getOutputConfig(optionsManager: ExecutionOptionsManager): OutputConfig = {
    if (emitOneFilePerModule) OneFilePerModule(optionsManager.targetDirName)
    else SingleFile(optionsManager.getBuildFileName(outputSuffix, outputFileNameOverride))
  }
  /** Get the user-specified targetFile assuming [[OutputConfig]] is [[SingleFile]]
    *
    * @param optionsManager this is needed to access build function and its common options
    * @return the targetFile as a String
    */
  def getTargetFile(optionsManager: ExecutionOptionsManager): String = {
    getOutputConfig(optionsManager) match {
      case SingleFile(targetFile) => targetFile
      case other => throw new Exception("OutputConfig is not SingleFile!")
    }
  }
  /** Gives annotations based on the output configuration
    *
    * @param optionsManager this is needed to access build function and its common options
    * @return Annotations that will be consumed by emitter Transforms
    */
  def getEmitterAnnos(optionsManager: ExecutionOptionsManager): Seq[Annotation] = {
    // TODO should this be a public function?
    val emitter = compilerName match {
      case "high" => classOf[HighFirrtlEmitter]
      case "middle" => classOf[MiddleFirrtlEmitter]
      case "low" => classOf[LowFirrtlEmitter]
      case "verilog" => classOf[VerilogEmitter]
      case "sverilog" => classOf[VerilogEmitter]
    }
    getOutputConfig(optionsManager) match {
      case SingleFile(_) => Seq(EmitCircuitAnnotation(emitter))
      case OneFilePerModule(_) => Seq(EmitAllModulesAnnotation(emitter))
    }
  }
  /**
    * build the annotation file name, taking overriding parameters
    *
    * @param optionsManager this is needed to access build function and its common options
    * @return
    */
  @deprecated("Use FirrtlOptions.annotationFileNames instead", "1.1")
  def getAnnotationFileName(optionsManager: ExecutionOptionsManager): String = {
    optionsManager.getBuildFileName("anno", annotationFileNameOverride)
  }
}

trait FirrtlOption
case class InputFileAnnotation(value: String) extends SingleStringAnnotation with FirrtlOption
case class OutputFileAnnotation(value: String) extends SingleStringAnnotation with FirrtlOption
case class InputAnnotationFileAnnotation(value: String) extends SingleStringAnnotation with FirrtlOption
case class OutputAnnotationFileAnnotation(value: String) extends SingleStringAnnotation with FirrtlOption
case class CompilerAnnotation(value: String) extends SingleStringAnnotation with FirrtlOption
case class InfoModeAnnotation(value: String) extends SingleStringAnnotation with FirrtlOption
case class FirrtlSourceAnnotation(value: String) extends SingleStringAnnotation with FirrtlOption
case class EmitOneFilePerModuleAnnotation() extends NoTargetAnnotation with FirrtlOption
case class NoCheckCombLoopsAnnotation() extends NoTargetAnnotation with FirrtlOption
case class NoDceAnnotation() extends NoTargetAnnotation with FirrtlOption
case class AnnotationFileNamesAnnotation(value: String) extends SingleStringAnnotation with FirrtlOption

trait HasFirrtlOptions {
  self: ExecutionOptionsManager =>

  lazy val firrtlOptions = options.annotations.collect{ case opt: FirrtlOption => opt }
    .foldLeft(FirrtlExecutionOptions()){
      case (old, x) => x match {
        case InputFileAnnotation(f) => old.copy(inputFileNameOverride = f)
        case OutputFileAnnotation(f) => old.copy(outputFileNameOverride = f)
        case InputAnnotationFileAnnotation(f) => old.copy(annotationFileNameOverride = f)
        case OutputAnnotationFileAnnotation(f) => old.copy(outputAnnotationFileName = f)
        case CompilerAnnotation(c) => old.copy(compilerName = c)
        case InfoModeAnnotation(i) => old.copy(infoModeName = i)
        case FirrtlSourceAnnotation(s) => old.copy(firrtlSource = Some(s))
        case _: EmitOneFilePerModuleAnnotation => old.copy(emitOneFilePerModule = true)
        case _: NoCheckCombLoopsAnnotation => old.copy(dontCheckCombLoops = true)
        case _: NoDceAnnotation => old.copy(noDCE = true)
        case AnnotationFileNamesAnnotation(s) => old.copy(annotationFileNames = old.annotationFileNames :+ s)
      }
    }
    .copy( customTransforms = options.customTransforms,
           annotations = options.annotations.toList )

  parser.note("firrtl options")

  parser.opt[String]("input-file")
    .abbr("i")
    .valueName ("<firrtl-source>")
    .action( (x, c) => c.copy(annotations = c.annotations :+ InputFileAnnotation(x)) )
    .maxOccurs(1)
    .text("use this to override the default input file name , default is empty")

  parser.opt[String]("output-file")
    .abbr("o")
    .valueName("<output>")
    .action( (x, c) => c.copy(annotations = c.annotations :+ OutputFileAnnotation(x)) )
    .maxOccurs(1)
    .text("use this to override the default output file name, default is empty")

  parser.opt[String]("annotation-file")
    .abbr("faf")
    .unbounded()
    .valueName("<input-anno-file>")
    .action( (x, c) => c.copy(annotations = c.annotations :+ InputAnnotationFileAnnotation(x)) )
    .maxOccurs(1)
    .text("Used to specify annotation files (can appear multiple times)")

  parser.opt[Unit]("force-append-anno-file")
    .abbr("ffaaf")
    .hidden()
    .action( (x, c) => {
              val msg = "force-append-anno-file is deprecated and will soon be removed\n" +
                (" "*9) + "(It does not do anything anymore)"
              c } )

  parser.opt[String]("output-annotation-file")
    .abbr("foaf")
    .valueName ("<output-anno-file>")
    .action( (x, c) => c.copy(annotations = c.annotations :+ OutputAnnotationFileAnnotation(x)) )
    .maxOccurs(1)
    .text("use this to set the annotation output file")

  parser.opt[String]("compiler")
    .abbr("X")
    .valueName ("<high|middle|low|verilog|sverilog>")
    .action( (x, c) => c.copy(annotations = c.annotations :+ CompilerAnnotation(x)) )
    .maxOccurs(1)
    .validate { x =>
      if (Array("high", "middle", "low", "verilog", "sverilog").contains(x.toLowerCase)) parser.success
      else parser.failure(s"$x not a legal compiler") }
    .text(s"compiler to use, default is 'verilog'")

  parser.opt[String]("info-mode")
    .valueName ("<ignore|use|gen|append>")
    .action( (x, c) => c.copy(annotations = c.annotations :+ InfoModeAnnotation(x.toLowerCase)) )
    .maxOccurs(1)
    .validate( x =>
      if (Array("ignore", "use", "gen", "append").contains(x.toLowerCase)) parser.success
      else parser.failure(s"$x bad value must be one of ignore|use|gen|append"))
    .text(s"specifies the source info handling, default is ${FirrtlExecutionOptions().infoModeName}")

  parser.opt[String]("firrtl-source")
    .valueName ("A FIRRTL string")
    .action( (x, c) => c.copy(annotations = c.annotations :+ FirrtlSourceAnnotation(x)) )
    .maxOccurs(1)
    .text(s"A FIRRTL circuit as a string")

  parser.opt[Seq[String]]("custom-transforms")
    .abbr("fct")
    .valueName ("<package>.<class>")
    .validate( x => {
                x.map(x =>
                  try { Class.forName(x).asInstanceOf[Class[_ <: Transform]].newInstance() }
                  catch {
                    case e: ClassNotFoundException => throw new FIRRTLException(s"Unable to locate custom transform $x (did you misspell it?)", e)
                    case e: InstantiationException => throw new FIRRTLException(s"Unable to create instance of Transform $x (is this an anonymous class?)", e)
                    case e: Throwable => throw new FIRRTLException(s"Unknown error when instantiating class $x", e) } )
                parser.success } )
    .action( (x, c) => c.copy(customTransforms = c.customTransforms ++
                                x.map( xx => {
                                        Class.forName(xx).asInstanceOf[Class[_ <: Transform]].newInstance()
                                      })) )
    .text("runs these custom transforms during compilation.")

  parser.opt[Seq[String]]("inline")
    .abbr("fil")
    .valueName ("<circuit>[.<module>[.<instance>]][,..],")
    .action( (x, c) => {
              val newAnnotations = x.map { value =>
                value.split('.') match {
                  case Array(circuit) =>
                    passes.InlineAnnotation(CircuitName(circuit))
                  case Array(circuit, module) =>
                    passes.InlineAnnotation(ModuleName(module, CircuitName(circuit)))
                  case Array(circuit, module, inst) =>
                    passes.InlineAnnotation(ComponentName(inst, ModuleName(module, CircuitName(circuit))))
                }
              }
              c.copy(annotations = c.annotations ++ newAnnotations,
                     customTransforms = c.customTransforms :+ new passes.InlineInstances) })
    .text(
      """Inline one or more module (comma separated, no spaces) module looks like "MyModule" or "MyModule.myinstance""")

  ExecutionUtils.generateAnnotationOptions(parser)

  parser.opt[String]("repl-seq-mem")
    .abbr("frsq")
    .valueName ("-c:<circuit>:-i:<filename>:-o:<filename>")
    .action( (x, c) => c.copy(
              annotations = c.annotations :+ passes.memlib.ReplSeqMemAnnotation.parse(x),
              customTransforms = c.customTransforms :+ new passes.memlib.ReplSeqMem) )
    .maxOccurs(1)
    .text("Replace sequential memories with blackboxes + configuration file")

  parser.opt[String]("list-clocks")
    .abbr("clks")
    .valueName ("-c:<circuit>:-m:<module>:-o:<filename>")
    .action( (x, c) => c.copy(
              annotations = c.annotations :+ passes.clocklist.ClockListAnnotation.parse(x),
              customTransforms = c.customTransforms :+ new passes.clocklist.ClockListTransform) )
    .maxOccurs(1)
    .text("List which signal drives each clock of every descendent of specified module")

  parser.opt[Unit]("split-modules")
    .abbr("fsm")
    .action( (x, c) => c.copy(annotations = c.annotations :+ EmitOneFilePerModuleAnnotation()) )
    .maxOccurs(1)
    .text ("Emit each module to its own file in the target directory.")

  parser.opt[Unit]("no-check-comb-loops")
    .action( (x, c) => c.copy(annotations = c.annotations :+ NoCheckCombLoopsAnnotation()) )
    .maxOccurs(1)
    .text("Do NOT check for combinational loops (not recommended)")

  parser.opt[Unit]("no-dce")
    .action( (x, c) => c.copy(annotations = c.annotations :+ NoDceAnnotation()) )
    .maxOccurs(1)
    .text("Do NOT run dead code elimination")

  parser.opt[Seq[String]]("annotation-files")
    .action( (x, c) => c.copy(annotations = c.annotations ++ x.map(AnnotationFileNamesAnnotation(_))) )
    .text("List of annotation files")

  parser.note("")

  /*
  parser.checkConfig( c =>
    if (c.emitOneFilePerModule && (c.outputFileNameOverride != FirrtlExecutionOptions().outputFileNameOverride))
      parser.failure("Cannot override output-file if split-modules is specified")
    else
      parser.success )
  parser.checkConfig( c =>
    if (c.outputFileNameOverride.nonEmpty && (c.emitOneFilePerModule != FirrtlExecutionOptions().outputFileNameOverride))
      parser.failure("Cannot split-modules if output-file is specified")
    else
      parser.success )
  */
}

sealed trait FirrtlExecutionResult

object FirrtlExecutionSuccess {
  def apply(
    emitType    : String,
    emitted     : String,
    circuitState: CircuitState
  ): FirrtlExecutionSuccess = new FirrtlExecutionSuccess(emitType, emitted, circuitState)


  def unapply(arg: FirrtlExecutionSuccess): Option[(String, String)] = {
    Some((arg.emitType, arg.emitted))
  }
}
/**
  * Indicates a successful execution of the firrtl compiler, returning the compiled result and
  * the type of compile
  *
  * @param emitType  The name of the compiler used, currently "high", "middle", "low", "verilog", or "sverilog"
  * @param emitted   The emitted result of compilation
  */
class FirrtlExecutionSuccess(
  val emitType: String,
  val emitted : String,
  val circuitState: CircuitState
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
class ExecutionOptionsManager(val applicationName: String, args: Array[String] = Array.empty) extends HasParser(applicationName) with HasCommonOptions {

  lazy val options: ComposableAnnotationOptions = parser.parse(args, ComposableAnnotationOptions()) match {
    case Some(opts) => opts
    case _ => throw new Exception("Failed to parse command line options")
  }

  lazy val topName: String = commonOptions.topName
  lazy val targetDirName: String = commonOptions.targetDirName

  def showUsageAsError(): Unit = parser.showUsageAsError()

  def makeTargetDir(): Boolean = FileUtils.makeDirectory(targetDirName)

  /**
    * return a file based on targetDir, topName and suffix
    * Will not add the suffix if the topName already ends with that suffix
    *
    * @param suffix suffix to add, removes . if present
    * @param fileNameOverride this will override the topName if nonEmpty, when using this targetDir is ignored
    * @return
    */
  def getBuildFileName(suffix: String, fileNameOverride: String = ""): String = {
    makeTargetDir()

    val baseName = if(fileNameOverride.nonEmpty) fileNameOverride else topName
    val directoryName = {
      if(fileNameOverride.nonEmpty) {
        ""
      }
      else if(baseName.startsWith("./") || baseName.startsWith("/")) {
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
    val path = directoryName + baseName.split("/").dropRight(1).mkString("/")
    FileUtils.makeDirectory(path)
    s"$directoryName$baseName$normalizedSuffix"
  }
}

object ExecutionUtils {

  /** Generate options from annotations
    *
    * This looks through every Annotation which mixes in
    * [[ProvidesOptions]]. It then grabs their companion objects and calls
    * the [[provideOptions]] method to add scopt parsing for their
    * provided options.
    *
    * @todo: to prevent inordinate slowdown, this ignores classes in the
    * ".ivy2" directory. We need a better solution of what to search. */
  def generateAnnotationOptions(parser: OptionParser[ComposableAnnotationOptions]): Unit = {
    val files = getClass.getClassLoader.asInstanceOf[URLClassLoader].getURLs
      .filterNot(_.toString.contains(".ivy2"))
      .map(x => new File(x.getFile))
    val finder = ClassFinder(files)
    val classes = finder.getClasses
    val annotationsWithOptions = ClassFinder.concreteSubclasses(classOf[annotations.ProvidesOptions], classes).toList
    annotationsWithOptions.map( name => Class.forName(name.toString)
                                 .getField("MODULE$")
                                 .get(null)
                                 .asInstanceOf[Annotation with ProvidesOptions]
                                 .provideOptions(parser) )
  }
}
