// See LICENSE for license details.

package firrtl

import firrtl.annotations.{
  Annotation,
  DeletedAnnotation,
  SingleStringAnnotation,
  NoTargetAnnotation,
  LegacyAnnotation }
import firrtl.transforms.{
  BlackBoxTargetDirAnno,
  DontCheckCombLoopsAnnotation,
  NoDCEAnnotation }
import logger.LogLevel
import java.io.File

/** Indicates that a subclass is an [[annotations.Annotation]] with an option consummable by [[HasFirrtlOptions]]
  *
  * This must be mixed into a subclass of [[annotations.Annotation]]
  */
trait FirrtlOption { self: Annotation => }

/** Holds the name of the top module
  *  - maps to [[FirrtlExecutionOptions.topName]]
  *  - set on the command line with `-tn/--top-name`
  * @param value top module name
  */
case class TopNameAnnotation(value: String) extends SingleStringAnnotation with FirrtlOption

/** Holds the name of the target directory
  *  - maps to [[FirrtlExecutionOptions.targetDirName]]
  *  - set with `-td/--target-dir`
  *  - if unset, a [[TargetDirAnnotation]] will be generated with the
  * @param value target directory name
  */
case class TargetDirAnnotation(value: String = ".") extends SingleStringAnnotation with FirrtlOption

/** Describes the verbosity of information to log
  *  - maps to [[FirrtlExecutionOptions.globalLogLevel]]
  *  - set with `-ll/--log-level`
  *  - if unset, a [[LogLevelAnnotation]] with the default log level will be emitted
  * @param level the level of logging
  */
case class LogLevelAnnotation(level: LogLevel.Value = LogLevel.None) extends SingleStringAnnotation with FirrtlOption {
  val value = level.toString }

/** Describes a mapping of a class to a specific log level
  *  - maps to [[FirrtlExecutionOptions.classLogLevels]]
  *  - set with `-cll/--class-log-level`
  * @param name the class name to log
  * @param level the verbosity level
  */
case class ClassLogLevelAnnotation(name: String, level: LogLevel.Value) extends NoTargetAnnotation with FirrtlOption

/** Enables logging to a file (as opposed to STDOUT)
  *  - maps to [[FirrtlExecutionOptions.logToFile]]
  *  - enabled with `-ltf/--log-to-file`
  */
case object LogToFileAnnotation extends NoTargetAnnotation with FirrtlOption

/** Enables class names in log output
  *  - maps to [[FirrtlExecutionOptions.logClassNames]]
  *  - enabled with `-lcn/--log-class-names`
  */
case object LogClassNamesAnnotation extends NoTargetAnnotation with FirrtlOption

/** Additional arguments
  *  - maps to [[FirrtlExecutionOptions.programArgs]]
  *  - set with any trailing option on the command line
  * @param value one [[scala.String]] argument
  */
case class ProgramArgsAnnotation(value: String) extends SingleStringAnnotation with FirrtlOption

/** An explicit input FIRRTL file to read
  *  - maps to [[FirrtlExecutionOptions.inputFileNameOverride]]
  *  - set with `-i/--input-file`
  *  - If unset, an [[InputFileAnnotation]] with the default input file __will not be generated__
  * @param value input filename
  */
case class InputFileAnnotation(value: String) extends SingleStringAnnotation with FirrtlOption

/** An explicit output file the emitter will write to
  *   - maps to [[FirrtlExecutionOptions.outputFileNameOverride]]
  *   - set with `-o/--output-file`
  *  @param value output filename
  */
case class OutputFileAnnotation(value: String) extends SingleStringAnnotation with FirrtlOption

/** An explicit output _annotation_ file to write to
  *  - maps to [[FirrtlExecutionOptions.outputAnnotationFileName]]
  *  - set with `-foaf/--output-annotation-file`
  * @param value output annotation filename
  */
case class OutputAnnotationFileAnnotation(value: String) extends SingleStringAnnotation with FirrtlOption

/** Sets the info mode style
  *  - maps to [[FirrtlExecutionOptions.infoModeName]]
  *  - set with `--info-mode`
  * @param value info mode name
  */
case class InfoModeAnnotation(value: String = "append") extends SingleStringAnnotation with FirrtlOption

/** Holds a [[scala.String]] containing FIRRTL source to read as input
  *  - maps to [[FirrtlExecutionOptions.firrtlSource]]
  *  - set with `--firrtl-source`
  * @param value FIRRTL source as a [[scala.String]]
  */
case class FirrtlSourceAnnotation(value: String) extends SingleStringAnnotation with FirrtlOption

/** Holds a filename containing one or more [[annotations.Annotation]] to be read
  *  - this is not stored in [[FirrtlExecutionOptions]]
  *  - set with `-faf/--annotation-file`
  * @param value input annotation filename
  */
case class InputAnnotationFileAnnotation(value: String) extends SingleStringAnnotation with FirrtlOption

/** Holds the name of the compiler to run
  *  - maps to [[FirrtlExecutionOptions.compilerName]]
  *  - set with `-X/--compiler`
  *  - If unset, a [[CompilerNameAnnotation]] with the default compiler ("verilog") __will be generated__
  * @param value compiler name
  */
case class CompilerNameAnnotation(value: String = "verilog") extends SingleStringAnnotation with FirrtlOption

/** Holds the unambiguous class name of a [[Transform]] to run
  *  - will be append to [[FirrtlExecutionOptions.customTransforms]]
  *  - set with `-fct/--custom-transforms`
  * @param value the full class name of the transform
  */
case class RunFirrtlTransformAnnotation(value: String) extends SingleStringAnnotation with FirrtlOption

trait HasFirrtlOptions {
  self: ExecutionOptionsManager =>

  /* Add the implicit annotaiton file annotation if such a file exists */
  private def addImplicitAnnotationFile(annos: Seq[Annotation]): Seq[Annotation] = annos.toList ++ (
    annos.collectFirst{ case a: InputAnnotationFileAnnotation => a } match {
      case Some(_) => List()
      case None =>
        val file = ExecutionUtils.Early.targetDir(annos) + "/" +
          ExecutionUtils.Early.topName(annos) + ".anno"
        if (new File(file).exists) {
          Driver.dramaticWarning(
            s"Implicit reading of the annotation file is deprecated! Use an explict --annotation-file argument.")
          List(InputAnnotationFileAnnotation(file))
        } else {
          List()
        }
    } )

  /* Pull in whatever the user tells us to from files until we can't find
   * any more. Remember what we've already imported to prevent a
   * loop. */
  private var includeGuard = Set[String]()
  private def getIncludes(annos: Seq[Annotation]): Seq[Annotation] = annos
    .flatMap( _ match {
               case a: InputAnnotationFileAnnotation =>
                 if (includeGuard.contains(a.value)) {
                   Driver.dramaticWarning("Tried to import the same annotation file twice! (Did you include it twice?)")
                   List(DeletedAnnotation(applicationName, a))
                 } else {
                   includeGuard += a.value
                   List(DeletedAnnotation(applicationName, a)) ++ getIncludes(ExecutionUtils.readAnnotationsFromFile(a.value))
                 }
               case x => List(x)
             })

  /* Add any missing default annotations */
  private def addDefaults(annos: Seq[Annotation]): Seq[Annotation] = {
    var Seq(addTargetDir, addBlackBoxDir, addLogLevel, addCompiler, addTopName) = Seq.fill(5)(true)
    annos.collect{ case a: FirrtlOption => a }.map{
      case _: TargetDirAnnotation    => addTargetDir   = false
      case _: BlackBoxTargetDirAnno  => addBlackBoxDir = false
      case _: LogLevelAnnotation     => addLogLevel    = false
      case _: CompilerNameAnnotation => addCompiler    = false
      case _: TopNameAnnotation      => addTopName     = false
      case _ =>
    }

    annos ++
      (if (addTargetDir)   Seq(TargetDirAnnotation(FirrtlExecutionOptions().targetDirName))   else Seq() ) ++
      (if (addBlackBoxDir) Seq(BlackBoxTargetDirAnno(FirrtlExecutionOptions().targetDirName)) else Seq() ) ++
      (if (addLogLevel)    Seq(LogLevelAnnotation(FirrtlExecutionOptions().globalLogLevel))   else Seq() ) ++
      (if (addCompiler)    Seq(CompilerNameAnnotation(FirrtlExecutionOptions().compilerName)) else Seq() ) ++
      (if (addTopName)     Seq(TopNameAnnotation(ExecutionUtils.Early.topName(annos)))        else Seq() )
  }

  /* Convert an emitter to an annotation */
  private def getEmitterAnnotations(compiler: String): Seq[Annotation] = {
    val emitter = compiler match {
      case "high"      => classOf[HighFirrtlEmitter]
      case "low"       => classOf[LowFirrtlEmitter]
      case "middle"    => classOf[MiddleFirrtlEmitter]
      case "verilog"   => classOf[VerilogEmitter]
      case "sverilog"  => classOf[SystemVerilogEmitter]
    }
    Seq(EmitterAnnotation(emitter))
  }

  /** A [[FirrtlExecutionOptions]] generated from all command line options (subclasses of [[FirrtlOption]]) */
  lazy val firrtlOptions: FirrtlExecutionOptions = {
    val annotationTransforms: Seq[Seq[Annotation] => Seq[Annotation]] =
      Seq( addImplicitAnnotationFile(_),
           getIncludes(_),
           addDefaults(_),
           LegacyAnnotation.convertLegacyAnnos(_) )

    val preprocessedAnnotations: AnnotationSeq = annotationTransforms
      .foldLeft(options)( (old, tx) => tx(old) )

    val (firrtlAnnos, nonFirrtlAnnos) = preprocessedAnnotations.partition{
      case opt: FirrtlOption => true
      case _                 => false }

    firrtlAnnos
      .foldLeft(FirrtlExecutionOptions(annotations = nonFirrtlAnnos.toList)){ (old, x) =>
        val processed = x match {
          case InputFileAnnotation(f) => old.copy(inputFileNameOverride = Some(f))
          case OutputFileAnnotation(f) => old.copy(outputFileNameOverride = Some(f))
          case OutputAnnotationFileAnnotation(f) => old.copy(outputAnnotationFileName = Some(f))
          case InfoModeAnnotation(i) => old.copy(infoModeName = i)
          case FirrtlSourceAnnotation(s) => old.copy(firrtlSource = Some(s))
          case EmitOneFilePerModuleAnnotation => old.copy(emitOneFilePerModule = true)
          case InputAnnotationFileAnnotation(f) => old
          case CompilerNameAnnotation(c) => old.copy(compilerName = c,
                                                     annotations = old.annotations ++ getEmitterAnnotations(c))
          case TopNameAnnotation(name) => old.copy(topName = Some(name))
          case TargetDirAnnotation(dir) => old.copy(targetDirName = dir)
          case LogLevelAnnotation(level) => old.copy(globalLogLevel = level)
          case ClassLogLevelAnnotation(name, level) => old.copy(classLogLevels = old.classLogLevels ++ Map(name -> level))
          case LogToFileAnnotation => old.copy(logToFile = true)
          case LogClassNamesAnnotation => old.copy(logClassNames = true)
          case ProgramArgsAnnotation(s) => old.copy(programArgs = old.programArgs :+ s)
          case RunFirrtlTransformAnnotation(x) => old.copy(
            customTransforms = old.customTransforms :+ Class.forName(x).asInstanceOf[Class[_<:Transform]].newInstance())
        }
        // [todo] Delete FirrtlExecutionOptions annotations here
        processed
          .copy(annotations = processed.annotations :+ x)
      }
  }

  /** Return the name of the top module */
  def topName(): String = firrtlOptions.topName.get

  /** Return the name of the target directory */
  def targetDirName(): String = firrtlOptions.targetDirName

  /** Create the specified Target Directory (--target-dir)
    *
    * @return true if successful, false if not
    */
  def makeTargetDir(): Boolean = FileUtils.makeDirectory(targetDirName)

  /** Return a file based on targetDir, topName and suffix Will not add the
    * suffix if the topName already ends with that suffix
    *
    * @param suffix suffix to add, removes . if present
    * @param fileNameOverride this will override the topName if nonEmpty, when using this targetDir is ignored
    * @return
    */
  def getBuildFileName(suffix: String, fileNameOverride: Option[String] = None): String = {
    makeTargetDir()

    val baseName: String = fileNameOverride.getOrElse(topName)
    val baseNameIsFullPath: Boolean = baseName.startsWith("./") || baseName.startsWith("/")
    val directoryName: String = if (fileNameOverride.nonEmpty || baseNameIsFullPath) {
      ""
    } else if (targetDirName.endsWith("/")) {
      targetDirName
    } else {
      targetDirName + "/"
    }
    val normalizedSuffix: String = {
      val dottedSuffix = if(suffix.startsWith(".")) suffix else s".$suffix"
      if(baseName.endsWith(dottedSuffix)) "" else dottedSuffix
    }
    val path: String = directoryName + baseName.split("/").dropRight(1).mkString("/")

    FileUtils.makeDirectory(path)
    s"$directoryName$baseName$normalizedSuffix"
  }

  /** Return a filename of form "topName.log"
    *
    * @return the filename
    */
  def getLogFileName(): String = getBuildFileName("log")

  /** Build the input file name, taking overriding parameters
    *
    * @param optionsManager this is needed to access build function and its common options
    * @return a properly constructed input file name
    */
  def getInputFileName(): String = getBuildFileName("fir", firrtlOptions.inputFileNameOverride)

  /** Get the user-specified [[OutputConfig]]
    *
    * @param optionsManager this is needed to access build function and its common options
    * @return the output configuration
    */
  def getOutputConfig(): OutputConfig =
    if (firrtlOptions.emitOneFilePerModule)
      OneFilePerModule(targetDirName)
    else
      SingleFile(getBuildFileName(firrtlOptions.outputSuffix, firrtlOptions.outputFileNameOverride))

  /** Get the user-specified targetFile assuming [[OutputConfig]] is [[SingleFile]]
    *
    * @param optionsManager this is needed to access build function and its common options
    * @return the targetFile as a String
    */
  def getTargetFile(): String = getOutputConfig match {
    case SingleFile(targetFile) => targetFile
    case _                      => throw new Exception("OutputConfig is not SingleFile!") }

  parser.note("Common Options")

  parser.opt[String]("top-name")
    .abbr("tn")
    .valueName("<top-level-circuit-name>")
    .action( (x, c) => c :+ TopNameAnnotation(x) )
    .maxOccurs(1)
    .text("This options defines the top level circuit, defaults to dut when possible")

  parser.opt[String]("target-dir")
    .abbr("td")
    .valueName("<target-directory>")
    .action( (x, c) => c ++ Seq(TargetDirAnnotation(x), BlackBoxTargetDirAnno(x)) )
    .maxOccurs(1)
    .text(s"This options defines a work directory for intermediate files and blackboxes, default is ${FirrtlExecutionOptions().targetDirName}")

  parser.opt[String]("log-level")
    .abbr("ll")
    .valueName("<Error|Warn|Info|Debug|Trace>")
    .action( (x, c) => c :+ LogLevelAnnotation(LogLevel(x)) )
    .validate( x =>
      if (Array("error", "warn", "info", "debug", "trace").contains(x.toLowerCase)) parser.success
      else parser.failure(s"$x bad value must be one of error|warn|info|debug|trace") )
    .maxOccurs(1)
    .text(s"Sets the verbosity level of logging, default is ${FirrtlExecutionOptions().globalLogLevel}")

  parser.opt[Seq[String]]("class-log-level")
    .abbr("cll")
    .valueName("<FullClassName:[Error|Warn|Info|Debug|Trace]>[,...]")
    .action( (x, c) => c ++ (x.map { y =>
                val className :: levelName :: _ = y.split(":").toList
                val level = LogLevel(levelName)
                ClassLogLevelAnnotation(className, level) }) )
    .maxOccurs(1)
    .text(s"This defines per-class verbosity of logging")

  parser.opt[Unit]("log-to-file")
    .abbr("ltf")
    .action( (x, c) => c :+ LogToFileAnnotation )
    .maxOccurs(1)
    .text(s"default logs to stdout, this flags writes to topName.log or firrtl.log if no topName")

  parser.opt[Unit]("log-class-names")
    .abbr("lcn")
    .action( (x, c) => c :+ LogClassNamesAnnotation )
    .maxOccurs(1)
    .text(s"shows class names and log level in logging output, useful for target --class-log-level")

  parser.arg[String]("<arg>...")
    .unbounded()
    .optional()
    .action( (x, c) => c :+ ProgramArgsAnnotation(x) )
    .text("optional unbounded args")

  parser.help("help").text("prints this usage text")

  parser.note("FIRRTL Compiler Options")

  parser.opt[String]("input-file")
    .abbr("i")
    .valueName ("<firrtl-source>")
    .action( (x, c) => c :+ InputFileAnnotation(x) )
    .maxOccurs(1)
    .text("use this to override the default input file name, default is empty")

  parser.opt[String]("output-file")
    .abbr("o")
    .valueName("<output>")
    .action( (x, c) => c :+ OutputFileAnnotation(x) )
    .maxOccurs(1)
    .text("use this to override the default output file name, default is empty")

  parser.opt[String]("annotation-file")
    .abbr("faf")
    .unbounded()
    .valueName("<input-anno-file>")
    .action( (x, c) => c :+ InputAnnotationFileAnnotation(x) )
    .text("Used to specify annotation file")

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
    .action( (x, c) => c :+ OutputAnnotationFileAnnotation(x) )
    .maxOccurs(1)
    .text("use this to set the annotation output file")

  parser.opt[String]("compiler")
    .abbr("X")
    .valueName ("<high|middle|low|verilog|sverilog>")
    .action( (x, c) => c :+ CompilerNameAnnotation(x) )
    .maxOccurs(1)
    .validate { x =>
      if (Array("high", "middle", "low", "verilog", "sverilog").contains(x.toLowerCase)) parser.success
      else parser.failure(s"$x not a legal compiler") }
    .text(s"compiler to use, default is 'verilog'")

  parser.opt[String]("info-mode")
    .valueName ("<ignore|use|gen|append>")
    .action( (x, c) => c :+ InfoModeAnnotation(x.toLowerCase) )
    .maxOccurs(1)
    .validate( x =>
      if (Array("ignore", "use", "gen", "append").contains(x.toLowerCase)) parser.success
      else parser.failure(s"$x bad value must be one of ignore|use|gen|append"))
    .text(s"specifies the source info handling, default is ${FirrtlExecutionOptions().infoModeName}")

  parser.opt[String]("firrtl-source")
    .valueName ("A FIRRTL string")
    .action( (x, c) => c :+ FirrtlSourceAnnotation(x) )
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
    .action( (x, c) => c ++ x.map(RunFirrtlTransformAnnotation(_)) )
    .text("runs these custom transforms during compilation.")

  parser.opt[Unit]("split-modules")
    .abbr("fsm")
    .action( (x, c) => c :+ EmitOneFilePerModuleAnnotation )
    .maxOccurs(1)
    .text ("Emit each module to its own file in the target directory.")

  parser.note("FIRRTL Transform Options")
  Seq( transforms.DeadCodeElimination,
       transforms.CheckCombLoops,
       passes.InlineInstances,
       passes.memlib.InferReadWrite,
       passes.memlib.ReplSeqMem,
       passes.clocklist.ClockListTransform )
    .map(_.provideOptions(parser))

  parser.checkConfig{ c =>
    var Seq(hasTopName, hasInputFile, hasOneFilePerModule, hasOutputFile, hasFirrtlSource) = Seq.fill(5)(false)
    c.foreach(
      _ match {
        case TopNameAnnotation(_)           => hasTopName          = true
        case InputFileAnnotation(_)         => hasInputFile        = true
        case EmitOneFilePerModuleAnnotation => hasOneFilePerModule = true
        case OutputFileAnnotation(_)        => hasOutputFile       = true
        case FirrtlSourceAnnotation(_)      => hasFirrtlSource     = true
        case _                              =>                            })
    if (!(hasTopName || hasInputFile || hasFirrtlSource))
      parser.failure("At least one of --top-name, --input-file, or --firrtl-source must be specified")
    else if (hasInputFile && hasFirrtlSource)
      parser.failure("Only one of --input-file or --firrtl-source may be specified (not both!)")
    else if (hasOneFilePerModule && hasOutputFile)
      parser.failure("Output override (--output-file) is incompatible with one file per module (--split-modules)")
    else
      parser.success
  }
}

/** Firrtl output configuration specified by [[FirrtlExecutionOptions]]
  *
  * Derived from the fields of the execution options
  * @see [[FirrtlExecutionOptions.getOutputConfig]]
  */
sealed abstract class OutputConfig
final case class SingleFile(targetFile: String) extends OutputConfig
final case class OneFilePerModule(targetDir: String) extends OutputConfig

/** Internal options used to control the FIRRTL compiler
  *
  * @param topName the name of the top module
  * @param targetDirName name of the target directory (default ".")
  * @param globalLogLevel the verbosity of logging
  * @param classLogLevels the individual verbosity of logging for specific classes
  * @param programArgs explicit program arguments
  * @param inputFileNameOverride input FIRRTL file, default: `targetDir/topName.fir`
  * @param outputFileNameOverride output file, default: `targetDir/topName.SUFFIX` with `SUFFIX` determined by the compiler
  * @param compilerName which compiler to use
  * @param infoModeName the policy for generating [[firrtl.ir.Info]] when processing FIRRTL
  * @param customTransforms any custom [[Transform]] to run
  * @param firrtlSource explicit input FIRRTL as a [[scala.String]]
  * @param annotations a sequence of [[annotations.Annotation]] passed to the compiler
  * @param emitOneFilePerModule enables one-file-per-module output in the [[Emitter]]
  */
final case class FirrtlExecutionOptions(
  topName:                  Option[String]              = None,
  targetDirName:            String                      = ".",
  globalLogLevel:           LogLevel.Value              = LogLevel.None,
  classLogLevels:           Map[String, LogLevel.Value] = Map.empty,
  logToFile:                Boolean                     = false,
  logClassNames:            Boolean                     = false,
  programArgs:              Seq[String]                 = Seq.empty,
  inputFileNameOverride:    Option[String]              = None,
  outputFileNameOverride:   Option[String]              = None,
  outputAnnotationFileName: Option[String]              = None,
  compilerName:             String                      = "verilog",
  infoModeName:             String                      = "append",
  customTransforms:         Seq[Transform]              = List.empty,
  firrtlSource:             Option[String]              = None,
  annotations:              List[Annotation]            = List.empty,
  emitOneFilePerModule:     Boolean                     = false) {

  /** Return the info mode */
  def infoMode(): Parser.InfoMode = {
    infoModeName match {
      case "use"    => Parser.UseInfo
      case "ignore" => Parser.IgnoreInfo
      case "gen"    => Parser.GenInfo(inputFileNameOverride.getOrElse("[not defined]"))
      case "append" => Parser.AppendInfo(inputFileNameOverride.getOrElse("[not defined]"))
      case other    => Parser.UseInfo
    }
  }

  /** Return the compiler */
  def compiler(): Compiler = compilerName match {
    case "high"      => new HighFirrtlCompiler()
    case "low"       => new LowFirrtlCompiler()
    case "middle"    => new MiddleFirrtlCompiler()
    case "verilog"   => new VerilogCompiler()
    case "sverilog"  => new SystemVerilogCompiler()
  }

  /** Return the output file suffix */
  def outputSuffix(): String = compilerName match {
    case "verilog"   => "v"
    case "sverilog"  => "sv"
    case "low"       => "lo.fir"
    case "high"      => "hi.fir"
    case "middle"    => "mid.fir"
    case _ =>
      throw new Exception(s"Illegal compiler name $compilerName")
  }

  /** Build the input file name, taking overriding parameters
    *
    * @param optionsManager this is needed to access build function and its common options
    * @return a properly constructed input file name
    */
  @deprecated("Use ExecutionOptionsManager.getInputFileName", "3.2.0")
  def getInputFileName(optionsManager: ExecutionOptionsManager with HasFirrtlOptions ): String = {
    optionsManager.getBuildFileName("fir", inputFileNameOverride)
  }
  /** Get the user-specified [[OutputConfig]]
    *
    * @param optionsManager this is needed to access build function and its common options
    * @return the output configuration
    */
  @deprecated("Use ExecutionOptionsManager.getOutputConfig", "3.2.0")
  def getOutputConfig(optionsManager: ExecutionOptionsManager with HasFirrtlOptions): OutputConfig = {
    if (emitOneFilePerModule) OneFilePerModule(optionsManager.targetDirName)
    else SingleFile(optionsManager.getBuildFileName(outputSuffix, outputFileNameOverride))
  }

  /** Get the user-specified targetFile assuming [[OutputConfig]] is [[SingleFile]]
    *
    * @param optionsManager this is needed to access build function and its common options
    * @return the targetFile as a String
    */
  @deprecated("Use ExecutionOptionsManager.getTargetFile", "3.2.0")
  def getTargetFile(optionsManager: ExecutionOptionsManager with HasFirrtlOptions): String = {
    getOutputConfig(optionsManager) match {
      case SingleFile(targetFile) => targetFile
      case other => throw new Exception("OutputConfig is not SingleFile!")
    }
  }
}

/** A result of running the FIRRTL compiler */
sealed trait FirrtlExecutionResult

/** A successful result from running the FIRRTL compiler */
object FirrtlExecutionSuccess {

  /** Create a new successful execution
    *
    * @param emitType the compiler name used
    * @param emitted the resulting FIRRTL circuit (as a [[scala.String]]
    * @param circuitState the final [[CircuitState]]
    * @return a [[FirrtlExecutionSuccess]] object
    */
  def apply(
    emitType    : String,
    emitted     : String,
    circuitState: CircuitState
  ): FirrtlExecutionSuccess = new FirrtlExecutionSuccess(emitType, emitted, circuitState)


  /** Extractor to get at the compiler name and resulting FIRRTL */
  def unapply(arg: FirrtlExecutionSuccess): Option[(String, String)] = {
    Some((arg.emitType, arg.emitted))
  }
}
/** A successful result from running the FIRRTL compiler
  *
  * @param emitType the name of the compiler used, currently "high", "middle", "low", "verilog", or "sverilog"
  * @param emitted the emitted result of compilation
  * @param circuitState the resulting [[CircuitState]]
  */
class FirrtlExecutionSuccess(
  val emitType: String,
  val emitted : String,
  val circuitState: CircuitState
) extends FirrtlExecutionResult

/** An _unsuccessful_ result from running the FIRRTL compiler
  *
  * @param message some kind of hint as to what went wrong.
  */
case class FirrtlExecutionFailure(message: String) extends FirrtlExecutionResult
