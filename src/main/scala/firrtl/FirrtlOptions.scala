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

trait FirrtlOption
case class TopNameAnnotation(value: String) extends SingleStringAnnotation with FirrtlOption
case class TargetDirAnnotation(value: String) extends SingleStringAnnotation with FirrtlOption
case class LogLevelAnnotation(level: LogLevel.Value) extends SingleStringAnnotation with FirrtlOption {
  val value = level.toString }
case class ClassLogLevelAnnotation(name: String, level: LogLevel.Value) extends NoTargetAnnotation with FirrtlOption
case object LogToFileAnnotation extends NoTargetAnnotation with FirrtlOption
case object LogClassNamesAnnotation extends NoTargetAnnotation with FirrtlOption
case class ProgramArgsAnnotation(value: String) extends SingleStringAnnotation with FirrtlOption
case class InputFileAnnotation(value: String) extends SingleStringAnnotation with FirrtlOption
case class OutputFileAnnotation(value: String) extends SingleStringAnnotation with FirrtlOption
case class OutputAnnotationFileAnnotation(value: String) extends SingleStringAnnotation with FirrtlOption
case class InfoModeAnnotation(value: String) extends SingleStringAnnotation with FirrtlOption
case class FirrtlSourceAnnotation(value: String) extends SingleStringAnnotation with FirrtlOption
case class InputAnnotationFileAnnotation(file: String) extends NoTargetAnnotation with FirrtlOption
case class CompilerNameAnnotation(value: String) extends SingleStringAnnotation with FirrtlOption
case class RunFirrtlTransformAnnotation(value: String) extends SingleStringAnnotation with FirrtlOption

trait HasFirrtlOptions {
  self: ExecutionOptionsManager =>

  lazy val firrtlOptions: FirrtlOptions = {
    // println("---------------------------------------- options")
    // options.foreach( x => println(s"[info]  - $x") )

    /** Set default annotations */
    def addDefaults(annotations: AnnotationSeq): AnnotationSeq = {
      val addTargetDir   = annotations.collect{ case a: TargetDirAnnotation    => a }.isEmpty
      val addBlackBoxDir = annotations.collect{ case a: BlackBoxTargetDirAnno  => a }.isEmpty
      val addLogLevel    = annotations.collect{ case a: LogLevelAnnotation     => a }.isEmpty
      val addCompiler    = annotations.collect{ case a: CompilerNameAnnotation => a }.isEmpty
      val addTopName     = annotations.collect{ case a: TopNameAnnotation      => a }.isEmpty
      // The EmitterAnnotation is a derivative of the Compiler and is added in later

      annotations ++
        (if (addTargetDir)   Seq(TargetDirAnnotation(FirrtlOptions().targetDirName))           else Seq() ) ++
        (if (addBlackBoxDir) Seq(BlackBoxTargetDirAnno(FirrtlOptions().targetDirName))         else Seq() ) ++
        (if (addLogLevel)    Seq(LogLevelAnnotation(FirrtlOptions().globalLogLevel))           else Seq() ) ++
        (if (addCompiler)    Seq(CompilerNameAnnotation(FirrtlOptions().compilerName))         else Seq() ) ++
        (if (addTopName)     Seq(TopNameAnnotation(ExecutionUtils.Early.topName(annotations))) else Seq() )
    }

    val options1: AnnotationSeq = addDefaults(options)

    // println("---------------------------------------- options1 (with defaults)")
    // options1.foreach( x => println(s"[info]  - $x") )

    /** Add the implicit annotaiton file annotation if such a file exists */
    def addImplicitAnnotations(annotations: AnnotationSeq): AnnotationSeq = annotations.toList ++ (
        annotations.collectFirst{ case a: InputAnnotationFileAnnotation => a } match {
          case Some(_) => List()
          case None =>
            val file = ExecutionUtils.Early.targetDir(annotations) + "/" +
              ExecutionUtils.Early.topName(annotations) + ".anno"
            if (new File(file).exists) {
              Driver.dramaticWarning(
                s"Implicit reading of the annotation file is deprecated! Use an explict --annotation-file argument.")
              List(InputAnnotationFileAnnotation(file))
            } else {
              List()
            }
        } )

    val options2: AnnotationSeq = addImplicitAnnotations(options1)

    // println("---------------------------------------- options2 (with implicit annotation file)")
    // options2.foreach( x => println(s"[info]  - $x") )

    /* Pull in whatever the user tells us to from files until we can't find
     * any more. Remember what we've already imported to prevent a
     * loop. */
    var includeGuard = Set[String]()
    def getIncludes(annotations: AnnotationSeq): Seq[Annotation] = annotations
      .flatMap( _ match {
                 case a: InputAnnotationFileAnnotation =>
                   if (includeGuard.contains(a.file)) {
                     Driver.dramaticWarning("Tried to import the same annotation file twice! (Did you include it twice?)")
                     Seq(DeletedAnnotation("HasFirrtlOptions", a))
                   } else {
                     includeGuard += a.file
                     Seq(DeletedAnnotation("HasFirrtlOptions", a)) ++ getIncludes(ExecutionUtils.readAnnotationsFromFile(a.file))
                   }
                 case a: Annotation =>
                   Seq(a)
               })

    val options3: AnnotationSeq = getIncludes(options2)

    // println("---------------------------------------- options3 (with all annotation files included)")
    // options3.foreach( x => println(s"[info]  - $x") )

    def getEmitterAnnotations(compiler: String): Seq[Annotation] = {
      val emitter = compiler match {
        case "high"      => classOf[HighFirrtlEmitter]
        case "low"       => classOf[LowFirrtlEmitter]
        case "middle"    => classOf[MiddleFirrtlEmitter]
        case "verilog"   => classOf[VerilogEmitter]
        case "sverilog"  => classOf[SystemVerilogEmitter]
      }
      Seq(EmitterAnnotation(emitter))
    }

    val nonFirrtlOptionsAnnotations = options3.filter{
      case opt: FirrtlOption => false
      case _ => true }

    val options4: FirrtlOptions = options3
      .collect{ case opt: FirrtlOption => opt }
      .foldLeft(FirrtlOptions(annotations = nonFirrtlOptionsAnnotations.toList)){
        case (old, x) =>
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
            // [todo] this is a kludge, the transform should really be extracted here
            case RunFirrtlTransformAnnotation(x) => old.copy(
              customTransforms = old.customTransforms :+ Class.forName(x).asInstanceOf[Class[_<:Transform]].newInstance())
          }
          // [todo] Delete FirrtlOptions annotations here
          processed
            .copy(annotations = processed.annotations :+ x)
      }

    // println("---------------------------------------- options4 (with annotations converted to transforms)")
    // options4.foreach( x => println(s"[info]  - $x") )

    val options5: FirrtlOptions = options4.copy(annotations = LegacyAnnotation.convertLegacyAnnos(options4.annotations).toList)

    println("---------------------------------------- options5 (converting LegacyAnnotations)")
    options5.annotations.foreach( x => println(s"[info]  - $x") )

    options5
  }

  lazy val topName: Option[String] = firrtlOptions.topName
  lazy val targetDirName: String = firrtlOptions.targetDirName

  def makeTargetDir(): Boolean = FileUtils.makeDirectory(targetDirName)

  /**
    * return a file based on targetDir, topName and suffix
    * Will not add the suffix if the topName already ends with that suffix
    *
    * @param suffix suffix to add, removes . if present
    * @param fileNameOverride this will override the topName if nonEmpty, when using this targetDir is ignored
    * @return
    */
  def getBuildFileName(suffix: String, fileNameOverride: Option[String] = None): String = {
    makeTargetDir()

    val baseName: String = fileNameOverride.getOrElse(topName.get)
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
    .text(s"This options defines a work directory for intermediate files and blackboxes, default is ${FirrtlOptions().targetDirName}")

  parser.opt[String]("log-level")
    .abbr("ll")
    .valueName("<Error|Warn|Info|Debug|Trace>")
    .action( (x, c) => c :+ LogLevelAnnotation(LogLevel(x)) )
    .validate( x =>
      if (Array("error", "warn", "info", "debug", "trace").contains(x.toLowerCase)) parser.success
      else parser.failure(s"$x bad value must be one of error|warn|info|debug|trace") )
    .maxOccurs(1)
    .text(s"This options defines a work directory for intermediate files, default is ${FirrtlOptions().targetDirName}")

  parser.opt[Seq[String]]("class-log-level")
    .abbr("cll")
    .valueName("<FullClassName:[Error|Warn|Info|Debug|Trace]>[,...]")
    .action( (x, c) => c ++ (x.map { y =>
                val className :: levelName :: _ = y.split(":").toList
                val level = LogLevel(levelName)
                ClassLogLevelAnnotation(className, level) }) )
    .maxOccurs(1)
    .text(s"This options defines a work directory for intermediate files, default is ${FirrtlOptions().targetDirName}")

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
    .text(s"specifies the source info handling, default is ${FirrtlOptions().infoModeName}")

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
      parser.failure("At least one of --top-name, --input-file, or --has-firrtl-source must be specified")
    else if (hasInputFile && hasFirrtlSource)
      parser.failure("Only one of --input-file or --firrtl-source may be specified")
    else if (hasOneFilePerModule && hasOutputFile)
      parser.failure("Output override (--output-file) is incompatible with one file per module (--split-modules)")
    else if (hasInputFile && hasFirrtlSource)
      parser.failure("Specify one of --input-file or --firrtl-source (not both!)")
    else
      parser.success
  }
}

/** Firrtl output configuration specified by [[FirrtlOptions]]
  *
  * Derived from the fields of the execution options
  * @see [[FirrtlOptions.getOutputConfig]]
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
case class FirrtlOptions(
  topName:                  Option[String]              = None,
  targetDirName:            String                      = ".",
  globalLogLevel:           LogLevel.Value              = LogLevel.None,
  logToFile:                Boolean                     = false,
  logClassNames:            Boolean                     = false,
  classLogLevels:           Map[String, LogLevel.Value] = Map.empty,
  programArgs:              Seq[String]                 = Seq.empty,
  inputFileNameOverride:    Option[String]              = None,
  outputFileNameOverride:   Option[String]              = None,
  outputAnnotationFileName: Option[String]              = None,
  compilerName:             String                      = "verilog",
  infoModeName:             String                      = "append",
  customTransforms:         Seq[Transform]              = List.empty,
  firrtlSource:             Option[String]              = None,
  annotations:              List[Annotation]            = List.empty,
  emitOneFilePerModule:     Boolean                     = false,
  annotationFileNames:      List[String]                = List.empty)
extends ComposableOptions {

  require(!(emitOneFilePerModule && outputFileNameOverride.nonEmpty),
          "Cannot both specify the output filename and emit one file per module!!!")

  def getLogFileName(optionsManager: ExecutionOptionsManager with HasFirrtlOptions): String = {
    if(topName.isEmpty) {
      optionsManager.getBuildFileName("log", Some("firrtl"))
    } else {
      optionsManager.getBuildFileName("log")
    }
  }

  def infoMode: Parser.InfoMode = {
    infoModeName match {
      case "use"    => Parser.UseInfo
      case "ignore" => Parser.IgnoreInfo
      case "gen"    => Parser.GenInfo(inputFileNameOverride.getOrElse("[not defined]"))
      case "append" => Parser.AppendInfo(inputFileNameOverride.getOrElse("[not defined]"))
      case other    => Parser.UseInfo
    }
  }

  def compiler: Compiler = compilerName match {
    case "high"      => new HighFirrtlCompiler()
    case "low"       => new LowFirrtlCompiler()
    case "middle"    => new MiddleFirrtlCompiler()
    case "verilog"   => new VerilogCompiler()
    case "sverilog"  => new SystemVerilogCompiler()
  }

  def outputSuffix: String = compilerName match {
    case "verilog"   => "v"
    case "sverilog"  => "sv"
    case "low"       => "lo.fir"
    case "high"      => "hi.fir"
    case "middle"    => "mid.fir"
    case _ =>
      throw new Exception(s"Illegal compiler name $compilerName")
  }

  /**
    * build the input file name, taking overriding parameters
    *
    * @param optionsManager this is needed to access build function and its common options
    * @return a properly constructed input file name
    */
  def getInputFileName(optionsManager: ExecutionOptionsManager with HasFirrtlOptions ): String = {
    optionsManager.getBuildFileName("fir", inputFileNameOverride)
  }
  /** Get the user-specified [[OutputConfig]]
    *
    * @param optionsManager this is needed to access build function and its common options
    * @return the output configuration
    */
  def getOutputConfig(optionsManager: ExecutionOptionsManager with HasFirrtlOptions): OutputConfig = {
    if (emitOneFilePerModule) OneFilePerModule(optionsManager.targetDirName)
    else SingleFile(optionsManager.getBuildFileName(outputSuffix, outputFileNameOverride))
  }
  /** Get the user-specified targetFile assuming [[OutputConfig]] is [[SingleFile]]
    *
    * @param optionsManager this is needed to access build function and its common options
    * @return the targetFile as a String
    */
  def getTargetFile(optionsManager: ExecutionOptionsManager with HasFirrtlOptions): String = {
    getOutputConfig(optionsManager) match {
      case SingleFile(targetFile) => targetFile
      case other => throw new Exception("OutputConfig is not SingleFile!")
    }
  }
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
