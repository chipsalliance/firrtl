// See LICENSE for license details.

package firrtl

import firrtl.ir.Circuit
import firrtl.annotations.{
  Annotation,
  LegacyAnnotation,
  AnnotationFileNotFoundException,
  JsonProtocol }
import firrtl.transforms.BlackBoxTargetDirAnno
import firrtl.options.{OptionsView, ExecutionOptionsManager, RegisteredLibrary, OptionsException, DriverExecutionResult}
import logger.LogLevel
import java.io.{File, FileNotFoundException}
import net.jcazevedo.moultingyaml._
import firrtl.annotations.AnnotationYamlProtocol._
import scala.util.{Try, Failure}
import scopt.OptionParser

object FirrtlViewer {
  implicit object FirrtlOptionsView extends OptionsView[FirrtlExecutionOptions] {
    def view(options: AnnotationSeq): Option[FirrtlExecutionOptions] = {
      /* Pull in whatever the user tells us to from files until we can't find
       * any more. Remember what we've already imported to prevent a
       * loop. */
      var includeGuard = Set[String]()
      def getIncludes(annos: AnnotationSeq): AnnotationSeq = annos
        .flatMap {
          case a @ InputAnnotationFileAnnotation(value) =>
            if (includeGuard.contains(value)) {
              Driver.dramaticWarning("Tried to import the same annotation file twice! (Did you include it twice?)")
              Seq(a)
            } else {
              includeGuard += value
              Seq(a) ++ getIncludes(FirrtlExecutionUtils.readAnnotationsFromFile(value))
            }
          case x => Seq(x)
        }

      val annotationTransforms: Seq[AnnotationSeq => AnnotationSeq] =
        Seq( FirrtlExecutionUtils.addImplicitAnnotationFile(_),
             getIncludes(_),
             FirrtlExecutionUtils.addDefaults(_),
             LegacyAnnotation.convertLegacyAnnos(_),
             FirrtlExecutionUtils.checkAnnotations(_) )

      val preprocessedAnnotations: AnnotationSeq = annotationTransforms
        .foldLeft(options)( (old, tx) => tx(old) )

      val (firrtlAnnos, nonFirrtlAnnos) = preprocessedAnnotations.partition{
        case opt: FirrtlOption => true
        case _                 => false }

      val x = firrtlAnnos
        .foldLeft(FirrtlExecutionOptions(annotations = nonFirrtlAnnos.toList)){ (c, x) =>
          val processed = x match {
            case TopNameAnnotation(n)              => c.copy(topName = Some(n))
            case TargetDirAnnotation(d)            => c.copy(targetDirName = d)
            case LogLevelAnnotation(l)             => c.copy(globalLogLevel = l)
            case ClassLogLevelAnnotation(n, l)     => c.copy(classLogLevels = c.classLogLevels ++ Map(n -> l))
            case LogToFileAnnotation               => c.copy(logToFile = true)
            case LogClassNamesAnnotation           => c.copy(logClassNames = true)
            case ProgramArgsAnnotation(s)          => c.copy(programArgs = c.programArgs :+ s)
            case InputFileAnnotation(f)            => c.copy(inputFileNameOverride = Some(f))
            case OutputFileAnnotation(f)           => c.copy(outputFileNameOverride = Some(f))
            case OutputAnnotationFileAnnotation(f) => c.copy(outputAnnotationFileName = Some(f))
            case InfoModeAnnotation(i)             => c.copy(infoModeName = i)
            case FirrtlSourceAnnotation(s)         => c.copy(firrtlSource = Some(s))
            case EmitOneFilePerModuleAnnotation    => c.copy(emitOneFilePerModule = true)
            case InputAnnotationFileAnnotation(f)  => c
            case CompilerNameAnnotation(cx)        => c.copy(compilerName = cx,
                                                             annotations = c.annotations :+
                                                               FirrtlExecutionUtils.getEmitterAnnotation(cx))
            case RunFirrtlTransformAnnotation(x)   => c.copy(customTransforms = c.customTransforms :+ x.newInstance())
            case FirrtlCircuitAnnotation(cir)      => c.copy(firrtlCircuit = Some(cir))
          }
          // [todo] Delete FirrtlExecutionOptions annotations here
          processed
            .copy(annotations = processed.annotations :+ x)
        }
      Some(x)
    }
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
  * @param targetDirName name of the target directory (default: ".")
  * @param globalLogLevel the verbosity of logging (default: [[logger.LogLevel.None]])
  * @param classLogLevels the individual verbosity of logging for specific classes
  * @param logToFile if true, log to a file
  * @param logClassNames indicates logging verbosity on a class-by-class basis
  * @param programArgs explicit program arguments
  * @param inputFileNameOverride input FIRRTL file, default: `targetDir/topName.fir`
  * @param outputFileNameOverride output file, default: `targetDir/topName.SUFFIX` with `SUFFIX`
  *                               as determined by the compiler
  * @param outputAnnotationFileName the output annotation filename, default: `targetDir/topName.anno`
  * @param compilerName which compiler to use (default: "verilog")
  * @param infoModeName the policy for generating [[firrtl.ir.Info]] when processing FIRRTL (default: "append")
  * @param customTransforms any custom [[Transform]] to run
  * @param firrtlSource explicit input FIRRTL as a [[scala.String]]
  * @param annotations a sequence of [[annotations.Annotation]] passed to the compiler
  * @param emitOneFilePerModule enables one-file-per-module output in the [[Emitter]]
  */
final case class FirrtlExecutionOptions(
  topName:                  Option[String]              = None,
  targetDirName:            String                      = TargetDirAnnotation().targetDirName,
  globalLogLevel:           LogLevel.Value              = LogLevelAnnotation().globalLogLevel,
  classLogLevels:           Map[String, LogLevel.Value] = Map.empty,
  logToFile:                Boolean                     = false,
  logClassNames:            Boolean                     = false,
  programArgs:              Seq[String]                 = Seq.empty,
  inputFileNameOverride:    Option[String]              = None,
  outputFileNameOverride:   Option[String]              = None,
  outputAnnotationFileName: Option[String]              = None,
  compilerName:             String                      = CompilerNameAnnotation().value,
  infoModeName:             String                      = InfoModeAnnotation().value,
  customTransforms:         Seq[Transform]              = List.empty,
  firrtlSource:             Option[String]              = None,
  annotations:              List[Annotation]            = List.empty,
  emitOneFilePerModule:     Boolean                     = false,
  firrtlCircuit:            Option[Circuit]             = None) {

  /** Build the input file name, taking overriding parameters
    *
    * @param optionsManager this is needed to access build function and its common options
    * @return a properly constructed input file name
    */
  def getInputFileName(): String = inputFileNameOverride.getOrElse(getBuildFileName("fir"))

  /** Get the user-specified [[OutputConfig]]
    *
    * @param optionsManager this is needed to access build function and its common options
    * @return the output configuration
    */
  def getOutputConfig(): OutputConfig =
    if (emitOneFilePerModule) { OneFilePerModule(targetDirName)                                    }
    else                      { SingleFile(getBuildFileName(outputSuffix, outputFileNameOverride)) }

  /** Get the user-specified targetFile assuming [[OutputConfig]] is [[SingleFile]]
    *
    * @param optionsManager this is needed to access build function and its common options
    * @return the targetFile as a String
    */
  def getTargetFile(): String = getOutputConfig() match {
    case SingleFile(targetFile) => targetFile
    case _                      => throw new Exception("OutputConfig is not SingleFile!") }

  def makeTargetDir(): Boolean = FileUtils.makeDirectory(targetDirName)

  def getBuildFileName(suffix: String, fileNameOverride: Option[String] = None): String = {
    makeTargetDir

    val baseName: String = fileNameOverride.getOrElse(topName.get)
    val baseNameIsFullPath: Boolean = baseName.startsWith("./") || baseName.startsWith("/")
    val directoryName: String =
      if (fileNameOverride.nonEmpty || baseNameIsFullPath) { ""                  }
      else if (targetDirName.endsWith("/"))                { targetDirName       }
      else                                                 { targetDirName + "/" }
    val normalizedSuffix: String = {
      val dottedSuffix = if(suffix.startsWith(".")) suffix else s".$suffix"
      if(baseName.endsWith(dottedSuffix)) "" else dottedSuffix
    }
    val path: String = directoryName + baseName.split("/").dropRight(1).mkString("/")

    FileUtils.makeDirectory(path)
    s"$directoryName$baseName$normalizedSuffix"
  }

  def getLogFileName(): String = getBuildFileName("log")

  def getCircuit(): Try[ir.Circuit] = {
    Try {
      // Check that only one "override" is used
      firrtlCircuit.getOrElse {
        firrtlSource.map(x => Parser.parseString(x, infoMode)).getOrElse {
          val inputFileName = getInputFileName
          try {
            // TODO What does InfoMode mean to ProtoBuf?
            FirrtlExecutionUtils.getFileExtension(inputFileName) match {
              case ProtoBufFile => proto.FromProto.fromFile(inputFileName)
              case FirrtlFile => Parser.parseFile(inputFileName, infoMode)
            }
          }
          catch {
            case _: FileNotFoundException =>
              val message = s"Input file $inputFileName not found"
              throw new OptionsException(message)
          }
        }
      }
    }
  }

  /** Return the info mode */
  def infoMode(): Parser.InfoMode = {
    infoModeName match {
      case "use"    => Parser.UseInfo
      case "ignore" => Parser.IgnoreInfo
      case "gen"    => Parser.GenInfo(inputFileNameOverride.getOrElse("[not defined]"))
      case "append" => Parser.AppendInfo(inputFileNameOverride.getOrElse("[not defined]"))
      case _        => throw new Exception(s"Illegale info mode name $infoModeName")
    }
  }

  /** Return the compiler */
  def compiler(): Compiler = compilerName match {
    case "high"      => new HighFirrtlCompiler()
    case "low"       => new LowFirrtlCompiler()
    case "middle"    => new MiddleFirrtlCompiler()
    case "verilog"   => new VerilogCompiler()
    case "sverilog"  => new SystemVerilogCompiler()
    case _           => throw new Exception(s"Illegal compiler name $compilerName")
  }

  /** Return the output file suffix */
  def outputSuffix(): String = compilerName match {
    case "verilog"  => "v"
    case "sverilog" => "sv"
    case "low"      => "lo.fir"
    case "high"     => "hi.fir"
    case "middle"   => "mid.fir"
    case _          => throw new Exception(s"Illegal compiler name $compilerName")
  }
}

/** A result of running the FIRRTL compiler */
sealed trait FirrtlExecutionResult extends DriverExecutionResult

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

private[firrtl] sealed trait FileExtension
private[firrtl] case object FirrtlFile extends FileExtension
private[firrtl] case object ProtoBufFile extends FileExtension

/** Utilities that help with processing FIRRTL options */
object FirrtlExecutionUtils {
  /** Determine the target directory with the following precedence (highest to lowest):
    *  - Explicitly from the user-specified `--target-dir`
    *  - Implicitly from the default of [[FirrtlExecutionOptions.targetDirName]]
    *
    * @param annotations input annotations to extract targetDir from
    * @return the target directory
    * @note this is safe to use before [[HasFirrtlExecutionOptions.firrtlOptions]] is set
    */
  def targetDir(annotations: AnnotationSeq): String = annotations
    .collectFirst{ case TargetDirAnnotation(dir) => dir }
    .getOrElse(new FirrtlExecutionOptions().targetDirName)

  private[firrtl] def getFileExtension(file: String): FileExtension = file.drop(file.lastIndexOf('.')) match {
    case ".pb" => ProtoBufFile
    case _     => FirrtlFile
  }

  /** Determine the top name using the following precedence (highest to lowest):
    *  - Explicitly from the user-specified `--top-name`
    *  - Implicitly from the top module ([[ir.Circuit.main]]) of [[FirrtlCircuitAnnotation]]
    *  - Implicitly from the top module ([[ir.Circuit.main]]) of `--firrtl-source`/[[FirrtlSourceAnnotation]]
    *  - Implicitly from the top module ([[ir.Circuit.main]]) of `--input-file`/[[InputFileAnnotation]]
    *
    * @param annotations annotations to extract topName from
    * @return the top module _if it can be determined_
    * @note [[FirrtlCircuitAnnotation]], [[FirrtlSourceAnnotation]], and [[InputFileAnnotation]]
    * @note this is safe to use before [[HasFirrtlExecutionOptions.firrtlOptions]] is set
    */
  def topName(annotations: AnnotationSeq): Option[String] =
    annotations.collectFirst{ case TopNameAnnotation(n) => n }.orElse(
      annotations.collectFirst{ case FirrtlCircuitAnnotation(c) => c.main }.orElse(
        annotations.collectFirst{ case FirrtlSourceAnnotation(s) => Parser.parse(s).main }.orElse(
          annotations.collectFirst{ case InputFileAnnotation(f) =>
            getFileExtension(f) match {
              case ProtoBufFile => proto.FromProto.fromFile(f).main
              case FirrtlFile   => Parser.parse(io.Source.fromFile(f).getLines().mkString("\n")).main } } )))

  /** Read all [[annotations.Annotation]] from a file in JSON or YAML format
    *
    * @param filename a JSON or YAML file of [[annotations.Annotation]]
    * @throws annotations.AnnotationFileNotFoundException if the file does not exist
    */
  def readAnnotationsFromFile(filename: String): AnnotationSeq = {
    val file = new File(filename).getCanonicalFile
    if (!file.exists) { throw new AnnotationFileNotFoundException(file) }
    JsonProtocol.deserializeTry(file).recoverWith { case jsonException =>
      // Try old protocol if new one fails
      Try {
        val yaml = io.Source.fromFile(file).getLines().mkString("\n").parseYaml
        val result = yaml.convertTo[List[LegacyAnnotation]]
        val msg = s"$file is a YAML file!\n" + (" "*9) + "YAML Annotation files are deprecated! Use JSON"
        Driver.dramaticWarning(msg)
        result
      }.orElse { // Propagate original JsonProtocol exception if YAML also fails
        Failure(jsonException)
      }
    }.get
  }

  /** Add the implicit annotation file if no [[InputAnnotationFileAnnotation]] is present
    *
    * @param annos annotations that may contain an [[InputAnnotationFileAnnotation]]
    * @return a sequence of annotations that includes an [[InputAnnotationFileAnnotation]]
    * @note The implicit annotation file is in `targetDir/topName.anno`
    */
  def addImplicitAnnotationFile(annos: AnnotationSeq): AnnotationSeq = annos
    .collectFirst{ case a: InputAnnotationFileAnnotation => a } match {
      case Some(_) => annos
      case None => FirrtlExecutionUtils.topName(annos) match {
        case Some(n) =>
          val filename = FirrtlExecutionUtils.targetDir(annos) + "/" + n + ".anno"
          if (new File(filename).exists) {
            Driver.dramaticWarning(
              s"Implicit reading of the annotation file is deprecated! Use an explict --annotation-file argument.")
            annos :+ InputAnnotationFileAnnotation(filename)
          } else {
            annos
          }
        case None => annos
      } }

  /** Append any missing default annotations to an annotation sequence
    *
    * @param annos annotation sequence to examine
    * @return the annotation sequence with default annotations added
    */
  def addDefaults(annos: AnnotationSeq): AnnotationSeq = { //scalastyle:off cyclomatic.complexity
    var Seq(td, bb, ll, c, tn) = Seq.fill(5)(true) //scalastyle:ignore
    annos.collect{ case a: FirrtlOption => a }.map{
      case _: TargetDirAnnotation       => td = false
      case _: BlackBoxTargetDirAnno     => bb = false
      case _: LogLevelAnnotation        => ll = false
      case _: CompilerNameAnnotation    => c  = false
      case _: TopNameAnnotation         => tn = false
      case _ =>
    }

    // [todo] Once the implicit annotation file is deprecated, this complexity decreases
    val default = FirrtlExecutionOptions()
    val name = FirrtlExecutionUtils.topName(annos)
    val targetDir = FirrtlExecutionUtils.targetDir(annos)
    annos ++
      (if (td)                 Seq(TargetDirAnnotation(targetDir))               else Seq() ) ++
      (if (bb)                 Seq(BlackBoxTargetDirAnno(targetDir))             else Seq() ) ++
      (if (ll)                 Seq(LogLevelAnnotation(default.globalLogLevel))   else Seq() ) ++
      (if (c)                  Seq(CompilerNameAnnotation(default.compilerName)) else Seq() ) ++
      (if (tn & name.nonEmpty) Seq(TopNameAnnotation(name.get))                  else Seq() )
  } //scalastyle:on cyclomatic.complexity

  /** Convert a string to an [[EmitterAnnotation]]
    *
    * @param compiler a compiler name
    * @return the corresponding [[EmitterAnnotation]]
    */
  def getEmitterAnnotation(compiler: String): EmitterAnnotation = {
    val emitter = compiler match {
      case "high"      => classOf[HighFirrtlEmitter]
      case "low"       => classOf[LowFirrtlEmitter]
      case "middle"    => classOf[MiddleFirrtlEmitter]
      case "verilog"   => classOf[VerilogEmitter]
      case "sverilog"  => classOf[SystemVerilogEmitter]
    }
    EmitterAnnotation(emitter)
  }

  /** Determine if an annotations are sane
    *
    * @param annos a sequence of [[annotation.Annotation]]
    * @return true if all checks pass
    */
  def checkAnnotations(annos: AnnotationSeq): AnnotationSeq = {
    val Seq(tn, inF, inS, ofpm, outF, td, ll, i, foaf, comp, info, c) =
      Seq.fill(12)(collection.mutable.ListBuffer[Annotation]())
    annos.foreach(
      _ match {
        case a: TopNameAnnotation                   => tn   += a
        case a: InputFileAnnotation                 => inF  += a
        case a: FirrtlSourceAnnotation              => inS  += a
        case a: EmitOneFilePerModuleAnnotation.type => ofpm += a
        case a: OutputFileAnnotation                => outF += a
        case a: TargetDirAnnotation                 => td   += a
        case a: LogLevelAnnotation                  => ll   += a
        case a: OutputAnnotationFileAnnotation      => foaf += a
        case a: CompilerNameAnnotation              => comp += a
        case a: InfoModeAnnotation                  => info += a
        case a: FirrtlCircuitAnnotation             => c    += a
        case _                                      =>           })
    if (tn.isEmpty && inF.isEmpty && inS.isEmpty) {
      throw new FIRRTLException(
        s"""|Unable to determine FIRRTL source to read. None of the following were found:
            |    - a top module name: -tn, --top-name,      TopNameAnnotation
            |    - an input file:     -i,  --input-file,    InputFileAnnotation
            |    - FIRRTL source:          --firrtl-source, FirrtlSourceAnnotation""".stripMargin )}
    if (inF.size + inS.size + c.size > 1) {
      throw new FIRRTLException(
        s"""|Multiply defined input FIRRTL sources. More than one of the following was found:
            |    - an input file (${inF.size} times): -i, --input-file,    InputFileAnnotation
            |    - FIRRTL source (${inS.size} times):     --firrtl-source, FirrtlSourceAnnotation
            |    - a FIRRTL circuit (${c.size} times):                     FirrtlCircuitAnnotation""".stripMargin )}
    if (ofpm.nonEmpty && outF.nonEmpty) {
      throw new FIRRTLException(
        s"""|Output file is incompatible with one file per module, but multiples were found:
            |    - explicit output file (${outF.size} times): -o,   --output-file,   OutputFileAnnotation
            |    - one file per module (${ofpm.size} times):  -fsm, --split-modules, EmitOneFilePerModuleAnnotation"""
          .stripMargin )}
    if (outF.size > 1) {
      val x = outF.map{ case OutputFileAnnotation(x) => x }
      throw new FIRRTLException(
        s"""|No more than one output file can be specified, but found '${x.mkString(", ")}' specified via:
            |    - option or annotation: -o, --output-file, OutputFileAnnotation""".stripMargin) }
    if (tn.size != 1) {
      val n = tn.map{ case TopNameAnnotation(x) => x }
      throw new FIRRTLException(
        s"""|Exactly one top name must be determinable, but found `${n.mkString(", ")}` specified via:
            |    - explicit top name: -tn, --top-name, TopNameAnnotation
            |    - fallback implicit determination from input file or input firrtl source""".stripMargin) }
    if (td.size != 1) {
      val d = td.map{ case TargetDirAnnotation(x) => x }
      throw new FIRRTLException(
        s"""|Exactly one target directory must be specified/determinable, but found `${d.mkString(", ")}` specified via:
            |    - explicit target directory: -td, --target-dir, TargetDirAnnotation
            |    - fallback default value""".stripMargin )}
    if (ll.size > 1) {
      val l = ll.map{ case LogLevelAnnotation(x) => x }
      throw new FIRRTLException(
        s"""|At most one log level can be specified, but found '${l.mkString(", ")}' specified via:
            |    - an option or annotation: -ll, --log-level, LogLevelAnnotation""".stripMargin )}
    if (foaf.size > 1) {
      val x = foaf.map{ case OutputAnnotationFileAnnotation(x) => x }
      throw new FIRRTLException(
        s"""|At most one output annotation file can be specified, but found '${x.mkString(", ")}' specified via:
            |    - an option or annotation: -foaf, --output-annotation-file, OutputAnnotationFileAnnotation"""
          .stripMargin )}
    comp.foreach{ case CompilerNameAnnotation(x) =>
      if (!Set("high", "middle", "low", "verilog", "sverilog").contains(x)) {
        throw new FIRRTLException(s"Unknown compiler '$x' (did you misspell it?)") }}
    if (comp.size > 1) {
      val x = comp.map{ case CompilerNameAnnotation(x) => x }
      throw new FIRRTLException(
        s"""|FIRRTL currently only supports one target compiler, but found '${x.mkString(", ")}' specified via:
            |    - an option or annotation: -X, --compiler, CompilerNameAnnotation""".stripMargin )}
    info.foreach{ case InfoModeAnnotation(x) =>
      if (!(Set("ignore", "use", "gen", "append").contains(x))) {
        throw new FIRRTLException(s"Unknown info mode '$x' (did you misspell it?)") }}
    if (info.size > 1) {
      val x = info.map{ case InfoModeAnnotation(x) => x }
      throw new FIRRTLException(
        s"""|Only one info mode may be specified, but found '${x.mkString(", ")}' specified via:
            |    - an option or annotation: --info-mode, InfoModeAnnotation""".stripMargin )}

    annos
  }
}

trait HasFirrtlExecutionOptions { this: ExecutionOptionsManager =>
  import firrtl.options.Viewer._
  import firrtl.FirrtlViewer._

  parser.note("Common Options")
  /* [Note 1] Any validation related to these options is removed here. Since
   * we need to check annotations anyway, arguments here are purposefully
   * marked as `unbounded` and no validation checking occurs (except that
   * which is related to ensuring that a command line string is validly
   * converted to some type, e.g., --log-level). All of the actual option
   * validation happens when the annotations are processed in
   * [[FirrtlExecutionUtils.checkAnnotations]]. */
  Seq( TopNameAnnotation(),
       TargetDirAnnotation(),
       LogLevelAnnotation(),
       ClassLogLevelAnnotation(),
       LogToFileAnnotation,
       LogClassNamesAnnotation,
       ProgramArgsAnnotation() )
    .map(_.addOptions(parser))

  parser.help("help").text("prints this usage text")

  parser.note("FIRRTL Compiler Options")
  Seq( InputFileAnnotation(),
       OutputFileAnnotation(),
       OutputAnnotationFileAnnotation(),
       InfoModeAnnotation(),
       FirrtlSourceAnnotation(),
       EmitOneFilePerModuleAnnotation,
       InputAnnotationFileAnnotation(),
       CompilerNameAnnotation(),
       RunFirrtlTransformAnnotation() )
    .map(_.addOptions(parser))

  parser.opt[Unit]("force-append-anno-file")
    .abbr("ffaaf")
    .hidden()
    .unbounded()
    .action{ (x, c) =>
      val msg = "force-append-anno-file is deprecated\n" + (" "*9) + "(It does not do anything anymore)"
      Driver.dramaticWarning(msg)
      c }
}
