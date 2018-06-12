// See LICENSE for license details.

package firrtl

import firrtl.ir.Circuit
import firrtl.annotations.{
  Annotation,
  NoTargetAnnotation,
  LegacyAnnotation,
  AnnotationFileNotFoundException,
  JsonProtocol }
import firrtl.transforms.{
  BlackBoxTargetDirAnno,
  DontCheckCombLoopsAnnotation,
  NoDCEAnnotation }
import logger.LogLevel
import java.io.File
import net.jcazevedo.moultingyaml._
import firrtl.annotations.AnnotationYamlProtocol._
import scala.util.{Try, Failure}
import scopt.OptionParser

/** Indicates that a subclass is an [[annotations.Annotation]] with an
  * option consummable by [[HasFirrtlExecutionOptions]]
  *
  * This must be mixed into a subclass of [[annotations.Annotation]]
  */
trait FirrtlOption { this: Annotation => }

/** Holds the name of the top module
  *  - maps to [[FirrtlExecutionOptions.topName]]
  *  - set on the command line with `-tn/--top-name`
  * @param value top module name
  */
case class TopNameAnnotation(value: String) extends NoTargetAnnotation with FirrtlOption

/** Holds the name of the target directory
  *  - maps to [[FirrtlExecutionOptions.targetDirName]]
  *  - set with `-td/--target-dir`
  *  - if unset, a [[TargetDirAnnotation]] will be generated with the
  * @param value target directory name
  */
case class TargetDirAnnotation(value: String = ".") extends NoTargetAnnotation with FirrtlOption

/** Describes the verbosity of information to log
  *  - maps to [[FirrtlExecutionOptions.globalLogLevel]]
  *  - set with `-ll/--log-level`
  *  - if unset, a [[LogLevelAnnotation]] with the default log level will be emitted
  * @param level the level of logging
  */
case class LogLevelAnnotation(level: LogLevel.Value = LogLevel.None) extends NoTargetAnnotation with FirrtlOption {
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
case class ProgramArgsAnnotation(value: String) extends NoTargetAnnotation with FirrtlOption

/** An explicit input FIRRTL file to read
  *  - maps to [[FirrtlExecutionOptions.inputFileNameOverride]]
  *  - set with `-i/--input-file`
  *  - If unset, an [[InputFileAnnotation]] with the default input file __will not be generated__
  * @param value input filename
  */
case class InputFileAnnotation(value: String) extends NoTargetAnnotation with FirrtlOption

/** An explicit output file the emitter will write to
  *   - maps to [[FirrtlExecutionOptions.outputFileNameOverride]]
  *   - set with `-o/--output-file`
  *  @param value output filename
  */
case class OutputFileAnnotation(value: String) extends NoTargetAnnotation with FirrtlOption

/** An explicit output _annotation_ file to write to
  *  - maps to [[FirrtlExecutionOptions.outputAnnotationFileName]]
  *  - set with `-foaf/--output-annotation-file`
  * @param value output annotation filename
  */
case class OutputAnnotationFileAnnotation(value: String) extends NoTargetAnnotation with FirrtlOption

/** Sets the info mode style
  *  - maps to [[FirrtlExecutionOptions.infoModeName]]
  *  - set with `--info-mode`
  * @param value info mode name
  */
case class InfoModeAnnotation(value: String = "append") extends NoTargetAnnotation with FirrtlOption

/** Holds a [[scala.String]] containing FIRRTL source to read as input
  *  - maps to [[FirrtlExecutionOptions.firrtlSource]]
  *  - set with `--firrtl-source`
  * @param value FIRRTL source as a [[scala.String]]
  */
case class FirrtlSourceAnnotation(value: String) extends NoTargetAnnotation with FirrtlOption

/** Holds a filename containing one or more [[annotations.Annotation]] to be read
  *  - this is not stored in [[FirrtlExecutionOptions]]
  *  - set with `-faf/--annotation-file`
  * @param value input annotation filename
  */
case class InputAnnotationFileAnnotation(value: String) extends NoTargetAnnotation with FirrtlOption

/** Holds the name of the compiler to run
  *  - maps to [[FirrtlExecutionOptions.compilerName]]
  *  - set with `-X/--compiler`
  *  - If unset, a [[CompilerNameAnnotation]] with the default compiler ("verilog") __will be generated__
  * @param value compiler name
  */
case class CompilerNameAnnotation(value: String = "verilog") extends NoTargetAnnotation with FirrtlOption

/** Holds the unambiguous class name of a [[Transform]] to run
  *  - will be append to [[FirrtlExecutionOptions.customTransforms]]
  *  - set with `-fct/--custom-transforms`
  * @param value the full class name of the transform
  */
case class RunFirrtlTransformAnnotation(value: String) extends NoTargetAnnotation with FirrtlOption

/** Holds a FIRRTL [[Circuit]]
  * @param value a circuit
  */
case class FirrtlCircuitAnnotation(value: Circuit) extends NoTargetAnnotation with FirrtlOption

object FirrtlViewer {
  implicit object FirrtlOptionsView extends OptionsView[FirrtlExecutionOptions] {
    def view(implicit options: AnnotationSeq): Option[FirrtlExecutionOptions] = {
      /* Pull in whatever the user tells us to from files until we can't find
       * any more. Remember what we've already imported to prevent a
       * loop. */
      var includeGuard = Set[String]()
      def getIncludes(annos: Seq[Annotation]): Seq[Annotation] = annos
        .flatMap {
          case a: InputAnnotationFileAnnotation =>
            if (includeGuard.contains(a.value)) {
              Driver.dramaticWarning("Tried to import the same annotation file twice! (Did you include it twice?)")
              Seq(a)
            } else {
              includeGuard += a.value
              Seq(a) ++ getIncludes(FirrtlExecutionUtils.readAnnotationsFromFile(a.value))
            }
          case x => Seq(x)
        }

      val annotationTransforms: Seq[Seq[Annotation] => Seq[Annotation]] =
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
            case TopNameAnnotation(n)              => c.copy(topName = Some(n))
            case TargetDirAnnotation(d)            => c.copy(targetDirName = d)
            case LogLevelAnnotation(l)             => c.copy(globalLogLevel = l)
            case ClassLogLevelAnnotation(n, l)     => c.copy(classLogLevels = c.classLogLevels ++ Map(n -> l))
            case LogToFileAnnotation               => c.copy(logToFile = true)
            case LogClassNamesAnnotation           => c.copy(logClassNames = true)
            case ProgramArgsAnnotation(s)          => c.copy(programArgs = c.programArgs :+ s)
            case RunFirrtlTransformAnnotation(x)   => c.copy(
              customTransforms = c.customTransforms :+ Class.forName(x).asInstanceOf[Class[_<:Transform]].newInstance())
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
  emitOneFilePerModule:     Boolean                     = false,
  firrtlCircuit:            Option[Circuit]             = None) {

  /** Return the info mode */
  def infoMode(): Parser.InfoMode = {
    infoModeName match {
      case "use"    => Parser.UseInfo
      case "ignore" => Parser.IgnoreInfo
      case "gen"    => Parser.GenInfo(inputFileNameOverride.getOrElse("[not defined]"))
      case "append" => Parser.AppendInfo(inputFileNameOverride.getOrElse("[not defined]"))
      case _        => Parser.UseInfo
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
    case "verilog"  => "v"
    case "sverilog" => "sv"
    case "low"      => "lo.fir"
    case "high"     => "hi.fir"
    case "middle"   => "mid.fir"
    case _          => throw new Exception(s"Illegal compiler name $compilerName")
  }

  /** Build the input file name, taking overriding parameters
    *
    * @param optionsManager this is needed to access build function and its common options
    * @return a properly constructed input file name
    */
  @deprecated("Use ExecutionOptionsManager.getInputFileName", "1.2.0")
  def getInputFileName(optionsManager: ExecutionOptionsManager with HasFirrtlExecutionOptions ): String =
    optionsManager.getBuildFileName("fir", inputFileNameOverride)
  /** Get the user-specified [[OutputConfig]]
    *
    * @param optionsManager this is needed to access build function and its common options
    * @return the output configuration
    */
  @deprecated("Use ExecutionOptionsManager.getOutputConfig", "1.2.0")
  def getOutputConfig(optionsManager: ExecutionOptionsManager with HasFirrtlExecutionOptions): OutputConfig =
    if (emitOneFilePerModule) { OneFilePerModule(optionsManager.targetDirName)                                    }
    else                      { SingleFile(optionsManager.getBuildFileName(outputSuffix, outputFileNameOverride)) }

  /** Get the user-specified targetFile assuming [[OutputConfig]] is [[SingleFile]]
    *
    * @param optionsManager this is needed to access build function and its common options
    * @return the targetFile as a String
    */
  @deprecated("Use ExecutionOptionsManager.getTargetFile", "1.2.0")
  def getTargetFile(optionsManager: ExecutionOptionsManager with HasFirrtlExecutionOptions): String =
    getOutputConfig(optionsManager) match {
      case SingleFile(targetFile) => targetFile
      case _                      => throw new Exception("OutputConfig is not SingleFile!") }
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
  def targetDir(annotations: Seq[Annotation]): String = annotations
    .collectFirst{ case TargetDirAnnotation(dir) => dir }
    .getOrElse(new FirrtlExecutionOptions().targetDirName)

  /** Determine the top name using the following precedence (highest to lowest):
    *  - Explicitly from the user-specified `--top-name`
    *  - Implicitly from the top module ([[ir.Circuit.main]]) of `--input-file`
    *  - Implicitly from the top module ([[ir.Circuit.main]]) of `--firrtl-source`
    *
    * @param annotations annotations to extract topName from
    * @return the top module _if it can be determined_
    * @note `--input-file` and `--firrtl-source` are mutually exclusive
    * @note this is safe to use before [[HasFirrtlExecutionOptions.firrtlOptions]] is set
    */
  def topName(annotations: Seq[Annotation]): Option[String] = {
    var Seq(topName, inputFileName, firrtlSource) = Seq.fill(3)(None: Option[String]) //scalastyle:ignore
    annotations.foreach{
      case TopNameAnnotation(name)         => topName       = Some(name)
      case InputFileAnnotation(file)       => inputFileName = Some(file)
      case FirrtlSourceAnnotation(circuit) => firrtlSource  = Some(circuit)
      case _ => }
    topName.orElse {
      if (inputFileName.nonEmpty) {
        Some(io.Source.fromFile(inputFileName.get).getLines().mkString("\n"))
      } else if (firrtlSource.nonEmpty) {
        Some(Parser.parse(firrtlSource.get).main)
      } else {
        None
      }
    }
  }

  /** Read all [[annotations.Annotation]] from a file in JSON or YAML format
    *
    * @param filename a JSON or YAML file of [[annotations.Annotation]]
    * @throws annotations.AnnotationFileNotFoundException if the file does not exist
    */
  def readAnnotationsFromFile(filename: String): Seq[Annotation] = {
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
  def addImplicitAnnotationFile(annos: Seq[Annotation]): Seq[Annotation] = annos
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
  def addDefaults(annos: Seq[Annotation]): Seq[Annotation] = { //scalastyle:off cyclomatic.complexity
    var Seq(td, bb, ll, c, tn) = Seq.fill(5)(true) //scalastyle:ignore
    annos.collect{ case a: FirrtlOption => a }.map{
      case _: TargetDirAnnotation    => td = false
      case _: BlackBoxTargetDirAnno  => bb = false
      case _: LogLevelAnnotation     => ll = false
      case _: CompilerNameAnnotation => c  = false
      case _: TopNameAnnotation      => tn = false
      case _ =>
    }

    // [todo] Once the implicit annotation file is deprecated, this complexity decreases
    val default = FirrtlExecutionOptions()
    val name = FirrtlExecutionUtils.topName(annos)
    annos ++
      (if (td)                 Seq(TargetDirAnnotation(default.targetDirName))   else Seq() ) ++
      (if (bb)                 Seq(BlackBoxTargetDirAnno(default.targetDirName)) else Seq() ) ++
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
  def checkAnnotations(annos: Seq[Annotation]): Seq[Annotation] = {
    val Seq(tn, inF, inS, ofpm, outF, td, ll, i, foaf, comp, info) =
      Seq.fill(11)(collection.mutable.ListBuffer[Annotation]())
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
        case _                                      =>           })
    if (tn.isEmpty && inF.isEmpty && inS.isEmpty) {
      throw new FIRRTLException(
        s"""|Unable to determine FIRRTL source to read. None of the following were found:
            |    - a top module name: -tn, --top-name,      TopNameAnnotation
            |    - an input file:     -i,  --input-file,    InputFileAnnotation
            |    - FIRRTL source:          --firrtl-source, FirrtlSourceAnnotation""".stripMargin )}
    if (inF.size + inS.size > 1) {
      throw new FIRRTLException(
        s"""|Multiply defined input FIRRTL sources. More than one of the following was found:
            |    - an input file (${inF.size} times): -i, --input-file, InputFileAnnotation
            |    - FIRRTL source (${inS.size} times):     --firrtl-source, FirrtlSourceAnnotation""".stripMargin )}
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
  import firrtl.Viewer._
  import firrtl.FirrtlViewer._

  @deprecated("Use view[FirrtlExecutionOptions]", "1.2.0")
  def firrtlOptions: FirrtlExecutionOptions = view[FirrtlExecutionOptions].get

  /** Return the name of the top module */
  @deprecated("Use view[FirrtlExecutionOptions].get.topName.get", "1.2.0")
  def topName(): String = firrtlOptions.topName.get

  /** Return the name of the target directory */
  @deprecated("Use view[FirrtlExecutionOptions].get.tagetDirName", "1.2.0")
  def targetDirName(): String = firrtlOptions.targetDirName

  /** Create the specified Target Directory (--target-dir)
    *
    * @return true if successful, false if not
    */
  def makeTargetDir(): Boolean = FileUtils.makeDirectory(firrtlOptions.targetDirName)

  /** Return a file based on targetDir, topName and suffix. Will not add the
    * suffix if the topName already ends with that suffix.
    *
    * @param suffix suffix to add, removes . if present
    * @param fileNameOverride this will override the topName if nonEmpty, when using this targetDir is ignored
    * @return
    */
  def getBuildFileName(suffix: String, fileNameOverride: Option[String] = None): String = {
    makeTargetDir()

    val baseName: String = fileNameOverride.getOrElse(firrtlOptions.topName.get)
    val baseNameIsFullPath: Boolean = baseName.startsWith("./") || baseName.startsWith("/")
    val directoryName: String = if (fileNameOverride.nonEmpty || baseNameIsFullPath) {
      ""
    } else if (firrtlOptions.targetDirName.endsWith("/")) {
      firrtlOptions.targetDirName
    } else {
      firrtlOptions.targetDirName + "/"
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
    * @return a properly constructed input file name
    */
  def getInputFileName(): String = getBuildFileName("fir", firrtlOptions.inputFileNameOverride)

  /** Get the user-specified [[OutputConfig]]
    *
    * @return the output configuration
    */
  def getOutputConfig(): OutputConfig =
    if (firrtlOptions.emitOneFilePerModule) {
      OneFilePerModule(firrtlOptions.targetDirName)
    } else {
      SingleFile(getBuildFileName(firrtlOptions.outputSuffix, firrtlOptions.outputFileNameOverride))
    }

  /** Get the user-specified targetFile assuming [[OutputConfig]] is [[SingleFile]]
    *
    * @return the targetFile as a String
    */
  def getTargetFile(): String = getOutputConfig match {
    case SingleFile(targetFile) => targetFile
    case _                      => throw new Exception("OutputConfig is not SingleFile!") }


  parser.note("Common Options")

  /* [Note 1] Any validation related to these options is removed here. Since
   * we need to check annotations anyway, arguments here are purposefully
   * marked as `unbounded` and no validation checking occurs (except that
   * which is related to ensuring that a command line string is validly
   * converted to some type, e.g., --log-level). All of the actual option
   * validation happens when the annotations are processed in
   * [[FirrtlExecutionUtils.checkAnnotations]]. */
  parser.opt[String]("top-name")
    .abbr("tn")
    .valueName("<top-level-circuit-name>")
    .action( (x, c) => c :+ TopNameAnnotation(x) )
    .unbounded() // See [Note 1]
    .text("This options defines the top level circuit, defaults to dut when possible")

  parser.opt[String]("target-dir")
    .abbr("td")
    .valueName("<target-directory>")
    .action( (x, c) => c ++ Seq(TargetDirAnnotation(x), BlackBoxTargetDirAnno(x)) )
    .unbounded() // See [Note 1]
    .text(s"Work directory for intermediate files/blackboxes, default is ${FirrtlExecutionOptions().targetDirName}")

  parser.opt[String]("log-level")
    .abbr("ll")
    .valueName("<Error|Warn|Info|Debug|Trace>")
    .action( (x, c) => c :+ LogLevelAnnotation(LogLevel(x)) )
    .validate{ x =>
      lazy val msg = s"$x bad value must be one of error|warn|info|debug|trace"
      if (Array("error", "warn", "info", "debug", "trace").contains(x.toLowerCase)) { parser.success      }
      else                                                                          { parser.failure(msg) }}
    .unbounded() // See [Note 1]
    .text(s"Sets the verbosity level of logging, default is ${FirrtlExecutionOptions().globalLogLevel}")

  parser.opt[Seq[String]]("class-log-level")
    .abbr("cll")
    .valueName("<FullClassName:[Error|Warn|Info|Debug|Trace]>[,...]")
    .action( (x, c) => c ++ (x.map { y =>
                               val className :: levelName :: _ = y.split(":").toList
                               val level = LogLevel(levelName)
                               ClassLogLevelAnnotation(className, level) }) )
    .unbounded() // This can actually occur any number of times safely
    .text(s"This defines per-class verbosity of logging")

  parser.opt[Unit]("log-to-file")
    .abbr("ltf")
    .action( (x, c) => c :+ LogToFileAnnotation )
    .unbounded()
    .text(s"default logs to stdout, this flags writes to topName.log or firrtl.log if no topName")

  parser.opt[Unit]("log-class-names")
    .abbr("lcn")
    .action( (x, c) => c :+ LogClassNamesAnnotation )
    .unbounded()
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
    .unbounded() // See [Note 1]
    .text("use this to override the default input file name, default is empty")

  parser.opt[String]("output-file")
    .abbr("o")
    .valueName("<output>")
    .action( (x, c) => c :+ OutputFileAnnotation(x) )
    .unbounded()
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
    .unbounded()
    .action{ (x, c) =>
      val msg = "force-append-anno-file is deprecated\n" + (" "*9) + "(It does not do anything anymore)"
      Driver.dramaticWarning(msg)
      c }

  parser.opt[String]("output-annotation-file")
    .abbr("foaf")
    .valueName ("<output-anno-file>")
    .action( (x, c) => c :+ OutputAnnotationFileAnnotation(x) )
    .unbounded() // See [Note 1]
    .text("use this to set the annotation output file")

  parser.opt[String]("compiler")
    .abbr("X")
    .valueName ("<high|middle|low|verilog|sverilog>")
    .action( (x, c) => c :+ CompilerNameAnnotation(x) )
    .unbounded() // See [Note 1]
    .text(s"compiler to use, default is 'verilog'")

  parser.opt[String]("info-mode")
    .valueName ("<ignore|use|gen|append>")
    .action( (x, c) => c :+ InfoModeAnnotation(x.toLowerCase) )
    .unbounded() // See [Note 1]
    .text(s"specifies the source info handling, default is ${FirrtlExecutionOptions().infoModeName}")

  parser.opt[String]("firrtl-source")
    .valueName ("A FIRRTL string")
    .action( (x, c) => c :+ FirrtlSourceAnnotation(x) )
    .unbounded() // See [Note 1]
    .text(s"A FIRRTL circuit as a string")

  parser.opt[Seq[String]]("custom-transforms")
    .abbr("fct")
    .valueName ("<package>.<class>")
    .validate( x => {
                x.map(x =>
                  try { Class.forName(x).asInstanceOf[Class[_ <: Transform]].newInstance() }
                  catch {
                    case e: ClassNotFoundException => throw new FIRRTLException(
                      s"Unable to locate custom transform $x (did you misspell it?)", e)
                    case e: InstantiationException => throw new FIRRTLException(
                      s"Unable to create instance of Transform $x (is this an anonymous class?)", e)
                    case e: Throwable => throw new FIRRTLException(s"Unknown error when instantiating class $x", e) } )
                parser.success } )
    .action( (x, c) => c ++ x.map(RunFirrtlTransformAnnotation(_)) )
    .unbounded()
    .text("runs these custom transforms during compilation.")

  parser.opt[Unit]("split-modules")
    .abbr("fsm")
    .action( (x, c) => c :+ EmitOneFilePerModuleAnnotation )
    .unbounded()
    .text ("Emit each module to its own file in the target directory.")

  parser.note("FIRRTL Transform Options")
  Seq( transforms.DeadCodeElimination,
       transforms.CheckCombLoops,
       passes.InlineInstances,
       passes.memlib.InferReadWrite,
       passes.memlib.ReplSeqMem,
       passes.clocklist.ClockListTransform )
    .map(_.provideOptions(parser))
}
