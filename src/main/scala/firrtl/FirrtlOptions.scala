// See LICENSE for license details

package firrtl

import firrtl.ir.Circuit
import firrtl.annotations.{NoTargetAnnotation, HasScoptOptions}
import firrtl.transforms.BlackBoxTargetDirAnno
import logger.LogLevel
import scopt.OptionParser

/** Indicates that a subclass is an [[annotations.Annotation]] with an
  * option consummable by [[HasFirrtlExecutionOptions]]
  *
  * This must be mixed into a subclass of [[annotations.Annotation]]
  */
sealed trait FirrtlOption extends HasScoptOptions

/** Holds the name of the top module
  *  - maps to [[FirrtlExecutionOptions.topName]]
  *  - set on the command line with `-tn/--top-name`
  * @param value top module name
  */
case class TopNameAnnotation(topName: Option[String] = None) extends NoTargetAnnotation with FirrtlOption {
  def addOptions(implicit p: OptionParser[AnnotationSeq]): Unit = p.opt[String]("top-name")
    .abbr("tn")
    .valueName("<top-level-circuit-name>")
    .action( (x, c) => c :+ TopNameAnnotation(Some(x)) )
    .unbounded() // See [Note 1]
    .text("This options defines the top level circuit, defaults to dut when possible")
}

object TopNameAnnotation {
  def apply(topName: String): TopNameAnnotation = TopNameAnnotation(Some(topName))
}

/** Holds the name of the target directory
  *  - maps to [[FirrtlExecutionOptions.targetDirName]]
  *  - set with `-td/--target-dir`
  *  - if unset, a [[TargetDirAnnotation]] will be generated with the
  * @param value target directory name
  */
case class TargetDirAnnotation(targetDirName: String = ".") extends NoTargetAnnotation with FirrtlOption {
  def addOptions(implicit p: OptionParser[AnnotationSeq]): Unit = p.opt[String]("target-dir")
    .abbr("td")
    .valueName("<target-directory>")
    .action( (x, c) => c ++ Seq(TargetDirAnnotation(x), BlackBoxTargetDirAnno(x)) )
    .unbounded() // See [Note 1]
    .text(s"Work directory for intermediate files/blackboxes, default is ${FirrtlExecutionOptions().targetDirName}")
}

/** Describes the verbosity of information to log
  *  - maps to [[FirrtlExecutionOptions.globalLogLevel]]
  *  - set with `-ll/--log-level`
  *  - if unset, a [[LogLevelAnnotation]] with the default log level will be emitted
  * @param level the level of logging
  */
case class LogLevelAnnotation(globalLogLevel: LogLevel.Value = LogLevel.None) extends NoTargetAnnotation with FirrtlOption {
  val value = globalLogLevel.toString

  def addOptions(implicit p: OptionParser[AnnotationSeq]): Unit = p.opt[String]("log-level")
    .abbr("ll")
    .valueName("<Error|Warn|Info|Debug|Trace>")
    .action( (x, c) => c :+ LogLevelAnnotation(LogLevel(x)) )
    .validate{ x =>
      lazy val msg = s"$x bad value must be one of error|warn|info|debug|trace"
      if (Array("error", "warn", "info", "debug", "trace").contains(x.toLowerCase)) { p.success      }
      else                                                                          { p.failure(msg) }}
    .unbounded() // See [Note 1]
    .text(s"Sets the verbosity level of logging, default is ${FirrtlExecutionOptions().globalLogLevel}")
}

/** Describes a mapping of a class to a specific log level
  *  - maps to [[FirrtlExecutionOptions.classLogLevels]]
  *  - set with `-cll/--class-log-level`
  * @param name the class name to log
  * @param level the verbosity level
  */
case class ClassLogLevelAnnotation(mapping: Option[(String, LogLevel.Value)] = None) extends NoTargetAnnotation
    with FirrtlOption {
  def addOptions(implicit p: OptionParser[AnnotationSeq]): Unit = p.opt[Seq[String]]("class-log-level")
    .abbr("cll")
    .valueName("<FullClassName:[Error|Warn|Info|Debug|Trace]>[,...]")
    .action( (x, c) => c ++ (x.map { y =>
                               val className :: levelName :: _ = y.split(":").toList
                               val level = LogLevel(levelName)
                               ClassLogLevelAnnotation(Some((className, level))) }) )
    .unbounded() // This can actually occur any number of times safely
    .text(s"This defines per-class verbosity of logging")
}

/** Enables logging to a file (as opposed to STDOUT)
  *  - maps to [[FirrtlExecutionOptions.logToFile]]
  *  - enabled with `-ltf/--log-to-file`
  */
case object LogToFileAnnotation extends NoTargetAnnotation with FirrtlOption {
  def addOptions(implicit p: OptionParser[AnnotationSeq]): Unit = p.opt[Unit]("log-to-file")
    .abbr("ltf")
    .action( (x, c) => c :+ LogToFileAnnotation )
    .unbounded()
    .text(s"default logs to stdout, this flags writes to topName.log or firrtl.log if no topName")
}

/** Enables class names in log output
  *  - maps to [[FirrtlExecutionOptions.logClassNames]]
  *  - enabled with `-lcn/--log-class-names`
  */
case object LogClassNamesAnnotation extends NoTargetAnnotation with FirrtlOption {
  def addOptions(implicit p: OptionParser[AnnotationSeq]): Unit = p.opt[Unit]("log-class-names")
    .abbr("lcn")
    .action( (x, c) => c :+ LogClassNamesAnnotation )
    .unbounded()
    .text(s"shows class names and log level in logging output, useful for target --class-log-level")
}

/** Additional arguments
  *  - maps to [[FirrtlExecutionOptions.programArgs]]
  *  - set with any trailing option on the command line
  * @param value one [[scala.String]] argument
  */
case class ProgramArgsAnnotation(value: Option[String] = None) extends NoTargetAnnotation with FirrtlOption {
  def addOptions(implicit p: OptionParser[AnnotationSeq]): Unit = p.arg[String]("<arg>...")
    .unbounded()
    .optional()
    .action( (x, c) => c :+ ProgramArgsAnnotation(Some(x)) )
    .text("optional unbounded args")
}

/** An explicit input FIRRTL file to read
  *  - maps to [[FirrtlExecutionOptions.inputFileNameOverride]]
  *  - set with `-i/--input-file`
  *  - If unset, an [[InputFileAnnotation]] with the default input file __will not be generated__
  * @param value input filename
  */
case class InputFileAnnotation(value: Option[String] = None) extends NoTargetAnnotation with FirrtlOption {
  def addOptions(implicit p: OptionParser[AnnotationSeq]): Unit = p.opt[String]("input-file")
    .abbr("i")
    .valueName ("<firrtl-source>")
    .action( (x, c) => c :+ InputFileAnnotation(Some(x)) )
    .unbounded() // See [Note 1]
    .text("use this to override the default input file name, default is empty")
}

object InputFileAnnotation {
  def apply(value: String): InputFileAnnotation = InputFileAnnotation(Some(value))
}

/** An explicit output file the emitter will write to
  *   - maps to [[FirrtlExecutionOptions.outputFileNameOverride]]
  *   - set with `-o/--output-file`
  *  @param value output filename
  */
case class OutputFileAnnotation(value: Option[String] = None) extends NoTargetAnnotation with FirrtlOption {
  def addOptions(implicit p: OptionParser[AnnotationSeq]): Unit = p.opt[String]("output-file")
    .abbr("o")
    .valueName("<output>")
    .action( (x, c) => c :+ OutputFileAnnotation(Some(x)) )
    .unbounded()
    .text("use this to override the default output file name, default is empty")
}

object OutputFileAnnotation {
  def apply(value: String): OutputFileAnnotation = OutputFileAnnotation(Some(value))
}

/** An explicit output _annotation_ file to write to
  *  - maps to [[FirrtlExecutionOptions.outputAnnotationFileName]]
  *  - set with `-foaf/--output-annotation-file`
  * @param value output annotation filename
  */
case class OutputAnnotationFileAnnotation(value: Option[String] = None) extends NoTargetAnnotation with FirrtlOption {
  def addOptions(implicit p: OptionParser[AnnotationSeq]): Unit = p.opt[String]("output-annotation-file")
    .abbr("foaf")
    .valueName ("<output-anno-file>")
    .action( (x, c) => c :+ OutputAnnotationFileAnnotation(Some(x)) )
    .unbounded() // See [Note 1]
    .text("use this to set the annotation output file")
}

object OutputAnnotationFileAnnotation {
  def apply(value: String): OutputAnnotationFileAnnotation = OutputAnnotationFileAnnotation(Some(value))
}

/** Sets the info mode style
  *  - maps to [[FirrtlExecutionOptions.infoModeName]]
  *  - set with `--info-mode`
  * @param value info mode name
  */
case class InfoModeAnnotation(value: String = "append") extends NoTargetAnnotation with FirrtlOption {
  def addOptions(implicit p: OptionParser[AnnotationSeq]): Unit = p.opt[String]("info-mode")
    .valueName ("<ignore|use|gen|append>")
    .action( (x, c) => c :+ InfoModeAnnotation(x.toLowerCase) )
    .unbounded() // See [Note 1]
    .text(s"specifies the source info handling, default is ${FirrtlExecutionOptions().infoModeName}")
}

/** Holds a [[scala.String]] containing FIRRTL source to read as input
  *  - maps to [[FirrtlExecutionOptions.firrtlSource]]
  *  - set with `--firrtl-source`
  * @param value FIRRTL source as a [[scala.String]]
  */
case class FirrtlSourceAnnotation(value: Option[String] = None) extends NoTargetAnnotation with FirrtlOption {
  def addOptions(implicit p: OptionParser[AnnotationSeq]): Unit = p.opt[String]("firrtl-source")
    .valueName ("A FIRRTL string")
    .action( (x, c) => c :+ FirrtlSourceAnnotation(Some(x)) )
    .unbounded() // See [Note 1]
    .text(s"A FIRRTL circuit as a string")
}

object FirrtlSourceAnnotation {
  def apply(value: String): FirrtlSourceAnnotation = FirrtlSourceAnnotation(Some(value))
}

/**  Indicates that an emitted circuit (FIRRTL, Verilog, etc.) will be one file per module
  *   - maps to [[FirrtlExecutionOptions.emitOneFilePerModule]]
  *   - set with `--split-modules`
  */
case object EmitOneFilePerModuleAnnotation extends NoTargetAnnotation with FirrtlOption {
  def addOptions(implicit p: OptionParser[AnnotationSeq]): Unit = p.opt[Unit]("split-modules")
    .abbr("fsm")
    .action( (x, c) => c :+ EmitOneFilePerModuleAnnotation )
    .unbounded()
    .text ("Emit each module to its own file in the target directory.")
}

/** Holds a filename containing one or more [[annotations.Annotation]] to be read
  *  - this is not stored in [[FirrtlExecutionOptions]]
  *  - set with `-faf/--annotation-file`
  * @param value input annotation filename
  */
case class InputAnnotationFileAnnotation(value: Option[String] = None) extends NoTargetAnnotation with FirrtlOption {
  def addOptions(implicit p: OptionParser[AnnotationSeq]): Unit = p.opt[String]("annotation-file")
    .abbr("faf")
    .unbounded()
    .valueName("<input-anno-file>")
    .action( (x, c) => c :+ InputAnnotationFileAnnotation(x) )
    .text("Used to specify annotation file")
}

object InputAnnotationFileAnnotation {
  def apply(value: String): InputAnnotationFileAnnotation = InputAnnotationFileAnnotation(Some(value))
}

/** Holds the name of the compiler to run
  *  - maps to [[FirrtlExecutionOptions.compilerName]]
  *  - set with `-X/--compiler`
  *  - If unset, a [[CompilerNameAnnotation]] with the default compiler ("verilog") __will be generated__
  * @param value compiler name
  */
case class CompilerNameAnnotation(value: String = "verilog") extends NoTargetAnnotation with FirrtlOption {
  def addOptions(implicit p: OptionParser[AnnotationSeq]): Unit = p.opt[String]("compiler")
    .abbr("X")
    .valueName ("<high|middle|low|verilog|sverilog>")
    .action( (x, c) => c :+ CompilerNameAnnotation(x) )
    .unbounded() // See [Note 1]
    .text(s"compiler to use, default is 'verilog'")
}

/** Holds the unambiguous class name of a [[Transform]] to run
  *  - will be append to [[FirrtlExecutionOptions.customTransforms]]
  *  - set with `-fct/--custom-transforms`
  * @param value the full class name of the transform
  */
case class RunFirrtlTransformAnnotation(value: Option[String] = None) extends NoTargetAnnotation with FirrtlOption {
  def addOptions(implicit p: OptionParser[AnnotationSeq]): Unit = p.opt[Seq[String]]("custom-transforms")
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
                p.success } )
    .action( (x, c) => c ++ x.map(xx => RunFirrtlTransformAnnotation(Some(xx))) )
    .unbounded()
    .text("runs these custom transforms during compilation.")
}

object RunFirrtlTransformAnnotation {
  def apply(value: String): RunFirrtlTransformAnnotation = RunFirrtlTransformAnnotation(Some(value))
}

/** Holds a FIRRTL [[Circuit]]
  * @param value a circuit
  */
case class FirrtlCircuitAnnotation(value: Option[Circuit] = None) extends NoTargetAnnotation with FirrtlOption {
  def addOptions(implicit p: OptionParser[AnnotationSeq]): Unit = Unit
}
