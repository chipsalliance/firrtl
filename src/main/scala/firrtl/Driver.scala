// See License

package firrtl

import java.io.FileNotFoundException

import scopt.OptionParser

import scala.io.Source
import scala.collection.mutable
import Annotations._

import Parser.{InfoMode, IgnoreInfo, UseInfo, GenInfo, AppendInfo}
import scala.collection._

/**
  * The driver provides methods to access the firrtl compiler.
  * Invoke the compiler with either a FirrtlExecutionOption
  * @example
  *          {{{
  *          val options = new FirrtlExecutionOptions()
  *          firrtlOptions.topName = "Dummy"
  *          firrtlOptions.compilerName = "verilog"
  *          firrtl.Driver.execute(options)
  *          }}}
  *  or a series of command line arguments
  * @example
  *          {{{
  *          firrtl.Driver.execute(Array("--top-name Dummy --compiler verilog".split(" +"))
  *          }}}
  * each approach has its own endearing aspects
  * @see  firrtlTests.DriverSpec.scala in the test directory for a lot more examples
  */

/**
  * Use this trait to define an options class that can add its private command line options to a externally
  * declared parser
  */
trait ComposableOptions {
  def addOptions(parser: OptionParser[Unit]): Unit
}

/**
  * Most of the chisel toolchain components require a topName which defines a circuit or a device under test.
  * Much of the work that is done takes place in a directory.
  * It would be simplest to require topName to be defined but in practice it is preferred to defer this.
  * For example, in chisel, by deferring this it is possible for the execute there to first elaborate the
  * circuit and then set the topName from that if it has not already been set.
  */
class CommonOptions extends ComposableOptions {
  //TODO: Figure out how to declare the following as parameters without having them being val when jar'ified
  var topName:       String = ""
  var targetDirName: String = "test_run_dir"

  def addOptions(parser: OptionParser[Unit]): Unit = {
    parser.note("common options")

    parser.opt[String]("top-name").abbr("tn").valueName("<top-level-circuit-name>").foreach { x =>
      topName = x
    }.text("This options defines the top level circuit, defaults to dut when possible")
    parser.opt[String]("target-dir").abbr("td").valueName("<target-directory>").foreach { x =>
      targetDirName = x
    }.text("This options defines a work directory for intermediate files")

    parser.help("help").text("prints this usage text")
  }

  /**
    * make sure that all levels of targetDirName exist
    *
    * @return true if directory exists
    */
  def makeTargetDir(): Boolean = {
    FileUtils.makeDirectory(targetDirName)
  }

  /**
    * return a file based on targetDir, topName and suffix
    * Will not add the suffix if the topName already ends with that suffix
    *
    * @param suffix suffix to add, removes . if present
    * @return
    */
  def getBuildFileName(suffix: String): String = {
    makeTargetDir()
    val normalizedSuffix = if(suffix.startsWith(".")) suffix.drop(1) else suffix

    s"$targetDirName/$topName.${if(topName.endsWith(normalizedSuffix)) "" else normalizedSuffix}"
  }
  /**
    * Use this to set the topName in cases when it has been discovered via circuit elaboration
    * This will not set the topName if it has already been set, presumably by users wishing to override the default
    * @param newTopName topName to set
    */
  def setTopNameIfUnset(newTopName: String) {
    if(topName.isEmpty) {
      topName = newTopName
    }
  }
}
/**
  * The options that firrtl supports in callable component sense
 *
  * @param inputFileNameOverride  default is targetDir/topName.fir
  * @param outputFileNameOverride default is targetDir/topName.v
  * @param compilerName           which compiler to use
  * @param infoModeName           use specific annotations
  * @param inferRW                use specific annotations
  * @param inLine                 in line modules
  */
class FirrtlExecutionOptions(
                              var inputFileNameOverride:  String = "",
                              var outputFileNameOverride: String = "",
                              var compilerName:           String = "verilog",
                              var infoModeName:           String = "use",
                              var inferRW:                Seq[String] = Seq(),
                              var inLine:                 Seq[String] = Seq(),
                              var firrtlSource:           Option[String] = None
                            ) extends CommonOptions
{
  val annotations = mutable.ArrayBuffer[Annotation]()


  override def addOptions(parser: OptionParser[Unit]): Unit = {
    super.addOptions(parser)

    import parser._

    note("firrtl options")

    opt[String]("input-file").abbr("fif").valueName ("<firrtl-source>").foreach { x =>
      inputFileNameOverride = x
    } text {
      "use this to override the top name default"
    }

    opt[String]("output-file").abbr("fof").valueName ("<output>").foreach { x =>
      outputFileNameOverride = x
    } text {
      "use this to override the default name"
    }

    opt[String]("compiler").abbr("fc").valueName ("<high|low|verilog>").foreach { x =>
      compilerName = x
    }.validate { x =>
      if (Array("high", "low", "verilog").contains(x.toLowerCase)) success
      else failure(s"$x not a legal compiler")
    }.text {
      "compiler to use, default is verilog"
    }

    opt[String]("info-mode").valueName ("<ignore|use|gen|append>").foreach { x =>
      infoModeName = x.toLowerCase
    }.validate { x =>
      if (Array("ignore", "use", "gen", "append").contains(x.toLowerCase)) success
      else failure(s"$x not a legal compiler")
    }.text {
      "specifies the source info handling"
    }

    opt[Seq[String]]("in-line").abbr("fil").valueName ("<circuit>[.<module>][.<instance>][,..],").foreach { x =>
      inLine = x
      annotations ++= x.map { value =>
        value.split('.') match {
          case Array(circuit) =>
            passes.InlineAnnotation(CircuitName(circuit), TransID(0))
          case Array(circuit, module) =>
            passes.InlineAnnotation(ModuleName(module, CircuitName(circuit)), TransID(0))
          case Array(circuit, module, inst) =>
            passes.InlineAnnotation(ComponentName(inst, ModuleName(module, CircuitName(circuit))), TransID(0))
        }
      }
    }.text {
      """Inline a module (e.g. "MyModule") or instance (e.g. "MyModule.myinstance"""
    }

    opt[Seq[String]]("infer-rw").abbr("firw").valueName ("<ignore|use|gen|append>[,...]").foreach { x =>
      inferRW = x
      annotations ++= x.map { value => passes.InferReadWriteAnnotation(value, TransID(-1))}
    }.text {
      "Enable readwrite port inference for the target circuit, for multiples separate with commas and no spaces"
    }

    opt[Seq[String]]("repl-seq-mem").abbr("frsq").valueName ("-c:<circuit>:-i:<filename>:-o:<filename>[,...]").foreach { x =>
      inferRW = x
      annotations ++= x.map { value => passes.ReplSeqMemAnnotation(value, TransID(-2))}
    }.text {
      "Replace sequential memories with blackboxes + configuration file"
    }
    note("Input configuration file optional")
    note("Note: sub-arguments to --replSeqMem should be delimited by : and not white space!")
    note("for multiples separate with commas and no spaces")
    note("")
  }

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

  def inputFileName: String = {
    if(inputFileNameOverride.isEmpty) {
      getBuildFileName("fir")
    }
    else {
      if(inputFileNameOverride.startsWith("./") ||
        inputFileNameOverride.startsWith("/")) {
        inputFileNameOverride
      }
      else {
        s"$targetDirName/$inputFileNameOverride"
      }
    }
  }
  def outputFileName: String = {
    def suffix: String = {
      compilerName match {
        case "verilog"   => "v"
        case "low"       => "lo.fir"
        case "high"      => "hi.fir"
        case _ =>
          throw new Exception(s"Illegal compiler name $compilerName")
      }
    }
    if(outputFileNameOverride.isEmpty) {
      getBuildFileName(suffix)
    }
    else {
      if(outputFileNameOverride.startsWith("./") ||
        outputFileNameOverride.startsWith("/")) {
        outputFileNameOverride
      }
      else {
        s"$targetDirName/$outputFileNameOverride"
      }
    }
  }
}

trait FirrtlExecutionResult

/**
  * Indicates a successful execution of the firrtl compiler, returning the compiled result and
  * the type of compile
  * @param emitType  The name of the compiler used, currently "high", "low", or "verilog"
  * @param emitted   The text result of the compilation, could be verilog or firrtl text.
  */
case class FirrtlExecutionSuccess(
                                emitType: String,
                                emitted:  String
                                ) extends FirrtlExecutionResult

/**
  * The firrtl compilation failed.
  * @param message  Some kind of hint as to what went wrong.
  */
case class FirrtlExecutionFailure(message: String) extends FirrtlExecutionResult

object Driver {
  // Compiles circuit. First parses a circuit from an input file,
  //  executes all compiler passes, and writes result to an output
  //  file.
  def compile(
               input: String,
               output: String,
               compiler: Compiler,
               infoMode: InfoMode = IgnoreInfo,
               annotations: AnnotationMap = new AnnotationMap(Seq.empty)): String = {
    val parsedInput = Parser.parse(Source.fromFile(input).getLines(), infoMode)
    val outputBuffer = new java.io.CharArrayWriter
    compiler.compile(parsedInput, annotations, outputBuffer)

    val outputFile = new java.io.PrintWriter(output)
    val outputString = outputBuffer.toString
    outputFile.write(outputString)
    outputFile.close()
    outputString
  }

  def dramaticError(message: String): Unit = {
    println(Console.RED + "-"*78)
    println(s"Error: $message")
    println("-"*78 + Console.RESET)
  }

  /**
    * Run the firrtl compiler using the provided option
    * @param firrtlConfig the desired flags to the compiler
    * @return a FirrtlExectionResult indicating success or failure, provide access to emitted data on success
    *         for downstream tools as desired
    */
  def execute(firrtlConfig: FirrtlExecutionOptions): FirrtlExecutionResult = {
    val firrtlSource = firrtlConfig.firrtlSource match {
      case Some(text) => text.split("\n").toIterator
      case None       =>
        if(firrtlConfig.topName.isEmpty && firrtlConfig.inputFileNameOverride.isEmpty) {
          val message = "either top-name or input-file-override must be set"
          dramaticError(message)
          return FirrtlExecutionFailure(message)
        }
        try {
          io.Source.fromFile(firrtlConfig.inputFileName).getLines()
        }
        catch {
          case e: FileNotFoundException =>
            val message = s"Input file ${firrtlConfig.inputFileName} not found"
            dramaticError(message)
            return FirrtlExecutionFailure(message)
          }
        }

    val parsedInput = Parser.parse(firrtlSource, firrtlConfig.infoMode)
    val outputBuffer = new java.io.CharArrayWriter
    firrtlConfig.compiler.compile(parsedInput, new AnnotationMap(Seq.empty), outputBuffer)

    val outputFile = new java.io.PrintWriter(firrtlConfig.outputFileName)
    val outputString = outputBuffer.toString
    outputFile.write(outputString)
    outputFile.close()

    FirrtlExecutionSuccess(firrtlConfig.compilerName, outputBuffer.toString)
  }

  /**
    * this is a wrapper for execute that builds the options from a standard command line args,
    * for example, like strings passed to main()
    * @param args  an Array of string s containing legal arguments
    * @return
    */
  def execute(args: Array[String]): FirrtlExecutionResult = {
    val firrtlOptions = new FirrtlExecutionOptions()

    val parser = new OptionParser[Unit]("firrtl") {}
    firrtlOptions.addOptions(parser)

    parser.parse(args) match {
      case true =>
        execute(firrtlOptions) match {
          case success: FirrtlExecutionSuccess =>
            success
          case failure: FirrtlExecutionFailure =>
            parser.showUsageAsError()
            failure
          case result =>
            throw new Exception(s"Error: Unknown Firrtl Execution result $result")
        }
      case _ =>
        FirrtlExecutionFailure("")
    }
  }

  def main(args: Array[String]): Unit = {
    execute(args)
  }
}

object FileUtils {
  /**
    * recursive create directory and all parents
    *
    * @param directoryName a directory string with one or more levels
    * @return
    */
  def makeDirectory(directoryName: String): Boolean = {
    val dirFile = new java.io.File(directoryName)
    if(dirFile.exists()) {
      if(dirFile.isDirectory) {
        true
      }
      else {
        false
      }
    }
    else {
      dirFile.mkdirs()
    }
  }

  /**
    * recursively delete all directories in a relative path
    * DO NOT DELETE absolute paths
    *
    * @param directoryPathName a directory hierarchy to delete
    */
  def deleteDirectoryHierarchy(directoryPathName: String): Unit = {
    if(directoryPathName.isEmpty || directoryPathName.startsWith("/")) {
      // don't delete absolute path
    }
    else {
      val directory = new java.io.File(directoryPathName)
      if(directory.isDirectory) {
        directory.delete()
        val directories = directoryPathName.split("/+").reverse.tail
        if (directories.nonEmpty) {
          deleteDirectoryHierarchy(directories.reverse.mkString("/"))
        }
      }
    }
  }
}

object OldDriver {
  /**
    * Implements the default Firrtl compilers and an inlining pass.
    *
    * Arguments specify the compiler, input file, output file, and
    * optionally the module/instances to inline.
    */
  @deprecated("This is on it's way out, use Driver intstead", "firrtl")
  def main(args: Array[String]) = {
    val usage =
      """
Usage: firrtl -i <input_file> -o <output_file> -X <compiler> [options]
     sbt "run-main firrtl.Driver -i <input_file> -o <output_file> -X <compiler> [options]"

Required Arguments:
-i <filename>         Specify the input *.fir file
-o <filename>         Specify the output file
-X <compiler>         Specify the target compiler
                      Currently supported: high low verilog

Optional Arguments:
  --info-mode <mode>             Specify Info Mode
                                 Supported modes: ignore, use, gen, append
  --inferRW <circuit>            Enable readwrite port inference for the target circuit
  --inline <module>|<instance>   Inline a module (e.g. "MyModule") or instance (e.g. "MyModule.myinstance")

  --replSeqMem -c:<circuit>:-i:<filename>:-o:<filename>
  *** Replace sequential memories with blackboxes + configuration file
  *** Input configuration file optional
  *** Note: sub-arguments to --replSeqMem should be delimited by : and not white space!

  [--help|-h]                    Print usage string
"""

    def handleInlineOption(value: String): Annotation =
      value.split('.') match {
        case Array(circuit) =>
          passes.InlineAnnotation(CircuitName(circuit), TransID(0))
        case Array(circuit, module) =>
          passes.InlineAnnotation(ModuleName(module, CircuitName(circuit)), TransID(0))
        case Array(circuit, module, inst) =>
          passes.InlineAnnotation(ComponentName(inst, ModuleName(module, CircuitName(circuit))), TransID(0))
        case _ => throw new Exception(s"Bad inline instance/module name: $value" + usage)
      }

    def handleInferRWOption(value: String) = 
      passes.InferReadWriteAnnotation(value, TransID(-1))

    def handleReplSeqMem(value: String) = 
      passes.ReplSeqMemAnnotation(value, TransID(-2))

    run(args: Array[String],
      Map( "high" -> new HighFirrtlCompiler(),
        "low" -> new LowFirrtlCompiler(),
        "verilog" -> new VerilogCompiler()),
      Map("--inline" -> handleInlineOption _,
          "--inferRW" -> handleInferRWOption _,
          "--replSeqMem" -> handleReplSeqMem _),
      usage
    )
  }


  // Compiles circuit. First parses a circuit from an input file,
  //  executes all compiler passes, and writes result to an output
  //  file.
  def compile(
               input: String,
               output: String,
               compiler: Compiler,
               infoMode: InfoMode = IgnoreInfo,
               annotations: AnnotationMap = new AnnotationMap(Seq.empty)) = {
    val parsedInput = Parser.parse(Source.fromFile(input).getLines, infoMode)
    val outputBuffer = new java.io.CharArrayWriter
    compiler.compile(parsedInput, annotations, outputBuffer)

    val outputFile = new java.io.PrintWriter(output)
    outputFile.write(outputBuffer.toString)
    outputFile.close()
  }

  /**
   * Runs a Firrtl compiler.
   *
   * @param args list of commandline arguments
   * @param compilers mapping a compiler name to a compiler
   * @param customOptions mapping a custom option name to a function that returns an annotation
   * @param usage describes the commandline API
   */
  def run(args: Array[String], compilers: Map[String,Compiler], customOptions: Map[String, String=>Annotation], usage: String) = {
    /**
     * Keys commandline values specified by user in OptionMap
     */
    sealed trait CompilerOption
    case object InputFileName extends CompilerOption
    case object OutputFileName extends CompilerOption
    case object CompilerName extends CompilerOption
    case object InfoModeOption extends CompilerOption
    /**
     * Maps compiler option to user-specified value
     */
    type OptionMap = Map[CompilerOption, String]

    /**
     * Populated by custom annotations returned from corresponding function
     * held in customOptions
     */
    val annotations = mutable.ArrayBuffer[Annotation]()
    def nextOption(map: OptionMap, list: List[String]): OptionMap = {
      list match {
        case Nil => map
        case "-i" :: value :: tail =>
          nextOption(map + (InputFileName -> value), tail)
        case "-o" :: value :: tail =>
          nextOption(map + (OutputFileName -> value), tail)
        case "-X" :: value :: tail =>
          nextOption(map + (CompilerName -> value), tail)
        case "--info-mode" :: value :: tail =>
          nextOption(map + (InfoModeOption -> value), tail)
        case flag :: value :: tail if customOptions.contains(flag) =>
          annotations += customOptions(flag)(value)
          nextOption(map, tail)
        case ("-h" | "--help") :: tail => println(usage); sys.exit(0)
        case option :: tail =>
          throw new Exception("Unknown option " + option + usage)
      }
    }

    val arglist = args.toList
    val options = nextOption(Map[CompilerOption, String](), arglist)

    // Get input circuit/output filenames
    val input = options.getOrElse(InputFileName, throw new Exception("No input file provided!" + usage))
    val output = options.getOrElse(OutputFileName, throw new Exception("No output file provided!" + usage))

    val infoMode = options.get(InfoModeOption) match {
      case (Some("append") | None) => AppendInfo(input)
      case Some("use") => UseInfo
      case Some("ignore") => IgnoreInfo
      case Some("gen") => GenInfo(input)
      case Some(other) => throw new Exception("Unknown info mode option: " + other + usage)
    }

    // Execute selected compiler - error if not recognized compiler
    options.get(CompilerName) match {
      case Some(name) =>
        compilers.get(name) match {
          case Some(compiler) => compile(input, output, compiler, infoMode, new AnnotationMap(annotations.toSeq))
          case None => throw new Exception("Unknown compiler option: " + name + usage)
        }
      case None => throw new Exception("No specified compiler option." + usage)
    }
  }
}
