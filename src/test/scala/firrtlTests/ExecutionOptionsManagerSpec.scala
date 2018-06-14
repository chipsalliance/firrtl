// See LICENSE for license details.

package firrtlTests

import firrtl._
import firrtl.options.ExecutionOptionsManager
import firrtl.annotations.Annotation
import logger.LogLevel
import org.scalatest.{Matchers, FreeSpec}

trait HasDuplicateLongOption {
  self: ExecutionOptionsManager =>
  parser.opt[String]("top-name")
    .abbr("herp")
}

trait HasDuplicateShortOption {
  self: ExecutionOptionsManager =>
  parser.opt[String]("very-unique-long-option")
    .abbr("tn")
}

class ExecutionOptionsManagerSpec extends FreeSpec with Matchers {
  val args = Array("--top-name", "spoon",
                   "--input-file", "fork")

  "ExecutionOptionsManager with HasFirrtlExecutionOptions" - {
    "when constructed sanely" - {
      "should have default FIRRTL options" in {
        val f = (new ExecutionOptionsManager("test", Array("--top-name", "null")) with HasFirrtlExecutionOptions)
          .firrtlOptions
        // This is explicitly enumerated (as opposed to being compared to
        // FirrtlExecutionOptions()) to catch changes in
        // FirrtlExecutionOptions that a developer may make, requiring
        // that they also change this test.
        f.targetDirName should be (".")
        f.globalLogLevel should be (LogLevel.None)
        f.logToFile should be (false)
        f.outputFileNameOverride should be (None)
        f.outputAnnotationFileName should be (None)
        f.compilerName should be ("verilog")
        f.infoModeName should be ("append")
        f.customTransforms should be (List.empty)
        f.firrtlSource should be (None)
        f.annotations should be (
          List(firrtl.transforms.BlackBoxTargetDirAnno("."),
               TopNameAnnotation("null"),
               TargetDirAnnotation("."),
               LogLevelAnnotation(LogLevel.None),
               EmitterAnnotation(classOf[VerilogEmitter]),
               CompilerNameAnnotation("verilog") ))
        f.emitOneFilePerModule should be (false)
      }
      "should be able to override the Top Module Name" in {
        val f = (new ExecutionOptionsManager("test",
                                             Array("--top-name", "dog")) with HasFirrtlExecutionOptions)
          .firrtlOptions
        f shouldBe a [FirrtlExecutionOptions]
        f.topName should be (Some("dog"))
      }
      "should be able to set different Top Module Name and Input File" in {
        val f = (new ExecutionOptionsManager("test", args) with HasFirrtlExecutionOptions)
          .firrtlOptions
        f.inputFileNameOverride should be (Some("fork"))
        f.topName should be (Some("spoon"))
      }
    }
    "when constructed insanely" - {
      def shouldExceptOnOptionsOrAnnotations(name: String, args: Array[String], annotations: Seq[Annotation]) {
        name - {
          "via command line options" in {
            val m = new ExecutionOptionsManager("t", Array("-tn", "foo") ++ args) with HasFirrtlExecutionOptions
            a [FIRRTLException] should be thrownBy (m.firrtlOptions) }
          "via annotations" in {
            val m = new ExecutionOptionsManager("t", Array("-tn", "foo"), annotations) with HasFirrtlExecutionOptions
            a [FIRRTLException] should be thrownBy (m.firrtlOptions) }}}

      shouldExceptOnOptionsOrAnnotations("should fail when --input-file and --firrtl-source are both specified",
                                         Array("--input-file", "foo", "--firrtl-source", "Circuit:"),
                                         Seq(InputFileAnnotation("bar"), FirrtlSourceAnnotation("Circuit")))
      shouldExceptOnOptionsOrAnnotations("should fail with multiple input files",
                                         Array("--input-file", "foo", "-i", "bar"),
                                         Seq(InputFileAnnotation("baz"), InputFileAnnotation("qux")))
      shouldExceptOnOptionsOrAnnotations("should fail with multiple firrtl sources",
                                         Array("--firrtl-source", "Circuit:", "--firrtl-source", "Circuit:"),
                                         Seq(FirrtlSourceAnnotation("Circuit"), FirrtlSourceAnnotation("Circuit")))
      shouldExceptOnOptionsOrAnnotations("should fail with multiple output files",
                                         Array("--output-file", "foo", "--output-file", "bar"),
                                         Seq(OutputFileAnnotation("foo"), OutputFileAnnotation("bar")))
      shouldExceptOnOptionsOrAnnotations("should fail when --output-file and --split-modules are both specified",
                                         Array("--output-file", "o", "--split-modules"),
                                         Seq(OutputFileAnnotation("foo"), EmitOneFilePerModuleAnnotation))
      shouldExceptOnOptionsOrAnnotations("should fail with multiple target directories",
                                         Array("-td", "foo", "--target-dir", "bar"),
                                         Seq(TargetDirAnnotation("foo"), TargetDirAnnotation("bar")))
      shouldExceptOnOptionsOrAnnotations("should fail with multiple top names",
                                         Array("-tn", "bar"),
                                         Seq(TopNameAnnotation("foo"), TopNameAnnotation("foo")))
      shouldExceptOnOptionsOrAnnotations("should fail with multiple log levels",
                                         Array("--log-level", "Info", "-ll", "debug"),
                                         Seq(LogLevelAnnotation(LogLevel.Info), LogLevelAnnotation(LogLevel.Info)))
      shouldExceptOnOptionsOrAnnotations("should fail with multiple output annotation files",
                                         Array("-foaf", "foo", "--output-annotation-file", "bar"),
                                         Seq(OutputAnnotationFileAnnotation("foo"),
                                             OutputAnnotationFileAnnotation("bar") ))
      shouldExceptOnOptionsOrAnnotations("should fail on an unknown compiler",
                                         Array("-X", "VHDL"),
                                         Seq(CompilerNameAnnotation("VHDL")))
      shouldExceptOnOptionsOrAnnotations("should fail on multiple compilers",
                                         Array("-X", "verilog", "--compiler", "sverilog"),
                                         Seq(CompilerNameAnnotation("Verilog"), CompilerNameAnnotation("SVerilog")))
      shouldExceptOnOptionsOrAnnotations("should fail on invalid info mode",
                                         Array("--info-mode", "foo"),
                                         Seq(InfoModeAnnotation("foo")))
      shouldExceptOnOptionsOrAnnotations("should fail on multiple info modes",
                                         Array("--info-mode", "use", "--info-mode", "gen"),
                                         Seq(InfoModeAnnotation("use"), InfoModeAnnotation("gen")))


      "should fail when input file is not explicit or implicit" in {
        val m = new ExecutionOptionsManager("test", Array[String]()) with HasFirrtlExecutionOptions
        a [FIRRTLException] should be thrownBy (m.firrtlOptions)
      }
      "should fail on duplicate long options" in {
        val m = new ExecutionOptionsManager("test", args) with HasFirrtlExecutionOptions with HasDuplicateLongOption
        a [FIRRTLException] should be thrownBy (m.firrtlOptions)
      }
      "should fail on duplicate short options" in {
        val m = new ExecutionOptionsManager("test", args) with HasFirrtlExecutionOptions with HasDuplicateShortOption
        a [FIRRTLException] should be thrownBy (m.firrtlOptions)
      }
    }
  }
}
