// See LICENSE for license details.

package firrtlTests

import firrtl._
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

  "ExecutionOptionsManager with HasFirrtlOptions" - {
    "when constructed sanely" - {
      "should have default FIRRTL options" in {
        val f = (new ExecutionOptionsManager("test", Array("--top-name", "null")) with HasFirrtlOptions)
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
                                             Array("--top-name", "dog")) with HasFirrtlOptions)
          .firrtlOptions
        f shouldBe a [FirrtlExecutionOptions]
        f.topName should be (Some("dog"))
      }
      "should be able to set different Top Module Name and Input File" in {
        val f = (new ExecutionOptionsManager("test", args) with HasFirrtlOptions)
          .firrtlOptions

        f.inputFileNameOverride should be (Some("fork"))
        f.topName should be (Some("spoon"))
      }
    }
    "when constructed insanely" - {
      "should fail when input file is not explicit or implicit" in {
        val m = new ExecutionOptionsManager("test", Array[String]()) with HasFirrtlOptions
        a [FIRRTLException] should be thrownBy (m.options)
      }
      "should fail when --input-file and --firrtl-source are both specified" in {
        val badArgs = args ++ Array("--firrtl-sourc", "Circuit:")
        val m = new ExecutionOptionsManager("test", badArgs) with HasFirrtlOptions
        a [FIRRTLException] should be thrownBy (m.options)
      }
      "should fail when --output-file and --split-modules are both specified" in {
        val badArgs = args ++ Array("--output-file", "o", "--split-modules")
        val m = new ExecutionOptionsManager("test", badArgs) with HasFirrtlOptions
        a [FIRRTLException] should be thrownBy (m.options)
      }
      "should fail on duplicate long options" in {
        val m = new ExecutionOptionsManager("test", args) with HasFirrtlOptions with HasDuplicateLongOption
        a [FIRRTLException] should be thrownBy (m.options)
      }
      "should fail on duplicate short options" in {
        val m = new ExecutionOptionsManager("test", args) with HasFirrtlOptions with HasDuplicateShortOption
        a [FIRRTLException] should be thrownBy (m.options)
      }
    }
  }
}
