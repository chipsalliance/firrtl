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
  "ExecutionOptionsManager is a container for all command line options" - {
    "It has default FIRRTL Options" in {
      val f = (new ExecutionOptionsManager("test", Array("--top-name", "null")) with HasFirrtlOptions)
        .firrtlOptions
      // This is explicitly enumerated (as opposed to being compared to
      // FirrtlOptions) is to catch changes in FirrtlOptions that a
      // developer may make, requiring that they also change this test.
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
      f.annotationFileNames should be (List.empty)
    }
    "But can override defaults like this" in {
      val manager = new ExecutionOptionsManager(
        "test",
        Array("--top-name", "dog")) with HasFirrtlOptions
      manager.firrtlOptions shouldBe a [FirrtlOptions]
      manager.topName should be (Some("dog"))
      manager.firrtlOptions.topName should be (Some("dog"))
    }
    "multiple composable blocks should be separable" in {
      val manager = new ExecutionOptionsManager(
        "test",
        Array("--top-name", "spoon",
              "--input-file", "fork") ) with HasFirrtlOptions

      manager.firrtlOptions.inputFileNameOverride should be (Some("fork"))
      manager.firrtlOptions.topName should be (Some("spoon"))
    }
    "duplicate long options should be detected" in {
      val manager = new ExecutionOptionsManager(
        "test",
        Array("--top-name", "spoon",
              "--input-file", "fork") ) with HasFirrtlOptions with HasDuplicateLongOption
      a [FIRRTLException] should be thrownBy (manager.options)
    }
    "duplicate short options should be detected" in {
      val manager = new ExecutionOptionsManager(
        "test",
        Array("--top-name", "spoon",
              "--input-file", "fork") ) with HasFirrtlOptions with HasDuplicateShortOption
      a [FIRRTLException] should be thrownBy (manager.options)
    }
  }
}
