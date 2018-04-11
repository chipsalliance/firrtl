// See LICENSE for license details.

package firrtlTests

import firrtl._
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
  "ExecutionOptionsManager is a container for one more more ComposableOptions Block" - {
    "It has a default CommonOptionsBlock" in {
      val manager = new ExecutionOptionsManager("test", Array("--top-name", "null")) with HasFirrtlOptions
      manager.firrtlOptions.targetDirName should be (".")
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
