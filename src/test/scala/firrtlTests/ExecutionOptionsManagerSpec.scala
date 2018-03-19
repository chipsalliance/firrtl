// See LICENSE for license details.

package firrtlTests

import firrtl._
import org.scalatest.{Matchers, FreeSpec}

class ExecutionOptionsManagerSpec extends FreeSpec with Matchers {
  "ExecutionOptionsManager is a container for one more more ComposableOptions Block" - {
    "It has a default CommonOptionsBlock" in {
      val manager = new ExecutionOptionsManager("test")
      manager.commonOptions.targetDirName should be (".")
    }
    "But can override defaults like this" in {
      val manager = new ExecutionOptionsManager(
        "test",
        Array("--top-name", "dog"))
      manager.commonOptions shouldBe a [CommonOptions]
      manager.topName should be ("dog")
      manager.commonOptions.topName should be ("dog")
    }
    "multiple composable blocks should be separable" in {
      val manager = new ExecutionOptionsManager(
        "test",
        Array("--top-name", "spoon",
              "--input-file", "fork") ) with HasFirrtlOptions

      manager.firrtlOptions.inputFileNameOverride should be ("fork")
      manager.commonOptions.topName should be ("spoon")
    }
  }
}
