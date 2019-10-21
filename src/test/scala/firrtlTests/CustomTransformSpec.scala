// See LICENSE for license details.

package firrtlTests

import firrtl.ir.Circuit
import firrtl._
import firrtl.passes.Pass
import firrtl.ir._

object CustomTransformSpec {
  class ReplaceExtModuleTransform extends SeqTransform with FirrtlMatchers {
    // Simple module
    val delayModuleString = """
      |circuit Delay :
      |  module Delay :
      |    input clock : Clock
      |    input reset : UInt<1>
      |    input a : UInt<32>
      |    input en : UInt<1>
      |    output b : UInt<32>
      |
      |    reg r : UInt<32>, clock
      |    r <= r
      |    when en :
      |      r <= a
      |    b <= r
      |""".stripMargin
    val delayModuleCircuit = parse(delayModuleString)
    val delayModule = delayModuleCircuit.modules.find(_.name == delayModuleCircuit.main).get

    class ReplaceExtModule extends Pass {
      def run(c: Circuit): Circuit = c.copy(
        modules = c.modules map {
          case ExtModule(_, "Delay", _, _, _) => delayModule
          case other => other
        }
      )
    }
    def transforms = Seq(new ReplaceExtModule)
    def inputForm = LowForm
    def outputForm = HighForm
  }

  val input = """
      |circuit test :
      |  module test :
      |    output out : UInt
      |    out <= UInt(123)""".stripMargin
  val errorString = "My Custom Transform failed!"
  class ErroringTransform extends Transform {
    def inputForm = HighForm
    def outputForm = HighForm
    def execute(state: CircuitState): CircuitState = {
      require(false, errorString)
      state
    }
  }

  object MutableState {
    var count: Int = 0
  }

  class FirstTransform extends Transform {
    def inputForm = HighForm
    def outputForm = HighForm

    def execute(state: CircuitState): CircuitState = {
      require(MutableState.count == 0, s"Count was ${MutableState.count}, expected 0")
      MutableState.count = 1
      state
    }
  }

  class SecondTransform extends Transform {
    def inputForm = HighForm
    def outputForm = HighForm

    def execute(state: CircuitState): CircuitState = {
      require(MutableState.count == 1, s"Count was ${MutableState.count}, expected 1")
      MutableState.count = 2
      state
    }
  }

  class ThirdTransform extends Transform {
    def inputForm = HighForm
    def outputForm = HighForm

    def execute(state: CircuitState): CircuitState = {
      require(MutableState.count == 2, s"Count was ${MutableState.count}, expected 2")
      MutableState.count = 3
      state
    }
  }

}

class CustomTransformSpec extends FirrtlFlatSpec {

  import CustomTransformSpec._

  behavior of "Custom Transforms"

  they should "be able to introduce high firrtl" in {
    runFirrtlTest("CustomTransform", "/features", customTransforms = List(new ReplaceExtModuleTransform))
  }

  they should "not cause \"Internal Errors\"" in {
    val optionsManager = new ExecutionOptionsManager("test") with HasFirrtlOptions {
      firrtlOptions = FirrtlExecutionOptions(
        firrtlSource = Some(input),
        customTransforms = List(new ErroringTransform))
    }
    (the [java.lang.IllegalArgumentException] thrownBy {
      Driver.execute(optionsManager)
    }).getMessage should include (errorString)
  }

  they should "preserve the input order" in {
    runFirrtlTest("CustomTransform", "/features", customTransforms = List(
                    new FirstTransform,
                    new SecondTransform,
                    new ThirdTransform,
                    new ReplaceExtModuleTransform
                  ))
  }
}
