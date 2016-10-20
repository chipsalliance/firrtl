// See LICENSE for license details.

package firrtlTests

import firrtl.ir.Circuit
import firrtl._
import firrtl.passes.Pass
import firrtl.ir._

class CustomTransformSpec extends FirrtlFlatSpec {
  val delayModuleString = """
    |circuit Delay :
    |  module Delay :
    |    input clk : Clock
    |    input reset : UInt<1>
    |    input a : UInt<32>
    |    input en : UInt<1>
    |    output b : UInt<32>
    |
    |    reg r : UInt<32>, clk
    |    r <= r
    |    when en :
    |      r <= a
    |    b <= r
    |""".stripMargin
  val delayModuleCircuit = parse(delayModuleString)
  val delayModule = delayModuleCircuit.modules.find(_.name == delayModuleCircuit.main).get

  behavior of "Custom Transforms"

  they should "be able to introduce high firrtl" in {
    // Simple testbench that just drives a single input
    val testbench = """
      |circuit Testbench :
      |  extmodule Delay :
      |    input clk : Clock
      |    input reset : UInt<1>
      |    input a : UInt<32>
      |    input en : UInt<1>
      |    output b : UInt<32>
      |
      |  module Testbench :
      |    input clk : Clock
      |    input reset : UInt<1>
      |
      |    reg cycle : UInt<32>, clk with : (reset => (reset, UInt<32>(0)))
      |    cycle <= tail(add(cycle, UInt<32>(1)), 1)
      |
      |    inst delay of Delay
      |    delay.clk <= clk
      |    delay.reset <= reset
      |    delay.a <= UInt(0)
      |    delay.en <= UInt(0)
      |
      |    when eq(cycle, UInt(0)) :
      |      delay.en <= UInt(1)
      |      delay.a <= UInt("hdeadbeef")
      |    when eq(cycle, UInt(1)) :
      |      when neq(delay.b, UInt("hdeadbeef")) :
      |        printf(clk, UInt(1), "Assertion failed!\n")
      |        stop(clk, UInt(1), 1)
      |    when eq(cycle, UInt(2)) :
      |      printf(clk, UInt(1), "Success!\n")
      |      stop(clk, UInt(1), 0)
      """.stripMargin

    class ReplaceExtModuleTransform extends PassBasedTransform {
      class ReplaceExtModule extends Pass {
        def name = "Insert Testbench"
        def run(c: Circuit): Circuit = c.copy(
          modules = c.modules map {
            case ExtModule(_, "Delay", _, _, _) => delayModule
            case other => other
          }
        )
      }
      def passSeq = Seq(new ReplaceExtModule)
      def inputForm = LowForm
      def outputForm = HighForm
    }

    val verilogCompiler = new VerilogCompiler
    val writer = new java.io.StringWriter
    val cs = CircuitState(parse(testbench), ChirrtlForm)
    verilogCompiler.compile(cs, writer, List(new ReplaceExtModuleTransform))
  }
}

