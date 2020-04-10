// See LICENSE for license details.

package firrtlTests

import firrtl._
import firrtl.testutils.FirrtlFlatSpec

class FlattenRegisterUpdateSpec extends FirrtlFlatSpec {
  "Emitted Verilog" should "not contain deeply nested register updates" in {
    val input = parse(
      """circuit test :
        |  module test :
        |    input clock : Clock
        |    input in : UInt<8>[8]
        |    input idx : UInt<3>
        |    output out : UInt<8>
        |    reg r : UInt, clock
        |    r <= in[idx]
        |    out <= r""".stripMargin
    )

    val state = CircuitState(input, ChirrtlForm)
    val result = (new VerilogCompiler).compileAndEmit(state, List.empty)
    val verilog = result.getEmittedCircuit.value
    for (i <- 0 to 3) {
      verilog shouldNot include regex (s"r <= in_$i")
    }
    for (i <- 4 to 7) {
      verilog should include regex (s"r <= in_$i")
    }
  }
}
