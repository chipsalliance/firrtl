// See LICENSE for license details.

package firrtlTests.formal

import firrtl._
import firrtl.testutils.FirrtlFlatSpec
import firrtl.transforms.formal.ConvertAsserts

class ConvertAssertsSpec extends FirrtlFlatSpec {
  "assert nodes" should "be converted to conditional print-and-stop blocks" in {
    val input =
      """circuit DUT:
        |  module DUT:
        |    input clock: Clock
        |    input reset: UInt<1>
        |    input x: UInt<8>
        |    output y: UInt<8>
        |    node ne5 = neq(x, UInt(5))
        |    assert(clock, ne5, not(reset), "x should not equal 5")
        |""".stripMargin

    val ref =
      """circuit DUT:
        |  module DUT:
        |    input clock: Clock
        |    input reset: UInt<1>
        |    input x: UInt<8>
        |    output y: UInt<8>
        |    node ne5 = neq(x, UInt(5))
        |    when not(ne5):
        |      printf(clock, not(reset), "x should not equal 5")
        |      stop(clock, not(reset), 1)
        |""".stripMargin

    val outputCS = ConvertAsserts.execute(CircuitState(parse(input), Nil))
    (parse(outputCS.circuit.serialize)) should be (parse(ref))
  }
}
