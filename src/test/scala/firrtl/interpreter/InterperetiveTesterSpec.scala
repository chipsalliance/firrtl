// See LICENSE for license details.

package firrtl.interpreter

import org.scalatest.{Matchers, FlatSpec}

class InterperetiveTesterSpec extends FlatSpec with Matchers {
  behavior of "cycle mechanism"

  it should "mark circuit as stale after poke" in {
    val input =
      """
        |circuit Stop0 :
        |  module Stop0 :
        |    input clk : Clock
        |    input a : UInt<16>
        |    input b : UInt<16>
        |    output c : UInt<16>
        |
        |    reg reg1 : UInt<16>, clk
        |
        |    reg1 <= add(a, b)
        |    c <= add(reg1, UInt(1))
        |
      """.stripMargin

    val tester = new InterpretiveTester(input)
    val interpreter = tester.interpreter

    interpreter.circuitState.isStale should be (false)

    tester.poke("a", 1)

    interpreter.circuitState.isStale should be (true)

    tester.peek("c")

    interpreter.circuitState.isStale should be (false)
  }
}
