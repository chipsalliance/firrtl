// See LICENSE for license details.

package firrtlTests

import firrtl.passes.Legalize
import firrtl.testutils.{ExecutionTest, FirrtlFlatSpec}

class LegalizeExecutionTest extends ExecutionTest("Legalize", "/passes/Legalize")

class LegalizeEquivalenceSpec extends FirrtlFlatSpec {
  "Legalize" should "handle casts of literals" in {
    val input =
      s"""circuit literalsel_fir:
         |  module literalsel_fir:
         |    input u1: UInt<4>
         |    output o: UInt<8>
         |    o <= asUInt(bits(dshl(pad(asSInt(UInt<2>("h1")), 8), asUInt(u1)), 7, 0))
         |""".stripMargin
    firrtlEquivalenceTest(input, Nil)
  }
}
