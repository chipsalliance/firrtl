// See LICENSE for license details.

package firrtlTests

import firrtl._
import firrtl.ir.Circuit
import firrtl.passes._
import org.scalatest.Matchers

class LegalizeExecutionTest extends ExecutionTest("Legalize", "/passes/Legalize")

class LegalizeSpec extends FirrtlFlatSpec with Matchers {
  private def executeTest(input: String, expected: Seq[String], passes: Seq[Pass]) = {
    val c = passes.foldLeft(Parser.parse(input.split("\n").toIterator)) {
      (c: Circuit, p: Pass) => p.run(c)
    }
    val lines = c.serialize.split("\n") map normalized

    expected foreach { e =>
      lines should contain(e)
    }
  }
  "shl by negative amount" should "result in an error" in {
    val passes = Seq(
      ToWorkingIR,
      CheckHighForm,
      ResolveKinds,
      InferTypes,
      CheckTypes,
      InferWidths,
      CheckWidths,
      Legalize
    )
    val amount = -1
    val input =
      s"""circuit Unit :
        |  module Unit :
        |    input x: UInt<3>
        |    output z: UInt
        |    z <= shl(x, $amount)""".stripMargin
    val exception = (intercept[FIRRTLException] {
      executeTest(input, Nil, passes)
    })
    exception.str should be (s"Argument $amount < 0 for Primop Shift Left")
  }
}
