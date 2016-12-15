// See LICENSE for license details.

package firrtlTests
package interval

import java.io._
import org.scalatest._
import org.scalatest.prop._
import firrtl._
import firrtl.ir.Circuit
import firrtl.passes._
import firrtl.Parser.IgnoreInfo

/** Add, Sub, Mul, Div
 ** Div by zero
 ** Rem
 ** Multiple assign
 ** Through ports
 ** DShr
 ** DShl
 */
class IntervalSpec extends FirrtlFlatSpec {
  private def executeTest(input: String, expected: Seq[String], passes: Seq[Pass]) = {
    val c = passes.foldLeft(Parser.parse(input.split("\n").toIterator)) {
      (c: Circuit, p: Pass) => p.run(c)
    }
    val lines = c.serialize.split("\n") map normalized
    println(c.serialize)

    expected foreach { e =>
      lines should contain(e)
    }
  }

  "Add, sub, mul, div, and rem" should "work" in {
    val passes = Seq(
      ToWorkingIR,
      CheckHighForm,
      ResolveKinds,
      InferTypes,
      InferIntervals)
    val input =
      """circuit Unit :
        |  module Unit :
        |    input x: Interval[0, 2]
        |    input y: Interval[0, 4]
        |    input z: Interval[-3, -1]
        |    input q: Interval[-3, -3]
        |    input r: Interval[4, 4]
        |    output add: Interval
        |    output sub: Interval
        |    output mul: Interval
        |    output div: Interval
        |    output rem: Interval
        |    output ndiv: Interval
        |    output nrem: Interval
        |    add <= add(x, y)
        |    sub <= sub(x, y)
        |    mul <= mul(x, y)
        |    div <= div(y, x)
        |    rem <= rem(y, x)
        |    ndiv <= div(r, q)
        |    nrem <= rem(r, q)
        |""".stripMargin
    val check = Seq(
      "output add : Interval[0, 6]",
      "output sub : Interval[2, -4]",
      "output mul : Interval[0, 8]",
      "output div : Interval[0, 4]",
      "output rem : Interval[0, 2]",
      "output ndiv : Interval[-1, -1]",
      "output nrem : Interval[1, 1]"
    )
    executeTest(input, check, passes)
  }
}
