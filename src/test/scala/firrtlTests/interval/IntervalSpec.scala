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
  private def executeTest(input: String, expected: Seq[String], compiler: Compiler) = {
    val writer = new StringWriter()
    compiler.compile(CircuitState(parse(input), ChirrtlForm), writer)
    val lines = writer.toString().split("\n") map normalized
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
        |    input p: Interval[0, 1]
        |    output add: Interval
        |    output sub: Interval
        |    output mul: Interval
        |    output div: Interval
        |    output rem: Interval
        |    output ndiv: Interval
        |    output nrem: Interval
        |    output mux: Interval
        |    add <= add(x, y)
        |    sub <= sub(x, y)
        |    mul <= mul(x, y)
        |    div <= div(y, x)
        |    rem <= rem(y, x)
        |    ndiv <= div(r, q)
        |    nrem <= rem(r, q)
        |    mux <= mux(asUInt(p), x, z)
        |""".stripMargin
    val check = Seq(
      "output add : Interval[0, 6]",
      "output sub : Interval[-4, 2]",
      "output mul : Interval[0, 8]",
      "output div : Interval[0, 4]",
      "output rem : Interval[0, 2]",
      "output ndiv : Interval[-1, -1]",
      "output nrem : Interval[1, 1]",
      "output mux : Interval[-3, 2]"
    )
    executeTest(input, check, passes)
  }

  "Multiple assigments and out of order assignments" should "work" in {
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
        |    output out: Interval
        |    output out2: Interval
        |    out <= add(x, y)
        |    out <= sub(x, y)
        |    wire temp: Interval
        |    out2 <= add(temp, y)
        |    temp <= add(z, x)
        |""".stripMargin
    val check = Seq(
      "output out : Interval[-4, 6]",
      "output out2 : Interval[-3, 5]",
      "wire temp : Interval[-3, 1]"
    )
    executeTest(input, check, passes)
  }

  "Long width constraints" should "work" in {
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
        |    output out: Interval
        |    wire temp: Interval
        |    node _0 = add(temp, asInterval(UInt(1), 1, 1))
        |    node _1 = add(_0, asInterval(UInt(1), 1, 1))
        |    node _2 = add(_1, asInterval(UInt(1), 1, 1))
        |    node _3 = add(_2, asInterval(UInt(1), 1, 1))
        |    node _4 = add(_3, asInterval(UInt(1), 1, 1))
        |    node _5 = add(_4, asInterval(UInt(1), 1, 1))
        |    out <= _5
        |    temp <= add(z, x)
        |""".stripMargin
    val check = Seq(
      "output out : Interval[3, 7]",
      "wire temp : Interval[-3, 1]"
    )
    executeTest(input, check, passes)
  }

  "Recursive width constraints" should "work" in {
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
        |    input p: UInt<1>
        |    input clock: Clock
        |    reg r: Interval, clock
        |    r <= sub(add(r, asInterval(UInt(1), 1, 1)), asInterval(UInt(1), 1, 1))
        |    when p :
        |      r <= x
        |""".stripMargin
    val check = Seq(
      "reg r : Interval[0, 2], clock with :"
    )
    executeTest(input, check, passes)
  }

  "Convert Intervals to SInts" should "work" in {
    val passes = Seq(
      ToWorkingIR,
      CheckHighForm,
      ResolveKinds,
      InferTypes,
      InferIntervals,
      ConvertIntervalToSInt)
    val input =
      """circuit Unit :
        |  module Unit :
        |    input x: Interval[-2, 0]
        |    node q = add(add(x, asInterval(UInt(1), 1, 1)), x)
        |""".stripMargin
    val check = Seq(
      """node q = asSInt(bits(add(add(x, cvt(UInt<1>("h1"))), x), 2, 0))"""
    )
    executeTest(input, check, passes)
  }

  "Intervals" should "lower properly" in {
    val compiler = new LowFirrtlCompiler
    val input =
      """circuit Xorr :
        |  module Xorr :
        |    input x: Interval[0, 2]
        |    input y: Interval[0, 4]
        |    output z: Interval
        |    z <= add(x, y)""".stripMargin
    val check =
      """circuit Xorr :
        |  module Xorr :
        |    input x : SInt<3>
        |    input y : SInt<4>
        |    output z : SInt<4>
        |    z <= asSInt(bits(add(x, y), 3, 0))""".stripMargin.split("\n") map normalized
    executeTest(input, check, compiler)
  }
}
