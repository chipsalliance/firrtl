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
import logger.LogLevel

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
  override def executeTest(input: String, expected: Seq[String], compiler: Compiler) = {
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
        |    output wrap: Interval
        |    output sat: Interval
        |    add <= add(x, y)
        |    sub <= sub(x, y)
        |    mul <= mul(x, y)
        |    div <= div(y, x)
        |    rem <= rem(y, x)
        |    ndiv <= div(r, q)
        |    nrem <= rem(r, q)
        |    mux <= mux(asUInt(p), x, z)
        |    wrap <= wrap(y, 4, 2)
        |    sat <= sat(y, 4, 2)
        |""".stripMargin
    val check = Seq(
      "output add : Interval[0, 6]",
      "output sub : Interval[-4, 2]",
      "output mul : Interval[0, 8]",
      "output div : Interval[0, 4]",
      "output rem : Interval[0, 2]",
      "output ndiv : Interval[-1, -1]",
      "output nrem : Interval[1, 1]",
      "output mux : Interval[-3, 2]",
      "output wrap : Interval[2, 4]",
      "output sat : Interval[2, 4]"
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

  "add and comparison" should "work" in {
    val input =
      """
        |circuit IntervalAddTester : @[:@2.0]
        |  module IntervalAddTester : @[:@3.2]
        |    input clock : Clock @[:@4.4]
        |    input reset : UInt<1> @[:@5.4]
        |
        |    wire in1 : Interval[0, 4] @[IntervalSpec.scala 53:17:@11.4]
        |    wire in2 : Interval[0, 4] @[IntervalSpec.scala 54:17:@13.4]
        |    node _T_6 = add(in1, in2) @[IntervalSpec.scala 59:20:@17.4]
        |    node _T_7 = tail(_T_6, 1) @[IntervalSpec.scala 59:20:@18.4]
        |    node result = asInterval(_T_7, 0, 4) @[IntervalSpec.scala 59:20:@19.4]
        |    node _T_9 = eq(result, asInterval(asSInt(UInt<4>("h4")), 4, 4)) @[IntervalSpec.scala 61:17:@20.4]
        |    node _T_10 = or(_T_9, reset) @[IntervalSpec.scala 61:9:@21.4]
        |    node _T_12 = eq(_T_10, UInt<1>("h0")) @[IntervalSpec.scala 61:9:@22.4]
        |    node _T_14 = eq(reset, UInt<1>("h0")) @[IntervalSpec.scala 63:7:@27.4]
        |    in1 <= asInterval(asSInt(UInt<3>("h2")), 2, 2)
        |    in2 <= asInterval(asSInt(UInt<3>("h2")), 2, 2)
        |    printf(clock, and(and(UInt<1>("h1"), _T_12), UInt<1>("h1")), "Assertion failed\n    at IntervalSpec.scala:61 assert(result === 4.I)\n") @[IntervalSpec.scala 61:9:@24.6]
        |    stop(clock, and(and(UInt<1>("h1"), _T_12), UInt<1>("h1")), 1) @[IntervalSpec.scala 61:9:@25.6]
        |    stop(clock, and(and(UInt<1>("h1"), _T_14), UInt<1>("h1")), 0) @[IntervalSpec.scala 63:7:@29.6]
        |
        |
      """.stripMargin
    val optionsManager = new ExecutionOptionsManager("chisel3") with HasFirrtlOptions {
      commonOptions = CommonOptions(topName = "interval", targetDirName = "test_run_dir", globalLogLevel = LogLevel.Debug)
      firrtlOptions = FirrtlExecutionOptions(compilerName = "verilog", firrtlSource = Some(input))
    }

    firrtl.Driver.execute(optionsManager) match {
      case _: FirrtlExecutionSuccess => true
      case _: FirrtlExecutionFailure => false
    }
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

  "Wrapping Intervals" should "optimize properly" in {
    val passes = Seq(
      ToWorkingIR,
      CheckHighForm,
      ResolveKinds,
      InferTypes,
      InferIntervals,
      ConvertIntervalToSInt)
    val input =
      """circuit Xorr :
        |  module Xorr :
        |    input x: Interval[0, 15]
        |    input y: Interval[-15, 15]
        |    output shr: Interval
        |    output sub: Interval
        |    output mod: Interval
        |    shr <= wrap(y, 7, 0)
        |    sub <= wrap(x, 13, -1)
        |    mod <= wrap(x, 2, -1)
        |""".stripMargin
    val check = Seq(
//      """shr <= bits(y, 2, 0)""",
      """sub <= mux(gt(x, SInt(13)), add(SInt(-1), sub(x, SInt(13))), mux(lt(x, SInt(-1)), sub(SInt(13), sub(SInt(-1), x)), x))""",
      """mod <= add(mod(sub(x, SInt(-1)), SInt(15)), SInt(-1))"""
    )
    executeTest(input, check, passes)
  }

  "Interval circuit" should "compile and run" in {
    val input =
      """
        |circuit IntervalTester :
        |  module IntervalTest1 :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    output io : {flip in1 : Interval[0 4], flip in2 : Interval[0 4], out : Interval[0 8]}
        |
        |    clock is invalid
        |    reset is invalid
        |    io is invalid
        |    node _T_5 = add(io.in1, io.in2) @[IntervalSpec.scala 18:20]
        |    node _T_6 = tail(_T_5, 1) @[IntervalSpec.scala 18:20]
        |    node _T_7 = asInterval(_T_6, 0, 4) @[IntervalSpec.scala 18:20]
        |    io.out <= _T_7 @[IntervalSpec.scala 18:10]
        |
        |  module IntervalTester :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    output io : {}
        |
        |    clock is invalid
        |    reset is invalid
        |    io is invalid
        |    reg value : UInt<4>, clock with : (reset => (reset, UInt<4>("h00"))) @[Counter.scala 17:33]
        |    when UInt<1>("h01") : @[Counter.scala 62:17]
        |      node _T_6 = eq(value, UInt<4>("h0a")) @[Counter.scala 25:24]
        |      node _T_8 = add(value, UInt<1>("h01")) @[Counter.scala 26:22]
        |      node _T_9 = tail(_T_8, 1) @[Counter.scala 26:22]
        |      value <= _T_9 @[Counter.scala 26:13]
        |      when _T_6 : @[Counter.scala 28:21]
        |        value <= UInt<1>("h00") @[Counter.scala 28:29]
        |        skip @[Counter.scala 28:21]
        |      skip @[Counter.scala 62:17]
        |    node done = and(UInt<1>("h01"), _T_6) @[Counter.scala 63:20]
        |    when done : @[CookbookSpec.scala 19:15]
        |      node _T_12 = eq(reset, UInt<1>("h00")) @[CookbookSpec.scala 19:21]
        |      when _T_12 : @[CookbookSpec.scala 19:21]
        |        stop(clock, UInt<1>(1), 0) @[CookbookSpec.scala 19:21]
        |        skip @[CookbookSpec.scala 19:21]
        |      skip @[CookbookSpec.scala 19:15]
        |    inst dut of IntervalTest1 @[IntervalSpec.scala 21:19]
        |    dut.io is invalid
        |    dut.clock <= clock
        |    dut.reset <= reset
        |    dut.io.in1 <= asInterval(asSInt(UInt<4>("h04")), 4, 4) @[IntervalSpec.scala 23:14]
        |    dut.io.in2 <= asInterval(asSInt(UInt<4>("h04")), 4, 4) @[IntervalSpec.scala 24:14]
        |    node _T_16 = eq(dut.io.out, asInterval(asSInt(UInt<5>("h08")), 8, 8)) @[IntervalSpec.scala 25:21]
        |    node _T_17 = or(_T_16, reset) @[IntervalSpec.scala 25:9]
        |    node _T_19 = eq(_T_17, UInt<1>("h00")) @[IntervalSpec.scala 25:9]
        |    when _T_19 : @[IntervalSpec.scala 25:9]
        |      printf(clock, UInt<1>(1), "Assertion failed\n    at IntervalSpec.scala:25 assert(dut.io.out === 8.I())\n") @[IntervalSpec.scala 25:9]
        |      stop(clock, UInt<1>(1), 1) @[IntervalSpec.scala 25:9]
        |      skip @[IntervalSpec.scala 25:9]
        |    node _T_21 = eq(reset, UInt<1>("h00")) @[IntervalSpec.scala 28:7]
        |    when _T_21 : @[IntervalSpec.scala 28:7]
        |      stop(clock, UInt<1>(1), 0) @[IntervalSpec.scala 28:7]
        |      skip @[IntervalSpec.scala 28:7]
      """.stripMargin

    val input2 =
      """
        |circuit SIntTest1Tester :
        |  module SIntTest1 :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    output io : {flip in1 : SInt<6>, flip in2 : SInt<6>, out : SInt<8>}
        |
        |    io is invalid
        |    io is invalid
        |    node _T_8 = add(io.in1, io.in2) @[IntervalSpec.scala 38:20]
        |    node _T_9 = tail(_T_8, 1) @[IntervalSpec.scala 38:20]
        |    node _T_10 = asSInt(_T_9) @[IntervalSpec.scala 38:20]
        |    io.out <= _T_10 @[IntervalSpec.scala 38:10]
        |
        |  module SIntTest1Tester :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    output io : {}
        |
        |    io is invalid
        |    io is invalid
        |    reg value : UInt<4>, clock with : (reset => (reset, UInt<4>("h00"))) @[Counter.scala 17:33]
        |    when UInt<1>("h01") : @[Counter.scala 62:17]
        |      node _T_6 = eq(value, UInt<4>("h0a")) @[Counter.scala 25:24]
        |      node _T_8 = add(value, UInt<1>("h01")) @[Counter.scala 26:22]
        |      node _T_9 = tail(_T_8, 1) @[Counter.scala 26:22]
        |      value <= _T_9 @[Counter.scala 26:13]
        |      when _T_6 : @[Counter.scala 28:21]
        |        value <= UInt<1>("h00") @[Counter.scala 28:29]
        |        skip @[Counter.scala 28:21]
        |      skip @[Counter.scala 62:17]
        |    node done = and(UInt<1>("h01"), _T_6) @[Counter.scala 63:20]
        |    when done : @[CookbookSpec.scala 19:15]
        |      node _T_12 = eq(reset, UInt<1>("h00")) @[CookbookSpec.scala 19:21]
        |      when _T_12 : @[CookbookSpec.scala 19:21]
        |        stop(clock, UInt<1>(1), 0) @[CookbookSpec.scala 19:21]
        |        skip @[CookbookSpec.scala 19:21]
        |      skip @[CookbookSpec.scala 19:15]
        |    inst dut of SIntTest1 @[IntervalSpec.scala 41:19]
        |    dut.io is invalid
        |    dut.clock <= clock
        |    dut.reset <= reset
        |    dut.io.in1 <= asSInt(UInt<4>("h04")) @[IntervalSpec.scala 43:14]
        |    dut.io.in2 <= asSInt(UInt<4>("h04")) @[IntervalSpec.scala 44:14]
        |    node _T_16 = eq(dut.io.out, asSInt(UInt<5>("h08"))) @[IntervalSpec.scala 45:21]
        |    node _T_17 = or(_T_16, reset) @[IntervalSpec.scala 45:9]
        |    node _T_19 = eq(_T_17, UInt<1>("h00")) @[IntervalSpec.scala 45:9]
        |    when _T_19 : @[IntervalSpec.scala 45:9]
        |      printf(clock, UInt<1>(1), "Assertion failed\n    at IntervalSpec.scala:45 assert(dut.io.out === 8.S)\n") @[IntervalSpec.scala 45:9]
        |      stop(clock, UInt<1>(1), 1) @[IntervalSpec.scala 45:9]
        |      skip @[IntervalSpec.scala 45:9]
        |    node _T_21 = eq(reset, UInt<1>("h00")) @[IntervalSpec.scala 48:7]
        |    when _T_21 : @[IntervalSpec.scala 48:7]
        |      stop(clock, UInt<1>(1), 0) @[IntervalSpec.scala 48:7]
        |      skip @[IntervalSpec.scala 48:7]
        |
        |
      """.stripMargin

    val compiler = new LowFirrtlCompiler()
    val parsedInput = Parser.parse(input)

    val finalState = compiler.compile(
      CircuitState(parsedInput,
        ChirrtlForm,
        None),
      Seq()
    )

    println(s"final state $finalState")
//    val passes = Seq(
//      ToWorkingIR,
//      CheckHighForm,
//      ResolveKinds,
//      InferTypes,
//      InferIntervals,
//      ConvertIntervalToSInt)
//
//    val check = Seq(
//      """shr <= bits(y, 2, 0)""",
//      """sub <= mux(gt(x, SInt(13)), add(SInt(-1), sub(x, SInt(13))), mux(lt(x, SInt(-1)), sub(SInt(13), sub(SInt(-1), x)), x))""",
//      """mod <= add(mod(sub(x, SInt(-1)), SInt(15)), SInt(-1))"""
//    )
//    executeTest(input, check, passes)
  }
}
