package firrtlTests
package interval

import java.io._
import firrtl._
import firrtl.ir.Circuit
import firrtl.passes._
import firrtl.Parser.IgnoreInfo

class IntervalSpec extends FirrtlFlatSpec {
  private def executeTest(input: String, expected: Seq[String], passes: Seq[Pass]) = {
    val c = passes.foldLeft(Parser.parse(input.split("\n").toIterator)) {
      (c: Circuit, p: Pass) => p.run(c)
    }
    val lines = c.serialize.split("\n") map normalized

    expected foreach { e =>
      lines should contain(e)
    }
  }

  "Interval types" should "parse correctly" in {
    val passes = Seq(ToWorkingIR)
    val input =
      """circuit Unit :
        |  module Unit :
        |    input in0 : Interval(-0.32, 10.1).4
        |    input in1 : Interval[0, 10.10].4
        |    input in2 : Interval(-0.32, 10].4
        |    input in3 : Interval[-3, 10.1).4
        |    input in4 : Interval(-0.32, 10.1)
        |    input in5 : Interval.4
        |    input in6 : Interval
        |    output out0 : Interval.2
        |    output out1 : Interval
        |    out0 <= add(in0, add(in1, add(in2, add(in3, add(in4, add(in5, in6))))))
        |    out1 <= add(in0, add(in1, add(in2, add(in3, add(in4, add(in5, in6))))))""".stripMargin
    executeTest(input, input.split("\n") map normalized, passes)
  }

  "Interval types" should "infer bp correctly" in {
    val passes = Seq(ToWorkingIR, InferTypes, ResolveGenders, new InferBinaryPoints())
    val input =
      """circuit Unit :
        |  module Unit :
        |    input in0 : Interval(-0.32, 10.1).4
        |    input in1 : Interval[0, 10.10].3
        |    input in2 : Interval(-0.32, 10].2
        |    output out0 : Interval
        |    out0 <= add(in0, add(in1, in2))""".stripMargin
    val check =
      """circuit Unit :
        |  module Unit :
        |    input in0 : Interval(-0.32, 10.1).4
        |    input in1 : Interval[0, 10.10].3
        |    input in2 : Interval(-0.32, 10].2
        |    output out0 : Interval.4
        |    out0 <= add(in0, add(in1, in2))""".stripMargin
    executeTest(input, check.split("\n") map normalized, passes)
  }

  "Interval types" should "infer intervals correctly" in {
    val passes = Seq(ToWorkingIR, InferTypes, ResolveGenders, new InferBinaryPoints(), new InferIntervals())
    val input =
      """circuit Unit :
        |  module Unit :
        |    input in0 : Interval(0, 10).4
        |    input in1 : Interval(0, 10].3
        |    input in2 : Interval(-1, 3].2
        |    output out0 : Interval
        |    output out1 : Interval
        |    output out2 : Interval
        |    out0 <= add(in0, add(in1, in2))
        |    out1 <= mul(in0, mul(in1, in2))
        |    out2 <= sub(in0, sub(in1, in2))""".stripMargin
    val check =
      """output out0 : Interval(-1, 23).4
        |output out1 : Interval(-100, 300).9
        |output out2 : Interval(-11, 13).4""".stripMargin
    executeTest(input, check.split("\n") map normalized, passes)
  }

  "Interval types" should "be removed correctly" in {
    val passes = Seq(ToWorkingIR, InferTypes, ResolveGenders, new InferBinaryPoints(), new InferIntervals(), new RemoveIntervals())
    val input =
      """circuit Unit :
        |  module Unit :
        |    input in0 : Interval(0, 10).4
        |    input in1 : Interval(0, 10].3
        |    input in2 : Interval(-1, 3].2
        |    output out0 : Interval
        |    output out1 : Interval
        |    output out2 : Interval
        |    out0 <= add(in0, add(in1, in2))
        |    out1 <= mul(in0, mul(in1, in2))
        |    out2 <= sub(in0, sub(in1, in2))""".stripMargin
    val check =
      """circuit Unit :
        |  module Unit :
        |    input in0 : SInt<9>
        |    input in1 : SInt<8>
        |    input in2 : SInt<5>
        |    output out0 : SInt<10>
        |    output out1 : SInt<19>
        |    output out2 : SInt<9>
        |    out0 <= add(in0, shl(add(in1, shl(in2, 1)), 1))
        |    out1 <= mul(in0, mul(in1, in2))
        |    out2 <= sub(in0, shl(sub(in1, shl(in2, 1)), 1))""".stripMargin
    executeTest(input, check.split("\n") map normalized, passes)
  }

  "Interval types" should "infer this example correctly" in {
    val passes = Seq(ToWorkingIR, InferTypes, ResolveGenders, new InferBinaryPoints(), new InferIntervals())
      val input =
        s"""circuit Unit :
        |  module Unit :
        |    input  in1 : Interval(0.5, 0.5].1
        |    input  in2 : Interval(-0.5, 0.5).1
        |    output sum : Interval
        |    sum <= add(in2, in1)
        |    """.stripMargin
    val check = s"""output sum : Interval(0.0, 1.0).1 """.stripMargin
    executeTest(input, check.split("\n") map normalized, passes)
  }

  "Interval types" should "infer multiplication by zero correctly" in {
    val passes = Seq(ToWorkingIR, InferTypes, ResolveGenders, new InferBinaryPoints(), new InferIntervals())
      val input =
        s"""circuit Unit :
        |  module Unit :
        |    input  in1 : Interval(0.0, 0.5).1
        |    input  in2 : Interval[0.0, 0.0].1
        |    output mul : Interval
        |    mul <= mul(in2, in1)
        |    """.stripMargin
    val check = s"""output mul : Interval[0, 0].2 """.stripMargin
    executeTest(input, check.split("\n") map normalized, passes)
  }

  "Interval types" should "infer muxes correctly" in {
    val passes = Seq(ToWorkingIR, InferTypes, ResolveGenders, new InferBinaryPoints(), new InferIntervals())
      val input =
        s"""circuit Unit :
        |  module Unit :
        |    input  p   : UInt<1>
        |    input  in1 : Interval(0.0, 0.5).1
        |    input  in2 : Interval[0.0, 0.0].1
        |    output out : Interval
        |    out <= mux(p, in2, in1)
        |    """.stripMargin
    val check = s"""output out : Interval[0.0, 0.5).1 """.stripMargin
    executeTest(input, check.split("\n") map normalized, passes)
  }
}
