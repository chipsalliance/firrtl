package firrtlTests.formal

import firrtl.VerilogCompiler
import firrtl.testutils.FirrtlFlatSpec

class CheckSpec extends FirrtlFlatSpec {
  behavior of "Check"

  it should "work" in {
    val compiler = new VerilogCompiler
    val input =
      """circuit Checking :
        |  module Checking :
        |    input in: UInt<8>
        |    output out: UInt<8>
        |    wire areEqual: UInt<1>
        |    out <= in
        |    areEqual <= eq(out, in)
        |    check(areEqual)
        |""".stripMargin
    val expected =
      """module Checking(
        |  input [7:0] in,
        |  output [7:0] out
        |);
        |  wire areEqual = out == in;
        |  assign out = in;
        |  assert(areEqual);
        |endmodule
        |""".stripMargin.split("\n") map normalized
    executeTest(input, expected, compiler)
  }
}
