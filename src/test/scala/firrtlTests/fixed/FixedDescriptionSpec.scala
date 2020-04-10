// See LICENSE for license details.

package firrtlTests
package fixed

import firrtl._
import firrtl.testutils.FirrtlCheckers._
import firrtl.testutils.FirrtlFlatSpec

class FixedDescriptionSpec extends FirrtlFlatSpec {
  "Fixed point numbers" should "have descriptions" in {
    val compiler = new VerilogCompiler
    val input =
      """circuit Example :
        |  module Example :
        |    input clk: Clock
        |    input a: Fixed<4><<2>>
        |    input b: Fixed<2><<1>>
        |    input c: Fixed<3><<0>>
        |    output out: Fixed
        |
        |    reg sum: Fixed<5>, clk
        |    reg prod: Fixed<7>, clk
        |
        |    sum <= add(a, b)
        |    prod <= mul(sum, c)
        |    out <= sub(prod, sum)""".stripMargin
    val check = Seq(
      """  // Fixed point with binary point 2
        |  input  [3:0] a,""".stripMargin,
      """  // Fixed point with binary point 1
        |  input  [1:0] b,""".stripMargin,
      """  // Fixed point with binary point 0
        |  input  [2:0] c,""".stripMargin,
      """  // Fixed point with binary point 2
        |  output [7:0] out""".stripMargin,
      """  // Fixed point with binary point 2
        |  reg [4:0] sum;""".stripMargin,
      """  // Fixed point with binary point 2
        |  reg [6:0] prod;""".stripMargin
    )
    val writer = new java.io.StringWriter
    val finalState = compiler.compileAndEmit(CircuitState(parse(input), ChirrtlForm, Seq.empty), Seq.empty)
    val output = finalState.getEmittedCircuit.value
    for (c <- check) {
      assert(output.contains(c))
    }
  }
}
