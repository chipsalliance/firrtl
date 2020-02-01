// See LICENSE for license details.

package firrtlTests

import firrtl.transforms.InlineBooleanExpressions

/**
 * Tests inline instances transformation
 */
class InlineBooleanExpressionsTests extends LowTransformSpec {
  def transform = new InlineBooleanExpressions
  // Set this to debug, this will apply to all tests
  // Logger.setLevel(this.getClass, Debug)
  "The module Top" should "have the boolean expressions combined" in {
    val input =
      """circuit Top :
        |  module Top :
        |    input a : UInt<1>
        |    input b : UInt<1>
        |    input c : UInt<1>
        |    output y : UInt<1>
        |    node _x = and(a, b)
        |    y <= and(_x, c)""".stripMargin
    val check =
      """circuit Top :
        |  module Top :
        |    input a : UInt<1>
        |    input b : UInt<1>
        |    input c : UInt<1>
        |    output y : UInt<1>
        |    node _x = and(a, b)
        |    y <= and(and(a, b), c)""".stripMargin
    execute(input, check, Nil)
  }

  // Set this to debug, this will apply to all tests
  // Logger.setLevel(this.getClass, Debug)
  "The module Top" should "not have the boolean expressions combined" in {
    val input =
      """circuit Top :
        |  module Top :
        |    input a : UInt<1>
        |    input b : UInt<1>
        |    input c : UInt<1>
        |    input d : UInt<1>
        |    output y : UInt<1>
        |    output z : UInt<1>
        |    node _x = and(a, b)
        |    y <= and(_x, c)
        |    z <= and(_x, d)
        |""".stripMargin
    val check =
      """circuit Top :
        |  module Top :
        |    input a : UInt<1>
        |    input b : UInt<1>
        |    input c : UInt<1>
        |    input d : UInt<1>
        |    output y : UInt<1>
        |    output z : UInt<1>
        |    node _x = and(a, b)
        |    y <= and(_x, c)
        |    z <= and(_x, d)
        |""".stripMargin
    execute(input, check, Nil)
  }

  "The module Top" should "not have the boolean expressions combined" in {
    val input =
      """circuit Top :
        |  module Top :
        |    input clk : Clock
        |    input a : UInt<1>
        |    input b : UInt<1>
        |    input s0 : UInt<1>
        |    input s1 : UInt<1>
        |    output x: UInt<1>
        |    node sel = and(s0, s1)
        |    printf(clk, not(sel), "xxxx\n")
        |""".stripMargin
    val check =
      """circuit Top :
        |  module Top :
        |    input clk : Clock
        |    input a : UInt<1>
        |    input b : UInt<1>
        |    input s0 : UInt<1>
        |    input s1 : UInt<1>
        |    output x: UInt<1>
        |    node sel = and(s0, s1)
        |    printf(clk, not(and(s0, s1)), "xxxx\n")
        |""".stripMargin
    execute(input, check, Nil)
  }

}