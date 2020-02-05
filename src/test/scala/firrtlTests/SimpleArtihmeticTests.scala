// See LICENSE for license details.

package firrtlTests

import firrtl.transforms.SimpleArithmetic

/**
 * Tests inline instances transformation
 */
class SimpleArtihmeticTests extends LowTransformSpec {
  def transform = new SimpleArithmetic
  // Set this to debug, this will apply to all tests
  // Logger.setLevel(this.getClass, Debug)
  "The module Top" should "convert add(_, -1) to sub(_, 1)" in {
    val input =
      """circuit Top :
        |  module Top :
        |    input a : SInt<2>
        |    output y : SInt<2>
        |    y <= add(a, SInt(-1))""".stripMargin
    val check =
      """circuit Top :
        |  module Top :
        |    input a : SInt<2>
        |    output y : SInt<2>
        |    y <= asSInt(bits(sub(a, SInt<1>("h1")), 1, 0))
        |    """.stripMargin
    execute(input, check, Nil)
  }

  "The module Top" should "convert sub(_, -1) to add(_, 1)" in {
    val input =
      """circuit Top :
        |  module Top :
        |    input a : SInt<2>
        |    output y : SInt<2>
        |    y <= sub(a, SInt(-1))""".stripMargin
    val check =
      """circuit Top :
        |  module Top :
        |    input a : SInt<2>
        |    output y : SInt<2>
        |    y <= asSInt(bits(add(a, SInt<1>("h1")), 1, 0))
        """.stripMargin
    execute(input, check, Nil)
  }

  "The module Top" should "not convert sub(_, -<max value>) to add(_, 1)" in {
    val input =
      """circuit Top :
        |  module Top :
        |    input a : SInt<2>
        |    output y : SInt<2>
        |    y <= sub(a, SInt(-3))""".stripMargin
    val check =
      """circuit Top :
        |  module Top :
        |    input a : SInt<2>
        |    output y : SInt<2>
        |    y <= asSInt(bits(sub(a, SInt(-3))), 1, 0)
        """.stripMargin
    execute(input, check, Nil)
  }

}