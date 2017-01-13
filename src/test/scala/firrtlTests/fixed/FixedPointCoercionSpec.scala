// See LICENSE for license details.

package firrtlTests.fixed

import firrtlTests.FirrtlFlatSpec

class FixedPointCoercionSpec extends FirrtlFlatSpec {
  "conversions between fixed numbers with different binary points" should "be intuitive" in {
    val input =
      """
        |circuit FixedPrecisionChanger :
        |  module FixedPrecisionChanger :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    output io : {flip in : Fixed<8><<3>>, out : Fixed<8><<6>>}
        |
        |    io is invalid
        |    io is invalid
        |    reg reg : Fixed, clock @[FixedPrecisionChangerSpec.scala 17:16]
        |    reg <= io.in @[FixedPrecisionChangerSpec.scala 18:7]
        |    io.out <= reg @[FixedPrecisionChangerSpec.scala 19:10]
        |
        |
      """.stripMargin
  }
}
