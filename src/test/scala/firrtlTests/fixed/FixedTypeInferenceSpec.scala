// See LICENSE for license details.

package firrtlTests
package fixed

import firrtl._
import firrtl.ir.Circuit
import firrtl.passes._
import firrtl.Parser.IgnoreInfo

class FixedTypeInferenceSpec extends FirrtlFlatSpec {
  private def executeTest(input: String, expected: Seq[String], passes: Seq[Pass]) = {
    val c = passes.foldLeft(Parser.parse(input.split("\n").toIterator)) {
      (c: Circuit, p: Pass) => p.run(c)
    }
    val lines = c.serialize.split("\n") map normalized

    expected foreach { e =>
      lines should contain(e)
    }
  }

  "Fixed types" should "infer add correctly" in {
    val passes = Seq(
      ToWorkingIR,
      CheckHighForm,
      ResolveKinds,
      InferTypes,
      CheckTypes,
      ResolveGenders,
      CheckGenders,
      InferWidths,
      CheckWidths)
    val input =
      """circuit Unit :
        |  module Unit :
        |    input a : Fixed<10><<2>>
        |    input b : Fixed<10>
        |    input c : Fixed<4><<3>>
        |    output d : Fixed
        |    d <= add(a, add(b, c))""".stripMargin
    val check =
      """circuit Unit :
        |  module Unit :
        |    input a : Fixed<10><<2>>
        |    input b : Fixed<10><<0>>
        |    input c : Fixed<4><<3>>
        |    output d : Fixed<15><<3>>
        |    d <= add(a, add(b, c))""".stripMargin
    executeTest(input, check.split("\n") map normalized, passes)
  }

  "Fixed types" should "be correctly shifted left" in {
    val passes = Seq(
      ToWorkingIR,
      CheckHighForm,
      ResolveKinds,
      InferTypes,
      CheckTypes,
      ResolveGenders,
      CheckGenders,
      InferWidths,
      CheckWidths)
    val input =
      """circuit Unit :
        |  module Unit :
        |    input a : Fixed<10><<2>>
        |    output d : Fixed
        |    d <= shl(a, 2)""".stripMargin
    val check =
      """circuit Unit :
        |  module Unit :
        |    input a : Fixed<10><<2>>
        |    output d : Fixed<12><<2>>
        |    d <= shl(a, 2)""".stripMargin
    executeTest(input, check.split("\n") map normalized, passes)
  }

  "Fixed types" should "be correctly shifted right" in {
    val passes = Seq(
      ToWorkingIR,
      CheckHighForm,
      ResolveKinds,
      InferTypes,
      CheckTypes,
      ResolveGenders,
      CheckGenders,
      InferWidths,
      CheckWidths)
    val input =
      """circuit Unit :
        |  module Unit :
        |    input a : Fixed<10><<2>>
        |    output d : Fixed
        |    d <= shr(a, 2)""".stripMargin
    val check =
      """circuit Unit :
        |  module Unit :
        |    input a : Fixed<10><<2>>
        |    output d : Fixed<8><<2>>
        |    d <= shr(a, 2)""".stripMargin
    executeTest(input, check.split("\n") map normalized, passes)
  }

  "Fixed types" should "relatively move binary point left" in {
    val passes = Seq(
      ToWorkingIR,
      CheckHighForm,
      ResolveKinds,
      InferTypes,
      CheckTypes,
      ResolveGenders,
      CheckGenders,
      InferWidths,
      CheckWidths)
    val input =
      """circuit Unit :
        |  module Unit :
        |    input a : Fixed<10><<2>>
        |    output d : Fixed
        |    d <= bpshl(a, 2)""".stripMargin
    val check =
      """circuit Unit :
        |  module Unit :
        |    input a : Fixed<10><<2>>
        |    output d : Fixed<12><<4>>
        |    d <= bpshl(a, 2)""".stripMargin
    executeTest(input, check.split("\n") map normalized, passes)
  }

  "Fixed types" should "relatively move binary point right" in {
    val passes = Seq(
      ToWorkingIR,
      CheckHighForm,
      ResolveKinds,
      InferTypes,
      CheckTypes,
      ResolveGenders,
      CheckGenders,
      InferWidths,
      CheckWidths)
    val input =
      """circuit Unit :
        |  module Unit :
        |    input a : Fixed<10><<2>>
        |    output d : Fixed
        |    d <= bpshr(a, 2)""".stripMargin
    val check =
      """circuit Unit :
        |  module Unit :
        |    input a : Fixed<10><<2>>
        |    output d : Fixed<8><<0>>
        |    d <= bpshr(a, 2)""".stripMargin
    executeTest(input, check.split("\n") map normalized, passes)
  }

  "Fixed types" should "absolutely set binary point correctly" in {
    val passes = Seq(
      ToWorkingIR,
      CheckHighForm,
      ResolveKinds,
      InferTypes,
      CheckTypes,
      ResolveGenders,
      CheckGenders,
      InferWidths,
      CheckWidths)
    val input =
      """circuit Unit :
        |  module Unit :
        |    input a : Fixed<10><<2>>
        |    output d : Fixed
        |    d <= bpset(a, 3)""".stripMargin
    val check =
      """circuit Unit :
        |  module Unit :
        |    input a : Fixed<10><<2>>
        |    output d : Fixed<11><<3>>
        |    d <= bpset(a, 3)""".stripMargin
    executeTest(input, check.split("\n") map normalized, passes)
  }

  "Fixed types" should "cat, head, tail, bits" in {
    val passes = Seq(
      ToWorkingIR,
      CheckHighForm,
      ResolveKinds,
      InferTypes,
      CheckTypes,
      ResolveGenders,
      CheckGenders,
      InferWidths,
      CheckWidths)
    val input =
      """circuit Unit :
        |  module Unit :
        |    input a : Fixed<10><<2>>
        |    input b : Fixed<7><<3>>
        |    input c : UInt<2>
        |    output cat : Fixed
        |    output head : UInt
        |    output tail : UInt
        |    output bits : UInt
        |    cat <= cat(a, c)
        |    head <= head(a, 3)
        |    tail <= tail(a, 3)
        |    bits <= bits(a, 6, 3)""".stripMargin
    val check =
      """circuit Unit :
        |  module Unit :
        |    input a : Fixed<10><<2>>
        |    input b : Fixed<7><<3>>
        |    input c : UInt<2>
        |    output cat : Fixed<12><<4>>
        |    output head : UInt<3>
        |    output tail : UInt<7>
        |    output bits : UInt<4>
        |    cat <= cat(a, c)
        |    head <= head(a, 3)
        |    tail <= tail(a, 3)
        |    bits <= bits(a, 6, 3)""".stripMargin
    executeTest(input, check.split("\n") map normalized, passes)
  }

  "Fixed types" should "be cast to" in {
    val passes = Seq(
      ToWorkingIR,
      CheckHighForm,
      ResolveKinds,
      InferTypes,
      CheckTypes,
      ResolveGenders,
      CheckGenders,
      InferWidths,
      CheckWidths)
    val input =
      """circuit Unit :
        |  module Unit :
        |    input a : SInt<10>
        |    output d : Fixed
        |    d <= asFixedPoint(a, 2)""".stripMargin
    val check =
      """circuit Unit :
        |  module Unit :
        |    input a : SInt<10>
        |    output d : Fixed<10><<2>>
        |    d <= asFixedPoint(a, 2)""".stripMargin
    executeTest(input, check.split("\n") map normalized, passes)
  }

  "Fixed types" should "support binary point of zero" in {
    val passes = Seq(
      ToWorkingIR,
      CheckHighForm,
      ResolveKinds,
      InferTypes,
      CheckTypes,
      ResolveGenders,
      CheckGenders,
      InferWidths,
      CheckWidths,
      ConvertFixedToSInt)
    val input =
      """
        |circuit Unit :
        |  module Unit :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    input io_in : Fixed<6><<0>>
        |    output io_out : Fixed<6><<0>>
        |
        |    io_in is invalid
        |    io_out is invalid
        |    io_out <= io_in
      """.stripMargin
    val check =
      """
        |circuit Unit :
        |  module Unit :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    input io_in : SInt<6>
        |    output io_out : SInt<6>
        |
        |    io_out <= io_in
        |
      """.stripMargin
    executeTest(input, check.split("\n") map normalized, passes)
  }
}

