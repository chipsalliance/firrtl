
package firrtlTests

import java.io._
import org.scalatest._
import org.scalatest.prop._
import firrtl.{Parser,Circuit}
import firrtl.passes._

class LowerTypesSpec extends FirrtlFlatSpec {
  private val passes = Seq(
    ToWorkingIR,
    CheckHighForm,
    ResolveKinds,
    InferTypes,
    CheckTypes,
    ResolveGenders,
    CheckGenders,
    InferWidths,
    CheckWidths,
    PullMuxes,
    ExpandConnects,
    RemoveAccesses,
    ExpandWhens,
    CheckInitialization,
    Legalize,
    ConstProp,
    ResolveKinds,
    InferTypes,
    ResolveGenders,
    InferWidths,
    LowerTypes)

  private def executeTest(input: String, expected: Seq[String]) = {
    val c = passes.foldLeft(Parser.parse("", input.split("\n").toIterator)) {
      (c: Circuit, p: Pass) => p.run(c)
    }
    val lines = c.serialize.split("\n") map normalized

    expected foreach { e =>
      lines should contain(e)
    }
  }

  behavior of "Lower Types"

  it should "lower ports" in {
    val input =
      """circuit Test :
        |  module Test :
        |    input w : UInt<1>
        |    input x : {a : UInt<1>, b : UInt<1>}
        |    input y : UInt<1>[4]
        |    input z : { c : { d : UInt<1>, e : UInt<1>}, f : UInt<1>[2] }[2]
      """.stripMargin
    val expected = Seq("w", "x_a", "x_b", "y_0", "y_1", "y_2", "y_3", "z_0_c_d",
      "z_0_c_e", "z_0_f_0", "z_0_f_1", "z_1_c_d", "z_1_c_e", "z_1_f_0",
      "z_1_f_1") map (x => s"input $x : UInt<1>") map normalized

    executeTest(input, expected)
  }

  it should "lower registers" in {
    val input =
      """circuit Test :
        |  module Test :
        |    input clk : Clock
        |    input reset : UInt<1>
        |    reg w : UInt<1>, clk
        |    reg x : {a : UInt<1>, b : UInt<1>}, clk
        |    reg y : UInt<1>[4], clk
        |    reg z : { c : { d : UInt<1>, e : UInt<1>}, f : UInt<1>[2] }[2], clk
      """.stripMargin
    val expected = Seq("w", "x_a", "x_b", "y_0", "y_1", "y_2", "y_3", "z_0_c_d",
      "z_0_c_e", "z_0_f_0", "z_0_f_1", "z_1_c_d", "z_1_c_e", "z_1_f_0",
      "z_1_f_1") map (x => s"reg $x : UInt<1>, clk with :") map normalized

    executeTest(input, expected)
  }
}
