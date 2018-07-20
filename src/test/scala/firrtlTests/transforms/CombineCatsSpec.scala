// See LICENSE for license details.

package firrtlTests.transforms

import firrtl.PrimOps._
import firrtl._
import firrtl.ir.DoPrim
import firrtl.transforms.CombineCats
import firrtlTests.FirrtlFlatSpec
import firrtlTests.FirrtlCheckers._

class CombineCatsSpec extends FirrtlFlatSpec {
  private val maxCatLen = 12
  private val transforms = Seq(new IRToWorkingIR, new CombineCats(maxCatLen))

  private def execute(input: String, transforms: Seq[Transform]): CircuitState = {
    val c = transforms.foldLeft(CircuitState(parse(input), UnknownForm)) {
      (c: CircuitState, t: Transform) => t.runTransform(c)
    }.circuit
    CircuitState(c, UnknownForm, Seq(), None)
  }

  "circuit1 with combined cats" should "be equivalent to one without" in {
    val input =
      """circuit Test_CombinedCats1 :
        |  module Test_CombinedCats1 :
        |    input in1 : UInt<1>
        |    input in2 : UInt<2>
        |    input in3 : UInt<3>
        |    input in4 : UInt<4>
        |    output out : UInt<10>
        |    out <= cat(in4, cat(in3, cat(in2, in1)))
        |""".stripMargin
    firrtlEquivalenceTest(input, transforms)
  }

  "circuit2 with combined cats" should "be equivalent to one without" in {
    val input =
      """circuit Test_CombinedCats2 :
        |  module Test_CombinedCats2 :
        |    input in1 : UInt<1>
        |    input in2 : UInt<2>
        |    input in3 : UInt<3>
        |    input in4 : UInt<4>
        |    output out : UInt<10>
        |    out <= cat(cat(in4, in1), cat(cat(in4, in3), cat(in2, in1)))
        |""".stripMargin
    firrtlEquivalenceTest(input, transforms)
  }

  "circuit3 with combined cats" should "be equivalent to one without" in {
    val input =
      """circuit Test_CombinedCats3 :
        |  module Test_CombinedCats3 :
        |    input in1 : UInt<1>
        |    input in2 : UInt<2>
        |    input in3 : UInt<3>
        |    input in4 : UInt<4>
        |    output out : UInt<10>
        |    node temp1 = cat(cat(in4, in3), cat(in2, in1))
        |    node temp2 = cat(in4, cat(in3, cat(in2, in1)))
        |    out <= add(temp1, temp2)
        |""".stripMargin
    firrtlEquivalenceTest(input, transforms)
  }

  "nested cats" should "be combined" in {
    val input =
      """circuit Test_CombinedCats4 :
        |  module Test_CombinedCats4 :
        |    input in1 : UInt<1>
        |    input in2 : UInt<2>
        |    input in3 : UInt<3>
        |    input in4 : UInt<4>
        |    output out : UInt<10>
        |    node temp1 = cat(in2, in1)
        |    node temp2 = cat(in3, in2)
        |    node temp3 = cat(in4, in3)
        |    node temp4 = cat(temp1, temp2)
        |    node temp5 = cat(temp4, temp3)
        |    out <= temp5
        |""".stripMargin

    firrtlEquivalenceTest(input, transforms)
    val result = execute(input, transforms)

    // temp5 should get cat(cat(cat(in3, in2), cat(in4, in3)), cat(cat(in3, in2), cat(in4, in3)))
    result should containTree {
      case DoPrim(Cat, Seq(
        DoPrim(Cat, Seq(
          DoPrim(Cat, Seq(WRef("in2", _, _, _), WRef("in1", _, _, _)), _, _),
          DoPrim(Cat, Seq(WRef("in3", _, _, _), WRef("in2", _, _, _)), _, _)), _, _),
        DoPrim(Cat, Seq(WRef("in4", _, _, _), WRef("in3", _, _, _)), _, _)), _, _) => true
    }
  }

  "cats" should "not be longer than maxCatLen" in {
    val input =
      """circuit Test_CombinedCats5 :
        |  module Test_CombinedCats5 :
        |    input in1 : UInt<1>
        |    input in2 : UInt<2>
        |    input in3 : UInt<3>
        |    input in4 : UInt<4>
        |    input in5 : UInt<5>
        |    output out : UInt<10>
        |    node temp1 = cat(in2, in1)
        |    node temp2 = cat(in3, temp1)
        |    node temp3 = cat(in4, temp2)
        |    node temp4 = cat(in5, temp3)
        |    out <= temp4
        |""".stripMargin
    val transformsMaxCatLen3 = Seq(new IRToWorkingIR, new CombineCats(3))

    firrtlEquivalenceTest(input, transformsMaxCatLen3)
    val result = execute(input, transformsMaxCatLen3)

    // should not contain any cat chains greater than 3
    result shouldNot containTree {
      case DoPrim(Cat, Seq(_, DoPrim(Cat, Seq(_, DoPrim(Cat, _, _, _)), _, _)), _, _) => true
      case DoPrim(Cat, Seq(_, DoPrim(Cat, Seq(_, DoPrim(Cat, Seq(_, DoPrim(Cat, _, _, _)), _, _)), _, _)), _, _) => true
    }

    // temp2 should get cat(in3, cat(in2, in1))
    result should containTree {
      case DoPrim(Cat, Seq(
        WRef("in3", _, _, _),
        DoPrim(Cat, Seq(
          WRef("in2", _, _, _),
          WRef("in1", _, _, _)), _, _)), _, _) => true
    }
  }

  "nested nodes that are not cats" should "not be expanded" in {
    val input =
      """circuit Test_CombinedCats5 :
        |  module Test_CombinedCats5 :
        |    input in1 : UInt<1>
        |    input in2 : UInt<2>
        |    input in3 : UInt<3>
        |    input in4 : UInt<4>
        |    input in5 : UInt<5>
        |    output out : UInt<10>
        |    node temp1 = add(in2, in1)
        |    node temp2 = cat(in3, temp1)
        |    node temp3 = sub(in4, temp2)
        |    node temp4 = cat(in5, temp3)
        |    out <= temp4
        |""".stripMargin

    firrtlEquivalenceTest(input, transforms)

    val result = execute(input, transforms)
    result shouldNot containTree {
      case DoPrim(Cat, Seq(_, DoPrim(Add, _, _, _)), _, _) => true
      case DoPrim(Cat, Seq(_, DoPrim(Sub, _, _, _)), _, _) => true
      case DoPrim(Cat, Seq(_, DoPrim(Cat, Seq(_, DoPrim(Cat, _, _, _)), _, _)), _, _) => true
    }
  }
}
