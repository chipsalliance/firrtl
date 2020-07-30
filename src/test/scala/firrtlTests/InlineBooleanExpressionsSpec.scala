
// See LICENSE for license details.

package firrtlTests

import firrtl._
import firrtl.annotations.Annotation
import firrtl.options.Dependency
import firrtl.passes._
import firrtl.transforms._
import firrtl.testutils._
import firrtl.stage.TransformManager

class InlineBooleanExpressionsSpec extends FirrtlFlatSpec {
  val transform = new InlineBooleanExpressions
  val transforms: Seq[Transform] = new TransformManager(
      transform.prerequisites
    ).flattenedTransformOrder :+ transform

  protected def exec(input: String, annos: Seq[Annotation] = Nil) = {
    transforms.foldLeft(CircuitState(parse(input), UnknownForm, AnnotationSeq(annos))) {
      (c: CircuitState, t: Transform) => t.runTransform(c)
    }.circuit.serialize
  }

  it should "inline mux operands" in {
    val input =
      """circuit Top :
        |  module Top :
        |    output out : UInt<1>
        |    node x1 = UInt<1>(0)
        |    node x2 = UInt<1>(1)
        |    node _t = head(x1, 1)
        |    node _f = head(x2, 1)
        |    node _c = lt(x1, x2)
        |    node _y = mux(_c, _t, _f)
        |    out <= _y""".stripMargin
    val check =
      """circuit Top :
        |  module Top :
        |    output out : UInt<1>
        |    node x1 = UInt<1>(0)
        |    node x2 = UInt<1>(1)
        |    node _t = head(x1, 1)
        |    node _f = head(x2, 1)
        |    node _c = lt(x1, x2)
        |    node _y = mux(lt(x1, x2), head(x1, 1), head(x2, 1))
        |    out <= mux(lt(x1, x2), head(x1, 1), head(x2, 1))""".stripMargin
    val result = exec(input)
    (result) should be (parse(check).serialize)
    firrtlEquivalenceTest(input, Seq(new InlineBooleanExpressions))
  }

  it should "only inline expressions with the same info" in {
    val input =
      """circuit Top :
        |  module Top :
        |    output outA : UInt<1>
        |    output outB : UInt<1>
        |    node x1 = UInt<1>(0)
        |    node x2 = UInt<1>(1)
        |
        |    node _t = head(x1, 1) @[A]
        |    node _f = head(x2, 1) @[A]
        |    node _y = mux(lt(x1, x2), _t, _f) @[A]
        |    outA <= _y @[A]
        |
        |    outB <= _y @[B]""".stripMargin
    val check =
      """circuit Top :
        |  module Top :
        |    output outA : UInt<1>
        |    output outB : UInt<1>
        |    node x1 = UInt<1>(0)
        |    node x2 = UInt<1>(1)
        |
        |    node _t = head(x1, 1) @[A]
        |    node _f = head(x2, 1) @[A]
        |    node _y = mux(lt(x1, x2), head(x1, 1), head(x2, 1)) @[A]
        |    outA <= mux(lt(x1, x2), head(x1, 1), head(x2, 1)) @[A]
        |
        |    outB <= _y @[B]""".stripMargin
    val result = exec(input)
    (result) should be (parse(check).serialize)
    firrtlEquivalenceTest(input, Seq(new InlineBooleanExpressions))
  }

  it should "only inline ops with greater precedence" in {
    val input =
      """circuit Top :
        |  module Top :
        |    output outA : UInt<1>
        |    output outB : UInt<1>
        |    node x1 = UInt<3>(0)
        |    node x2 = UInt<3>(1)
        |
        |    node _a = lt(x1, x2)
        |    node _b = eq(_a, x2)
        |    node _c = and(_b, x2)
        |    outA <= _c
        |
        |    node _d = head(_c, 1)
        |    node _e = andr(_d)
        |    node _f = lt(_e, x2)
        |    outB <= _f""".stripMargin
    val check =
      """circuit Top :
        |  module Top :
        |    output outA : UInt<1>
        |    output outB : UInt<1>
        |    node x1 = UInt<3>(0)
        |    node x2 = UInt<3>(1)
        |
        |    node _a = lt(x1, x2)
        |    node _b = eq(lt(x1, x2), x2)
        |    node _c = and(eq(lt(x1, x2), x2), x2)
        |
        |    node _d = head(_c, 1)
        |    node _e = andr(head(_c, 1))
        |    node _f = lt(andr(head(_c, 1)), x2)
        |
        |    outA <= and(eq(lt(x1, x2), x2), x2)
        |    outB <= lt(andr(head(_c, 1)), x2)""".stripMargin
    val result = exec(input)
    (result) should be (parse(check).serialize)
    firrtlEquivalenceTest(input, Seq(new InlineBooleanExpressions))
  }

  it should "only inline ops with equal precedence if it is the left operand" in {
    val input =
      """circuit Top :
        |  module Top :
        |    output outA : UInt<1>
        |    output outB : UInt<1>
        |    node x1 = UInt<3>(0)
        |    node x2 = UInt<3>(1)
        |
        |    node _a = lt(x1, x2)
        |    node _b = leq(_a, x2)
        |    node _c = gt(_b, x2)
        |    node _d = geq(_c, x2)
        |    outA <= _d
        |
        |    node _e = lt(x1, x2)
        |    node _f = leq(x1, _e)
        |    node _g = gt(x1, _f)
        |    node _h = geq(x1, _g)
        |    outB <= _h""".stripMargin
    val check =
      """circuit Top :
        |  module Top :
        |    output outA : UInt<1>
        |    output outB : UInt<1>
        |    node x1 = UInt<3>(0)
        |    node x2 = UInt<3>(1)
        |
        |    node _a = lt(x1, x2)
        |    node _b = leq(lt(x1, x2), x2)
        |    node _c = gt(leq(lt(x1, x2), x2), x2)
        |    node _d = geq(gt(leq(lt(x1, x2), x2), x2), x2)
        |
        |    node _e = lt(x1, x2)
        |    node _f = leq(x1, _e)
        |    node _g = gt(x1, _f)
        |    node _h = geq(x1, _g)
        |
        |    outA <= geq(gt(leq(lt(x1, x2), x2), x2), x2)
        |    outB <= geq(x1, _g)""".stripMargin
    val result = exec(input)
    (result) should be (parse(check).serialize)
    firrtlEquivalenceTest(input, Seq(new InlineBooleanExpressions))
  }

  it should "only inline nested mux in false branch" in {
    val input =
      """circuit Top :
        |  module Top :
        |    output out : UInt<1>
        |    node c = UInt<1>(0)
        |    node t = UInt<1>(1)
        |    node f = UInt<1>(1)
        |
        |    node _a = mux(c, t, f)
        |    node _b = mux(c, t, _a)
        |    node _c = mux(c, t, _b)
        |    out <= _c
        |
        |    node _d = mux(c, _c, f)
        |    node _e = mux(_c, t, f)""".stripMargin
    val check =
      """circuit Top :
        |  module Top :
        |    output out : UInt<1>
        |    node c = UInt<1>(0)
        |    node t = UInt<1>(1)
        |    node f = UInt<1>(1)
        |
        |    node _a = mux(c, t, f)
        |    node _b = mux(c, t, mux(c, t, f))
        |    node _c = mux(c, t, mux(c, t, mux(c, t, f)))
        |
        |    node _d = mux(c, _c, f)
        |    node _e = mux(_c, t, f)
        |
        |    out <= mux(c, t, mux(c, t, mux(c, t, f)))""".stripMargin
    val result = exec(input)
    (result) should be (parse(check).serialize)
    firrtlEquivalenceTest(input, Seq(new InlineBooleanExpressions))
  }
}
