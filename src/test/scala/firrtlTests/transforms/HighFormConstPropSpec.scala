// See LICENSE for license details.

package firrtlTests.transforms

import firrtl._
import firrtl.annotations.{CircuitName, ComponentName, ModuleName}
import firrtl.ir._
import firrtl.transforms.{DontTouchAnnotation, HighFormConstProp, NoDCEAnnotation}
import firrtlTests.FirrtlFlatSpec
import firrtlTests.FirrtlCheckers.containTree

class HighFormConstPropSpec extends FirrtlFlatSpec {
  private val transforms = Seq(new ChirrtlToHighFirrtl,
    new IRToWorkingIR,
    new ResolveAndCheck,
    new HighFormConstProp)

  private def execute(input: String, transforms: Seq[Transform], annotations: AnnotationSeq): CircuitState = {
    val c = transforms.foldLeft(CircuitState(parse(input), UnknownForm, annotations)) {
      (c: CircuitState, t: Transform) => t.runTransform(c)
    }.circuit
    CircuitState(c, UnknownForm, Seq(), None)
  }

  "constant nodes" should "be propagated and removed" in {
    val input =
      """circuit Test_HighFormConstProp1 :
        |  module Test_HighFormConstProp1 :
        |    input in1 : UInt<1>
        |    input in2 : UInt<2>
        |    input in3 : UInt<3>
        |    input in4 : UInt<4>
        |    input in5 : UInt<5>
        |    output out : UInt<10>
        |    node temp1 = add(in1, UInt<2>("h0"))
        |    node temp2 = and(in3, UInt<5>("h0"))
        |    node temp3 = and(in4, temp2)
        |    node temp4 = and(UInt<4>("h0"), and(in5, temp3))
        |    out <= temp4
        |""".stripMargin

    firrtlEquivalenceTest(input, transforms)

    val result = execute(input, transforms, Seq.empty)
    val UIntLit5 = UIntLiteral(0, IntWidth(5))
    val UInt10 = UIntType(IntWidth(10))

    result should containTree {
      case Connect(_, WRef("out", UInt10, PortKind, FEMALE), UIntLit5) => true
    }

    result shouldNot containTree {
      case DefNode(_, _, _) => true
    }
  }

  "nodes with better names" should "be propagated" in {
    val input =
      """circuit Test_HighFormConstProp2 :
        |  module Test_HighFormConstProp2 :
        |    input in1 : UInt<1>
        |    input in2 : UInt<2>
        |    input in3 : UInt<3>
        |    output out : UInt<10>
        |    node temp1 = add(in1, UInt<2>("h0"))
        |    node temp2 = add(and(in3, in2), temp1)
        |    node _GEN_0 = temp2
        |    out <= _GEN_0
        |""".stripMargin

    firrtlEquivalenceTest(input, transforms)

    val result = execute(input, transforms, Seq.empty)
    val UInt10 = UIntType(IntWidth(10))
    val UInt4 = UIntType(IntWidth(4))

    result should containTree {
      case Connect(_, WRef("out", UInt10, PortKind, FEMALE), WRef("temp2", UInt4, NodeKind, MALE)) => true
    }

    result shouldNot containTree {
      case DefNode(_, "_GEN_0", _) => true
    }
  }

  "nodes marked DontTouch" should "not be removed" in {
    val input =
      """circuit Test_HighFormConstProp3 :
        |  module Test_HighFormConstProp3 :
        |    input in1 : UInt<1>
        |    input in2 : UInt<2>
        |    input in3 : UInt<3>
        |    input in4 : UInt<4>
        |    input in5 : UInt<5>
        |    output out : UInt<10>
        |    node temp1 = add(in1, UInt<2>("h0"))
        |    node temp2 = and(in3, UInt<5>("h0"))
        |    node temp3 = and(in4, temp2)
        |    node temp4 = and(in5, temp3)
        |    out <= temp4
        |""".stripMargin

    val dontTouches = Seq(
      DontTouchAnnotation(ComponentName("temp1",
        ModuleName("Test_HighFormConstProp3",
          CircuitName("Test_HighFormConstProp3")))),
      DontTouchAnnotation(ComponentName("temp3",
        ModuleName("Test_HighFormConstProp3",
          CircuitName("Test_HighFormConstProp3")))))

    firrtlEquivalenceTest(input, transforms, dontTouches)
    val result = execute(input, transforms, dontTouches)

    result should containTree {
      case DefNode(_, "temp1", _) => true
    }
    result should containTree {
      case DefNode(_, "temp3", _) => true
    }

    val UInt10 = UIntType(IntWidth(10))
    val UIntLit5 = UIntLiteral(0, IntWidth(5))
    val UInt5 = UIntType(IntWidth(5))
    result should containTree {
      case Connect(_, WRef("out", UInt10, PortKind, FEMALE),
      WRef("temp4", UInt5, NodeKind, MALE)) => true
    }
  }

  "if there is a NoDCEAnnotation, nodes" should "not be removed" in {
    val input =
      """circuit Test_HighFormConstProp4 :
        |  module Test_HighFormConstProp4 :
        |    input in1 : UInt<1>
        |    input in2 : UInt<2>
        |    input in3 : UInt<3>
        |    input in4 : UInt<4>
        |    input in5 : UInt<5>
        |    output out : UInt<10>
        |    node temp1 = add(in1, UInt<2>("h0"))
        |    node temp2 = and(in3, UInt<5>("h0"))
        |    node temp3 = and(in4, temp2)
        |    node temp4 = and(UInt<4>("h0"), and(in5, temp3))
        |    out <= temp4
        |""".stripMargin

    val noDCEAnnotation = Seq(NoDCEAnnotation)

    firrtlEquivalenceTest(input, transforms, noDCEAnnotation)
    val result = execute(input, transforms, noDCEAnnotation)

    val UIntLit5 = UIntLiteral(0, IntWidth(5))
    result should containTree {
      case DefNode(_, "temp1", _) => true
    }
    result should containTree {
      case DefNode(_, "temp2", UIntLit5) => true
    }
    result should containTree {
      case DefNode(_, "temp3", UIntLit5) => true
    }
    result should containTree {
      case DefNode(_, "temp4", UIntLit5) => true
    }

    val UInt10 = UIntType(IntWidth(10))
    result should containTree {
      case Connect(_, WRef("out", UInt10, PortKind, FEMALE), UIntLit5) => true
    }
  }

  "aggregate references" should "be propagated" in {
    val input =
      """circuit Test_HighFormConstProp5 :
        |  module Test_HighFormConstProp5 :
        |    input in1 : {a: UInt<1>, b: UInt<4>[4]}
        |    input select : UInt<2>
        |    output out : {a: UInt<1>, b: UInt<4>[4]}
        |    output selected : UInt<4>
        |
        |    node _GEN_0 = in1
        |    out <- _GEN_0
        |    out.a <= _GEN_0.a
        |
        |    node temp1 = in1.b[2]
        |    selected <= temp1
        |
        |    node temp2 = in1.b[select]
        |    selected <= temp2
        |""".stripMargin

    firrtlEquivalenceTest(input, transforms, Seq.empty)
    val result = execute(input, transforms, Seq.empty)

    val BoolType = UIntType(IntWidth(1))
    val UInt2 = UIntType(IntWidth(2))
    val UInt4 = UIntType(IntWidth(4))
    val VecType = VectorType(UIntType(IntWidth(4)), 4)
    val MyBundleType = BundleType(Seq(
      Field("a", Default, BoolType),
      Field("b", Default, VecType)))

    result should containTree {
      case PartialConnect(_, WRef("out", MyBundleType, PortKind, FEMALE),
      WRef("in1", MyBundleType, PortKind, MALE)) => true
    }
    result should containTree {
      case Connect(_, WSubField(WRef("out", MyBundleType, PortKind, FEMALE), "a", BoolType, FEMALE),
      WSubField(WRef("in1", MyBundleType, PortKind, MALE), "a", BoolType, MALE)) => true
    }

    result should containTree {
      case Connect(_, WRef("selected", UInt4, PortKind, FEMALE),
      WSubIndex(WSubField(WRef("in1", MyBundleType, PortKind, MALE), "b", VecType, MALE), 2, UInt4, MALE)) => true
    }

    result should containTree {
      case Connect(_, WRef("selected", UInt4, PortKind, FEMALE),
      WSubAccess(WSubField(WRef("in1", MyBundleType, PortKind, MALE), "b", VecType, MALE),
      WRef("select", UInt2, PortKind, MALE), UInt4, MALE)) => true
    }

    result shouldNot containTree {
      case DefNode(_, "_GEN_0", _) => true
      case DefNode(_, "temp1", _) => true
      case DefNode(_, "temp2", _) => true
    }
  }
}
