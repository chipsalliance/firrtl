package firrtlTests.annotationTests

import firrtl.annotations.transforms.EliminateTargetPaths
import firrtl.{ChirrtlForm, CircuitForm, CircuitState, LowFirrtlCompiler, LowFirrtlOptimization, LowForm, MiddleFirrtlCompiler, ResolvedAnnotationPaths, Transform}
import firrtl.annotations.{Annotation, Target, SingleTargetAnnotation}
import firrtl.annotations.analysis.DuplicationHelper
import firrtl.transforms.DontTouchAnnotation
import firrtlTests.{FirrtlMatchers, FirrtlPropSpec}

class EliminateTargetPathsSpec extends FirrtlPropSpec with FirrtlMatchers {
  val input =
    """circuit Top:
      |  module Leaf:
      |    input i: UInt<1>
      |    output o: UInt<1>
      |    o <= i
      |    node a = i
      |  module Middle:
      |    input i: UInt<1>
      |    output o: UInt<1>
      |    inst l1 of Leaf
      |    inst l2 of Leaf
      |    l1.i <= i
      |    l2.i <= l1.o
      |    o <= l2.o
      |  module Top:
      |    input i: UInt<1>
      |    output o: UInt<1>
      |    inst m1 of Middle
      |    inst m2 of Middle
      |    m1.i <= i
      |    m2.i <= m1.o
      |    o <= m2.o
    """.stripMargin
  println(input)
  val Top = Target(Some("Top"), Some("Top"), Nil)
  val Middle = Target(Some("Top"), Some("Middle"), Nil)
  val Leaf = Target(Some("Top"), Some("Leaf"), Nil)

  val Top_m1_l1_a = Top.inst("m1").of("Middle").inst("l1").of("Leaf").ref("a")
  val Top_m2_l1_a = Top.inst("m2").of("Middle").inst("l1").of("Leaf").ref("a")
  val Middle_l1_a = Middle.inst("l1").of("Leaf").ref("a")
  val Middle_l2_a = Middle.inst("l2").of("Leaf").ref("a")
  val Leaf_a = Leaf.ref("a")

  val inputState = CircuitState(parse(input), ChirrtlForm)
  property("Hierarchical reference should be expanded properly") {
    val dupMap = new DuplicationHelper(inputState.circuit.modules.map(_.name).toSet)

    val outputState = new MiddleFirrtlCompiler().compile(inputState, Nil)

    val all = Seq(Top_m1_l1_a, Top_m2_l1_a, Middle_l1_a, Middle_l2_a, Leaf_a, Top, Middle, Leaf)
    //val all = Seq(Leaf)

    dupMap.expandHierarchy(Top_m1_l1_a)
    dupMap.expandHierarchy(Top_m2_l1_a)
    dupMap.expandHierarchy(Middle_l1_a)
    dupMap.expandHierarchy(Leaf_a)

    all.foreach { c =>
      val cs = dupMap.makePathless(c)
      println(s"""${c.serialize} => \n${cs.map("    " + _.serialize).mkString("\n")}""")
    }
  }
  property("Hierarchical donttouch should be resolved properly") {
    val inputState = CircuitState(parse(input), ChirrtlForm, Seq(DontTouchAnnotation(Top_m1_l1_a)))
    println(input)
    println(Top_m1_l1_a.serialize)
    val customTransforms = Seq(new EliminateTargetPaths(), new LowFirrtlOptimization())
    val outputState = new LowFirrtlCompiler().compile(inputState, customTransforms)
    println(outputState.circuit.serialize)
    println(outputState.annotations.collectFirst{case x: DontTouchAnnotation => x.target}.get.serialize)
  }

  property("No name conflicts between old and new modules") {
    val input =
      """circuit Top:
        |  module Middle:
        |    input i: UInt<1>
        |    output o: UInt<1>
        |    o <= i
        |  module Top:
        |    input i: UInt<1>
        |    output o: UInt<1>
        |    inst m1 of Middle
        |    inst m2 of Middle
        |    inst x of Middle___Top_m1
        |    x.i <= i
        |    m1.i <= i
        |    m2.i <= m1.o
        |    o <= m2.o
        |  module Middle___Top_m1:
        |    input i: UInt<1>
        |    output o: UInt<1>
        |    o <= i
        |    node a = i
      """.stripMargin
    val checks =
      """circuit Top :
        |  module Middle :
        |  module Top :
        |  module Middle___Top_m1 :
        |  module Middle___Top_m1_1 :""".stripMargin.split("\n")
    case class DummyAnnotation(target: Target) extends SingleTargetAnnotation[Target] {
      override def duplicate(n: Target): Annotation = DummyAnnotation(target)
    }
    class DummyTransform() extends Transform with ResolvedAnnotationPaths {
      override def inputForm: CircuitForm = LowForm
      override def outputForm: CircuitForm = LowForm

      override val annotationClasses: Traversable[Class[_]] = Seq(classOf[DummyAnnotation])

      override def execute(state: CircuitState): CircuitState = state
    }
    val customTransforms = Seq(new DummyTransform())
    val Top_m1 = Top.inst("m1").of("Middle")
    val inputState = CircuitState(parse(input), ChirrtlForm, Seq(DummyAnnotation(Top_m1)))
    val outputState = new LowFirrtlCompiler().compile(inputState, customTransforms)
    val outputLines = outputState.circuit.serialize.split("\n")
    checks.foreach { line =>
      outputLines should contain (line)
    }
  }
}
