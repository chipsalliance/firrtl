package firrtlTests.annotationTests

import java.io.StringWriter

import firrtl.analyses.IRLookup
import firrtl.annotations.transforms.EliminateComponentPaths
import firrtl.{ChirrtlForm, CircuitState, LowFirrtlCompiler, LowFirrtlOptimization, MiddleFirrtlCompiler}
import firrtl.annotations.Component
import firrtl.transforms.DontTouchAnnotation
import firrtlTests.{FirrtlMatchers, FirrtlPropSpec}

class EliminateComponentPathsSpec extends FirrtlPropSpec with FirrtlMatchers {
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
  val Top = Component(Some("Top"), Some("Top"), Nil, None)
  val Middle = Component(Some("Top"), Some("Middle"), Nil, None)
  val Leaf = Component(Some("Top"), Some("Leaf"), Nil, None)

  val Top_m1_l1_a = Top.inst("m1").of("Middle").inst("l1").of("Leaf").ref("a")
  val Top_m2_l1_a = Top.inst("m2").of("Middle").inst("l1").of("Leaf").ref("a")
  val Middle_l1_a = Middle.inst("l1").of("Leaf").ref("a")
  val Middle_l2_a = Middle.inst("l2").of("Leaf").ref("a")
  val Leaf_a = Leaf.ref("a")

  val inputState = CircuitState(parse(input), ChirrtlForm)
  property("Hierarchical reference should be expanded properly") {
    val dupMap = new ResolveInstanceComponents.DupMap()

    val outputState = new MiddleFirrtlCompiler().compile(inputState, Nil)
    val iRLookup = IRLookup(outputState)

    val all = Seq(Top_m1_l1_a, Top_m2_l1_a, Middle_l1_a, Middle_l2_a, Leaf_a, Top, Middle, Leaf)
    //val all = Seq(Leaf)

    ResolveInstanceComponents.expandHierarchy(iRLookup, dupMap)(Top_m1_l1_a)
    ResolveInstanceComponents.expandHierarchy(iRLookup, dupMap)(Top_m2_l1_a)
    ResolveInstanceComponents.expandHierarchy(iRLookup, dupMap)(Middle_l1_a)
    //EliminateComponentPaths.expandHierarchy(iRLookup, dupMap)(Middle_l2_a)
    ResolveInstanceComponents.expandHierarchy(iRLookup, dupMap)(Leaf_a)
    dupMap.foreach(println)

    all.foreach { c =>
      val cs = ResolveInstanceComponents.remap(dupMap)(c)
      println(s"""${c.serialize} => \n${cs.map("    " + _.serialize).mkString("\n")}""")
    }
  }
  property("Hierarchical donttouch should be resolved properly") {

    val inputState = CircuitState(parse(input), ChirrtlForm, Seq(DontTouchAnnotation(Top_m1_l1_a)))
    val outputState = new LowFirrtlCompiler().compile(inputState, Seq(new EliminateComponentPaths(), new LowFirrtlOptimization()))
    println(outputState.circuit.serialize)
  }

  property("No name conflicts between old and new modules") { }
  property("DupMap is its own class") {}
  property("") {}
}
