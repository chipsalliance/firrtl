// See LICENSE for license details.

package firrtlTests

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import firrtl.ir.Circuit
import firrtl.Parser
import firrtl.passes.PassExceptions
import firrtl.annotations._
import firrtl.passes.{InlineAnnotation, InlineInstances}
import firrtl.transforms.{InlineBooleanExpressions, NoCircuitDedupAnnotation}
import logger.{LogLevel, Logger}
import logger.LogLevel.Debug

/**
 * Tests inline instances transformation
 */
class InlineBooleanExpressionsTests extends LowTransformSpec {
  def transform = new InlineBooleanExpressions
  def inline(mod: String): Annotation = {
    val parts = mod.split('.')
    val modName = ModuleName(parts.head, CircuitName("Top")) // If this fails, bad input
    val name = if (parts.size == 1) modName else ComponentName(parts.tail.mkString("."), modName)
    InlineAnnotation(name)
  }
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
           |    y <= and(and(a, b), c)""".stripMargin
      execute(input, check, Seq(inline("Inline")))
   }

  case class DummyAnno(target: ReferenceTarget) extends SingleTargetAnnotation[ReferenceTarget] {
    override def duplicate(n: ReferenceTarget): Annotation = DummyAnno(n)
  }
  "annotations" should "be renamed" in {
     val input =
        """circuit Top :
          |  module Top :
          |    input a : UInt<32>
          |    output b : UInt<32>
          |    inst i of Inline
          |    i.a <= a
          |    b <= i.b
          |  module Inline :
          |    input a : UInt<32>
          |    output b : UInt<32>
          |    inst foo of NestedInline
          |    inst bar of NestedNoInline
          |    foo.a <= a
          |    bar.a <= foo.b
          |    b <= bar.b
          |  module NestedInline :
          |    input a : UInt<32>
          |    output b : UInt<32>
          |    b <= a
          |  module NestedNoInline :
          |    input a : UInt<32>
          |    output b : UInt<32>
          |    b <= a
          |""".stripMargin
     val check =
        """circuit Top :
          |  module Top :
          |    input a : UInt<32>
          |    output b : UInt<32>
          |    wire i_a : UInt<32>
          |    wire i_b : UInt<32>
          |    wire i_foo_a : UInt<32>
          |    wire i_foo_b : UInt<32>
          |    i_foo_b <= i_foo_a
          |    inst i_bar of NestedNoInline
          |    i_b <= i_bar.b
          |    i_foo_a <= i_a
          |    i_bar.a <= i_foo_b
          |    b <= i_b
          |    i_a <= a
          |  module NestedNoInline :
          |    input a : UInt<32>
          |    output b : UInt<32>
          |    b <= a
          |""".stripMargin
    val top = CircuitTarget("Top").module("Top")
    val inlined = top.instOf("i", "Inline")
    val nestedInlined = top.instOf("i", "Inline").instOf("foo", "NestedInline")
    val nestedNotInlined = top.instOf("i", "Inline").instOf("bar", "NestedNoInline")

     executeWithAnnos(input, check,
       Seq(
         inline("Inline"),
         inline("NestedInline"),
         NoCircuitDedupAnnotation,
         DummyAnno(inlined.ref("a")),
         DummyAnno(inlined.ref("b")),
         DummyAnno(nestedInlined.ref("a")),
         DummyAnno(nestedInlined.ref("b")),
         DummyAnno(nestedNotInlined.ref("a")),
         DummyAnno(nestedNotInlined.ref("b"))
       ),
       Seq(
         DummyAnno(top.ref("i_a")),
         DummyAnno(top.ref("i_b")),
         DummyAnno(top.ref("i_foo_a")),
         DummyAnno(top.ref("i_foo_b")),
         DummyAnno(top.instOf("i_bar", "NestedNoInline").ref("a")),
         DummyAnno(top.instOf("i_bar", "NestedNoInline").ref("b"))
       )
     )
  }

  "inlining both grandparent and grandchild" should "should work" in {
     val input =
        """circuit Top :
          |  module Top :
          |    input a : UInt<32>
          |    output b : UInt<32>
          |    inst i of Inline
          |    i.a <= a
          |    b <= i.b
          |  module Inline :
          |    input a : UInt<32>
          |    output b : UInt<32>
          |    inst foo of NestedInline
          |    inst bar of NestedNoInline
          |    foo.a <= a
          |    bar.a <= foo.b
          |    b <= bar.b
          |  module NestedInline :
          |    input a : UInt<32>
          |    output b : UInt<32>
          |    b <= a
          |  module NestedNoInline :
          |    input a : UInt<32>
          |    output b : UInt<32>
          |    inst foo of NestedInline
          |    foo.a <= a
          |    b <= foo.b
          |""".stripMargin
     val check =
        """circuit Top :
          |  module Top :
          |    input a : UInt<32>
          |    output b : UInt<32>
          |    wire i_a : UInt<32>
          |    wire i_b : UInt<32>
          |    wire i_foo_a : UInt<32>
          |    wire i_foo_b : UInt<32>
          |    i_foo_b <= i_foo_a
          |    inst i_bar of NestedNoInline
          |    i_b <= i_bar.b
          |    i_foo_a <= i_a
          |    i_bar.a <= i_foo_b
          |    b <= i_b
          |    i_a <= a
          |  module NestedNoInline :
          |    input a : UInt<32>
          |    output b : UInt<32>
          |    wire foo_a : UInt<32>
          |    wire foo_b : UInt<32>
          |    foo_b <= foo_a
          |    b <= foo_b
          |    foo_a <= a
          |""".stripMargin
    val top = CircuitTarget("Top").module("Top")
    val inlined = top.instOf("i", "Inline")
    val nestedInlined = inlined.instOf("foo", "NestedInline")
    val nestedNotInlined = inlined.instOf("bar", "NestedNoInline")
    val innerNestedInlined = nestedNotInlined.instOf("foo", "NestedInline")

     executeWithAnnos(input, check,
       Seq(
         inline("Inline"),
         inline("NestedInline"),
         DummyAnno(inlined.ref("a")),
         DummyAnno(inlined.ref("b")),
         DummyAnno(nestedInlined.ref("a")),
         DummyAnno(nestedInlined.ref("b")),
         DummyAnno(nestedNotInlined.ref("a")),
         DummyAnno(nestedNotInlined.ref("b")),
         DummyAnno(innerNestedInlined.ref("a")),
         DummyAnno(innerNestedInlined.ref("b"))
       ),
       Seq(
         DummyAnno(top.ref("i_a")),
         DummyAnno(top.ref("i_b")),
         DummyAnno(top.ref("i_foo_a")),
         DummyAnno(top.ref("i_foo_b")),
         DummyAnno(top.instOf("i_bar", "NestedNoInline").ref("a")),
         DummyAnno(top.instOf("i_bar", "NestedNoInline").ref("b")),
         DummyAnno(top.instOf("i_bar", "NestedNoInline").ref("foo_a")),
         DummyAnno(top.instOf("i_bar", "NestedNoInline").ref("foo_b"))
       )
     )
  }
}

// Execution driven tests for inlining modules
// TODO(izraelevitz) fix this test
//class InlineBooleanExpressionsIntegrationSpec extends FirrtlPropSpec {
//  // Shorthand for creating annotations to inline modules
//  def inlineModules(names: Seq[String]): Seq[CircuitAnnotation] =
//    Seq(StickyCircuitAnnotation(InlineCAKind, names.map(n => ModuleName(n) -> TagAnnotation).toMap))
//
//  case class Test(name: String, dir: String, ann: Seq[CircuitAnnotation])
//
//  val runTests = Seq(
//    Test("GCDTester", "/integration", inlineModules(Seq("DecoupledGCD")))
//  )
//
//  runTests foreach { test =>
//    property(s"${test.name} should execute correctly with inlining") {
//      println(s"Got annotations ${test.ann}")
//      runFirrtlTest(test.name, test.dir, test.ann)
//    }
//  }
//}
