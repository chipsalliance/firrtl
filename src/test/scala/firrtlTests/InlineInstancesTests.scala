package firrtlTests

import java.io.StringWriter

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner

import firrtl.{Parser,Circuit}
import firrtl.passes.PassExceptions
import firrtl.{
   Named,
   ModuleName,
   ComponentName,
   Annotation,
   TagAnnotation,
   StickyCircuitAnnotation
}
import firrtl.passes.{InlineInstances, InlineCAKind}


/**
 * Tests inline instances transformation
 */
class InlineInstancesTests extends HighTransformSpec {
   val transform = InlineInstances
   "The module Inline" should "be inlined" in {
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
           |    b <= a""".stripMargin
      val check =
         """circuit Top :
           |  module Top :
           |    input a : UInt<32>
           |    output b : UInt<32>
           |    wire i$a : UInt<32>
           |    wire i$b : UInt<32>
           |    i$b <= i$a
           |    i$a <= a
           |    b <= i$b
           |  module Inline :
           |    input a : UInt<32>
           |    output b : UInt<32>
           |    b <= a""".stripMargin
      val writer = new StringWriter()
      val map: Map[Named, Annotation] = Map(ModuleName("Inline") -> TagAnnotation)
      val annotation = StickyCircuitAnnotation(InlineCAKind, map)
      execute(writer, Seq(annotation), input, check)
   }

   "The all instances of Simple" should "be inlined" in {
      val input =
         """circuit Top :
           |  module Top :
           |    input a : UInt<32>
           |    output b : UInt<32>
           |    inst i0 of Simple
           |    inst i1 of Simple
           |    i0.a <= a
           |    i1.a <= i0.b
           |    b <= i1.b
           |  module Simple :
           |    input a : UInt<32>
           |    output b : UInt<32>
           |    b <= a""".stripMargin
      val check =
         """circuit Top :
           |  module Top :
           |    input a : UInt<32>
           |    output b : UInt<32>
           |    wire i0$a : UInt<32>
           |    wire i0$b : UInt<32>
           |    i0$b <= i0$a
           |    wire i1$a : UInt<32>
           |    wire i1$b : UInt<32>
           |    i1$b <= i1$a
           |    i0$a <= a
           |    i1$a <= i0$b
           |    b <= i1$b
           |  module Simple :
           |    input a : UInt<32>
           |    output b : UInt<32>
           |    b <= a""".stripMargin
      val writer = new StringWriter()
      val map: Map[Named, Annotation] = Map(ModuleName("Simple") -> TagAnnotation)
      val annotation = StickyCircuitAnnotation(InlineCAKind, map)
      execute(writer, Seq(annotation), input, check)
   }

   "Only one instance of Simple" should "be inlined" in {
      val input =
         """circuit Top :
           |  module Top :
           |    input a : UInt<32>
           |    output b : UInt<32>
           |    inst i0 of Simple
           |    inst i1 of Simple
           |    i0.a <= a
           |    i1.a <= i0.b
           |    b <= i1.b
           |  module Simple :
           |    input a : UInt<32>
           |    output b : UInt<32>
           |    b <= a""".stripMargin
      val check =
         """circuit Top :
           |  module Top :
           |    input a : UInt<32>
           |    output b : UInt<32>
           |    wire i0$a : UInt<32>
           |    wire i0$b : UInt<32>
           |    i0$b <= i0$a
           |    inst i1 of Simple
           |    i0$a <= a
           |    i1.a <= i0$b
           |    b <= i1.b
           |  module Simple :
           |    input a : UInt<32>
           |    output b : UInt<32>
           |    b <= a""".stripMargin
      val writer = new StringWriter()
      val map: Map[Named, Annotation] = Map(ComponentName("i0",ModuleName("Top")) -> TagAnnotation)
      val annotation = StickyCircuitAnnotation(InlineCAKind, map)
      execute(writer, Seq(annotation), input, check)
   }

   "All instances of A" should "be inlined" in {
      val input =
         """circuit Top :
           |  module Top :
           |    input a : UInt<32>
           |    output b : UInt<32>
           |    inst i0 of A
           |    inst i1 of B
           |    i0.a <= a
           |    i1.a <= i0.b
           |    b <= i1.b
           |  module A :
           |    input a : UInt<32>
           |    output b : UInt<32>
           |    b <= a
           |  module B :
           |    input a : UInt<32>
           |    output b : UInt<32>
           |    inst i of A
           |    i.a <= a
           |    b <= i.b""".stripMargin
      val check =
         """circuit Top :
           |  module Top :
           |    input a : UInt<32>
           |    output b : UInt<32>
           |    wire i0$a : UInt<32>
           |    wire i0$b : UInt<32>
           |    i0$b <= i0$a
           |    inst i1 of B
           |    i0$a <= a
           |    i1.a <= i0$b
           |    b <= i1.b
           |  module A :
           |    input a : UInt<32>
           |    output b : UInt<32>
           |    b <= a
           |  module B :
           |    input a : UInt<32>
           |    output b : UInt<32>
           |    wire i$a : UInt<32>
           |    wire i$b : UInt<32>
           |    i$b <= i$a
           |    i$a <= a
           |    b <= i$b""".stripMargin
      val writer = new StringWriter()
      val map: Map[Named, Annotation] = Map(ModuleName("A") -> TagAnnotation)
      val annotation = StickyCircuitAnnotation(InlineCAKind, map)
      execute(writer, Seq(annotation), input, check)
   }


   // ---- Errors ----
   // 1) ext module
   "External module" should "not be inlined" in {
      val input =
         """circuit Top :
           |  module Top :
           |    input a : UInt<32>
           |    output b : UInt<32>
           |    inst i of A
           |    i.a <= a
           |    b <= i.b
           |  extmodule A :
           |    input a : UInt<32>
           |    output b : UInt<32>""".stripMargin
      val writer = new StringWriter()
      val map: Map[Named, Annotation] = Map(ModuleName("A") -> TagAnnotation)
      val annotation = StickyCircuitAnnotation(InlineCAKind,map)
      failingexecute(writer, Seq(annotation), input)
   }
   // 2) ext instance
   "External instance" should "not be inlined" in {
      val input =
         """circuit Top :
           |  module Top :
           |    input a : UInt<32>
           |    output b : UInt<32>
           |    inst i of A
           |    i.a <= a
           |    b <= i.b
           |  extmodule A :
           |    input a : UInt<32>
           |    output b : UInt<32>""".stripMargin
      val writer = new StringWriter()
      val map: Map[Named, Annotation] = Map(ModuleName("A") -> TagAnnotation)
      val annotation = StickyCircuitAnnotation(InlineCAKind,map)
      failingexecute(writer, Seq(annotation), input)
   }
   // 3) no module
   "Inlined module" should "exist" in {
      val input =
         """circuit Top :
           |  module Top :
           |    input a : UInt<32>
           |    output b : UInt<32>
           |    b <= a""".stripMargin
      val writer = new StringWriter()
      val map: Map[Named, Annotation] = Map(ModuleName("A") -> TagAnnotation)
      val annotation = StickyCircuitAnnotation(InlineCAKind,map)
      failingexecute(writer, Seq(annotation), input)
   }
   // 4) no inst
   "Inlined instance" should "exist" in {
      val input =
         """circuit Top :
           |  module Top :
           |    input a : UInt<32>
           |    output b : UInt<32>
           |    b <= a""".stripMargin
      val writer = new StringWriter()
      val map: Map[Named, Annotation] = Map(ModuleName("A") -> TagAnnotation)
      val annotation = StickyCircuitAnnotation(InlineCAKind,map)
      failingexecute(writer, Seq(annotation), input)
   }
}



