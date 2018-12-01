// See LICENSE for license details.

package firrtlTests.transforms

import firrtlTests.FirrtlFlatSpec
import org.scalatest._
import org.scalatest.prop._
import firrtl._
import firrtl.passes._
import firrtl.passes.wiring.{WiringTransform, SourceAnnotation, SinkAnnotation}
import firrtl.ir.Circuit
import firrtl.annotations._
import firrtl.transforms.{WidthGeqConstraintAnnotation, InferWidthsWithAnnos}


class InferWidthsWithAnnosSpec extends FirrtlFlatSpec {
  private def executeTest(input: String,
    check: String,
    transforms: Seq[Transform],
    annotations: Seq[Annotation]) = {
    val start = CircuitState(parse(input), ChirrtlForm, annotations)
    val end = transforms.foldLeft(start) {
      (c: CircuitState, t: Transform) => t.runTransform(c)
    }
    val resLines = end.circuit.serialize.split("\n") map normalized
    val checkLines = parse(check).serialize.split("\n") map normalized
    println(end.circuit.serialize)

    resLines should be (checkLines)
  }

  "CheckWidths on wires with unknown widths" should "result in an error" in {
    val transforms = Seq(
      ToWorkingIR,
      CheckHighForm,
      ResolveKinds,
      InferTypes,
      CheckTypes,
      ResolveGenders,
      InferWidthsWithAnnos,
      CheckWidths)

    val input =
      """circuit Top :
        |  module Top :
        |    inst b of B
        |    inst a of A
        |
        |  module B :
        |    wire x: UInt<3>
        |    x <= UInt(0)
        |
        |  module A :
        |    wire y: UInt""".stripMargin

    // A.y should have uninferred width
    intercept[CheckWidths.UninferredWidth] {
      executeTest(input, "", transforms, Seq.empty)
    }
  }

  "InferWidthsWithAnnos" should "infer widths using WidthGeqConstraintAnnotation" in {
    val transforms = Seq(
      ToWorkingIR,
      CheckHighForm,
      ResolveKinds,
      InferTypes,
      CheckTypes,
      ResolveGenders,
      InferWidthsWithAnnos,
      CheckWidths)

    val annos = Seq(WidthGeqConstraintAnnotation(
      ReferenceTarget("Top", "A", Nil, "y", Nil),
      ReferenceTarget("Top", "B", Nil, "x", Nil)))

    val input =
      """circuit Top :
        |  module Top :
        |    inst b of B
        |    inst a of A
        |
        |  module B :
        |    wire x: UInt<3>
        |    x <= UInt(0)
        |
        |  module A :
        |    wire y: UInt""".stripMargin

    val output =
      """circuit Top :
        |  module Top :
        |    inst b of B
        |    inst a of A
        |
        |  module B :
        |    wire x: UInt<3>
        |    x <= UInt(0)
        |
        |  module A :
        |    wire y: UInt<3>""".stripMargin

    // A.y should have same width as B.x
    executeTest(input, output, transforms, annos)
  }

  "InferWidthsWithAnnos" should "work with aggregates" in {
    val transforms = Seq(
      ToWorkingIR,
      CheckHighForm,
      ResolveKinds,
      InferTypes,
      CheckTypes,
      ResolveGenders,
      InferWidthsWithAnnos,
      CheckWidths)

    val annos = Seq(WidthGeqConstraintAnnotation(
      ReferenceTarget("Top", "A", Nil, "bundle", Nil),
      ReferenceTarget("Top", "B", Nil, "bundle", Nil)))

    val input =
      """circuit Top :
        |  module Top :
        |    inst b of B
        |    inst a of A
        |
        |  module B :
        |    wire bundle : {x : UInt<1>, y: UInt<2>, z: {zz : UInt<3>} }
        |    bundle.x <= UInt(0)
        |    bundle.y <= UInt(0)
        |    bundle.z.zz <= UInt(0)
        |
        |  module A :
        |    wire bundle : {x : UInt, y: UInt, z: {zz : UInt} }""".stripMargin

    val output =
      """circuit Top :
        |  module Top :
        |    inst b of B
        |    inst a of A
        |
        |  module B :
        |    wire bundle : {x : UInt<1>, y: UInt<2>, z: {zz : UInt<3>} }
        |    bundle.x <= UInt(0)
        |    bundle.y <= UInt(0)
        |    bundle.z.zz <= UInt(0)
        |
        |  module A :
        |    wire bundle : {x : UInt<1>, y: UInt<2>, z: {zz : UInt<3>} }""".stripMargin

    // A.y should have same width as B.x
    executeTest(input, output, transforms, annos)
  }

  "InferWidthsWithAnnos" should "work with WiringTransform" in {
    def transforms = Seq(
      ToWorkingIR,
      ResolveKinds,
      InferTypes,
      ResolveGenders,
      InferWidthsWithAnnos,
      CheckWidths,
      new WiringTransform,
      new ResolveAndCheck
    )
    val sourceTarget = ComponentName("bundle", ModuleName("A", CircuitName("Top")))
    val source = SourceAnnotation(sourceTarget, "pin")

    val sinkTarget = ComponentName("bundle", ModuleName("B", CircuitName("Top")))
    val sink = SinkAnnotation(sinkTarget, "pin")

    val wgeq = WidthGeqConstraintAnnotation(sinkTarget, sourceTarget)
    val failAnnos = Seq(source, sink)
    val successAnnos = wgeq +: failAnnos

    val input =
      """|circuit Top :
         |  module Top :
         |    inst a of A
         |    inst b of B
         |
         |  module A :
         |    wire bundle : {x : UInt<1>, y: UInt<2>, z: {zz : UInt<3>} }
         |
         |  module B :
         |    wire bundle : {x : UInt, y: UInt, z: {zz : UInt} }""".stripMargin

    val output =
      """|circuit Top :
         |  module Top :
         |    wire bundle : {x : UInt<1>, y: UInt<2>, z: {zz : UInt<3>} }
         |    inst a of A
         |    inst b of B
         |    b.pin <= bundle
         |    bundle <= a.bundle_0
         |
         |  module A :
         |    output bundle_0 : {x : UInt<1>, y: UInt<2>, z: {zz : UInt<3>} }
         |    wire bundle : {x : UInt<1>, y: UInt<2>, z: {zz : UInt<3>} }
         |    bundle_0 <= bundle
         |
         |  module B :
         |    input pin : {x : UInt<1>, y: UInt<2>, z: {zz : UInt<3>} }
         |    wire bundle : {x : UInt<1>, y: UInt<2>, z: {zz : UInt<3>} }
         |    bundle <= pin"""
        .stripMargin

    // should fail without extra constraint annos due to UninferredWidths
    val exceptions = intercept[PassExceptions] {
      executeTest(input, "", transforms, failAnnos)
    }.exceptions.reverse

    val msg = exceptions.head.toString
    assert(msg.contains(s"3 errors detected!"))
    assert(exceptions.tail.forall(_.isInstanceOf[CheckWidths.UninferredWidth]))

    // should pass with extra constraints
    executeTest(input, output, transforms, successAnnos)
  }
}
