// See LICENSE for license details.

package firrtlTests.annotationTests

import firrtl._
import firrtl.annotations.{CircuitTarget, DeletedAnnotation, SingleTargetAnnotation, Target}
import firrtl.annotations.transforms.ResolvePaths

import logger.{Logger, LogLevel, LogLevelAnnotation}

import org.scalatest.{FlatSpec, Matchers}

class MorphismSpec extends FlatSpec with Matchers {

  case class AnAnnotation(target: Target) extends SingleTargetAnnotation[Target] {

    override def duplicate(a: Target) = this.copy(a)

  }

  object StripDeleted extends Transform {

    override def inputForm = UnknownForm
    override def outputForm = UnknownForm

    override def execute(a: CircuitState): CircuitState = {

      val annotationsx = a.annotations.filter {
        case a: DeletedAnnotation => false
        case _                    => true
      }

      a.copy(annotations = annotationsx)

    }

  }

  trait CircuitFixture {

    /** An input FIRRTL string */
    val input: String

    /** Input annotations */
    val annotations: AnnotationSeq = Seq.empty

    lazy val state = CircuitState(Parser.parse(input), UnknownForm, annotations)
  }

  trait RightInverseFixture extends CircuitFixture {

    /** An endomorphism */
    val f: Seq[Transform]

    /** The right inverse of f */
    val g: Seq[Transform]

    val setup: Seq[Transform] = Seq(
      firrtl.passes.ToWorkingIR,
      new firrtl.ResolveAndCheck
    )

    val cleanup: Seq[Transform] = Seq(
      StripDeleted
    )

    def apply(a: CircuitState): CircuitState = Logger.makeScope(Seq(LogLevelAnnotation(LogLevel.None))) {
      val ax = (setup ++ f ++ g).foldLeft(a){
        case (state, transform) => transform.runTransform(state)
      }

      cleanup.foldLeft(ax){
        case (state, transform) => transform.transform(state)
      }
    }

    lazy val output = apply(state)

    def test(): Unit = {

      /* The output circuit should be the same as the input circuit */
      info("the circuits are the same")
      output.circuit.serialize should be (state.circuit.serialize)

      /* The output annotations should match the input annotations */
      info("each annotation is the same")
      output.annotations.zip(state.annotations).foreach{
        case (a, b) => a should be (b)
      }
      info("the number of annotations is the same")
      output.annotations.size should be (state.annotations.size)

    }

  }

  trait IdempotencyFixture extends CircuitFixture {

    /** An endomorphism */
    val f: Seq[Transform]

    val setup: Seq[Transform] = Seq(
      firrtl.passes.ToWorkingIR,
      new firrtl.ResolveAndCheck
    )

    val cleanup: Seq[Transform] = Seq(
      StripDeleted
    )

    def apply(a: CircuitState): (CircuitState, CircuitState) = {

      val once = (setup ++ f).foldLeft(a){
        case (state, transform) => transform.runTransform(state)
      }

      val twice = f.foldLeft(once){
        case (state, transform) => transform.runTransform(state)
      }

      (once, twice)

    }

    lazy val (oneApplication, twoApplications) = apply(state)

    def test(): Unit = {

      info("a second application does not change the circuit")
      twoApplications.circuit.serialize should be (oneApplication.circuit.serialize)

      info("each annotation is the same after a second application")
      twoApplications.annotations.zip(oneApplication.annotations).foreach{
        case (a, b) => a should be (b)
      }

      info("the number of annotations after a second application is the same")
      twoApplications.annotations.size should be (oneApplication.annotations.size)

    }

  }

  trait RightInverseEliminateTargetsFixture extends RightInverseFixture {

    override val input =
      """|circuit Top:
         |  module Foo:
         |    node a = UInt<1>(0)
         |    skip
         |  module Bar:
         |    node b = UInt<1>(0)
         |    skip
         |  module Top:
         |    inst foo of Foo
         |    inst bar of Bar""".stripMargin

    override val f: Seq[Transform] = Seq(new firrtl.transforms.DedupModules)

    override val g: Seq[Transform] = Seq(new firrtl.annotations.transforms.EliminateTargetPaths)

  }

  behavior of "EliminateTargetPaths"

  it should "invert DedupModules with no annotations" in new RightInverseEliminateTargetsFixture {
    override val annotations: AnnotationSeq = Seq(
      ResolvePaths(Seq(CircuitTarget("Top").module("Top").instOf("foo", "Foo"),
                       CircuitTarget("Top").module("Top").instOf("bar", "Bar")))
    )
    test()
  }

  it should "invert DedupModules with InstanceTarget annotations" in new RightInverseEliminateTargetsFixture {
    override val annotations: AnnotationSeq = Seq(
      AnAnnotation(CircuitTarget("Top").module("Top").instOf("foo", "Foo")),
      AnAnnotation(CircuitTarget("Top").module("Top").instOf("bar", "Bar")),
      ResolvePaths(Seq(CircuitTarget("Top").module("Top").instOf("foo", "Foo"),
                       CircuitTarget("Top").module("Top").instOf("bar", "Bar")))
    )
    test()
  }

  it should "invert DedupModules with a ModuleTarget annotation" in new RightInverseEliminateTargetsFixture {
    override val annotations: AnnotationSeq = Seq(
      AnAnnotation(CircuitTarget("Top").module("Top")),
      ResolvePaths(Seq(CircuitTarget("Top").module("Top").instOf("foo", "Foo"),
                       CircuitTarget("Top").module("Top").instOf("bar", "Bar")))
    )
    test()
  }

  it should "invert DedupModules with a ReferenceTarget annotation" in new RightInverseEliminateTargetsFixture {
    override val annotations: AnnotationSeq = Seq(
      AnAnnotation(CircuitTarget("Top").module("Top").ref("x")),
      ResolvePaths(Seq(CircuitTarget("Top").module("Top").instOf("foo", "Foo"),
                       CircuitTarget("Top").module("Top").instOf("bar", "Bar")))
    )
    test()
  }

  it should "be idempotent" in (pending)

  behavior of "DedupModules"
  it should "invert EliminateTargetPaths with not annotations" in (pending)
  it should "invert EliminateTargetPaths with InstanceTarget annotations" in (pending)
  it should "invert EliminateTargetPaths with a ModuleTarget annotation" in (pending)
  it should "invert EliminateTargetPaths with a ReferenceTarget annotation" in (pending)
  it should "be idempotent" in (pending)

  behavior of "GroupComponents"
  it should "invert InlineInstances with not annotations" in (pending)
  it should "invert InlineInstances with InstanceTarget annotations" in (pending)
  it should "invert InlineInstances with a ModuleTarget annotation" in (pending)
  it should "invert InlineInstances with a ReferenceTarget annotation" in (pending)
  it should "be idempotent" in (pending)

  behavior of "InlineInstances"
  it should "invert GroupComponents with not annotations" in (pending)
  it should "invert GroupComponents with InstanceTarget annotations" in (pending)
  it should "invert GroupComponents with a ModuleTarget annotation" in (pending)
  it should "invert GroupComponents with a ReferenceTarget annotation" in (pending)
  it should "be idempotent" in (pending)

}
