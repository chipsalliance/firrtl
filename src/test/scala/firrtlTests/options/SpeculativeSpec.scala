// See LICENSE for license details.

package firrtlTests.options

import firrtl.AnnotationSeq
import firrtl.options.{
  Dependency,
  IdentityLike,
  Phase,
  PhaseManager
}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

object SpeculativeSpec {

  class Foo extends Phase with IdentityLike[AnnotationSeq] {

    override def prerequisites = Seq.empty
    override def optionalPrerequisites = Seq.empty
    override def optionalPrerequisiteOf = Seq.empty
    override def invalidates(a: Phase) = false

    override def internalTransform(a: AnnotationSeq) = {
      println("Starting Foo")
      Thread.sleep(1000)
      println("Foo done!")
    }

  }

  class Bar extends Phase {

    override def prerequisites = Seq(Dependency[Foo])
    override def optionalPrerequisites = Seq.empty
    override def optionalPrerequisiteOf = Seq.empty
    override def invalidates(a: Phase) = false

    override def transform(a: AnnotationSeq) = {
      println("Starting Bar")
      Thread.sleep(1000)
      println("Bar done!")
      a
    }

  }

}

class SpeculativeSpec extends AnyFlatSpec with Matchers {

  import SpeculativeSpec._

  behavior of "Speculative TransformLikes"

  it should "run in parallel" in {

    val pm = new PhaseManager(Seq(Dependency[Foo], Dependency[Bar]))

    pm.transform(Seq.empty)

  }

}
