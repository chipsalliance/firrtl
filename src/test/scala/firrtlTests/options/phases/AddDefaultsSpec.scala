// See LICENSE for license details.

package firrtlTests.options.phases

import org.scalatest.{FlatSpec, Matchers}

import firrtl.AnnotationSeq
import firrtl.options.{OptionsException, Phase, TargetDirAnnotation}
import firrtl.options.phases.AddDefaults

class AddDefaultsSpec extends FlatSpec with Matchers {

  val targetDir = TargetDirAnnotation("foo")

  class Fixture { val phase: Phase = new AddDefaults }

  behavior of classOf[AddDefaults].toString

  it should "add a TargetDirAnnotation if it does not exist" in new Fixture {
    val Occurrences = phase.transform(Seq.empty) count {
      a => a match {
        case _: TargetDirAnnotation => true
        case _ => false
      }
    }
    assert(Occurrences == 1)
  }

  it should "don't add a TargetDirAnnotation if it exists" in new Fixture {
    val Occurrences = phase.transform(Seq(targetDir)) count {
      a => a match {
        case _: TargetDirAnnotation => true
        case _ => false
      }
    }
    assert(Occurrences == 1)
  }
}
