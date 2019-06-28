// See LICENSE for license details.

package firrtlTests.options.phases

import org.scalatest.{FlatSpec, Matchers}

import firrtl.options.{Phase, TargetDirAnnotation}
import firrtl.options.phases.AddDefaults

class AddDefaultsSpec extends FlatSpec with Matchers {

  class Fixture {
    val phase: Phase = new AddDefaults
    val targetDir = TargetDirAnnotation("foo")
    val defaultDir = TargetDirAnnotation(".")
  }

  behavior of classOf[AddDefaults].toString

  it should "add a TargetDirAnnotation if it does not exist" in new Fixture {
    phase.transform(Seq.empty).toSeq should be (Seq(defaultDir))
  }

  it should "don't add a TargetDirAnnotation if it exists" in new Fixture {
    phase.transform(Seq(targetDir)).toSeq should be (Seq(targetDir))
  }
}
