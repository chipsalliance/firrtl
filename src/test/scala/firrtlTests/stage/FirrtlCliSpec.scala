// See LICENSE for license details.

package firrtlTests.stage


import firrtl.stage.RunFirrtlTransformAnnotation
import firrtl.options.{Dependency, Shell}
import firrtl.stage.FirrtlCli
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FirrtlCliSpec extends AnyFlatSpec with Matchers {

  behavior of "FirrtlCli for RunFirrtlTransformAnnotation / -fct / --custom-transforms"

  it should "preserver transform order" in {
    val shell = new Shell("foo") with FirrtlCli
    val args = Array(
      "--custom-transforms", "firrtl.transforms.BlackBoxSourceHelper,firrtl.transforms.CheckCombLoops",
      "--custom-transforms", "firrtl.transforms.CombineCats",
      "--custom-transforms", "firrtl.transforms.ConstantPropagation",
      "--custom-transforms", "firrtl.passes.InferTypes$")
    val expected = Seq(
      Dependency[firrtl.transforms.BlackBoxSourceHelper],
      Dependency[firrtl.transforms.CheckCombLoops],
      Dependency[firrtl.transforms.CombineCats],
      Dependency[firrtl.transforms.ConstantPropagation],
      Dependency(firrtl.passes.InferTypes))

    shell
      .parse(args)
      .collect{ case a: RunFirrtlTransformAnnotation => a }
      .zip(expected)
      .map{ case (RunFirrtlTransformAnnotation(a), b) => a should be (b) }
  }

}
