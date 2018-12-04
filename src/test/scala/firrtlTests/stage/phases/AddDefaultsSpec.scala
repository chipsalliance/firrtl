// See LICENSE for license details.

package firrtlTests.stage.phases

import org.scalatest.{FlatSpec, Matchers}

import firrtl.NoneCompiler
import firrtl.annotations.Annotation
import firrtl.stage.phases.AddDefaults
import firrtl.transforms.BlackBoxTargetDirAnno
import firrtl.stage.{CompilerAnnotation, InfoModeAnnotation}
import firrtl.options.TargetDirAnnotation

class AddDefaultsSpec extends FlatSpec with Matchers {

  behavior of AddDefaults.getClass.getName

  it should "add expected default annotations and nothing else" in {
    val expected = Seq(
      (a: Annotation) => a match { case BlackBoxTargetDirAnno(b) => b == TargetDirAnnotation().directory },
      (a: Annotation) => a match { case CompilerAnnotation(b) => b.getClass == CompilerAnnotation().compiler.getClass },
      (a: Annotation) => a match { case InfoModeAnnotation(b) => b == InfoModeAnnotation().modeName } )

    AddDefaults.transform(Seq.empty).zip(expected).map { case (x, f) => f(x) should be (true) }
  }

  it should "not overwrite existing annotations" in {
    val input = Seq(
      BlackBoxTargetDirAnno("foo"),
      CompilerAnnotation(new NoneCompiler()),
      InfoModeAnnotation("ignore"))

    AddDefaults.transform(input).toSeq should be (input)
  }
}
