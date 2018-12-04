// See LICENSE for license details.

package firrtlTests.stage.phases

import org.scalatest.{FlatSpec, Matchers}

import firrtl.{EmitAllModulesAnnotation, EmitCircuitAnnotation, HighFirrtlEmitter, VerilogCompiler}
import firrtl.annotations.NoTargetAnnotation
import firrtl.stage.{CompilerAnnotation, RunFirrtlTransformAnnotation}
import firrtl.stage.phases.AddImplicitEmitter

class AddImplicitEmitterSpec extends FlatSpec with Matchers {

  case class FooAnnotation(x: Int) extends NoTargetAnnotation
  case class BarAnnotation(x: String) extends NoTargetAnnotation

  val someAnnos = Seq(FooAnnotation(1), FooAnnotation(2), BarAnnotation("bar"))

  behavior of AddImplicitEmitter.getClass.getName

  it should "do nothing if no CompilerAnnotation is present" in {
    AddImplicitEmitter.transform(someAnnos).toSeq should be (someAnnos)
  }

  it should "add an EmitCircuitAnnotation derived from a CompilerAnnotation" in {
    val input = CompilerAnnotation(new VerilogCompiler) +: someAnnos
    val expected = input.flatMap{
      case a@ CompilerAnnotation(b) => Seq(a,
                                           RunFirrtlTransformAnnotation(b.emitter),
                                           EmitCircuitAnnotation(b.emitter.getClass))
      case a => Some(a)
    }
    AddImplicitEmitter.transform(input).toSeq should be (expected)
  }

  it should "not add an EmitCircuitAnnotation if an EmitAnnotation already exists" in {
    val input = Seq(CompilerAnnotation(new VerilogCompiler),
                    EmitAllModulesAnnotation(classOf[HighFirrtlEmitter])) ++ someAnnos
    AddImplicitEmitter.transform(input).toSeq should be (input)
  }

}
