// See LICENSE for license details.

package firrtlTests

import java.io.File

import firrtl._
import firrtl.annotations.Annotation
import firrtl.annotations.AnnotationYamlProtocol._
import firrtl.passes.InlineInstances
import firrtl.passes.memlib.{InferReadWrite, ReplSeqMem}
import net.jcazevedo.moultingyaml._
import org.scalatest.{FreeSpec, Matchers}
import java.io.StringWriter

class InternalErrorSpec extends FreeSpec with Matchers with BackendCompilationUtilities {
  "MatchErrors appear as InternalErrors" - {
    val input =
      """
        |circuit Dummy :
        |  module Dummy :
        |    input clock : Clock
        |    input x : UInt<1>
        |    output y : UInt<1>
        |    output io : { flip in : UInt<16>, out : UInt<16> }
        |;    io is invalid
        |;    io.out <= io.in
        |    y <= shr(x, UInt(1))
        |      """.stripMargin
    val manager = new ExecutionOptionsManager("test") with HasFirrtlOptions {
      commonOptions = CommonOptions(topName = "Dummy")
      firrtlOptions = FirrtlExecutionOptions(firrtlSource = Some(input), compilerName = "low")
    }

    val output = firrtl.Driver.execute(manager)
    println(output)
  }
}
