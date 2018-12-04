// See LICENSE for license details.

package firrtlTests.stage.phases

import org.scalatest.{FlatSpec, Matchers}

import firrtl.{ChirrtlEmitter, EmitAllModulesAnnotation, Parser}
import firrtl.stage.{FirrtlCircuitAnnotation, OutputFileAnnotation}
import firrtl.stage.phases.AddImplicitOutputFile

class AddImplicitOutputFileSpec extends FlatSpec with Matchers {

  val foo = """|circuit Foo:
               |  module Foo:
               |    node a = UInt<1>("h0")
               |""".stripMargin

  val circuit = Parser.parse(foo)

  behavior of AddImplicitOutputFile.getClass.getName

  it should "default to an output file named 'a'" in {
    AddImplicitOutputFile.transform(Seq.empty).toSeq should be (Seq(OutputFileAnnotation("a")))
  }

  it should "set the output file based on a FirrtlCircuitAnnotation's main" in {
    val in = Seq(FirrtlCircuitAnnotation(circuit))
    val out = OutputFileAnnotation(circuit.main) +: in
    AddImplicitOutputFile.transform(in).toSeq should be (out)
  }

  it should "do nothing if an OutputFileAnnotation or EmitAllModulesAnnotation already exists" in {

    info("OutputFileAnnotation works")
    val outputFile = Seq(OutputFileAnnotation("Bar"), FirrtlCircuitAnnotation(circuit))
    AddImplicitOutputFile.transform(outputFile).toSeq should be (outputFile)

    info("EmitAllModulesAnnotation works")
    val eam = Seq(EmitAllModulesAnnotation(classOf[ChirrtlEmitter]), FirrtlCircuitAnnotation(circuit))
    AddImplicitOutputFile.transform(eam).toSeq should be (eam)
  }

}
