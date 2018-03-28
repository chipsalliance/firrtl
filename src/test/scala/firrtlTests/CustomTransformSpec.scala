// See LICENSE for license details.

package firrtlTests

import firrtl.ir.Circuit
import firrtl._
import firrtl.passes.Pass
import firrtl.ir._
import firrtl.annotations.SingleStringAnnotation

case class ReplaceExtModuleAnnotation(value: String) extends SingleStringAnnotation

class ReplaceExtModuleTransform extends Transform {
  def inputForm = LowForm
  def outputForm = HighForm

  def execute(state: CircuitState): CircuitState = {state
    .annotations
    .collect{ case a: ReplaceExtModuleAnnotation => a }
    .foldLeft(state){
      case (s, ReplaceExtModuleAnnotation(circuitString)) => {
        val circuit = Parser.parse(circuitString)
        val module = circuit.modules.find(_.name == circuit.main).get
        state.copy(circuit = run(module)(state.circuit)) } }
  }

  def run(module: DefModule)(c: Circuit): Circuit = c.copy(
    modules = c.modules map {
      case ExtModule(_, name, _, _, _) if (name == module.name) => module
      case other => other
    }
  )
}

class CustomTransformSpec extends FirrtlFlatSpec {
  behavior of "Custom Transforms"

  // Simple module
  val delayModuleString = """
      |circuit Delay :
      |  module Delay :
      |    input clock : Clock
      |    input reset : UInt<1>
      |    input a : UInt<32>
      |    input en : UInt<1>
      |    output b : UInt<32>
      |
      |    reg r : UInt<32>, clock
      |    r <= r
      |    when en :
      |      r <= a
      |    b <= r
      |""".stripMargin

  they should "be able to introduce high firrtl" in {
    runFirrtlTest("CustomTransform", "/features",
                  customTransforms = List(new ReplaceExtModuleTransform),
                  annotations = AnnotationSeq(
                    List(ReplaceExtModuleAnnotation(delayModuleString))))
  }

  they should "be able to be run from an annotation" in {
    runFirrtlTest("CustomTransform", "/features",
                  customTransforms = List.empty,
                  annotations = AnnotationSeq(
                    List(ReplaceExtModuleAnnotation(delayModuleString),
                         RunFirrtlTransformAnnotation("firrtlTests.ReplaceExtModuleTransform"))))
  }
}
