package firrtlTests.coreir

import java.io._

import org.scalatest._
import org.scalatest.prop._
import firrtl.{ChirrtlForm, CircuitState, CoreIRCompiler, Parser}
import firrtl.ir.Circuit
import firrtl.passes.{CheckGenders, CheckHighForm, CheckTypes, CheckWidths, InferTypes, InferWidths, Pass, PassExceptions, ResolveGenders, ResolveKinds, ToWorkingIR}
import firrtlTests.FirrtlFlatSpec

class CoreIRTests extends FirrtlFlatSpec {
  "Fixed types" should "infer add correctly" in {
    val input =
      s"""circuit add4:
          |  module add4:
          |    input in: UInt<16>[4]
          |    output out: UInt<16>
          |    out <= add(add(in[0], in[1]), add(in[2], in[3]))
          |    """.stripMargin
    val check =
      s"""{
         |  "top": "global.add4",
         |  "namespaces": {
         |    "global": {
         |      "modules": {
         |        "add4": {
         |          "type": ["Record",{
         |            "in": ["Array",4,["Array",16,"BitIn"]],
         |            "out": ["Array",16,"Bit"]
         |          }],
         |          "instances": {
         |            "i00": {
         |              "genref": "coreir.add",
         |              "genargs": {"width":["Int", 16]}
         |            },
         |            "i01": {
         |              "genref": "coreir.add",
         |              "genargs": {"width":["Int", 16]}
         |            },
         |            "i1": {
         |              "genref": "coreir.add",
         |              "genargs": {"width":["Int", 16]}
         |            }
         |          },
         |          "connections": [
         |            ["self.in.0","i00.in0"],
         |            ["self.in.1","i00.in1"],
         |            ["self.in.2","i01.in0"],
         |            ["self.in.3","i01.in1"],
         |            ["i00.out","i1.in0"],
         |            ["i01.out","i1.in1"],
         |            ["i1.out","self.out"]
         |          ]
         |        }
         |      }
         |    }
         |  }
         |}
       """.stripMargin

    val lowerer = new CoreIRCompiler()

    val res = lowerer.compileAndEmit(CircuitState(parse(input), ChirrtlForm))

    val output = res.getEmittedCircuit.value
    output should be (check)
  }

}
