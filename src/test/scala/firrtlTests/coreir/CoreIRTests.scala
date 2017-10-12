package firrtlTests.coreir

import firrtl.{ChirrtlForm, CircuitState, CoreIRCompiler}
import firrtlTests.FirrtlFlatSpec

import play.api.libs.json._

class CoreIRTests extends FirrtlFlatSpec {
  "Fixed types" should "infer add correctly" in {
    val input =
      s"""circuit add4:
          |  module add4:
          |    input in: UInt<16>[4]
          |    output out: UInt<16>
          |    out <= tail(add(tail(add(in[0], in[1]), 1), tail(add(in[2], in[3]), 1)), 1)
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
         |            "addw": {
         |              "genref": "coreir.add",
         |              "genargs": {"width":["Int", 16]}
         |            },
         |            "addw_0": {
         |              "genref": "coreir.add",
         |              "genargs": {"width":["Int", 16]}
         |            },
         |            "addw_1": {
         |              "genref": "coreir.add",
         |              "genargs": {"width":["Int", 16]}
         |            }
         |          },
         |          "connections": [
         |            ["self.in.0","addw.in0"],
         |            ["self.in.1","addw.in1"],
         |            ["self.in.2","addw_0.in0"],
         |            ["self.in.3","addw_0.in1"],
         |            ["addw.out","addw_1.in0"],
         |            ["addw_0.out","addw_1.in1"],
         |            ["addw_1.out","self.out"]
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
    Json.parse(output) should be (Json.parse(check))
  }

}
