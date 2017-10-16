package firrtlTests.coreir

import firrtl.{ChirrtlForm, CircuitState, CoreIRCompiler}
import firrtlTests.FirrtlFlatSpec

import play.api.libs.json._

class CoreIRTests extends FirrtlFlatSpec {
  "4-bit add" should "emit CoreIR correctly" in {
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

  "Constant add" should "emit CoreIR correctly" in {
    val input =
      s"""circuit addconst:
         |  module addconst:
         |    input in: UInt<1>
         |    output out: UInt<1>
         |    out <= tail(add(in, UInt(1)), 1)
         |    """.stripMargin
    val check =
      s"""{
         |  "top": "global.addconst",
         |  "namespaces": {
         |    "global": {
         |      "modules": {
         |        "addconst": {
         |          "type": ["Record",{
         |            "in": ["Array",1,"BitIn"],
         |            "out": ["Array",1,"Bit"]
         |          }],
         |          "instances": {
         |            "uint1":{
         |              "genref": "coreir.const",
         |              "genargs": {"width":["Int", 1]},
         |              "modargs": {"value":[["BitVector", 1], 1]}
         |            },
         |            "addw": {
         |              "genref": "coreir.add",
         |              "genargs": {"width":["Int", 1]}
         |            }
         |          },
         |          "connections": [
         |            ["self.in","addw.in0"],
         |            ["uint1.out","addw.in1"],
         |            ["addw.out","self.out"]
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

  "Padded constant add" should "emit CoreIR correctly" in {
    val input =
      s"""circuit addconst:
         |  module addconst:
         |    input in: UInt<16>
         |    output out: UInt<16>
         |    out <= tail(add(in, pad(UInt(1), 16)), 1)
         |    """.stripMargin
    val check =
      s"""{
         |  "top": "global.addconst",
         |  "namespaces": {
         |    "global": {
         |      "modules": {
         |        "addconst": {
         |          "type": ["Record",{
         |            "in": ["Array",16,"BitIn"],
         |            "out": ["Array",16,"Bit"]
         |          }],
         |          "instances": {
         |            "uint0": {
         |              "genref": "coreir.const",
         |              "genargs": {"width":["Int", 15]},
         |              "modargs": {"value":[["BitVector", 15], 0]}
         |            },
         |            "uint1":{
         |              "genref": "coreir.const",
         |              "genargs": {"width":["Int", 1]},
         |              "modargs": {"value":[["BitVector", 1], 1]}
         |            },
         |            "cat": {
         |              "genref": "coreir.cat",
         |              "genargs": {
         |                "width0":["Int", 15],
         |                "width1":["Int", 1]
         |              }
         |            },
         |            "addw": {
         |              "genref": "coreir.add",
         |              "genargs": {"width":["Int", 16]}
         |            }
         |          },
         |          "connections": [
         |            ["uint0.out","cat.in0"],
         |            ["uint1.out","cat.in1"],
         |            ["self.in","addw.in0"],
         |            ["cat.out","addw.in1"],
         |            ["addw.out","self.out"]
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

  "Constant mixed width add" should "emit CoreIR correctly" in {
    val input =
      s"""circuit addconst:
         |  module addconst:
         |    input in: UInt<16>
         |    output out: UInt<16>
         |    out <= tail(add(in, UInt(1)), 1)
         |    """.stripMargin
    val check =
      s"""{
         |  "top": "global.addconst",
         |  "namespaces": {
         |    "global": {
         |      "modules": {
         |        "addconst": {
         |          "type": ["Record",{
         |            "in": ["Array",16,"BitIn"],
         |            "out": ["Array",16,"Bit"]
         |          }],
         |          "instances": {
         |            "uint0": {
         |              "genref": "coreir.const",
         |              "genargs": {"width":["Int", 15]},
         |              "modargs": {"value":[["BitVector", 15], 0]}
         |            },
         |            "uint1":{
         |              "genref": "coreir.const",
         |              "genargs": {"width":["Int", 1]},
         |              "modargs": {"value":[["BitVector", 1], 1]}
         |            },
         |            "cat": {
         |              "genref": "coreir.cat",
         |              "genargs": {
         |                "width0":["Int", 15],
         |                "width1":["Int", 1]
         |              }
         |            },
         |            "addw": {
         |              "genref": "coreir.add",
         |              "genargs": {"width":["Int", 16]}
         |            }
         |          },
         |          "connections": [
         |            ["uint0.out","cat.in0"],
         |            ["uint1.out","cat.in1"],
         |            ["self.in","addw.in0"],
         |            ["cat.out","addw.in1"],
         |            ["addw.out","self.out"]
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

  "Bit select" should "emit CoreIR correctly" in {
    val input =
      s"""circuit addconst:
         |  module addconst:
         |    input in: UInt<16>
         |    output out: UInt<5>
         |    out <= bits(in, 13, 9)
         |    """.stripMargin
    val check =
      s"""{
         |  "top": "global.addconst",
         |  "namespaces": {
         |    "global": {
         |      "modules": {
         |        "addconst": {
         |          "type": ["Record",{
         |            "in": ["Array",16,"BitIn"],
         |            "out": ["Array",5,"Bit"]
         |          }],
         |          "instances": {
         |            "bits": {
         |              "genref": "coreir.slice",
         |              "genargs": {
         |                "width":["Int", 5],
         |                "low":["Int", 9],
         |                "hi":["Int", 14]
         |              }
         |            }
         |          },
         |          "connections": [
         |            ["self.in","bits.in"],
         |            ["bits.out","self.out"]
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
