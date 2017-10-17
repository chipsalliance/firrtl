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
         |              "genref": "coreir.concat",
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
         |              "genref": "coreir.concat",
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
         |                "width":["Int", 16],
         |                "lo":["Int", 9],
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

  "And, or, xor, not" should "emit CoreIR correctly" in {
    val input =
      s"""circuit addconst:
         |  module addconst:
         |    input a: UInt<16>
         |    input b: UInt<16>
         |    input c: UInt<16>
         |    output out: UInt<16>
         |    out <= not(xor(and(a, b), or(b, c)))
         |    """.stripMargin
    val check =
      s"""{
         |  "top": "global.addconst",
         |  "namespaces": {
         |    "global": {
         |      "modules": {
         |        "addconst": {
         |          "type": ["Record",{
         |            "a": ["Array",16,"BitIn"],
         |            "b": ["Array",16,"BitIn"],
         |            "c": ["Array",16,"BitIn"],
         |            "out": ["Array",16,"Bit"]
         |          }],
         |          "instances": {
         |            "and": {
         |              "genref": "coreir.and",
         |              "genargs": {"width":["Int", 16]}
         |            },
         |            "or": {
         |              "genref": "coreir.or",
         |              "genargs": {"width":["Int", 16]}
         |            },
         |            "xor": {
         |              "genref": "coreir.xor",
         |              "genargs": {"width":["Int", 16]}
         |            },
         |            "not": {
         |              "genref": "coreir.not",
         |              "genargs": {"width":["Int", 16]}
         |            }
         |          },
         |          "connections": [
         |            ["self.a","and.in0"],
         |            ["self.b","and.in1"],
         |            ["self.b","or.in0"],
         |            ["self.c","or.in1"],
         |            ["and.out","xor.in0"],
         |            ["or.out","xor.in1"],
         |            ["xor.out","not.in"],
         |            ["not.out","self.out"]
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

  "Register" should "emit CoreIR correctly" in {
    val input =
      s"""circuit addconst:
         |  module addconst:
         |    input in: UInt<16>
         |    input clk: Clock
         |    output out: UInt<16>
         |    reg r: UInt<16>, clk
         |    r <= in
         |    out <= r
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
         |            "clk": ["Named","coreir.clkIn"],
         |            "out": ["Array",16,"Bit"]
         |          }],
         |          "instances": {
         |            "r": {
         |              "genref": "mantle.reg",
         |              "genargs":{"has_en":["Bool",false], "has_rst":["Bool",false], "width":["Int",16], "has_clr":["Bool",false]},
         |              "modargs":{"init":[["BitVector",16],0]}
         |            }
         |          },
         |          "connections": [
         |            ["self.clk","r.clk"],
         |            ["r.out","self.out"],
         |            ["self.in","r.in"]
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

  "Register with reset" should "emit CoreIR correctly" in {
    val input =
      s"""circuit addconst:
         |  module addconst:
         |    input in: UInt<16>
         |    input clk: Clock
         |    input reset: UInt<1>
         |    output out: UInt<16>
         |    reg r: UInt<16>, clk with: (reset => (reset, UInt<16>(0)))
         |    r <= in
         |    out <= r
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
         |            "clk": ["Named","coreir.clkIn"],
         |            "reset": ["Array",1,"BitIn"],
         |            "out": ["Array",16,"Bit"]
         |          }],
         |          "instances": {
         |            "r": {
         |              "genref": "mantle.reg",
         |              "genargs":{"has_en":["Bool",false], "has_rst":["Bool",false], "width":["Int",16], "has_clr":["Bool",true]},
         |              "modargs":{"init":[["BitVector",16],0]}
         |            }
         |          },
         |          "connections": [
         |            ["self.clk","r.clk"],
         |            ["self.reset","r.clear"],
         |            ["r.out","self.out"],
         |            ["self.in","r.in"]
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

  "Unsigned comparison operators" should "emit CoreIR correctly" in {
    val input =
      s"""circuit addconst:
         |  module addconst:
         |    input a: UInt<16>
         |    input b: UInt<16>
         |    output out0: UInt<1>
         |    output out1: UInt<1>
         |    output out2: UInt<1>
         |    output out3: UInt<1>
         |    output out4: UInt<1>
         |    out0 <= eq(a, b)
         |    out1 <= lt(a, b)
         |    out2 <= leq(a, b)
         |    out3 <= gt(a, b)
         |    out4 <= geq(a, b)
         |    """.stripMargin
    val check =
      s"""{
         |  "top": "global.addconst",
         |  "namespaces": {
         |    "global": {
         |      "modules": {
         |        "addconst": {
         |          "type": ["Record",{
         |            "out2": ["Array",1,"Bit"],
         |            "a": ["Array",16,"BitIn"],
         |            "out3": ["Array",1,"Bit"],
         |            "out0": ["Array",1,"Bit"],
         |            "b": ["Array",16,"BitIn"],
         |            "out4": ["Array",1,"Bit"],
         |            "out1": ["Array",1,"Bit"]
         |          }],
         |          "instances": {
         |            "leq": {
         |              "genref": "coreir.ule",
         |              "genargs": {"width":["Int", 16]}
         |            },
         |            "geq": {
         |              "genref": "coreir.uge",
         |              "genargs": {"width":["Int", 16]}
         |            },
         |            "lt": {
         |              "genref": "coreir.ult",
         |              "genargs": {"width":["Int", 16]}
         |            },
         |            "gt": {
         |              "genref": "coreir.ugt",
         |              "genargs": {"width":["Int", 16]}
         |            },
         |            "eq": {
         |              "genref": "coreir.eq",
         |              "genargs": {"width":["Int", 16]}
         |            }
         |          },
         |          "connections": [
         |            ["self.a","eq.in0"],
         |            ["self.b","eq.in1"],
         |            ["eq.out", "self.out0"],
         |            ["self.a","lt.in0"],
         |            ["self.b","lt.in1"],
         |            ["lt.out", "self.out1"],
         |            ["self.a","leq.in0"],
         |            ["self.b","leq.in1"],
         |            ["leq.out", "self.out2"],
         |            ["self.a","gt.in0"],
         |            ["self.b","gt.in1"],
         |            ["gt.out", "self.out3"],
         |            ["self.a","geq.in0"],
         |            ["self.b","geq.in1"],
         |            ["geq.out", "self.out4"]
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

  "Signed comparison operators" should "emit CoreIR correctly" in {
    val input =
      s"""circuit addconst:
         |  module addconst:
         |    input a: SInt<16>
         |    input b: SInt<16>
         |    output out0: UInt<1>
         |    output out1: UInt<1>
         |    output out2: UInt<1>
         |    output out3: UInt<1>
         |    output out4: UInt<1>
         |    out0 <= eq(a, b)
         |    out1 <= lt(a, b)
         |    out2 <= leq(a, b)
         |    out3 <= gt(a, b)
         |    out4 <= geq(a, b)
         |    """.stripMargin
    val check =
      s"""{
         |  "top": "global.addconst",
         |  "namespaces": {
         |    "global": {
         |      "modules": {
         |        "addconst": {
         |          "type": ["Record",{
         |            "out2": ["Array",1,"Bit"],
         |            "a": ["Array",16,"BitIn"],
         |            "out3": ["Array",1,"Bit"],
         |            "out0": ["Array",1,"Bit"],
         |            "b": ["Array",16,"BitIn"],
         |            "out4": ["Array",1,"Bit"],
         |            "out1": ["Array",1,"Bit"]
         |          }],
         |          "instances": {
         |            "leq": {
         |              "genref": "coreir.sle",
         |              "genargs": {"width":["Int", 16]}
         |            },
         |            "geq": {
         |              "genref": "coreir.sge",
         |              "genargs": {"width":["Int", 16]}
         |            },
         |            "lt": {
         |              "genref": "coreir.slt",
         |              "genargs": {"width":["Int", 16]}
         |            },
         |            "gt": {
         |              "genref": "coreir.sgt",
         |              "genargs": {"width":["Int", 16]}
         |            },
         |            "eq": {
         |              "genref": "coreir.eq",
         |              "genargs": {"width":["Int", 16]}
         |            }
         |          },
         |          "connections": [
         |            ["self.a","eq.in0"],
         |            ["self.b","eq.in1"],
         |            ["eq.out", "self.out0"],
         |            ["self.a","lt.in0"],
         |            ["self.b","lt.in1"],
         |            ["lt.out", "self.out1"],
         |            ["self.a","leq.in0"],
         |            ["self.b","leq.in1"],
         |            ["leq.out", "self.out2"],
         |            ["self.a","gt.in0"],
         |            ["self.b","gt.in1"],
         |            ["gt.out", "self.out3"],
         |            ["self.a","geq.in0"],
         |            ["self.b","geq.in1"],
         |            ["geq.out", "self.out4"]
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

  "Constant left shifts" should "emit CoreIR correctly" in {
    val input =
      s"""circuit addconst:
         |  module addconst:
         |    input in: UInt<5>
         |    output out: UInt<10>
         |    out <= shl(in, 5)
         |    """.stripMargin
    val check =
      s"""{
         |  "top": "global.addconst",
         |  "namespaces": {
         |    "global": {
         |      "modules": {
         |        "addconst": {
         |          "type": ["Record",{
         |            "in": ["Array",5,"BitIn"],
         |            "out": ["Array",10,"Bit"]
         |          }],
         |          "instances": {
         |            "uint0": {
         |              "genref": "coreir.const",
         |              "genargs": {"width":["Int", 5]},
         |              "modargs": {"value":[["BitVector", 5], 0]}
         |            },
         |            "cat": {
         |              "genref": "coreir.concat",
         |              "genargs": {
         |                "width0":["Int", 5],
         |                "width1":["Int", 5]
         |              }
         |            },
         |            "uint5": {
         |              "genref": "coreir.const",
         |              "genargs": {"width":["Int", 10]},
         |              "modargs": {"value":[["BitVector", 10], 5]}
         |            },
         |            "dshl": {
         |              "genref": "coreir.shl",
         |              "genargs": { "width":["Int", 10] }
         |            }
         |          },
         |          "connections": [
         |            ["uint0.out","cat.in0"],
         |            ["self.in","cat.in1"],
         |            ["cat.out","dshl.in0"],
         |            ["uint5.out","dshl.in1"],
         |            ["dshl.out","self.out"]
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
