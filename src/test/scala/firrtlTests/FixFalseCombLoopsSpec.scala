package firrtlTests

import firrtl.AnnotationSeq
import firrtl.annotations.ModuleTarget
import firrtl.options.Dependency
import firrtl.testutils.LeanTransformSpec
import firrtl.transforms.{CheckCombLoops, EnableFixFalseCombLoops, ExtModulePathAnnotation}

class FixFalseCombLoopsSpec extends LeanTransformSpec(Seq(Dependency[CheckCombLoops])) {

  "False combinational loop" should "not throw an exception" in {
    val input = """circuit hasloops :
                  |  module hasloops :
                  |    input clk : Clock
                  |    input c : UInt<1>
                  |    input d : UInt<1>
                  |    output a_output : UInt<2>
                  |    output b_output : UInt<1>
                  |    wire a : UInt<2>
                  |    wire b : UInt<1>
                  |
                  |    a <= cat(b, c)
                  |    b <= xor(bits(a, 0, 0), d)
                  |    a_output <= a
                  |    b_output <= b
                  |""".stripMargin

    val result = compile(parse(input), Seq(EnableFixFalseCombLoops))

    val resultSerialized = result.circuit.serialize
    val correctForm = """circuit hasloops :
                        |  module hasloops :
                        |    input clk : Clock
                        |    input c : UInt<1>
                        |    input d : UInt<1>
                        |    output a_output : UInt<2>
                        |    output b_output : UInt<1>
                        |
                        |    wire a0 : UInt<1>
                        |    wire a1 : UInt<1>
                        |    node a = cat(a1, a0)
                        |    wire b : UInt<1>
                        |    a_output <= a
                        |    b_output <= b
                        |    a0 <= c
                        |    a1 <= b
                        |    b <= xor(a0, d)
                        |""".stripMargin

    if (resultSerialized == correctForm) {
      print("Output has correct form\n")
    } else {
      print("ERROR: Incorrect output form\n")
    }

    print(resultSerialized)
    compile(parse(resultSerialized))

  }

  "False combinational loop with an intermediate variable" should "not throw an exception" in {
    val input =
      """circuit hasloops :
        |  module hasloops :
        |    input clk : Clock
        |    input c : UInt<1>
        |    input d : UInt<1>
        |    output a_output : UInt<2>
        |    output b_output : UInt<1>
        |    wire a : UInt<2>
        |    wire e : UInt<2>
        |    wire b : UInt<1>
        |
        |    a <= e
        |    e <= cat(b, c)
        |    b <= xor(bits(a, 0, 0), d)
        |    a_output <= a
        |    b_output <= b
        |""".stripMargin

    val correctForm = """circuit hasloops :
                        |  module hasloops :
                        |    input clk : Clock
                        |    input c : UInt<1>
                        |    input d : UInt<1>
                        |    output a_output : UInt<2>
                        |    output b_output : UInt<1>
                        |
                        |    wire a0 : UInt<1>
                        |    wire a1 : UInt<1>
                        |    node a = cat(a1, a0)
                        |    wire e0 : UInt<1>
                        |    wire e1 : UInt<1>
                        |    node e = cat(e1, e0)
                        |    wire b : UInt<1>
                        |    a_output <= cat(a1, a0)
                        |    b_output <= b
                        |    a0 <= e0
                        |    a1 <= e1
                        |    e0 <= c
                        |    e1 <= b
                        |    b <= xor(a0, d)
                        |""".stripMargin

    val result = compile(parse(input), Seq(EnableFixFalseCombLoops))
    val resultSerialized = result.circuit.serialize

    if (resultSerialized == correctForm) {
      print("Output has correct form\n")
    } else {
      print("ERROR: Incorrect output form\n")
    }

    print(resultSerialized)
    assert(resultSerialized == correctForm)
    compile(parse(resultSerialized))
  }

  "False combinational loop where primitive inside cat" should "not throw an exception" in {
    val input = """circuit hasloops :
                  |  module hasloops :
                  |    input clk : Clock
                  |    input c : UInt<1>
                  |    input d : UInt<1>
                  |    input e : UInt<1>
                  |    output a_output : UInt<2>
                  |    output b_output : UInt<1>
                  |    wire a : UInt<2>
                  |    wire b : UInt<1>
                  |
                  |    a <= cat(xor(b, e), c)
                  |    b <= xor(bits(a, 0, 0), d)
                  |    a_output <= a
                  |    b_output <= b
                  |""".stripMargin

    val result = compile(parse(input), Seq(EnableFixFalseCombLoops))
    val resultSerialized = result.circuit.serialize
    print(resultSerialized)
    compile(parse(resultSerialized))
  }

  "False loop where there are two loops" should "not throw an exception" in {
    val input = """circuit hasloops :
                  |  module hasloops :
                  |    input clk : Clock
                  |    input d : UInt<1>
                  |    input e : UInt<1>
                  |    output a_output : UInt<2>
                  |    output b_output : UInt<1>
                  |    output c_output : UInt<2>
                  |    wire a : UInt<2>
                  |    wire b : UInt<1>
                  |    wire c : UInt<2>
                  |
                  |    a <= cat(b, bits(c, 0, 0))
                  |    b <= xor(bits(a, 0, 0), d)
                  |    c <= cat(bits(a, 1, 1), e)
                  |    a_output <= a
                  |    b_output <= b
                  |    c_output <= c
                  |""".stripMargin

    val result = compile(parse(input), Seq(EnableFixFalseCombLoops))
    val resultSerialized = result.circuit.serialize
    print(resultSerialized)
    compile(parse(resultSerialized))
  }

  "False loop with subword in a cat" should "not throw an exception" in {
    val input = """circuit hasloops :
                  |  module hasloops :
                  |    input clk : Clock
                  |    input c : UInt<1>
                  |    input d : UInt<1>
                  |    output a_output : UInt<3>
                  |    output b_output : UInt<2>
                  |    wire a : UInt<2>
                  |    wire b : UInt<1>
                  |
                  |    a <= cat(b, c)
                  |    b <= cat(bits(a, 0, 0), d)
                  |    a_output <= a
                  |    b_output <= b
                  |""".stripMargin

    val result = compile(parse(input), Seq(EnableFixFalseCombLoops))
    val resultSerialized = result.circuit.serialize
    print(resultSerialized)
    compile(parse(resultSerialized))
  }

  "False loop where output uses subword" should "not throw an exception" in {
    val input = """circuit hasloops :
                  |  module hasloops :
                  |    input clk : Clock
                  |    input c : UInt<1>
                  |    input d : UInt<1>
                  |    output a_output : UInt<3>
                  |    output b_output : UInt<2>
                  |    wire a : UInt<2>
                  |    wire b : UInt<1>
                  |
                  |
                  |    a <= cat(b, c)
                  |    b <= cat(bits(a, 0, 0), d)
                  |    a_output <= bits(a, 0, 0)
                  |    b_output <= b
                  |""".stripMargin

    val result = compile(parse(input), Seq(EnableFixFalseCombLoops))
    val resultSerialized = result.circuit.serialize
    print(resultSerialized)
    compile(parse(resultSerialized))
  }

  "False loop where a wider wire is assigned to a narrower value" should "not throw an exception" in {
    val input = """circuit hasloops :
                  |  module hasloops :
                  |    input clk : Clock
                  |    input c : UInt<3>
                  |    input d : UInt<1>
                  |    output a_output : UInt<4>
                  |    output b_output : UInt<2>
                  |    wire a : UInt<4>
                  |    wire b : UInt<2>
                  |
                  |    a <= cat(b, c)
                  |    b <= cat(bits(a, 0, 0), d)
                  |    a_output <= a
                  |    b_output <= b
                  |""".stripMargin

    val result = compile(parse(input), Seq(EnableFixFalseCombLoops))
    val resultSerialized = result.circuit.serialize
    print(resultSerialized)
    compile(parse(resultSerialized))
  }

  "False loop where a narrow wire is assigned to a wider value" should "not throw an exception" in {
    val input = """circuit hasloops :
                  |  module hasloops :
                  |    input clk : Clock
                  |    input c : UInt<3>
                  |    input d : UInt<1>
                  |    output a_output : UInt<4>
                  |    output b_output : UInt<2>
                  |    wire a : UInt<6>
                  |    wire b : UInt<2>
                  |
                  |    a <= cat(b, c)
                  |    b <= cat(bits(a, 0, 0), d)
                  |    a_output <= a
                  |    b_output <= b
                  |""".stripMargin

    val result = compile(parse(input), Seq(EnableFixFalseCombLoops))
    val resultSerialized = result.circuit.serialize
    print(resultSerialized)
    compile(parse(resultSerialized))
  }

  "False loop where output is assigned to a narrower wire" should "not throw an exception" in {
    val input = """circuit hasloops :
                  |  module hasloops :
                  |    input clk : Clock
                  |    input c : UInt<2>
                  |    input d : UInt<1>
                  |    output a_output : UInt<4>
                  |    output b_output : UInt<2>
                  |    wire a : UInt<4>
                  |    wire b : UInt<2>
                  |
                  |    a <= cat(b, c)
                  |    b <= cat(bits(a, 0, 0), d)
                  |    a_output <= bits(a, 3, 2)
                  |    b_output <= b
                  |""".stripMargin

    val result = compile(parse(input), Seq(EnableFixFalseCombLoops))
    val resultSerialized = result.circuit.serialize
    print(resultSerialized)
    compile(parse(resultSerialized))
  }

  //TODO: figure out if/how to do an SInt test
  "False loop where a wider wire is assigned to a narrower value SInt variation" should "not throw an exception" in {
    val input = """circuit hasloops :
                  |  module hasloops :
                  |    input clk : Clock
                  |    input c : UInt<3>
                  |    input d : UInt<1>
                  |    output a_output : SInt<4>
                  |    output b_output : UInt<2>
                  |    wire a : SInt<4>
                  |    wire b : UInt<2>
                  |
                  |    a <= asSInt(cat(b, c))
                  |    b <= cat(bits(asUInt(a), 0, 0), d)
                  |    a_output <= asSInt(a)
                  |    b_output <= b
                  |""".stripMargin

    val result = compile(parse(input), Seq(EnableFixFalseCombLoops))
    val resultSerialized = result.circuit.serialize
    print(resultSerialized)
    compile(parse(resultSerialized))
  }

  "False loop with nested cat with multiple bits" should "not throw an exception" in {
    val input = """circuit hasloops :
                  |  module hasloops :
                  |    input clk : Clock
                  |    input c : UInt<2>
                  |    input d : UInt<2>
                  |    output a_output : UInt<6>
                  |    output b_output : UInt<2>
                  |    wire a : UInt<6>
                  |    wire b : UInt<2>
                  |
                  |    a <= cat(b, cat(c, d))
                  |    b <= bits(a, 0, 0)
                  |    a_output <= a
                  |    b_output <= b
                  |""".stripMargin

    val result = compile(parse(input), Seq(EnableFixFalseCombLoops))
    val resultSerialized = result.circuit.serialize
    print(resultSerialized)
    compile(parse(resultSerialized))
  }

  "False loop with repeated names" should "not throw an exception" in {
    val input = """circuit hasloops :
                  |  module hasloops :
                  |    input clk : Clock
                  |    input c : UInt<1>
                  |    input d : UInt<1>
                  |    output a_output : UInt<2>
                  |    output a0_output : UInt<1>
                  |    wire a : UInt<2>
                  |    wire a0 : UInt<1>
                  |
                  |    a <= cat(a0, c)
                  |    a0 <= xor(bits(a, 0, 0), d)
                  |    a_output <= a
                  |    a0_output <= a0
                  |""".stripMargin

    val result = compile(parse(input), Seq(EnableFixFalseCombLoops))
    val resultSerialized = result.circuit.serialize
    print(resultSerialized)
    compile(parse(resultSerialized))
  }

  "False loop where subword is multiple bits" should "not throw an exception" in {
    val input = """circuit hasloops :
                  |  module hasloops :
                  |    input clk : Clock
                  |    input d : UInt<1>
                  |    input e : UInt<1>
                  |    input f : UInt<1>
                  |    output a_output : UInt<3>
                  |    output d_output : UInt<3>
                  |    wire a : UInt<3>
                  |
                  |    a <= cat(d, cat(e, f))
                  |    d_output <= bits(a, 2, 0)
                  |    a_output <= a
                  |""".stripMargin

    val result = compile(parse(input), Seq(EnableFixFalseCombLoops))
    val resultSerialized = result.circuit.serialize
    print(resultSerialized)
    compile(parse(resultSerialized))
  }

  "False combinational loop through a node" should "not throw an exception" in {
    val input = """circuit hasloops :
                  |  module hasloops :
                  |    input clk : Clock
                  |    input c : UInt<1>
                  |    input d : UInt<1>
                  |    output a_output : UInt<2>
                  |    output b_output : UInt<1>
                  |    wire a : UInt<2>
                  |    wire b : UInt<1>
                  |
                  |    a <= cat(b, c)
                  |    node z = bits(a, 0, 0)
                  |    b <= xor(z, d)
                  |    a_output <= a
                  |    b_output <= b
                  |""".stripMargin

    val result = compile(parse(input), Seq(EnableFixFalseCombLoops))
    val resultSerialized = result.circuit.serialize
    print(resultSerialized)
    compile(parse(resultSerialized))
  }

  "False combinational loop with a UInt literal" should "not throw an exception" in {
    val input = """circuit hasloops :
                  |  module hasloops :
                  |    input clk : Clock
                  |    input c : UInt<1>
                  |    input d : UInt<1>
                  |    output a_output : UInt<2>
                  |    output b_output : UInt<1>
                  |    wire a : UInt<2>
                  |    wire b : UInt<1>
                  |
                  |    a <= cat(b, c)
                  |    b <= xor(bits(a, 0, 0), UInt(1))
                  |    a_output <= a
                  |    b_output <= b
                  |""".stripMargin

    val result = compile(parse(input), Seq(EnableFixFalseCombLoops))
    val resultSerialized = result.circuit.serialize
    print(resultSerialized)
    compile(parse(resultSerialized))
  }

  "False combinational loop with a bundle" should "not throw an exception" in {
    val input = """circuit hasloops :
                  |  module hasloops :
                  |    wire x : { sign : UInt<1>, exponent : UInt<8>, significand : UInt<23>}
                  |    x.sign <= UInt(1)
                  |    x.exponent <= UInt(1)
                  |    x.significand <= UInt(1)
                  """.stripMargin
    val result = compile(parse(input), Seq(EnableFixFalseCombLoops))
    val resultSerialized = result.circuit.serialize
    print(resultSerialized)
    compile(parse(resultSerialized))
  }

  //TODO: Turn this into a recursive bits test
  "False loop where there is a bits within a bits" should "not throw an exception" in {
    val input = """circuit hasloops :
                  |  module hasloops :
                  |    input clk : Clock
                  |    input c : UInt<1>
                  |    input d : UInt<1>
                  |    output a_output : UInt<2>
                  |    output b_output : UInt<1>
                  |    wire a : UInt<3>
                  |    wire b : UInt<1>
                  |
                  |    a <= cat(b, c)
                  |    b <= xor(bits(bits(a, 2, 0), 0, 0), d)
                  |    a_output <= a
                  |    b_output <= b
                  |""".stripMargin

    val result = compile(parse(input), Seq(EnableFixFalseCombLoops))
    val resultSerialized = result.circuit.serialize
    print(resultSerialized)
    compile(parse(resultSerialized))
  }

  //All tests below should error/are not currently handled by this pass.
  "Combinational loop through a combinational memory read port" should "throw an exception" in {
    val input = """circuit hasloops :
                  |  module hasloops :
                  |    input clk : Clock
                  |    input a : UInt<1>
                  |    input b : UInt<1>
                  |    output c : UInt<1>
                  |    output d : UInt<1>
                  |    wire y : UInt<1>
                  |    wire z : UInt<1>
                  |    c <= b
                  |    mem m :
                  |      data-type => UInt<1>
                  |      depth => 2
                  |      read-latency => 0
                  |      write-latency => 1
                  |      reader => r
                  |      read-under-write => undefined
                  |    m.r.clk <= clk
                  |    m.r.addr <= y
                  |    m.r.en <= UInt(1)
                  |    z <= m.r.data
                  |    y <= z
                  |    d <= z
                  |""".stripMargin

    intercept[CheckCombLoops.CombLoopException] {
      compile(parse(input), Seq(EnableFixFalseCombLoops))
    }
  }

  "Real bit-level combinational loop" should "throw an exception" in {
    val input = """circuit hasloops :
                  |  module hasloops :
                  |    input clk : Clock
                  |    input a : UInt<1>
                  |    input b : UInt<1>
                  |    output c : UInt<1>
                  |    output d : UInt<1>
                  |    wire y : UInt<1>
                  |    wire z : UInt<1>
                  |    c <= b
                  |    z <= y
                  |    y <= z
                  |    d <= z
                  |""".stripMargin

    intercept[CheckCombLoops.CombLoopException] {
      compile(parse(input), Seq(EnableFixFalseCombLoops))
    }
  }

  "Combinational loop through an annotated ExtModule" should "throw an exception" in {
    val input = """circuit hasloops :
                  |  extmodule blackbox :
                  |    input in : UInt<1>
                  |    output out : UInt<1>
                  |  module hasloops :
                  |    input clk : Clock
                  |    input a : UInt<1>
                  |    input b : UInt<1>
                  |    output c : UInt<1>
                  |    output d : UInt<1>
                  |    wire y : UInt<1>
                  |    wire z : UInt<1>
                  |    c <= b
                  |    inst inner of blackbox
                  |    inner.in <= y
                  |    z <= inner.out
                  |    y <= z
                  |    d <= z
                  |""".stripMargin

    val mt = ModuleTarget("hasloops", "blackbox")
    val annos = AnnotationSeq(Seq(ExtModulePathAnnotation(mt.ref("in"), mt.ref("out")), EnableFixFalseCombLoops))
    intercept[CheckCombLoops.CombLoopException] {
      compile(parse(input), annos)
    }
  }

  "Combination loop through an instance" should "throw an exception" in {
    val input = """circuit hasloops :
                  |  module thru :
                  |    input in : UInt<1>
                  |    output out : UInt<1>
                  |    out <= in
                  |  module hasloops :
                  |    input clk : Clock
                  |    input a : UInt<1>
                  |    input b : UInt<1>
                  |    output c : UInt<1>
                  |    output d : UInt<1>
                  |    wire y : UInt<1>
                  |    wire z : UInt<1>
                  |    c <= b
                  |    inst inner of thru
                  |    inner.in <= y
                  |    z <= inner.out
                  |    y <= z
                  |    d <= z
                  |""".stripMargin

    intercept[CheckCombLoops.CombLoopException] {
      compile(parse(input), Seq(EnableFixFalseCombLoops))
    }
  }

  "Combinational loop through an output RHS reference" should "throw an exception" in {
    val input = """circuit hasloops :
                  |  module thru :
                  |    input in : UInt<1>
                  |    output tmp : UInt<1>
                  |    output out : UInt<1>
                  |    tmp <= in
                  |    out <= tmp
                  |  module hasloops :
                  |    input clk : Clock
                  |    input a : UInt<1>
                  |    input b : UInt<1>
                  |    output c : UInt<1>
                  |    output d : UInt<1>
                  |    wire y : UInt<1>
                  |    wire z : UInt<1>
                  |    c <= b
                  |    inst inner of thru
                  |    inner.in <= y
                  |    z <= inner.out
                  |    y <= z
                  |    d <= z
                  |""".stripMargin

    intercept[CheckCombLoops.CombLoopException] {
      compile(parse(input), Seq(EnableFixFalseCombLoops))
    }
  }

}
