package firrtlTests

import firrtl.options.Dependency
import firrtl.testutils.LeanTransformSpec
import firrtl.transforms.{CheckCombLoops, EnableFixFalseCombLoops}

class FixFalseCombLoopsSpec extends LeanTransformSpec(Seq(Dependency[CheckCombLoops])){

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

  "Breaking up direct assignments (not DoPrim on rhs)" should "not throw an exception" in {
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

    val result = compile(parse(input), Seq(EnableFixFalseCombLoops))
    val resultSerialized = result.circuit.serialize
    print(resultSerialized)
    compile(parse(resultSerialized))
  }

  "False combinational loop where var is not cat" should "not throw an exception" in {
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

  "False loop where two variables need to be split" should "not throw an exception" in {
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

  //TODO: Fix, turn into false loop for test
  "False loop where bits is over multiple values" should "not throw an exception" in {
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

  "False loop where a variable needs to be split within a cat" should "not throw an exception" in {
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
                  |    b <= cat(d, bits(a, 0, 0))
                  |    a_output <= a
                  |    b_output <= b
                  |""".stripMargin

    val result = compile(parse(input), Seq(EnableFixFalseCombLoops))
    val resultSerialized = result.circuit.serialize
    print(resultSerialized)
    compile(parse(resultSerialized))
  }

  "New false loop where output uses subword" should "not throw an exception" in {
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
                  |    b <= cat(d, bits(a, 0, 0))
                  |    a_output <= bits(a, 0, 0)
                  |    b_output <= b
                  |""".stripMargin

    val result = compile(parse(input), Seq(EnableFixFalseCombLoops))
    val resultSerialized = result.circuit.serialize
    print(resultSerialized)
    compile(parse(resultSerialized))
  }

  //TODO: Fix
  "New false loop where a is wider" should "not throw an exception" in {
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
                  |    b <= cat(d, bits(a, 0, 0))
                  |    a_output <= a
                  |    b_output <= b
                  |""".stripMargin

    val result = compile(parse(input), Seq(EnableFixFalseCombLoops))
    val resultSerialized = result.circuit.serialize
    print(resultSerialized)
    compile(parse(resultSerialized))
  }

  //TODO: Fix
  "New false loop where output uses a wider subword" should "not throw an exception" in {
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
                  |    b <= cat(d, bits(a, 0, 0))
                  |    a_output <= bits(a, 3, 2)
                  |    b_output <= b
                  |""".stripMargin

    val result = compile(parse(input), Seq(EnableFixFalseCombLoops))
    val resultSerialized = result.circuit.serialize
    print(resultSerialized)
    compile(parse(resultSerialized))
  }

  //TODO: Fix
  "New false loop where nested cat with multiple bits" should "not throw an exception" in {
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


}
