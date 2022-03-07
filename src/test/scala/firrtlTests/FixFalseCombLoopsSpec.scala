package firrtlTests

import firrtl.AnnotationSeq
import firrtl.annotations.ModuleTarget
import firrtl.options.Dependency
import firrtl.testutils.LeanTransformSpec
import firrtl.transforms.{CheckCombLoops, EnableFixFalseCombLoops, ExtModulePathAnnotation}

class FixFalseCombLoopsSpec extends LeanTransformSpec(Seq(Dependency[CheckCombLoops])) {

  private def runTest(input: String, expectedOut: String): Unit = {
    val result = compile(parse(input), Seq(EnableFixFalseCombLoops))
    val resultSerialized = result.circuit.serialize
    assert(logicalEquiv(resultSerialized, expectedOut))
    compile(parse(resultSerialized))
  }

  /** Ensures that the result and expectedOutput are permutations (i.e. logically equivalent) */
  private def logicalEquiv(resultSerialized: String, expectedOutput: String): Boolean = {
    println(resultSerialized)

    val resultLines = resultSerialized.split("(\\r\\n|\\r|\\n)").toSet
    val outputLines = expectedOutput.split("(\\r\\n|\\r|\\n)").toSet

    (resultLines.subsetOf(outputLines)) && (outputLines.subsetOf(resultLines))
  }

  "False combinational loop" should "not throw an exception" in {
    val firrtlInput = """circuit hasloops :
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

    val expectedOut = """circuit hasloops :
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
                        |    a_output <= cat(a1, a0)
                        |    b_output <= b
                        |    a0 <= c
                        |    a1 <= b
                        |    b <= xor(a0, d)
                        |""".stripMargin

    runTest(firrtlInput, expectedOut)
  }

  "False combinational loop with an intermediate variable" should "not throw an exception" in {
    val firrtlInput = """circuit hasloops :
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

    val expectedOut = """circuit hasloops :
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

    runTest(firrtlInput, expectedOut)
  }

  "False combinational loop where primitive inside cat" should "not throw an exception" in {
    val firrtlInput = """circuit hasloops :
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

    val expectedOut = """circuit hasloops :
                        |  module hasloops :
                        |    input clk : Clock
                        |    input c : UInt<1>
                        |    input d : UInt<1>
                        |    input e : UInt<1>
                        |    output a_output : UInt<2>
                        |    output b_output : UInt<1>
                        |
                        |    wire a0 : UInt<1>
                        |    wire a1 : UInt<1>
                        |    node a = cat(a1, a0)
                        |    wire b : UInt<1>
                        |    a_output <= cat(a1, a0)
                        |    b_output <= b
                        |    a1 <= xor(b, e)
                        |    a0 <= c
                        |    b <= xor(a0, d)
                        |""".stripMargin

    runTest(firrtlInput, expectedOut)
  }

  "False loop where there are two loops" should "not throw an exception" in {
    val firrtlInput = """circuit hasloops :
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

    val expectedOut = """circuit hasloops :
                        |  module hasloops :
                        |    input clk : Clock
                        |    input d : UInt<1>
                        |    input e : UInt<1>
                        |    output a_output : UInt<2>
                        |    output b_output : UInt<1>
                        |    output c_output : UInt<2>
                        |
                        |    wire a0 : UInt<1>
                        |    wire a1 : UInt<1>
                        |    node a = cat(a1, a0)
                        |    wire b : UInt<1>
                        |    wire c0 : UInt<1>
                        |    wire c1 : UInt<1>
                        |    node c = cat(c1, c0)
                        |    a_output <= cat(a1, a0)
                        |    b_output <= b
                        |    c_output <= cat(c1, c0)
                        |    a1 <= b
                        |    a0 <= c0
                        |    b <= xor(a0, d)
                        |    c0 <= e
                        |    c1 <= a1
                        |""".stripMargin

    runTest(firrtlInput, expectedOut)
  }

  "False loop with subword in a cat" should "not throw an exception" in {
    val firrtlInput = """circuit hasloops :
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

    val expectedOut = """circuit hasloops :
                        |  module hasloops :
                        |    input clk : Clock
                        |    input c : UInt<1>
                        |    input d : UInt<1>
                        |    output a_output : UInt<3>
                        |    output b_output : UInt<2>
                        |
                        |    wire a0 : UInt<1>
                        |    wire a1 : UInt<1>
                        |    node a = cat(a1, a0)
                        |    wire b : UInt<1>
                        |    a_output <= cat(a1, a0)
                        |    b_output <= b
                        |    a1 <= b
                        |    a0 <= c
                        |    b <= cat(a0, d)
                        |""".stripMargin

    runTest(firrtlInput, expectedOut)
  }

  //TODO: This output is wrong
  "False loop where output uses subword" should "not throw an exception" in {
    val firrtlInput = """circuit hasloops :
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

    val expectedOut = """circuit hasloops :
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
                        |    a_output <= a0
                        |    b_output <= b
                        |    a0 <= c
                        |    a1 <= b
                        |    b <= xor(a0, d)
                        |""".stripMargin

    runTest(firrtlInput, expectedOut)
  }

  "False loop where a wider wire is assigned to a narrower value" should "not throw an exception" in {
    val firrtlInput = """circuit hasloops :
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

    val expectedOut = """circuit hasloops :
                        |  module hasloops :
                        |    input clk : Clock
                        |    input c : UInt<3>
                        |    input d : UInt<1>
                        |    output a_output : UInt<4>
                        |    output b_output : UInt<2>
                        |
                        |    wire a0 : UInt<1>
                        |    wire a1 : UInt<1>
                        |    wire a2 : UInt<1>
                        |    wire a3 : UInt<1>
                        |    node a = cat(a3, cat(a2, cat(a1, a0)))
                        |    wire b0 : UInt<1>
                        |    wire b1 : UInt<1>
                        |    node b = cat(b1, b0)
                        |    a_output <= cat(a3, cat(a2, cat(a1, a0)))
                        |    b_output <= cat(b1, b0)
                        |    a1 <= bits(c, 1, 1)
                        |    a2 <= bits(c, 2, 2)
                        |    a0 <= bits(c, 0, 0)
                        |    a3 <= b0
                        |    b0 <= d
                        |    b1 <= a0
                        |""".stripMargin

    runTest(firrtlInput, expectedOut)
  }

  "False loop where a narrow wire is assigned to a wider value" should "not throw an exception" in {
    val firrtlInput = """circuit hasloops :
                        |  module hasloops :
                        |    input clk : Clock
                        |    input c : UInt<3>
                        |    input d : UInt<1>
                        |    output a_output : UInt<6>
                        |    output b_output : UInt<2>
                        |    wire a : UInt<6>
                        |    wire b : UInt<2>
                        |
                        |    a <= cat(b, c)
                        |    b <= cat(bits(a, 0, 0), d)
                        |    a_output <= a
                        |    b_output <= b
                        |""".stripMargin

    val expectedOut = """circuit hasloops :
                        |  module hasloops :
                        |    input clk : Clock
                        |    input c : UInt<3>
                        |    input d : UInt<1>
                        |    output a_output : UInt<6>
                        |    output b_output : UInt<2>
                        |
                        |    wire a0 : UInt<1>
                        |    wire a1 : UInt<1>
                        |    wire a2 : UInt<1>
                        |    wire a3 : UInt<1>
                        |    wire a4 : UInt<1>
                        |    wire a5 : UInt<1>
                        |    node a = cat(a5, cat(a4, cat(a3, cat(a2, cat(a1, a0)))))
                        |    wire b0 : UInt<1>
                        |    wire b1 : UInt<1>
                        |    node b = cat(b1, b0)
                        |    a_output <= cat(a5, cat(a4, cat(a3, cat(a2, cat(a1, a0)))))
                        |    b_output <= cat(b1, b0)
                        |    a5 <= UInt<1>("h0")
                        |    a4 <= b1
                        |    a1 <= bits(c, 1, 1)
                        |    a2 <= bits(c, 2, 2)
                        |    a0 <= bits(c, 0, 0)
                        |    a3 <= b0
                        |    b0 <= d
                        |    b1 <= a0
                        |""".stripMargin

    runTest(firrtlInput, expectedOut)
  }

  //TODO: Ensure this is the correct output
  "False loop where a narrow wire is assigned to a wider value SInt variation bad var extend" should "not throw an exception" in {
    val firrtlInput = """circuit hasloops :
                        |  module hasloops :
                        |    input clk : Clock
                        |    input c : UInt<3>
                        |    input d : UInt<1>
                        |    output a_output : SInt<6>
                        |    output b_output : UInt<2>
                        |    wire a : SInt<6>
                        |    wire b : UInt<2>
                        |
                        |    a <= asSInt(cat(b, c))
                        |    b <= cat(bits(a, 0, 0), d)
                        |    a_output <= asSInt(a)
                        |    b_output <= b
                        |""".stripMargin

    val expectedOut = """circuit hasloops :
                        |  module hasloops :
                        |    input clk : Clock
                        |    input c : UInt<3>
                        |    input d : UInt<1>
                        |    output a_output : SInt<6>
                        |    output b_output : UInt<2>
                        |
                        |    wire a0 : SInt<1>
                        |    wire a1 : SInt<1>
                        |    wire a2 : SInt<1>
                        |    wire a3 : SInt<1>
                        |    wire a4 : SInt<1>
                        |    wire a5 : SInt<1>
                        |    node a = cat(a5, cat(a4, cat(a3, cat(a2, cat(a1, a0)))))
                        |    wire b0 : UInt<1>
                        |    wire b1 : UInt<1>
                        |    node b = cat(b1, b0)
                        |    a_output <= asSInt(cat(a5, cat(a4, cat(a3, cat(a2, cat(a1, a0))))))
                        |    b_output <= cat(b1, b0)
                        |    a5 <= asSInt(b1)
                        |    a4 <= asSInt(b1)
                        |    a1 <= asSInt(bits(c, 1, 1))
                        |    a2 <= asSInt(bits(c, 2, 2))
                        |    a0 <= asSInt(bits(c, 0, 0))
                        |    a3 <= asSInt(b0)
                        |    b0 <= d
                        |    b1 <= asUInt(a0)
                        |""".stripMargin

    runTest(firrtlInput, expectedOut)
  }

  //TODO: Ensure this is the correct output
  "False loop where a narrow wire is assigned to a wider value SInt variation good var extend" should "not throw an exception" in {
    val firrtlInput = """circuit hasloops :
                        |  module hasloops :
                        |    input clk : Clock
                        |    input c : UInt<3>
                        |    input d : UInt<1>
                        |    output a_output : SInt<6>
                        |    output b_output : UInt<2>
                        |    wire a : SInt<6>
                        |    wire b : UInt<2>
                        |
                        |    a <= asSInt(cat(c, b))
                        |    b <= cat(bits(a, 0, 0), d)
                        |    a_output <= asSInt(a)
                        |    b_output <= b
                        |""".stripMargin

    val expectedOut = """circuit hasloops :
                        |  module hasloops :
                        |    input clk : Clock
                        |    input c : UInt<3>
                        |    input d : UInt<1>
                        |    output a_output : SInt<6>
                        |    output b_output : UInt<2>
                        |
                        |    wire a0 : SInt<1>
                        |    wire a1 : SInt<1>
                        |    wire a2 : SInt<1>
                        |    wire a3 : SInt<1>
                        |    wire a4 : SInt<1>
                        |    wire a5 : SInt<1>
                        |    node a = cat(a5, cat(a4, cat(a3, cat(a2, cat(a1, a0)))))
                        |    wire b0 : UInt<1>
                        |    wire b1 : UInt<1>
                        |    node b = cat(b1, b0)
                        |    a_output <= asSInt(cat(a5, cat(a4, cat(a3, cat(a2, cat(a1, a0))))))
                        |    b_output <= cat(b1, b0)
                        |    a5 <= asSInt(bits(c, 2, 2))
                        |    a4 <= asSInt(bits(c, 2, 2))
                        |    a1 <= asSInt(b1)
                        |    a2 <= asSInt(bits(c, 0, 0))
                        |    a0 <= asSInt(b0)
                        |    a3 <= asSInt(bits(c, 1, 1))
                        |    b0 <= d
                        |    b1 <= asUInt(a0)
                        |""".stripMargin

    runTest(firrtlInput, expectedOut)
  }

  "False loop where output is assigned to a narrower wire" should "not throw an exception" in {
    val firrtlInput = """circuit hasloops :
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

    val expectedOut = """circuit hasloops :
                        |  module hasloops :
                        |    input clk : Clock
                        |    input c : UInt<2>
                        |    input d : UInt<1>
                        |    output a_output : UInt<4>
                        |    output b_output : UInt<2>
                        |
                        |    wire a0 : UInt<1>
                        |    wire a1 : UInt<1>
                        |    wire a2 : UInt<1>
                        |    wire a3 : UInt<1>
                        |    node a = cat(a3, cat(a2, cat(a1, a0)))
                        |    wire b0 : UInt<1>
                        |    wire b1 : UInt<1>
                        |    node b = cat(b1, b0)
                        |    a_output <= cat(a3, a2)
                        |    b_output <= cat(b1, b0)
                        |    a1 <= bits(c, 1, 1)
                        |    a2 <= b0
                        |    a0 <= bits(c, 0, 0)
                        |    a3 <= b1
                        |    b0 <= d
                        |    b1 <= a0
                        |""".stripMargin

    runTest(firrtlInput, expectedOut)
  }

  "False loop where a wider wire is assigned to a narrower value SInt variation" should "not throw an exception" in {
    val firrtlInput = """circuit hasloops :
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
                        |    b <= cat(bits(a, 0, 0), d)
                        |    a_output <= asSInt(a)
                        |    b_output <= b
                        |""".stripMargin

    val expectedOut = """circuit hasloops :
                        |  module hasloops :
                        |    input clk : Clock
                        |    input c : UInt<3>
                        |    input d : UInt<1>
                        |    output a_output : SInt<4>
                        |    output b_output : UInt<2>
                        |
                        |    wire a0 : SInt<1>
                        |    wire a1 : SInt<1>
                        |    wire a2 : SInt<1>
                        |    wire a3 : SInt<1>
                        |    node a = cat(a3, cat(a2, cat(a1, a0)))
                        |    wire b0 : UInt<1>
                        |    wire b1 : UInt<1>
                        |    node b = cat(b1, b0)
                        |    a_output <= asSInt(cat(a3, cat(a2, cat(a1, a0))))
                        |    b_output <= cat(b1, b0)
                        |    a1 <= asSInt(bits(c, 1, 1))
                        |    a2 <= asSInt(bits(c, 2, 2))
                        |    a0 <= asSInt(bits(c, 0, 0))
                        |    a3 <= asSInt(b0)
                        |    b0 <= d
                        |    b1 <= a0
                        |""".stripMargin

    runTest(firrtlInput, expectedOut)
  }

  "False loop with nested cat with multiple bits" should "not throw an exception" in {
    val firrtlInput = """circuit hasloops :
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

    val expectedOut = """circuit hasloops :
                        |  module hasloops :
                        |    input clk : Clock
                        |    input c : UInt<2>
                        |    input d : UInt<2>
                        |    output a_output : UInt<6>
                        |    output b_output : UInt<2>
                        |
                        |    wire a0 : UInt<1>
                        |    wire a1 : UInt<1>
                        |    wire a2 : UInt<1>
                        |    wire a3 : UInt<1>
                        |    wire a4 : UInt<1>
                        |    wire a5 : UInt<1>
                        |    node a = cat(a5, cat(a4, cat(a3, cat(a2, cat(a1, a0)))))
                        |    wire b0 : UInt<1>
                        |    wire b1 : UInt<1>
                        |    node b = cat(b1, b0)
                        |    a_output <= cat(a5, cat(a4, cat(a3, cat(a2, cat(a1, a0)))))
                        |    b_output <= cat(b1, b0)
                        |    a5 <= b1
                        |    a4 <= b0
                        |    a1 <= bits(d, 1, 1)
                        |    a2 <= bits(c, 0, 0)
                        |    a0 <= bits(d, 0, 0)
                        |    a3 <= bits(c, 1, 1)
                        |    b0 <= a0
                        |    b1 <= UInt<1>("h0")
                        |""".stripMargin

    runTest(firrtlInput, expectedOut)
  }

  "False loop with repeated names" should "not throw an exception" in {
    val firrtlInput = """circuit hasloops :
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

    val expectedOut = """circuit hasloops :
                        |  module hasloops :
                        |    input clk : Clock
                        |    input c : UInt<1>
                        |    input d : UInt<1>
                        |    output a_output : UInt<2>
                        |    output a0_output : UInt<1>
                        |
                        |    wire a0_0 : UInt<1>
                        |    wire a1 : UInt<1>
                        |    node a = cat(a1, a0_0)
                        |    wire a0 : UInt<1>
                        |    a_output <= cat(a1, a0_0)
                        |    a0_output <= a0
                        |    a1 <= a0
                        |    a0_0 <= c
                        |    a0 <= xor(a0_0, d)
                        |""".stripMargin

    runTest(firrtlInput, expectedOut)
  }

  //TODO: This test doesn't do anything
  "False loop where subword is multiple bits" should "not throw an exception" in {
    val firrtlInput = """circuit hasloops :
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

    val expectedOut = """circuit hasloops :
                        |  module hasloops :
                        |    input clk : Clock
                        |    input d : UInt<1>
                        |    input e : UInt<1>
                        |    input f : UInt<1>
                        |    output a_output : UInt<3>
                        |    output d_output : UInt<3>
                        |
                        |    wire a : UInt<3>
                        |    a_output <= a
                        |    d_output <= bits(a, 2, 0)
                        |    a <= cat(d, cat(e, f))
                        |""".stripMargin

    runTest(firrtlInput, expectedOut)
  }

  "False combinational loop through a node" should "not throw an exception" in {
    val firrtlInput = """circuit hasloops :
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

    val expectedOut = """circuit hasloops :
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
                        |    node z = a0
                        |    a_output <= cat(a1, a0)
                        |    b_output <= b
                        |    a1 <= b
                        |    a0 <= c
                        |    b <= xor(z, d)
                        |""".stripMargin

    runTest(firrtlInput, expectedOut)
  }

  "False combinational loop with a UInt literal" should "not throw an exception" in {
    val firrtlInput = """circuit hasloops :
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

    val expectedOut = """circuit hasloops :
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
                        |    a_output <= cat(a1, a0)
                        |    b_output <= b
                        |    a1 <= b
                        |    a0 <= c
                        |    b <= xor(a0, UInt<1>("h1"))
                        |""".stripMargin

    runTest(firrtlInput, expectedOut)
  }

  //TODO: This test doesn't do anything
  "False combinational loop with a bundle" should "not throw an exception" in {
    val firrtlInput = """circuit hasloops :
                        |  module hasloops :
                        |    wire x : { sign : UInt<1>, exponent : UInt<8>, significand : UInt<23>}
                        |    x.sign <= UInt(1)
                        |    x.exponent <= UInt(1)
                        |    x.significand <= UInt(1)
                        |""".stripMargin

    val expectedOut = """circuit hasloops :
                        |  module hasloops :
                        |    wire x_sign : UInt<1>
                        |    wire x_exponent : UInt<8>
                        |    wire x_significand : UInt<23>
                        |    x_sign <= UInt<1>("h1")
                        |    x_exponent <= UInt<1>("h1")
                        |    x_significand <= UInt<1>("h1")
                        |""".stripMargin

    runTest(firrtlInput, expectedOut)
  }

  //TODO: This test is currently failing
  "False loop where there is a bits within a bits" should "not throw an exception" in {
    val firrtlInput = """circuit hasloops :
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

    val expectedOut = """circuit hasloops :
                        |  module hasloops :
                        |    input clk : Clock
                        |    input c : UInt<1>
                        |    input d : UInt<1>
                        |    output a_output : UInt<2>
                        |    output b_output : UInt<1>
                        |
                        |    wire a0 : UInt<1>
                        |    wire a1 : UInt<1>
                        |    wire a2 : UInt<1>
                        |    node a = cat(a2, cat(a1, a0))
                        |    wire b : UInt<1>
                        |    a_output <= cat(a2, cat(a1, a0))
                        |    b_output <= b
                        |    a1 <= b
                        |    a2 <= UInt<1>("h0")
                        |    a0 <= c
                        |    b <= xor(a0, d)
                        |""".stripMargin

    runTest(firrtlInput, expectedOut)
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
