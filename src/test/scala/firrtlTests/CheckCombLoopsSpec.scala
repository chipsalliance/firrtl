// SPDX-License-Identifier: Apache-2.0

package firrtlTests

import firrtl._
import firrtl.transforms._
import firrtl.testutils._
import annotations._

import java.io.File
import java.nio.file.Paths
import firrtl.options.Dependency
import firrtl.stage.FirrtlStage
import firrtl.util.BackendCompilationUtilities.createTestDirectory

class CheckCombLoopsSpec extends LeanTransformSpec(Seq(Dependency[CheckCombLoops])) {
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

    val result = compile(parse(input))

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
      print("Output has correct form")
    } else {
      print("ERROR: Incorrect output form\n")
    }

    print(resultSerialized)

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

    val result = compile(parse(input))
    print(result.circuit.serialize)
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

    val result = compile(parse(input))
    print(result.circuit.serialize)
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

    val result = compile(parse(input))
    print(result.circuit.serialize)
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

    val result = compile(parse(input))
    print(result.circuit.serialize)
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

    val result = compile(parse(input))
    print(result.circuit.serialize)
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

    val result = compile(parse(input))
    print(result.circuit.serialize)
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

    val result = compile(parse(input))
    print(result.circuit.serialize)
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

    val result = compile(parse(input))
    print(result.circuit.serialize)
  }

  "Loop-free circuit" should "not throw an exception" in {
    val input = """circuit hasnoloops :
                  |  module thru :
                  |    input in1 : UInt<1>
                  |    input in2 : UInt<1>
                  |    output out1 : UInt<1>
                  |    output out2 : UInt<1>
                  |    out1 <= in1
                  |    out2 <= in2
                  |  module hasnoloops :
                  |    input clk : Clock
                  |    input a : UInt<1>
                  |    output b : UInt<1>
                  |    wire x : UInt<1>
                  |    inst inner of thru
                  |    inner.in1 <= a
                  |    x <= inner.out1
                  |    inner.in2 <= x
                  |    b <= inner.out2
                  |""".stripMargin

    compile(parse(input))
  }

  "Simple combinational loop" should "throw an exception" in {
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
      compile(parse(input))
    }
  }

  "Single-element combinational loop" should "throw an exception" in {
    val input = """circuit loop :
                  |  module loop :
                  |    output y : UInt<8>
                  |    wire w : UInt<8>
                  |    w <= w
                  |    y <= w
                  |""".stripMargin

    intercept[CheckCombLoops.CombLoopException] {
      compile(parse(input))
    }
  }

  "Node combinational loop" should "throw an exception" in {
    val input = """circuit hasloops :
                  |  module hasloops :
                  |    input clk : Clock
                  |    input a : UInt<1>
                  |    input b : UInt<1>
                  |    output c : UInt<1>
                  |    output d : UInt<1>
                  |    wire y : UInt<1>
                  |    c <= b
                  |    node z = and(c,y)
                  |    y <= z
                  |    d <= z
                  |""".stripMargin

    intercept[CheckCombLoops.CombLoopException] {
      compile(parse(input))
    }
  }

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
      compile(parse(input))
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
      compile(parse(input))
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
    val annos = AnnotationSeq(Seq(ExtModulePathAnnotation(mt.ref("in"), mt.ref("out"))))
    intercept[CheckCombLoops.CombLoopException] {
      compile(parse(input), annos)
    }
  }

  "Loop-free circuit with ExtModulePathAnnotations" should "not throw an exception" in {
    val input = """circuit hasnoloops :
                  |  extmodule blackbox :
                  |    input in1 : UInt<1>
                  |    input in2 : UInt<1>
                  |    output out1 : UInt<1>
                  |    output out2 : UInt<1>
                  |  module hasnoloops :
                  |    input clk : Clock
                  |    input a : UInt<1>
                  |    output b : UInt<1>
                  |    wire x : UInt<1>
                  |    inst inner of blackbox
                  |    inner.in1 <= a
                  |    x <= inner.out1
                  |    inner.in2 <= x
                  |    b <= inner.out2
                  |""".stripMargin

    val mt = ModuleTarget("hasnoloops", "blackbox")
    val annos = AnnotationSeq(
      Seq(
        ExtModulePathAnnotation(mt.ref("in1"), mt.ref("out1")),
        ExtModulePathAnnotation(mt.ref("in2"), mt.ref("out2"))
      )
    )
    compile(parse(input), annos)
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
      compile(parse(input))
    }
  }

  "Multiple simple loops in one SCC" should "throw an exception" in {
    val input = """circuit hasloops :
                  |  module hasloops :
                  |    input i : UInt<1>
                  |    output o : UInt<1>
                  |    wire a : UInt<1>
                  |    wire b : UInt<1>
                  |    wire c : UInt<1>
                  |    wire d : UInt<1>
                  |    wire e : UInt<1>
                  |    a <= and(c,i)
                  |    b <= and(a,d)
                  |    c <= b
                  |    d <= and(c,e)
                  |    e <= b
                  |    o <= e
                  |""".stripMargin

    intercept[CheckCombLoops.CombLoopException] {
      compile(parse(input))
    }
  }

  "Circuit" should "create an annotation" in {
    val input = """circuit hasnoloops :
                  |  module thru :
                  |    input in1 : UInt<1>
                  |    input in2 : UInt<1>
                  |    output out1 : UInt<1>
                  |    output out2 : UInt<1>
                  |    out1 <= in1
                  |    out2 <= in2
                  |  module hasnoloops :
                  |    input clk : Clock
                  |    input a : UInt<1>
                  |    output b : UInt<1>
                  |    wire x : UInt<1>
                  |    inst inner of thru
                  |    inner.in1 <= a
                  |    x <= inner.out1
                  |    inner.in2 <= x
                  |    b <= inner.out2
                  |""".stripMargin

    val cs = compile(parse(input))
    val mt = ModuleTarget("hasnoloops", "hasnoloops")
    val anno = CombinationalPath(mt.ref("b"), Seq(mt.ref("a")))
    cs.annotations.contains(anno) should be(true)
  }
}

class CheckCombLoopsCommandLineSpec extends FirrtlFlatSpec {

  val testDir = createTestDirectory("CombLoopChecker")
  val inputFile = Paths.get(getClass.getResource("/features/HasLoops.fir").toURI()).toFile()
  val outFile = new File(testDir, "HasLoops.v")
  val args = Array("-i", inputFile.getAbsolutePath, "-o", outFile.getAbsolutePath, "-X", "verilog")

  "Combinational loops detection" should "run by default" in {
    a[CheckCombLoops.CombLoopException] should be thrownBy {
      (new FirrtlStage).execute(args, Seq())
    }
  }

  it should "not run when given --no-check-comb-loops option" in {
    (new FirrtlStage).execute(args :+ "--no-check-comb-loops", Seq())
  }
}
