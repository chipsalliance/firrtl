package firrtlTests

import firrtl.passes.CheckInitialization.RefNotInitializedException
import firrtl.passes.CheckWidths.BitsWidthException
import firrtl.stage.Forms
import firrtl.testutils.LeanTransformSpec

class SubWordAssignmentTests extends LeanTransformSpec(Forms.LowFormOptimized) {
  behavior.of("SubWordAssignment")

  private def check(input: String, expected: String) = {
    val r = compile(input)
    assert(removeSkip(r.circuit).serialize == removeSkip(parse(expected)).serialize)
  }

  it should "support assigning individual output bits" in {
    val src =
      """circuit m:
        |  module m:
        |    output x : UInt<2>
        |    x[0] <= UInt(1)
        |    x[1] <= UInt(0)
        |""".stripMargin
    val expected =
      """circuit m :
        |  module m :
        |    output x : UInt<2>
        |    x <= UInt<2>("h1")
        |""".stripMargin
    check(src, expected)
  }

  it should "support assigning output bit ranges" in {
    val src =
      """circuit m:
        |  module m:
        |    output x : UInt<3>
        |    x[0] <= UInt(1)
        |    x[2:1] <= UInt(2)
        |""".stripMargin
    val expected =
      """circuit m :
        |  module m :
        |    output x : UInt<3>
        |    x <= UInt<3>("h5")
        |""".stripMargin
    check(src, expected)
  }

  it should "throw an error on uninitialized bits" in {
    val src =
      """circuit m:
        |  module m:
        |    output x : UInt<3>
        |    x[2:1] <= UInt(2)
        |""".stripMargin
    val e = intercept[RefNotInitializedException] { compile(src) }
    assert(e.getMessage.contains("Reference x is not fully initialized"))
  }

  it should "allow marking individual bits as DontCare" in {
    val src =
      """circuit m:
        |  module m:
        |    output x : UInt<3>
        |    x[0] is invalid
        |    x[2:1] <= UInt(2)
        |""".stripMargin
    val expected =
      """circuit m :
        |  module m :
        |    output x : UInt<3>
        |    x <= UInt<3>("h4")
        |""".stripMargin
    check(src, expected)
  }

  it should "allow marking individual bits as DontCare with conditionals" in {
    val src =
      """circuit m:
        |  module m:
        |    input c : UInt<1>
        |    output x : UInt<2>
        |    x[0] <= UInt(1)
        |    when c:
        |      x[1] <= UInt(1)
        |    else:
        |      x[1] is invalid
        |""".stripMargin
    // TODO: currently we actually do not perform this optimization since we replace invalid with 0 for sub-word assignments
    val expected =
      """circuit m :
        |  module m :
        |    input c : UInt<1>
        |    output x : UInt<2>
        |    x <= mux(c, UInt<2>("h3"), UInt<2>("h1"))
        |""".stripMargin
    check(src, expected)
  }

  it should "take advantage of DontCare bits in different branches" in {
    val src =
      """circuit m:
        |  module m:
        |    input c : UInt<1>
        |    output x : UInt<2>
        |    when c:
        |      x[0] is invalid
        |      x[1] <= UInt(1)
        |    else:
        |      x[0] <= UInt(1)
        |      x[1] is invalid
        |""".stripMargin
    // TODO: currently we actually do not perform this optimization since we replace invalid with 0 for sub-word assignments
    val expected =
      """circuit m :
        |  module m :
        |    input c : UInt<1>
        |    output x : UInt<2>
        |    x <= mux(c, UInt<2>("h2"), UInt<2>("h1"))
        |""".stripMargin
    check(src, expected)
  }

  it should "support assignments in conditionals" in {
    val src =
      """circuit m:
        |  module m:
        |    input c : UInt<1>
        |    output x : UInt<2>
        |    x[0] <= UInt(1)
        |    when c:
        |      x[1] <= UInt(1)
        |    else:
        |      x[1] <= UInt(0)
        |""".stripMargin
    val expected =
      """circuit m :
        |  module m :
        |    input c : UInt<1>
        |    output x : UInt<2>
        |    x <= mux(c, UInt<2>("h3"), UInt<2>("h1"))
        |""".stripMargin
    check(src, expected)
  }

  it should "support assignments for bundles" in {
    val src =
      """circuit m:
        |  module m:
        |    input c : UInt<1>
        |    output x : { a : UInt<2>, b: UInt<2> }
        |    x.a[0] <= UInt(1)
        |    when c:
        |      x.a[1] <= UInt(1)
        |    else:
        |      x.a[1] <= UInt(0)
        |    x.b <= UInt(3)
        |""".stripMargin
    val expected =
      """circuit m :
        |  module m :
        |    input c : UInt<1>
        |    output x_a : UInt<2>
        |    output x_b : UInt<2>
        |    x_a <= mux(c, UInt<2>("h3"), UInt<2>("h1"))
        |    x_b <= UInt<2>("h3")
        |""".stripMargin
    check(src, expected)
  }

  it should "support partial assignments with invalidation for bundles" in {
    val src =
      """circuit m:
        |  module m:
        |    input c : UInt<1>
        |    output x : { a : UInt<2>, b: UInt<2> }
        |    x is invalid
        |    when c:
        |      x.a[1] <= UInt(1)
        |""".stripMargin
    val expected =
      """circuit m :
        |  module m :
        |    input c : UInt<1>
        |    output x_a : UInt<2>
        |    output x_b : UInt<2>
        |    x_a <= UInt<2>(2)
        |    x_b <= UInt<2>(0)
        |""".stripMargin
    check(src, expected)
  }

  it should "work with wires" in {
    val src =
      """circuit m:
        |  module m:
        |    input c : UInt<1>
        |    output y : UInt<2>
        |    wire x : UInt<2>
        |    y <= x
        |    x[0] <= UInt(1)
        |    when c:
        |      x[1] <= UInt(1)
        |    else:
        |      x[1] <= UInt(0)
        |""".stripMargin
    val expected =
      """circuit m :
        |  module m :
        |    input c : UInt<1>
        |    output y : UInt<2>
        |    y <= mux(c, UInt<2>("h3"), UInt<2>("h1"))
        |""".stripMargin
    check(src, expected)
  }

  it should "work with registers" in {
    val src =
      """circuit m:
        |  module m:
        |    input clock : Clock
        |    input c : UInt<1>
        |    output y : UInt<2>
        |    reg x : UInt<2>, clock
        |    y <= x
        |    x[0] <= UInt(1)
        |    when c:
        |      x[1] <= UInt(1)
        |    else:
        |      x[1] <= UInt(0)
        |""".stripMargin
    val expected =
      """circuit m :
        |  module m :
        |    input clock : Clock
        |    input c : UInt<1>
        |    output y : UInt<2>
        |    reg x : UInt<2>, clock with :
        |      reset => (UInt<1>("h0"), x)
        |    y <= x
        |    x <= mux(c, UInt<2>("h3"), UInt<2>("h1"))
        |""".stripMargin
    check(src, expected)
  }

  it should "allow for nested bit indices" in {
    val src =
      """circuit m:
        |  module m:
        |    output x : UInt<2>
        |    x[1:0][0][0] <= UInt(1)
        |    x[1][0][0][0] <= UInt(0)
        |""".stripMargin
    val expected =
      """circuit m :
        |  module m :
        |    output x : UInt<2>
        |    x <= UInt<2>("h1")
        |""".stripMargin
    check(src, expected)
  }

  it should "error on out of bounds hi index" in {
    val src =
      """circuit m:
        |  module m:
        |    output x : UInt<2>
        |    x[2:0] <= UInt(1)
        |""".stripMargin
    val e = intercept[BitsWidthException] {
      compile(src)
    }
    assert(e.getMessage.contains("High bit 2 in bits operator is larger than input width 2"))
  }

  it should "error on out of bounds lo index" in {
    val src =
      """circuit m:
        |  module m:
        |    output x : UInt<2>
        |    x[1:-1] <= UInt(1)
        |""".stripMargin
    val e = intercept[firrtl.passes.CheckHighForm.NegArgException] { compile(src) }
    assert(e.getMessage.contains("Primop bits argument -1 < 0"))
  }

  it should "error on hi < lo indices" in {
    val src =
      """circuit m:
        |  module m:
        |    output x : UInt<2>
        |    x[0:1] <= UInt(1)
        |""".stripMargin
    val e = intercept[firrtl.passes.CheckHighForm.LsbLargerThanMsbException] { compile(src) }
    assert(e.getMessage.contains("Primop bits lsb 1 > 0"))
  }

  it should "error when assigning an input" in {
    val src =
      """circuit m:
        |  module m:
        |    input x : UInt<2>
        |    x[0] <= UInt(1)
        |""".stripMargin
    val e = intercept[firrtl.passes.CheckFlows.WrongFlow] { compile(src) }
    assert(e.getMessage.contains("is used as a SinkFlow but can only be used as a SourceFlow"))
  }

  it should "error when assigning an input inside a bundle" in {
    val src =
      """circuit m:
        |  module m:
        |    output io : { flip x : UInt<2> }
        |    io.x[0] <= UInt(1)
        |""".stripMargin
    val e = intercept[firrtl.passes.CheckFlows.WrongFlow] { compile(src) }
    assert(e.getMessage.contains("is used as a SinkFlow but can only be used as a SourceFlow"))
  }

  // TODO: should we support these kinds of word-level, but not bit-level loops?
  it should "allow signals to be connected trough bit slices" ignore {
    val src =
      """circuit m:
        |  module m:
        |    input y : UInt<1>
        |    output x : UInt<1>
        |    wire tmp : UInt<4>
        |    x <= not(tmp[3])
        |    tmp[3:1] <= not(tmp[2:0])
        |    tmp[0] <= y
        |""".stripMargin
    val expected =
      """circuit m :
        |  module m :
        |    input y : UInt<1>
        |    output x : UInt<1>
        |    x <= y
        |""".stripMargin
    check(src, expected)
  }
}
