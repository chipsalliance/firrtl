// See LICENSE for license details.

package firrtlTests

import firrtl._
import firrtl.passes._
import firrtl.transforms._
import firrtl.testutils._

import logger.{LogLevel, Logger}

object MidFormConstantPropagationSpec {
  val transforms: Seq[Transform] = Seq(
      ToWorkingIR,
      ResolveKinds,
      InferTypes,
      ResolveFlows,
      new InferWidths,
      new MidFormConstantPropagation)
}

class MidFormConstantPropagationMultiModule extends ConstantPropagationMultiModule(MidFormConstantPropagationSpec.transforms)
class MidFormConstantPropagationSingleModule extends ConstantPropagationSingleModule(MidFormConstantPropagationSpec.transforms)

// More sophisticated tests of the full compiler
class MidFormConstantPropagationIntegrationSpec extends AnyTransformSpec {
  def transform = new MidFormConstantPropagation

  "ConstProp" should "NOT optimize across dontTouch on nodes" in {
      val input =
        """circuit Top :
          |  module Top :
          |    input io : { x: UInt<1>[1], flip y: UInt<1>[1] }
          |    node z = io.x[0]
          |    io.y[0] <= z""".stripMargin
      val check = input
    execute(input, check, Seq(dontTouch("Top.z")))
  }

  it should "NOT optimize across nodes marked dontTouch by other annotations" in {
      val input =
        """circuit Top :
          |  module Top :
          |    input io : { x: UInt<1>[1], flip y: UInt<1>[1] }
          |    node z = io.x[0]
          |    io.y[0] <= z""".stripMargin
      val check = input
      val dontTouchRT = annotations.ModuleTarget("Top", "Top").ref("z")
    execute(input, check, Seq(AnnotationWithDontTouches(dontTouchRT)))
  }

  it should "NOT optimize across dontTouch on registers" in {
      val input =
        """circuit Top :
          |  module Top :
          |    input io: { clk: Clock[1], reset: UInt<1>[1], flip y: UInt<1>[1] }
          |    reg z : UInt<1>, io.clk[0]
          |    io.y[0] <= z
          |    z <= mux(io.reset[0], UInt<1>("h0"), z)""".stripMargin
      val check = input
    execute(input, check, Seq(dontTouch("Top.z")))
  }


  it should "NOT optimize across dontTouch on wires" in {
      val input =
        """circuit Top :
          |  module Top :
          |    input io : { x: UInt<1>[1], flip y: UInt<1>[1] }
          |    wire z : UInt<1>
          |    io.y[0] <= z
          |    z <= io.x[0]""".stripMargin
      val check = input
    execute(input, check, Seq(dontTouch("Top.z")))
  }

  it should "NOT optimize across dontTouch on output ports" in {
    val input =
      """circuit Top :
         |  module Child :
         |    output out : UInt<1>
         |    out <= UInt<1>(0)
         |  module Top :
         |    input x : UInt<1>
         |    output z : UInt<1>
         |    inst c of Child
         |    z <= and(x, c.out)""".stripMargin
      val check = input
    execute(input, check, Seq(dontTouch("Child.out")))
  }

  it should "NOT optimize across dontTouch on input ports" in {
    val input =
      """circuit Top :
         |  module Child :
         |    input in0 : UInt<1>
         |    input in1 : UInt<1>
         |    output out : UInt<1>
         |    out <= and(in0, in1)
         |  module Top :
         |    input x : UInt<1>
         |    output z : UInt<1>
         |    inst c of Child
         |    z <= c.out
         |    c.in0 <= x
         |    c.in1 <= UInt<1>(1)""".stripMargin
      val check = input
    execute(input, check, Seq(dontTouch("Child.in1")))
  }

  it should "still propagate constants even when there is name swapping" in {
      val input =
        """circuit Top :
          |  module Top :
          |    input x : UInt<1>
          |    input y : UInt<1>
          |    output z : UInt<1>
          |    node _T_1 = and(and(x, y), UInt<1>(0))
          |    node n = _T_1
          |    z <= n""".stripMargin
      val check =
        """circuit Top :
          |  module Top :
          |    input x : UInt<1>
          |    input y : UInt<1>
          |    output z : UInt<1>
          |    z <= UInt<1>(0)""".stripMargin
    execute(input, check, Seq.empty)
  }

  it should "pad constant connections to wires when propagating" in {
      val input =
        """circuit Top :
          |  module Top :
          |    output z : UInt<16>
          |    wire w : { a : UInt<8>, b : UInt<8> }
          |    w.a <= UInt<2>("h3")
          |    w.b <= UInt<2>("h3")
          |    z <= cat(w.a, w.b)""".stripMargin
      val check =
        """circuit Top :
          |  module Top :
          |    output z : UInt<16>
          |    wire w : { a : UInt<8>, b : UInt<8> }
          |    z <= UInt<16>("h303")
          |    w.a <= UInt<2>("h3")
          |    w.b <= UInt<2>("h3")""".stripMargin
    execute(input, check, Seq.empty)
  }

  it should "pad constant connections to registers when propagating" in {
      val input =
        """circuit Top :
          |  module Top :
          |    input clock : Clock
          |    output z : UInt<16>
          |    reg r : { a : UInt<8>, b : UInt<8> }, clock
          |    r.a <= UInt<2>("h3")
          |    r.b <= UInt<2>("h3")
          |    z <= cat(r.a, r.b)""".stripMargin
      val check =
        """circuit Top :
          |  module Top :
          |    input clock : Clock
          |    output z : UInt<16>
          |    reg r : { a : UInt<8>, b : UInt<8> }, clock with: (reset => (UInt<1>("h0"), r))
          |    z <= UInt<16>("h303")
          |    r.a <= UInt<2>("h3")
          |    r.b <= UInt<2>("h3")""".stripMargin
    execute(input, check, Seq.empty)
  }

  it should "pad zero when constant propping a register replaced with zero" in {
      val input =
        """circuit Top :
          |  module Top :
          |    input clock : Clock
          |    output z : UInt<16>
          |    reg r : UInt<8>, clock
          |    r <= or(r, UInt(0))
          |    node n = UInt("hab")
          |    z <= cat(n, r)""".stripMargin
      val check =
        """circuit Top :
          |  module Top :
          |    input clock : Clock
          |    output z : UInt<16>
          |    reg r : UInt<8>, clock with : (reset => (UInt<1>("h0"), r))
          |    z <= UInt<16>("hab00")
          |    r <= UInt<8>("h0")""".stripMargin
    execute(input, check, Seq.empty)
  }

  it should "pad constant connections to outputs when propagating" in {
      val input =
        """circuit Top :
          |  module Child :
          |    output x : UInt<8>
          |    x <= UInt<2>("h3")
          |  module Top :
          |    output z : UInt<16>
          |    inst c of Child
          |    z <= cat(UInt<2>("h3"), c.x)""".stripMargin
      val check =
        """circuit Top :
          |  module Child :
          |    output x : UInt<8>
          |    x <= UInt<2>("h3")
          |  module Top :
          |    output z : UInt<16>
          |    inst c of Child
          |    z <= UInt<10>("h303")""".stripMargin
    execute(input, check, Seq.empty)
  }

  it should "pad constant connections to submodule inputs when propagating" in {
      val input =
        """circuit Top :
          |  module Child :
          |    input x : UInt<8>
          |    output y : UInt<16>
          |    y <= cat(UInt<2>("h3"), x)
          |  module Top :
          |    output z : UInt<16>
          |    inst c of Child
          |    c.x <= UInt<2>("h3")
          |    z <= c.y""".stripMargin
      val check =
        """circuit Top :
          |  module Child :
          |    input x : UInt<8>
          |    output y : UInt<16>
          |    y <= UInt<10>("h303")
          |  module Top :
          |    output z : UInt<16>
          |    inst c of Child
          |    z <= UInt<16>("h303")
          |    c.x <= UInt<2>("h3")""".stripMargin
    execute(input, check, Seq.empty)
  }

  it should "remove pads if the width is <= the width of the argument" in {
    def input(w: Int) =
     s"""circuit Top :
        |  module Top :
        |    input x : UInt<8>
        |    output z : UInt<8>
        |    z <= pad(x, $w)""".stripMargin
    val check =
      """circuit Top :
        |  module Top :
        |    input x : UInt<8>
        |    output z : UInt<8>
        |    z <= x""".stripMargin
    execute(input(6), check, Seq.empty)
    execute(input(8), check, Seq.empty)
  }


  "Registers with no reset or connections" should "be replaced with constant zero" in {
      val input =
        """circuit Top :
          |  module Top :
          |    input clock : Clock
          |    output z : UInt<8>
          |    reg r : UInt<8>, clock
          |    z <= r""".stripMargin
      val check =
        """circuit Top :
          |  module Top :
          |    input clock : Clock
          |    output z : UInt<8>
          |    reg r : UInt<8>, clock with : (reset => (UInt<1>("h0"), r))
          |    z <= UInt<8>(0)
          |    r <= UInt<8>("h0")""".stripMargin
    execute(input, check, Seq.empty)
  }

  "Registers with ONLY constant reset" should "be replaced with that constant" in {
      val input =
        """circuit Top :
          |  module Top :
          |    input clock : Clock
          |    input reset : UInt<1>
          |    output z : UInt<8>
          |    reg r : UInt<8>, clock with : (reset => (reset, UInt<4>("hb")))
          |    z <= r""".stripMargin
      val check =
        """circuit Top :
          |  module Top :
          |    input clock : Clock
          |    input reset : UInt<1>
          |    output z : UInt<8>
          |    reg r : UInt<8>, clock with : (reset => (UInt<1>(0), r))
          |    z <= UInt<8>("hb")
          |    r <= mux(reset, UInt<4>("hb"), UInt<8>("hb"))""".stripMargin
    execute(input, check, Seq.empty)
  }
/*
  "Registers async reset and a constant connection" should "NOT be removed" in {
      val input =
        """circuit Top :
          |  module Top :
          |    input clock : Clock
          |    input reset : AsyncReset
          |    input en : UInt<1>
          |    output z : UInt<8>
          |    reg r : UInt<8>, clock with : (reset => (reset, UInt<4>("hb")))
          |    when en :
          |      r <= UInt<4>("h0")
          |    z <= r""".stripMargin
      val check =
        """circuit Top :
          |  module Top :
          |    input clock : Clock
          |    input reset : AsyncReset
          |    input en : UInt<1>
          |    output z : UInt<8>
          |    reg r : UInt<8>, clock with :
          |      reset => (reset, UInt<8>("hb"))
          |    z <= r
          |    r <= mux(en, UInt<8>("h0"), r)""".stripMargin
    execute(input, check, Seq.empty)
  }

  "Registers with constant reset and connection to the same constant" should "be replaced with that constant" in {
      val input =
        """circuit Top :
          |  module Top :
          |    input clock : Clock
          |    input reset : UInt<1>
          |    input cond : UInt<1>
          |    output z : UInt<8>
          |    reg r : UInt<8>, clock with : (reset => (reset, UInt<4>("hb")))
          |    when cond :
          |      r <= UInt<4>("hb")
          |    z <= r""".stripMargin
      val check =
        """circuit Top :
          |  module Top :
          |    input clock : Clock
          |    input reset : UInt<1>
          |    input cond : UInt<1>
          |    output z : UInt<8>
          |    z <= UInt<8>("hb")""".stripMargin
    execute(input, check, Seq.empty)
  }

  "Const prop of registers" should "do limited speculative expansion of optimized muxes to absorb bigger cones" in {
      val input =
        """circuit Top :
          |  module Top :
          |    input clock : Clock
          |    input en : UInt<1>
          |    output out : UInt<1>
          |    reg r1 : UInt<1>, clock
          |    reg r2 : UInt<1>, clock
          |    when en :
          |      r1 <= UInt<1>(1)
          |    r2 <= UInt<1>(0)
          |    when en :
          |      r2 <= r2
          |    out <= xor(r1, r2)""".stripMargin
      val check =
        """circuit Top :
          |  module Top :
          |    input clock : Clock
          |    input en : UInt<1>
          |    output out : UInt<1>
          |    out <= UInt<1>("h1")""".stripMargin
    execute(input, check, Seq.empty)
  }

  "A register with constant reset and all connection to either itself or the same constant" should "be replaced with that constant" in {
      val input =
        """circuit Top :
          |  module Top :
          |    input clock : Clock
          |    input reset : UInt<1>
          |    input cmd : UInt<3>
          |    output z : UInt<8>
          |    reg r : UInt<8>, clock with : (reset => (reset, UInt<4>("h7")))
          |    r <= r
          |    when eq(cmd, UInt<3>("h0")) :
          |      r <= UInt<3>("h7")
          |    else :
          |      when eq(cmd, UInt<3>("h1")) :
          |        r <= r
          |      else :
          |        when eq(cmd, UInt<3>("h2")) :
          |          r <= UInt<4>("h7")
          |        else :
          |          r <= r
          |    z <= r""".stripMargin
      val check =
        """circuit Top :
          |  module Top :
          |    input clock : Clock
          |    input reset : UInt<1>
          |    input cmd : UInt<3>
          |    output z : UInt<8>
          |    z <= UInt<8>("h7")""".stripMargin
    execute(input, check, Seq.empty)
  }
*/

  "Registers with ONLY constant connection" should "be replaced with that constant" in {
      val input =
        """circuit Top :
          |  module Top :
          |    input clock : Clock
          |    input reset : UInt<1>
          |    output z : SInt<8>
          |    reg r : SInt<8>, clock
          |    r <= SInt<4>(-5)
          |    z <= r""".stripMargin
      val check =
        """circuit Top :
          |  module Top :
          |    input clock : Clock
          |    input reset : UInt<1>
          |    output z : SInt<8>
          |    reg r : SInt<8>, clock with : (reset => (UInt<1>("h0"), r))
          |    z <= SInt<8>(-5)
          |    r <= SInt<4>(-5)""".stripMargin
    execute(input, check, Seq.empty)
  }

  "Registers with identical constant reset and connection" should "be replaced with that constant" in {
      val input =
        """circuit Top :
          |  module Top :
          |    input clock : Clock
          |    input reset : UInt<1>
          |    output z : UInt<8>
          |    reg r : UInt<8>, clock with : (reset => (reset, UInt<4>("hb")))
          |    r <= UInt<4>("hb")
          |    z <= r""".stripMargin
      val check =
        """circuit Top :
          |  module Top :
          |    input clock : Clock
          |    input reset : UInt<1>
          |    output z : UInt<8>
          |    reg r : UInt<8>, clock with : (reset => (UInt<1>("h0"), r))
          |    z <= UInt<8>("hb")
          |    r <= UInt<4>("hb")""".stripMargin
    execute(input, check, Seq.empty)
  }

  "Connections to a node reference" should "be replaced with the rhs of that node" in {
      val input =
        """circuit Top :
          |  module Top :
          |    input a : UInt<8>
          |    input b : UInt<8>
          |    input c : UInt<1>
          |    output z : UInt<8>
          |    node x = mux(c, a, b)
          |    z <= x""".stripMargin
      val check =
        """circuit Top :
          |  module Top :
          |    input a : UInt<8>
          |    input b : UInt<8>
          |    input c : UInt<1>
          |    output z : UInt<8>
          |    node x = mux(c, a, b)
          |    z <= mux(c, a, b)""".stripMargin
    execute(input, check, Seq.empty)
  }

  "Registers connected only to themselves" should "be replaced with zero" in {
      val input =
        """circuit Top :
          |  module Top :
          |    input clock : Clock
          |    output a : UInt<8>
          |    reg ra : UInt<8>, clock
          |    ra <= ra
          |    a <= ra
          |""".stripMargin
      val check =
        """circuit Top :
          |  module Top :
          |    input clock : Clock
          |    output a : UInt<8>
          |    reg ra : UInt<8>, clock
          |    a <= UInt<8>(0)
          |    ra <= UInt<8>(0)
          |""".stripMargin
    execute(input, check, Seq.empty)
  }

  "Registers connected only to themselves from constant propagation" should "be replaced with zero" in {
      val input =
        """circuit Top :
          |  module Top :
          |    input clock : Clock
          |    output a : UInt<8>
          |    reg ra : UInt<8>, clock
          |    ra <= or(ra, UInt(0))
          |    a <= ra
          |""".stripMargin
      val check =
        """circuit Top :
          |  module Top :
          |    input clock : Clock
          |    output a : UInt<8>
          |    reg ra : UInt<8>, clock
          |    a <= UInt<8>(0)
          |    ra <= UInt<8>(0)
          |""".stripMargin
    execute(input, check, Seq.empty)
  }

  "Temporary named port" should "not be declared as a node" in {
    val input =
      """circuit Top :
        |  module Top :
        |    input _T_61 : UInt<1>
        |    output z : UInt<1>
        |    node a = _T_61
        |    z <= a""".stripMargin
    val check =
      """circuit Top :
        |  module Top :
        |    input _T_61 : UInt<1>
        |    output z : UInt<1>
        |    z <= _T_61""".stripMargin
    execute(input, check, Seq.empty)
  }

  behavior of "ConstProp"

  it should "optimize shl of constants" in {
    val input =
      """circuit Top :
        |  module Top :
        |    output z : UInt<7>
        |    z <= shl(UInt(5), 4)
      """.stripMargin
    val check =
      """circuit Top :
        |  module Top :
        |    output z : UInt<7>
        |    z <= UInt<7>("h50")
      """.stripMargin
    execute(input, check, Seq.empty)
  }

  it should "optimize shr of constants" in {
    val input =
      """circuit Top :
        |  module Top :
        |    output z : UInt<1>
        |    z <= shr(UInt(5), 2)
      """.stripMargin
    val check =
      """circuit Top :
        |  module Top :
        |    output z : UInt<1>
        |    z <= UInt<1>("h1")
      """.stripMargin
    execute(input, check, Seq.empty)
  }

  // Due to #866, we need dshl optimized away or it'll become a dshlw and error in parsing
  // Include cat to verify width is correct
  it should "optimize dshl of constant" in {
    val input =
      """circuit Top :
        |  module Top :
        |    output z : UInt<8>
        |    node n = dshl(UInt<1>(0), UInt<2>(0))
        |    z <= cat(UInt<4>("hf"), n)
      """.stripMargin
    val check =
      """circuit Top :
        |  module Top :
        |    output z : UInt<8>
        |    z <= UInt<8>("hf0")
      """.stripMargin
    execute(input, check, Seq.empty)
  }

  // Include cat and constants to verify width is correct
  it should "optimize dshr of constant" in {
    val input =
      """circuit Top :
        |  module Top :
        |    output z : UInt<8>
        |    node n = dshr(UInt<4>(0), UInt<2>(2))
        |    z <= cat(UInt<4>("hf"), n)
      """.stripMargin
    val check =
      """circuit Top :
        |  module Top :
        |    output z : UInt<8>
        |    z <= UInt<8>("hf0")
      """.stripMargin
    execute(input, check, Seq.empty)
  }

  private def matchingArgs(op: String, iType: String, oType: String, result: String): Unit = {
    val input =
      s"""circuit Top :
         |  module Top :
         |    input i : ${iType}
         |    output o : ${oType}
         |    o <= ${op}(i, i)
      """.stripMargin
    val check =
      s"""circuit Top :
         |  module Top :
         |    input i : ${iType}
         |    output o : ${oType}
         |    o <= ${result}
      """.stripMargin
    execute(input, check, Seq.empty)
  }

  it should "optimize some binary operations when arguments match" in {
    // Signedness matters
    matchingArgs("sub", "UInt<8>", "UInt<8>", """ UInt<8>("h0")  """ )
    matchingArgs("sub", "SInt<8>", "SInt<8>", """ SInt<8>("h0")  """ )
    matchingArgs("div", "UInt<8>", "UInt<8>", """ UInt<8>("h1")  """ )
    matchingArgs("div", "SInt<8>", "SInt<8>", """ SInt<8>("h1")  """ )
    matchingArgs("rem", "UInt<8>", "UInt<8>", """ UInt<8>("h0")  """ )
    matchingArgs("rem", "SInt<8>", "SInt<8>", """ SInt<8>("h0")  """ )
    matchingArgs("and", "UInt<8>", "UInt<8>", """ i              """ )
    matchingArgs("and", "SInt<8>", "UInt<8>", """ asUInt(i)      """ )
    // Signedness doesn't matter
    matchingArgs("or",  "UInt<8>", "UInt<8>", """ i """ )
    matchingArgs("or",  "SInt<8>", "UInt<8>", """ asUInt(i) """ )
    matchingArgs("xor", "UInt<8>", "UInt<8>", """ UInt<8>("h0")  """ )
    matchingArgs("xor", "SInt<8>", "UInt<8>", """ UInt<8>("h0")  """ )
    // Always true
    matchingArgs("eq",  "UInt<8>", "UInt<1>", """ UInt<1>("h1")  """ )
    matchingArgs("leq", "UInt<8>", "UInt<1>", """ UInt<1>("h1")  """ )
    matchingArgs("geq", "UInt<8>", "UInt<1>", """ UInt<1>("h1")  """ )
    // Never true
    matchingArgs("neq", "UInt<8>", "UInt<1>", """ UInt<1>("h0")  """ )
    matchingArgs("lt",  "UInt<8>", "UInt<1>", """ UInt<1>("h0")  """ )
    matchingArgs("gt",  "UInt<8>", "UInt<1>", """ UInt<1>("h0")  """ )
  }

  behavior of "Reduction operators"

  it should "optimize andr of a literal" in {
    val input =
      s"""|circuit Foo:
          |  module Foo:
          |    output _4b0: UInt<1>
          |    output _4b15: UInt<1>
          |    output _4b7: UInt<1>
          |    output _4b1: UInt<1>
          |    output _0b0: UInt<1>
          |    _4b0 <= andr(UInt<4>(0))
          |    _4b15 <= andr(UInt<4>(15))
          |    _4b7 <= andr(UInt<4>(7))
          |    _4b1 <= andr(UInt<4>(1))
          |    wire _0bI: UInt<0>
          |    _0bI is invalid
          |    _0b0 <= andr(_0bI)
          |""".stripMargin
    val check =
      s"""|circuit Foo:
          |  module Foo:
          |    output _4b0: UInt<1>
          |    output _4b15: UInt<1>
          |    output _4b7: UInt<1>
          |    output _4b1: UInt<1>
          |    output _0b0: UInt<1>
          |    _4b0 <= UInt<1>(0)
          |    _4b15 <= UInt<1>(1)
          |    _4b7 <= UInt<1>(0)
          |    _4b1 <= UInt<1>(0)
          |    _0b0 <= UInt<1>(1)
          |""".stripMargin
    execute(input, check, Seq.empty)
  }

  it should "optimize orr of a literal" in {
    val input =
      s"""|circuit Foo:
          |  module Foo:
          |    output _4b0: UInt<1>
          |    output _4b15: UInt<1>
          |    output _4b7: UInt<1>
          |    output _4b1: UInt<1>
          |    output _0b0: UInt<1>
          |    _4b0 <= orr(UInt<4>(0))
          |    _4b15 <= orr(UInt<4>(15))
          |    _4b7 <= orr(UInt<4>(7))
          |    _4b1 <= orr(UInt<4>(1))
          |    wire _0bI: UInt<0>
          |    _0bI is invalid
          |    _0b0 <= orr(_0bI)
          |""".stripMargin
    val check =
      s"""|circuit Foo:
          |  module Foo:
          |    output _4b0: UInt<1>
          |    output _4b15: UInt<1>
          |    output _4b7: UInt<1>
          |    output _4b1: UInt<1>
          |    output _0b0: UInt<1>
          |    _4b0 <= UInt<1>(0)
          |    _4b15 <= UInt<1>(1)
          |    _4b7 <= UInt<1>(1)
          |    _4b1 <= UInt<1>(1)
          |    _0b0 <= UInt<1>(0)
          |""".stripMargin
    execute(input, check, Seq.empty)
  }

  it should "optimize xorr of a literal" in {
    val input =
      s"""|circuit Foo:
          |  module Foo:
          |    output _4b0: UInt<1>
          |    output _4b15: UInt<1>
          |    output _4b7: UInt<1>
          |    output _4b1: UInt<1>
          |    output _0b0: UInt<1>
          |    _4b0 <= xorr(UInt<4>(0))
          |    _4b15 <= xorr(UInt<4>(15))
          |    _4b7 <= xorr(UInt<4>(7))
          |    _4b1 <= xorr(UInt<4>(1))
          |    wire _0bI: UInt<0>
          |    _0bI is invalid
          |    _0b0 <= xorr(_0bI)
          |""".stripMargin
    val check =
      s"""|circuit Foo:
          |  module Foo:
          |    output _4b0: UInt<1>
          |    output _4b15: UInt<1>
          |    output _4b7: UInt<1>
          |    output _4b1: UInt<1>
          |    output _0b0: UInt<1>
          |    _4b0 <= UInt<1>(0)
          |    _4b15 <= UInt<1>(0)
          |    _4b7 <= UInt<1>(1)
          |    _4b1 <= UInt<1>(1)
          |    _0b0 <= UInt<1>(0)
          |""".stripMargin
    execute(input, check, Seq.empty)
  }
}
