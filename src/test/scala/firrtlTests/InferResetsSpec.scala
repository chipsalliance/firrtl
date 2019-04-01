// See LICENSE for license details.

package firrtlTests

import firrtl._
import firrtl.ir._
import firrtl.passes.InferResets
import FirrtlCheckers._

class InferResetsSpec extends FirrtlFlatSpec {
  def compile(input: String, compiler: Compiler = new MiddleFirrtlCompiler): CircuitState =
    compiler.compileAndEmit(CircuitState(parse(input), ChirrtlForm), List.empty)

  behavior of "Reset Inference"

  val BoolType = UIntType(IntWidth(1))

  it should "work across Module boundaries" in {
    val result = compile(s"""
      |circuit top :
      |  module child :
      |    input clock : Clock
      |    input childReset : Reset
      |    input x : UInt<8>
      |    output z : UInt<8>
      |    reg r : UInt<8>, clock with : (reset => (childReset, UInt(123)))
      |    r <= x
      |    z <= r
      |  module top :
      |    input clock : Clock
      |    input reset : UInt<1>
      |    input x : UInt<8>
      |    output z : UInt<8>
      |    inst c of child
      |    c.clock <= clock
      |    c.childReset <= reset
      |    c.x <= x
      |    z <= c.z
      |""".stripMargin
    )
    result should containTree { case Port(_, "childReset", Input, BoolType) => true }
  }

  it should "work across multiple Module boundaries" in {
    val result = compile(s"""
      |circuit top :
      |  module child :
      |    input resetIn : Reset
      |    output resetOut : Reset
      |    resetOut <= resetIn
      |  module top :
      |    input clock : Clock
      |    input reset : UInt<1>
      |    input x : UInt<8>
      |    output z : UInt<8>
      |    inst c of child
      |    c.resetIn <= reset
      |    reg r : UInt<8>, clock with : (reset => (c.resetOut, UInt(123)))
      |    r <= x
      |    z <= r
      |""".stripMargin
    )
    result should containTree { case Port(_, "resetIn", Input, BoolType) => true }
    result should containTree { case Port(_, "resetOut", Output, BoolType) => true }
  }

  it should "work in nested aggregates" in {
    val result = compile(s"""
      |circuit top :
      |  module top :
      |    output io : { flip in : { a : AsyncReset, b: UInt<1> }, out : { a : Reset, b: Reset }[2] }
      |    io.out[0] <= io.in
      |    io.out[1] <= io.in
      |""".stripMargin,
      new LowFirrtlCompiler
    )
    result should containTree { case Port(_, "io_in_a", Input, AsyncResetType) => true }
    result should containTree { case Port(_, "io_in_b", Input, BoolType) => true }
    result should containTree { case Port(_, "io_out_0_a", Input, AsyncResetType) => true }
    result should containTree { case Port(_, "io_out_1_a", Input, AsyncResetType) => true }
    result should containTree { case Port(_, "io_out_0_b", Input, BoolType) => true }
    result should containTree { case Port(_, "io_out_1_b", Input, BoolType) => true }
  }

  it should "not allow different Reset Types to drive a single Reset" in {
    an [InferResets.DifferingDriverTypesException] shouldBe thrownBy {
      val result = compile(s"""
        |circuit top :
        |  module top :
        |    input reset0 : AsyncReset
        |    input reset1 : UInt<1>
        |    output out : Reset
        |    wire w1 : Reset
        |    wire w2 : Reset
        |    w1 <= reset0
        |    w2 <= reset1
        |    out <= w1
        |    out <= w2
        |""".stripMargin
      )
    }
  }

  it should "not allow Vecs to infer different Reset Types" in {
    an [InferResets.MultiResetTypesException] shouldBe thrownBy {
      val result = compile(s"""
        |circuit top :
        |  module top :
        |    input reset0 : AsyncReset
        |    input reset1 : UInt<1>
        |    output out : Reset[2]
        |    out[0] <= reset0
        |    out[1] <= reset1
        |""".stripMargin
      )
    }
  }

  it should "not allow Vecs only be partially inferred" in {
    // Some exception should be thrown, TODO figure out which one
      val result = compile(s"""
        |circuit top :
        |  module top :
        |    input reset : AsyncReset
        |    output out : Reset[2]
        |    out is invalid
        |    out[0] <= reset0
        |""".stripMargin
      )
  }
}

class TypeTreesSpec extends FirrtlFlatSpec {
  import firrtl.annotations.TargetToken
  import firrtl.annotations.TargetToken.{Field, Index}
  import firrtl.passes.InferResets._
  val BoolType = UIntType(IntWidth(1))
  "jack" should "be cool" in {
    //  Seq(Index(0)),
    //  Seq(Index(1))
    //)
    val input = Seq(
      (Seq(Field("a"), Field("x"), Field("m")), BoolType),
      (Seq(Field("a"), Field("x"), Field("n")), BoolType),
      (Seq(Field("a"), Field("y")), AsyncResetType),
      (Seq(Field("b"), Field("z")), BoolType)
    )
    //val input = Seq(
    //  (Seq(Index(0), Field("a")), BoolType),
    //  (Seq(Index(1), Field("a")), BoolType),
    //  (Seq(Index(0), Field("b")), AsyncResetType),
    //  (Seq(Index(1), Field("b")), AsyncResetType)
    //)
    println(TypeTree.fromTokens(input:_*))
  }
}
