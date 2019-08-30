// See LICENSE for license details.

package firrtlTests

import firrtl._
import firrtl.ir._
import firrtl.passes._
import firrtl.transforms._
import annotations._
import FirrtlCheckers._

class PromoteSubmoduleSpec extends SimpleTransformSpec {

  def emitter = new LowFirrtlEmitter
  def transforms = Seq(
    new IRToWorkingIR,
    new ResolveAndCheck,
    new HighFirrtlToMiddleFirrtl,
    new MiddleFirrtlToLowFirrtl,
    new PromoteSubmodule,
    new ResolveAndCheck,
    new HighFirrtlToMiddleFirrtl,
    new MiddleFirrtlToLowFirrtl
  )

  "Submodule" should "be promoted" in {
    val input = """
circuit top :
  module top :
    input i0 : UInt<8>
    input i1 : UInt<8>
    output o0 : UInt<8>
    output o1 : UInt<8>
    inst mid of middle
    mid.i0 <= i0
    mid.i1 <= and(i0, i1)
    o0 <= mid.o0
    o1 <= or(mid.o0, mid.o1)

  module middle :
    input i0 : UInt<8>
    input i1 : UInt<8>
    output o0 : UInt<8>
    output o1 : UInt<8>
    inst bot of bottom
    bot.i0 <= i0
    bot.i1 <= add(i0, i1)
    o0 <= bot.o0
    o1 <= xor(bot.o0, bot.o1)

  module bottom :
    input i0 : UInt<8>
    input i1 : UInt<8>
    output o0 : UInt<8>
    output o1 : UInt<8>
    o0 <= i0
    o1 <= i1
""".stripMargin

    val annos = Seq(new PromoteSubmoduleAnnotation(InstanceTarget("top", "middle", Nil, "bot", "bottom")))
    val res = compileAndEmit(CircuitState(parse(input), HighForm, annos))
    parse(res.getEmittedCircuit.value)
  }

   "Abe's test case" should "produce sane results" in {
     val input = """
circuit Top :
  module Top :
    input in0 : UInt<32>
    input in1_p : Analog<1>
    input in1_n : Analog<1>
    output out_p : Analog<1>
    output out_n : Analog<1>

    inst f of Foo
    f.in0 <= in0
    attach (in1_p, f.in1_p)
    attach (in1_n, f.in1_n)
    attach (f.out_p, out_p)
    attach (f.out_n, out_n)

  module Foo :
    input in0 : UInt<32>
    input in1_p : Analog<1>
    input in1_n : Analog<1>
    output out_p : Analog<1>
    output out_n : Analog<1>

    inst b of Bar
    b.in_control <= and(in0, UInt<2>("h3"))
    attach (b.in_data_p, in1_p)
    attach (b.in_data_n, in1_n)
    attach (b.out_data_p, out_p)
    attach (b.out_data_n, out_n)

  module Bar :
    input in_control : UInt<32>
    input in_data_p : Analog<1>
    input in_data_n : Analog<1>
    output out_control : UInt<32>
    output out_data_p : Analog<1>
    output out_data_n : Analog<1>

    out_control <= in_control
    attach (in_data_p, out_data_p)
    attach (in_data_n, out_data_n)
""".stripMargin

     val annos = Seq(new PromoteSubmoduleAnnotation(InstanceTarget("Top", "Foo", Nil, "b", "Bar")))
     val res = compileAndEmit(CircuitState(parse(input), HighForm, annos))
     println(res.getEmittedCircuit.value)
     parse(res.getEmittedCircuit.value)
   }

}
