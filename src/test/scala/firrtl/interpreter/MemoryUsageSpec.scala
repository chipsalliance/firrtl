/*
Copyright (c) 2014 - 2016 The Regents of the University of
California (Regents). All Rights Reserved.  Redistribution and use in
source and binary forms, with or without modification, are permitted
provided that the following conditions are met:
   * Redistributions of source code must retain the above
     copyright notice, this list of conditions and the following
     two paragraphs of disclaimer.
   * Redistributions in binary form must reproduce the above
     copyright notice, this list of conditions and the following
     two paragraphs of disclaimer in the documentation and/or other materials
     provided with the distribution.
   * Neither the name of the Regents nor the names of its contributors
     may be used to endorse or promote products derived from this
     software without specific prior written permission.
IN NO EVENT SHALL REGENTS BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS,
ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
REGENTS HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE. THE SOFTWARE AND ACCOMPANYING DOCUMENTATION, IF
ANY, PROVIDED HEREUNDER IS PROVIDED "AS IS". REGENTS HAS NO OBLIGATION
TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
MODIFICATIONS.
*/
package firrtl.interpreter

import firrtl.{DefMemory, IntWidth, NoInfo, UIntType}
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by chick on 4/30/16.
  */
class MemoryUsageSpec extends FlatSpec with Matchers {

  behavior of "chirrtl mems"

  it should "parse and run ok" in {
    val chirrltMemInput =
      """
        |circuit ChirrtlMems :
        |  module ChirrtlMems :
        |    input clk : Clock
        |    input reset : UInt<1>
        |    mem ram :
        |      data-type => UInt<32>
        |      depth => 16
        |      read-latency => 0
        |      write-latency => 1
        |      reader => r
        |      writer => w
        |      read-under-write => undefined
        |    node newClock = clk
        |    wire wen : UInt<1>
        |    reg raddr : UInt<4>, clk with :
        |      reset => (reset, UInt<1>("h0"))
        |    node newerClock = clk
        |    reg waddr : UInt<4>, clk with :
        |      reset => (reset, UInt<1>("h0"))
        |    node GEN_0 = not(reset)
        |    node GEN_1 = gt(waddr, UInt<1>("h1"))
        |    node GEN_2 = and(GEN_0, GEN_1)
        |    node GEN_3 = neq(ram.r.data, raddr)
        |    node GEN_4 = and(GEN_2, GEN_3)
        |    printf(clk, GEN_4, "Assertion failed! r =/= raddr\n")
        |    node GEN_5 = not(reset)
        |    node GEN_6 = gt(waddr, UInt<1>("h1"))
        |    node GEN_7 = and(GEN_5, GEN_6)
        |    node GEN_8 = neq(ram.r.data, raddr)
        |    node GEN_9 = and(GEN_7, GEN_8)
        |    stop(clk, GEN_9, 1)
        |    node GEN_10 = not(reset)
        |    node GEN_11 = eq(raddr, UInt<4>("hf"))
        |    node GEN_12 = and(GEN_10, GEN_11)
        |    stop(clk, GEN_12, 0)
        |    ram.r.addr <= raddr
        |    ram.r.en <= UInt<1>("h1")
        |    ram.r.clk <= clk
        |    ram.w.data <= validif(wen, waddr)
        |    ram.w.mask <= wen
        |    ram.w.addr <= validif(wen, waddr)
        |    ram.w.en <= wen
        |    ram.w.clk <= validif(wen, clk)
        |    wen <= not(reset)
        |    node GEN_13 = eq(waddr, UInt<1>("h0"))
        |    node GEN_14 = add(raddr, UInt<1>("h1"))
        |    node GEN_15 = mux(GEN_13, UInt<1>("h0"), GEN_14)
        |    node GEN_16 = add(raddr, UInt<1>("h1"))
        |    node GEN_17 = mux(wen, GEN_15, GEN_16)
        |    raddr <= bits(GEN_17, 3, 0)
        |    node GEN_18 = add(waddr, UInt<1>("h1"))
        |    waddr <= bits(GEN_18, 3, 0)
      """.stripMargin

    val tester = new InterpretiveTester(chirrltMemInput) {
      step(1)
    }
  }

  behavior of "memory primitives"

  it should "run this circuit" in {
    val input =
      """circuit Test :
        |  module Test :
        |    input clk : Clock
        |    input a : UInt<1>
        |    input b : UInt<1>
        |    input select : UInt<1>
        |    output c : UInt<1>
        |    mem m :
        |      data-type => { a : UInt<8>, b : UInt<8>}[2]
        |      depth => 32
        |      read-latency => 0
        |      write-latency => 1
        |      reader => read
        |      writer => write
        |    m.read.clk <= clk
        |    m.read.en <= UInt<1>(1)
        |    m.read.addr is invalid
        |    node x = m.read.data
        |    node y = m.read.data[0].b
        |
        |    m.write.clk <= clk
        |    m.write.en <= UInt<1>(0)
        |    m.write.mask is invalid
        |    m.write.addr is invalid
        |    wire w : { a : UInt<8>, b : UInt<8>}[2]
        |    w[0].a <= UInt<4>(2)
        |    w[0].b <= UInt<4>(3)
        |    w[1].a <= UInt<4>(4)
        |    w[1].b <= UInt<4>(5)
        |    m.write.data <= w
        |    c <= a
      """.stripMargin

    val tester = new InterpretiveTester(input) {
      poke("a", 1)
      poke("b", 0)
      poke("select", 0)

      step(1)

      def testC(): Unit = {
        val m = peek("c")
        println(s"got $m")
        step(1)
      }
      testC()

    }
  }
}
