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

import org.scalatest.{Matchers, FlatSpec}

/**
  * Created by chick on 4/29/16.
  */
class GCDTester extends FlatSpec with Matchers {
  behavior of "GCD"

  val gcdFirrtl =
    """
      |circuit GCD :
      |  module GCD :
      |    input clk : Clock
      |    input reset : UInt<1>
      |    input io_a : UInt<32>
      |    input io_b : UInt<32>
      |    input io_e : UInt<1>
      |    output io_z : UInt<32>
      |    output io_v : UInt<1>
      |    reg x : UInt<32>, clk with :
      |      reset => (UInt<1>("h0"), x)
      |    reg y : UInt<32>, clk with :
      |      reset => (UInt<1>("h0"), y)
      |    node T_13 = gt(x, y)
      |    node T_14 = sub(x, y)
      |    node T_15 = tail(T_14, 1)
      |    node T_17 = eq(T_13, UInt<1>("h0"))
      |    node T_18 = sub(y, x)
      |    node T_19 = tail(T_18, 1)
      |    node T_21 = eq(y, UInt<1>("h0"))
      |    node GEN_0 = mux(T_13, T_15, x)
      |    x <= mux(io_e, io_a, GEN_0)
      |    node GEN_1 = mux(T_17, T_19, y)
      |    y <= mux(io_e, io_b, GEN_1)
      |    io_z <= x
      |    io_v <= T_21
    """.stripMargin


  it should "run with InterpretedTester" in {
    new InterpretiveTester(gcdFirrtl) {
       interpreter.setVerbose()
      step(1)
      poke("io_a", 34)
      poke("io_b", 17)
      poke("io_e", 1)
      step(1)

      poke("io_e", 0)
      step(1)

      while(peek("io_v") != Big1) {
        step(1)
      }
      expect("io_z", 17)

    }
  }
}
