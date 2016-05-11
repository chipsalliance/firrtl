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

class LifeCellSpec extends FlatSpec with Matchers {
  behavior of "A Conway Life cell"

  it should "observe neighbor transition rules" in {
    val input =
      """
        | circuit LifeCell :
        |  module LifeCell :
        |    input clk : Clock
        |    input reset : UInt<1>
        |    output io : {flip running : UInt<1>, flip top_left : UInt<4>, flip top_center : UInt<4>, flip top_right : UInt<4>, flip mid_left : UInt<4>, flip mid_right : UInt<4>, flip bot_left : UInt<4>, flip bot_center : UInt<4>, flip bot_right : UInt<4>, flip F : UInt<1>, flip set_dead : UInt<1>, is_alive : UInt<1>}
        |
        |    io is invalid
        |    reg is_alive : UInt<1>, clk with : (reset => (reset, UInt<1>("h00")))
        |    node T_13 = add(io.top_left, io.top_center)
        |    node sum0 = tail(T_13, 1)
        |    node T_14 = add(io.top_right, io.mid_left)
        |    node sum1 = tail(T_14, 1)
        |    node T_15 = add(io.mid_right, io.bot_left)
        |    node sum2 = tail(T_15, 1)
        |    node T_16 = add(io.bot_center, io.bot_right)
        |    node sum3 = tail(T_16, 1)
        |    node T_17 = add(sum0, sum1)
        |    node sum4 = tail(T_17, 1)
        |    node T_18 = add(sum2, sum3)
        |    node sum5 = tail(T_18, 1)
        |    node T_19 = add(sum4, sum5)
        |    node neighbor_sum = tail(T_19, 1)
        |    node T_22 = and(io.running, is_alive)
        |    node T_24 = eq(neighbor_sum, UInt<2>("h02"))
        |    node T_26 = eq(neighbor_sum, UInt<2>("h03"))
        |    node T_27 = or(T_24, T_26)
        |    node T_29 = eq(is_alive, UInt<1>("h00"))
        |    node T_30 = and(io.running, T_29)
        |    node T_32 = eq(neighbor_sum, UInt<2>("h03"))
        |    node T_33 = mux(T_30, T_32, is_alive)
        |    node T_34 = mux(T_22, T_27, T_33)
        |    node T_35 = mux(io.set_dead, UInt<1>("h00"), T_34)
        |    node T_36 = mux(io.set_alive, UInt<1>("h01"), T_35)
        |    is_alive <= T_36
        |    io.is_alive <= is_alive
        |
      """.stripMargin

    new InterpretiveTester(input) {
      interpreter.setVerbose()
      def setAlive(alive: Boolean): Unit = {
        poke("io_running", 0)
        poke("io_set_alive", if(alive) 1 else 0)
        poke("io_set_dead",  if(alive) 0 else 1)
        step(1)
        poke("io_running", 1)
        poke("io_set_alive", 0)
        poke("io_set_dead",  1)
      }

      def setNeighborsIgnoreCenter(
                         ntl: Int, ntc: Int, ntr: Int,
                         nml: Int, nmc: Int, nmr: Int,
                         nbl: Int, nbc: Int, nbr: Int): Unit = {
        // center "neighbor" is the value of the cell itself
        //        poke("io_set_alive", nmc)
        poke("io_top_left", ntl)
        poke("io_top_center", ntc)
        poke("io_top_right", ntr)
        poke("io_mid_left", nml)

        poke("io_mid_right", nmr)
        poke("io_bot_left", nbl)
        poke("io_bot_center", nbc)
        poke("io_bot_right", nbr)
      }

      setAlive(true)
      // dead cell with no neighbors stays dead
      setNeighborsIgnoreCenter(
        0,0,0,
        0,0,0,
        0,0,0
      )
      step(1)
      expect("io_is_alive", 0)

      // dead cell with > 3 neighbors stays dead
      setNeighborsIgnoreCenter(
        1,1,1,
        1,0,1,
        1,1,1
      )
      step(1)
      expect("io_is_alive", 0)

      // live cell with > 3 neighbors stays dead
      setNeighborsIgnoreCenter(
        1,1,1,
        1,1,1,
        1,1,1
      )
      step(1)
      expect("io_is_alive", 0)

      // dead cell with exactly three neighbors becomes alive
      setNeighborsIgnoreCenter(
        1,0,0,
        1,0,0,
        1,0,0
      )
      step(1)
      expect("io_is_alive", 1)
      setNeighborsIgnoreCenter(
        1,0,0,
        0,0,1,
        0,1,0
      )
      step(1)
      expect("io_is_alive", 1)

      // live cell with one neighbor dies
      setNeighborsIgnoreCenter(
        0,0,0,
        0,1,1,
        0,0,0
      )
      step(1)
      expect("io_is_alive", 0)

      // live cell with exactly three neighbors stays alive
      setNeighborsIgnoreCenter(
        1,0,0,
        1,1,0,
        1,0,0
      )
      step(1)
      expect("io_is_alive", 1)

      // live cell with exactly four neighbors dies
      setNeighborsIgnoreCenter(
        1,0,0,
        1,1,1,
        1,0,0
      )
      step(1)
      expect("io_is_alive", 0)

      // test set_alive
      setNeighborsIgnoreCenter(
        0,0,0,
        0,0,0,
        0,0,0
      )

      step(1)
      poke("io_set_alive", 1)
      poke("io_set_dead", 0)
      poke("io_running", 1)
      step(1)
      expect("io_is_alive", 1)

      poke("io_set_alive", 1)
      poke("io_set_dead", 0)
      poke("io_running", 0)
      step(1)
      expect("io_is_alive", 1)

      poke("io_set_dead", 1)
      poke("io_set_alive", 0)
      poke("io_running", 1)
      step(1)
      expect("io_is_alive", 0)

      poke("io_set_dead", 1)
      poke("io_set_alive", 0)
      poke("io_running", 0)
      step(1)
      expect("io_is_alive", 0)

    }


  }

}
