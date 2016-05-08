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
  * Created by chick on 5/4/16.
  */
class DynamicMemorySearch extends FlatSpec with Matchers {
  behavior of "dynamic memory search"

  it should "run with correct results" in {
    val input =
    """
      |circuit DynamicMemorySearch :
      |  module DynamicMemorySearch :
      |    input clk : Clock
      |    input reset : UInt<1>
      |    output io : {flip isWr : UInt<1>, flip wrAddr : UInt<3>, flip data : UInt<6>, flip en : UInt<1>, target : UInt<3>, done : UInt<1>}
      |
      |    io is invalid
      |    reg index : UInt<3>, clk with : (reset => (reset, UInt<3>("h00")))
      |    cmem list : UInt<6>[8]
      |    infer mport memVal = list[index], clk
      |    node T_10 = eq(io.en, UInt<1>("h00"))
      |    node T_11 = eq(memVal, io.data)
      |    node T_13 = eq(index, UInt<3>("h07"))
      |    node T_14 = or(T_11, T_13)
      |    node over = and(T_10, T_14)
      |    when io.isWr :
      |      infer mport T_15 = list[io.wrAddr], clk
      |      T_15 <= io.data
      |      skip
      |    node T_17 = eq(io.isWr, UInt<1>("h00"))
      |    node T_18 = and(T_17, io.en)
      |    when T_18 :
      |      index <= UInt<1>("h00")
      |      skip
      |    node T_21 = eq(over, UInt<1>("h00"))
      |    node T_23 = eq(io.isWr, UInt<1>("h00"))
      |    node T_25 = eq(io.en, UInt<1>("h00"))
      |    node T_26 = and(T_23, T_25)
      |    node T_27 = and(T_26, T_21)
      |    when T_27 :
      |      node T_29 = add(index, UInt<1>("h01"))
      |      node T_30 = tail(T_29, 1)
      |      index <= T_30
      |      skip
      |    io.done <= over
      |    io.target <= index
    """.stripMargin

    val n = 8
    val w = 4

    new InterpretiveTester(input) {
      interpreter.setVerbose(true)
//      interpreter.sourceState.memories("list").setVerbose()

      val list = Array.fill(n)(0)
      random.setSeed(0L)

      // initialize memory
//      for(write_address <- 0 until n) {
//        println(s"Initializing memory address $write_address")
//        poke("io_en", 0)
//        poke("io_isWr", 1)
//        poke("io_wrAddr", write_address)
//        poke("io_data",   write_address)
//        step(1)
//      }

      for (k <- 0 until 16) {
        println(s"memory test iteration $k ${"X"*80}")

        // Compute a random address and value
        val wrAddr = random.nextInt(n - 1)
        val data   = random.nextInt((1 << w) - 1) + 10

        // poke it intro memory
        poke("io_en", 0)
        poke("io_isWr", 1)
        poke("io_wrAddr", wrAddr)
        poke("io_data",   data)
        println(s"memory test iteration $k setting mem($wrAddr) to $data ${"-"*80}")
        step(1)
        println(s"memory test iteration $k expected list is ${list.mkString(",")} ${"-"*80}")

        list(wrAddr) = data
        // SETUP SEARCH
        val target = if (k > 12) random.nextInt(1 << w) else data
        poke("io_isWr", 0)
        poke("io_data", target)
        poke("io_en", 1)
        step(1)
        poke("io_en", 0)
//        step(1)
        val expectedIndex = if (list.contains(target)) {
          list.indexOf(target)
        } else {
          list.length - 1
        }

        var waitCount = 0
        while(waitCount <= n && peek("io_done") == Big0) {
          println(s"Waiting for done $waitCount")
          step(1)
          waitCount += 1
        }

        println(s"Done wait count is $waitCount done is ${peek("io_done")} expected ${peek("io_target")} got $expectedIndex")
        expect("io_done", 1)
        expect("io_target", expectedIndex)
        step(1)
      }
    }
  }
}
