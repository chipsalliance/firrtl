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

class RegisterSpec extends FlatSpec with Matchers {
  behavior of "register reset"

  it should "reset registers when there condition is true" in {
    val input =
      """
        |circuit RegInc :
        |  module RegInc :
        |    input clk : Clock
        |    input reset1 : UInt<1>
        |
        |    reg reg1 : UInt<16>, clk with : (reset => (reset1, UInt<16>(3)))
        |
        |    reg1 <= add(reg1, UInt(1))
        |
      """.stripMargin

    val interpreter = FirrtlTerp(input)
    val inputUpdater = new MappedInputUpdater(interpreter.interpreterCircuit) {
      override def step_values: Array[Map[String, BigInt]] = Array(
        Map("reset1" -> 1),
        Map("reset1" -> 0),
        Map("reset1" -> 0),
        Map("reset1" -> 1),
        Map("reset1" -> 0)
      )
    }

    interpreter.setInputUpdater(inputUpdater)

    interpreter.setVerbose(true)
    interpreter.doOneCycle()
    interpreter.sourceState.registers("reg1").value should be (3)
    interpreter.doOneCycle()
    interpreter.sourceState.registers("reg1").value should be (4)
    interpreter.doOneCycle()
    interpreter.sourceState.registers("reg1").value should be (5)
    interpreter.doOneCycle()
    interpreter.sourceState.registers("reg1").value should be (3)
    interpreter.doOneCycle()
    interpreter.sourceState.registers("reg1").value should be (4)

  }
  it should "be able to initialize registers from other places" in {
    val input =
      """
        |circuit Stop0 :
        |  module Stop0 :
        |    input clk : Clock
        |    input reset1 : UInt<1>
        |    input reset2 : UInt<1>
        |
        |    reg reg1 : UInt<16>, clk with : (reset => (reset1, UInt<16>(0)))
        |    reg reg2 : UInt<16>, clk with : (reset => (reset2, reg1))
        |
        |    reg1 <= add(reg1, UInt(1))
        |    reg2 <= add(reg2, UInt(3))
        |
      """.stripMargin

    val interpreter = FirrtlTerp(input)
    val inputUpdater = new ManualMappedInputUpdater

    interpreter.setInputUpdater(inputUpdater)

    // interpreter.setVerbose(true)
    inputUpdater.setValues(Map("reset1" -> 1, "reset2" -> 1))
    interpreter.doCycles(1)
    interpreter.sourceState.registers("reg1").value should be (0)
    interpreter.sourceState.registers("reg2").value should be (0)

    inputUpdater.setValues(Map("reset1" -> 0, "reset2" -> 0))
    interpreter.doCycles(1)
    interpreter.sourceState.registers("reg1").value should be (1)
    interpreter.sourceState.registers("reg2").value should be (3)

    inputUpdater.setValues(Map("reset1" -> 0, "reset2" -> 0))
    interpreter.doCycles(1)
    interpreter.sourceState.registers("reg1").value should be (2)
    interpreter.sourceState.registers("reg2").value should be (6)

    inputUpdater.setValues(Map("reset1" -> 1, "reset2" -> 0))
    interpreter.doCycles(1)
    interpreter.sourceState.registers("reg1").value should be (0)
    interpreter.sourceState.registers("reg2").value should be (9)

    inputUpdater.setValues(Map("reset1" -> 0, "reset2" -> 0))
    interpreter.doCycles(1)
    interpreter.sourceState.registers("reg1").value should be (1)
    interpreter.sourceState.registers("reg2").value should be (12)

    inputUpdater.setValues(Map("reset1" -> 0, "reset2" -> 1))
    interpreter.doCycles(1)
    interpreter.sourceState.registers("reg1").value should be (2)
    interpreter.sourceState.registers("reg2").value should be (1)

  }
}
