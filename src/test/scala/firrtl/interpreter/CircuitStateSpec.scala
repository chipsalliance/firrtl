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

import firrtl._
import org.scalatest.{Matchers, FlatSpec}

import scala.collection.mutable

class CircuitStateSpec extends FlatSpec with Matchers {
  behavior of "CircuitState"

  val u1Type = UIntType(IntWidth(1))
  val u1Instance = TypeInstanceFactory(u1Type)
  val port0 = Port(NoInfo, "port0", INPUT, u1Type)
  val port1 = Port(NoInfo, "port1", OUTPUT, u1Type)
  val c = CircuitState(
    inputPorts  = mutable.Map(port0.name -> u1Instance),
    outputPorts = mutable.Map(port1.name -> u1Instance),
    registers   = mutable.Map("reg1" -> u1Instance, "reg2" -> u1Instance),
    memories    = mutable.Map())

  it should "be creatable" in {
    c.inputPorts.size should be (1)
    c.outputPorts.size should be (1)
    c.outputPorts.size should be (1)
    c.registers.size   should be (2)
    c.memories.size    should be (0)
  }

  it should "become stale when a value is set" in {
    c.isStale = false
    c.setValue("port0", ConcreteUInt(1, 1))
    c.isStale should be (true)
  }
  it should "not allow cycle to be called on a stale state" in {
    c.isStale = true
    intercept[AssertionError] {
      c.cycle()
    }
  }

  it should "clear registers and ephemera when cycle is called" in {
    c.setValue("reg1",   u1Instance)
    c.setValue("reg2",   u1Instance)
    c.setValue("wire0", u1Instance)
    c.nextRegisters("reg1").value should be (0)
    c.getValue("reg1").get.value should be (0)
    c.getValue("wire0").get.value should be (0)
    c.isStale = false
    c.cycle()
    c.nextRegisters.size should be (0)
    c.ephemera.size should be (0)
  }

  it should "have mutable type instances, distinct in copy" in {
    c.inputPorts(port0.name) = ConcreteUInt(5, 4)

    c.inputPorts(port0.name).value should be (5)
  }
}
