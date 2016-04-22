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
  val port = Port(NoInfo, "port0", INPUT, u1Type)
  val c = CircuitState(mutable.Map(port -> u1Instance), mutable.Map(), mutable.Map())

  it should "be creatable" in {
    c.inputPorts.size should be (1)
    c.outputPorts.size should be (0)
  }

  val new_c = c.copy

  it should "produce copies" in {
    new_c.inputPorts.size should be (1)
    new_c.outputPorts.size should be (0)
  }

  it should "have mutable type instances, distinct in copy" in {
    new_c.inputPorts(port) = UIntValue(5, IntWidth(4))

    c.inputPorts(port).value should be (0)
    new_c.inputPorts(port).value should be (5)
  }
}
