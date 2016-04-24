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

/**
  * Base class for tools used to update top level inputs
  *
  * @param interpreterCircuit The wrapped circuit to update, use it's inputs map
  *                           to determine names and widths
  */
abstract class InputUpdater(interpreterCircuit: InterpreterCircuit) {
  def getValue(name: String): ConcreteValue
  var calls = 0

  def updateAllInputs(circuitState: CircuitState): Unit = {
    calls += 1
    for(port <- interpreterCircuit.inputPortToValue.keys) {
      val value = getValue(port)
//      println(s"Updating input port ${port.name} <= $value")
      circuitState.inputPorts(port) = value
    }
  }
}
class RandomInputUpdater(val interpreterCircuit: InterpreterCircuit, randomSeed: Long = 0L)
  extends InputUpdater(interpreterCircuit) {
  val random = util.Random
  random.setSeed(randomSeed)

  def getValue(name: String): ConcreteValue = {
    val port = interpreterCircuit.nameToPort(name)
    def getWidth(width: Width): Int = width match {
      case iw: IntWidth => iw.width.toInt
    }
    port.tpe match {
      case u: UIntType =>
        TypeInstanceFactory(port.tpe, BigInt(getWidth(u.width), random))
//        TypeInstanceFactory(port.tpe, 1)
      case c: ClockType =>
        TypeInstanceFactory(port.tpe, calls % 2)
    }
  }

}
