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
  * Right now this class will be called once per cycle and
  * will update the inputs as the specific subclasses see fit.
  *
  */
abstract class InputUpdater {
  def getValue(name: String): BigInt
  var step = 0
  def nextStep(): Unit = {
    step += 1
  }

  def updateInput(circuitState: CircuitState, name: String): Unit = {
    val value = getValue(name)
    println(s"Updating input port $name <= $value")
    circuitState.setInput(name, value)
  }
  def updateInputs(circuitState: CircuitState, ports: Iterable[String] = Iterable.empty): Unit = {
    println(s"Update inputs step $step")
    val portsToDo = if(ports.nonEmpty) ports else circuitState.inputPorts.keys
    for(port <- portsToDo) {
      updateInput(circuitState, port)
    }
    nextStep()
  }
}

/**
  * pick a random number of suitable size for every input at each step
  * pathologically alternates clock for no good reason at the moment
  * @param interpreterCircuit used to find input ports
  * @param randomSeed randomize seed, defaults to 0L
  */
class RandomInputUpdater(interpreterCircuit: InterpreterCircuit, randomSeed: Long = 0L)
  extends InputUpdater {
  val random = util.Random
  random.setSeed(randomSeed)

  def getValue(name: String): BigInt = {
    def getPort(name: String) = {
      interpreterCircuit.dependencyGraph.nameToType(name)
    }
    def getWidth(width: Width): Int = width match {
      case iw: IntWidth => iw.width.toInt
    }

    getPort(name) match {
      case u: UIntType => BigInt(getWidth(u.width), random)
      case s: SIntType => BigInt(getWidth(s.width), random) * (if(random.nextBoolean()) 1 else -1)
      case c: ClockType => step % 2
    }
  }
}

/**
  * Allows for simpled hard coding of a series of inputs to be loaded into
  * desired inputs at each step
  * @param interpreterCircuit used to identify input ports
  */
abstract class MappedInputUpdater(val interpreterCircuit: InterpreterCircuit)
  extends InputUpdater {
  def step_values: Array[Map[String, BigInt]]

  def getValue(name: String): BigInt = {
    val value = step_values(step)(name)
    value
  }

  override def updateInputs(circuitState: CircuitState, ports: Iterable[String] = Iterable.empty): Unit = {
    super.updateInputs(circuitState, ports = step_values(step).keys)
  }
}

/**
  * Allows user to just set keys manually for the next step
  * just call set value before next interpreter.doCycle call
  * If set values is not called previous values will be used
  */
class ManualMappedInputUpdater extends InputUpdater {
  var stepValues: Map[String, BigInt] = Map.empty

  def getValue(name: String): BigInt = {
    val value = stepValues(name)
    value
  }
  /**
    * set a new map of keys to values
    * @param newValues complete map of new values for next cycle
    */
  def setValues(newValues: Map[String, BigInt]): Unit = {
    stepValues = newValues
  }
  override def updateInputs(circuitState: CircuitState, ports: Iterable[String] = Iterable.empty): Unit = {
    super.updateInputs(circuitState, ports = stepValues.keys)
  }
}

class EmptyUpdater extends InputUpdater {
  def getValue(name: String): BigInt = Big0
  override def updateInputs(circuitState: CircuitState, ports: Iterable[String] = Iterable.empty): Unit = {
  }
}
