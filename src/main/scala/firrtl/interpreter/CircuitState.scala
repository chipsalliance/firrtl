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

import scala.collection.mutable

object CircuitState {
  def apply(circuit: Circuit): CircuitState = apply(InterpreterCircuit(circuit))

  def apply(interpreterCircuit: InterpreterCircuit): CircuitState = {
    val circuitState = new CircuitState(
      interpreterCircuit.inputPortToValue,
      interpreterCircuit.outputPortToValue,
      interpreterCircuit.makeRegisterToConcreteValueMap,
      new mutable.HashMap[String, Concrete](),
      interpreterCircuit.dependencyGraph.memories
    )
    circuitState
  }

  def apply(state: CircuitState): CircuitState = {
    state.getNextState
  }
}

/**
  * Holds the state of the circuit at a particular time
  * State is kept for input, output and registers
  *
  * @param inputPorts  a map to current concrete value
  * @param outputPorts a map to current concrete value
  * @param registers   a map to current concrete value
  */
case class CircuitState(
                    inputPorts:  mutable.Map[String, Concrete],
                    outputPorts: mutable.Map[String, Concrete],
                    registers:   mutable.Map[String, Concrete],
                    ephemera:    mutable.Map[String, Concrete] = new mutable.HashMap[String, Concrete](),
                    memories:    mutable.Map[String, Memory]   = new mutable.HashMap[String, Memory]) {
  val nextRegisters = new mutable.HashMap[String, Concrete]()

  val nameToConcreteValue = mutable.HashMap((inputPorts ++ outputPorts ++ registers).toSeq:_*)

  var stateCounter = 0

  def getNextState: CircuitState = {
    val nextState = new CircuitState(
      inputPorts.clone(),
      outputPorts.clone(),
      nextRegisters.clone(),
      ephemera.empty,
      memories.clone()
    )

    nextState.stateCounter = stateCounter + 1
    nextState.memories.values.foreach { memory => memory.cycle() }

    nextState
  }

  def setValue(key: String, concreteValue: Concrete): Concrete = {
    if(outputPorts.contains(key)) {
      outputPorts(key) = concreteValue
      nameToConcreteValue(key) = concreteValue
    }
    else if(registers.contains(key)) {
      println(s"Updating nextRegister $key => $concreteValue")
      nextRegisters(key) = concreteValue
      // we continue to use the initial values of registers when they appear on RHS of an expression
    }
    else if(isMemory(key)) {
      println(s"Updating memory interface $key => $concreteValue")
      key match {
        case Memory.KeyPattern(memoryName, _, _) => memories(memoryName).setValue(key, concreteValue)
        case _ =>
          throw new InterpreterException(s"Error:failed memory($key).setValue($key, $concreteValue)")
      }
    }
    else {
      ephemera(key) = concreteValue
      nameToConcreteValue(key) = concreteValue
    }
    concreteValue
  }

  def setInput(key: String, value: BigInt): Concrete = {
    val concreteValue = TypeInstanceFactory(inputPorts(key), value)
    inputPorts(key) = concreteValue
    nameToConcreteValue(key) = concreteValue
    concreteValue
  }

  def getValue(key: String): Option[Concrete] = {
    nameToConcreteValue.get(key) match {
      case Some(value) => Some(value)
      case _=>
        key match {
          case Memory.KeyPattern(memoryName, _, _) => Some(memories(memoryName).getValue(key))
          case _ => None
        }
    }
  }

  def isInput(key: String): Boolean = inputPorts.contains(key)
  def isOutput(key: String): Boolean = outputPorts.contains(key)
  def isRegister(key: String): Boolean = registers.contains(key)
  def isEphemera(key:String): Boolean = {
    ! (isInput(key) || isOutput(key) || isRegister(key))
  }
  def isMemory(key: String): Boolean = {
    val memKey = Memory.memoryKey(key)
    memories.contains(memKey)
  }

  /**
    * prints a human readable version of the state,
    *
    * @param dense if true puts input, output and registers on one line each
    * @return
    */
  def prettyString(dense: Boolean = true): String = {
    val (prefix, separator, postfix) = if(dense) (": ", ", ", "") else (":\n  ", "\n  ", "")
    def expression_name(e: Expression): String = {
      e match {
        case w: WRef => w.name
        case _ => e.toString
      }
    }
    def showConcreteValues(msg: String, m: Map[String, Concrete]): String = {
      m.keys.toSeq.sorted.map { case key =>
        s"$key=${m(key).value}"
      }.mkString(msg+prefix, separator, postfix)
    }
    s"""
       |CircuitState $stateCounter
       |${showConcreteValues("Inputs", inputPorts.toMap)}
       |${showConcreteValues("Outputs", outputPorts.toMap)}
       |${showConcreteValues("BeforeRegisters", registers.toMap)}
       |${showConcreteValues("AfterRegisters", nextRegisters.toMap)}
       |${showConcreteValues("Ephemera", ephemera.toMap)}
     """.stripMargin
  }
}
