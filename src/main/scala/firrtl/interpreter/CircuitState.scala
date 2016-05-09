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
  def apply(dependencyGraph: DependencyGraph): CircuitState = {
    val inputPortToValue  = makePortToConcreteValueMap(dependencyGraph, INPUT)
    val outputPortToValue = makePortToConcreteValueMap(dependencyGraph, OUTPUT)
    val registerToValue   = makeRegisterToConcreteValueMap(dependencyGraph)

    val circuitState = new CircuitState(
      inputPortToValue,
      outputPortToValue,
      registerToValue,
      dependencyGraph.memories,
      dependencyGraph.validNames
    )
    circuitState
  }

  def makeRegisterToConcreteValueMap(dependencyGraph: DependencyGraph): mutable.Map[String, Concrete] = {
    mutable.Map(dependencyGraph.registerNames.map { name =>
      name -> TypeInstanceFactory(dependencyGraph.getType(name))
    }.toSeq:_*)
  }

  def makePortToConcreteValueMap(dependencyGraph: DependencyGraph, direction: Direction) = {
    mutable.Map(dependencyGraph.module.ports.filter(_.direction == direction).map { port =>
      port.name -> TypeInstanceFactory(port.tpe)
    }: _*)
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
                    memories:    mutable.Map[String, Memory],
                    validNames:  mutable.HashSet[String]) {
  val nextRegisters = new mutable.HashMap[String, Concrete]()
  val ephemera      = new mutable.HashMap[String, Concrete]()

  var nameToConcreteValue = mutable.HashMap((inputPorts ++ outputPorts ++ registers).toSeq:_*)

  var stateCounter = 0
  var isStale      = true

  def getMemoryDependencies(memoryName: String, portName: String): Seq[String] = {
    memories(memoryName).getFieldDependencies(portName)
  }

  /**
    * in order to compute dependencies, ephemera must be clear and their
    * associated values cleared
    */
  def prepareForDependencyResolution(): Unit = {
    nameToConcreteValue = mutable.HashMap((inputPorts ++ outputPorts ++ registers).toSeq:_*)
    ephemera.clear()
  }
  /**
    * prepare this cycle
    */
  def cycle(): Unit = {
    assert(!isStale, s"Cycle cannot be called when stale, refresh should occur at a higher level")
    registers.keys.foreach { key => registers(key) = nextRegisters(key) }
    nextRegisters.clear()
    ephemera.clear()
    cycleMemories()
    nameToConcreteValue = mutable.HashMap((inputPorts ++ outputPorts ++ registers).toSeq:_*)
    isStale = true
    stateCounter += 1
  }
  def cycleMemories(): Unit = {
    memories.values.foreach { memory => memory.cycle() }
  }
  def setValue(key: String, concreteValue: Concrete): Concrete = {
    if(isInput(key)) {
      inputPorts(key) = concreteValue
      nameToConcreteValue(key) = concreteValue
    }
    else if(isOutput(key)) {
      outputPorts(key) = concreteValue
      nameToConcreteValue(key) = concreteValue
    }
    else if(registers.contains(key)) {
//      println(s"Updating nextRegister $key => $concreteValue")
      nextRegisters(key) = concreteValue
      // we continue to use the initial values of registers when they appear on RHS of an expression
    }
    else if(isMemory(key)) {
//      println(s"Updating memory interface $key => $concreteValue")
      key match {
        case Memory.KeyPattern(memoryName, _, _) => memories(memoryName).setValue(key, concreteValue)
        case _ =>
          throw new InterpreterException(s"Error:failed memory($key).setValue($key, $concreteValue)")
      }
    }
    else if(validNames.contains(key)) {
      ephemera(key) = concreteValue
      nameToConcreteValue(key) = concreteValue
    }
    else {
      throw InterpreterException(s"Error: setValue($key, $concreteValue) $key is not an element of this circuit")
    }
    isStale = true
    concreteValue
  }

  def setInput(key: String, value: BigInt): Concrete = {
    if(!isInput(key)) {

    }
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
    def showConcreteValues(msg: String, m: Map[String, Concrete]): String = {
      m.keys.toSeq.sorted.map { case key =>
        s"$key=${m(key).value}"
      }.mkString(msg+prefix, separator, postfix)
    }
    s"""
       |CircuitState $stateCounter (${if(isStale) "STALE" else "FRESH"})
       |${showConcreteValues("Inputs", inputPorts.toMap)}
       |${showConcreteValues("Outputs", outputPorts.toMap)}
       |${showConcreteValues("BeforeRegisters", registers.toMap)}
       |${showConcreteValues("AfterRegisters", nextRegisters.toMap)}
       |${showConcreteValues("Ephemera", ephemera.toMap)}
       |Memories${memories.values.mkString("\n", "\n  ", "")}""".stripMargin
  }
}
