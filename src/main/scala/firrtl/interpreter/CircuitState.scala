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
    new CircuitState(
      interpreterCircuit.inputPortToValue,
      interpreterCircuit.outputPortToValue,
      interpreterCircuit.makeRegisterToConcreteValueMap
    )
  }

  def apply(state: CircuitState): CircuitState = {
    state.copy
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
                    inputPorts: mutable.Map[Port, ConcreteValue],
                    outputPorts: mutable.Map[Port, ConcreteValue],
                    registers: mutable.Map[String, ConcreteValue]) {
  def copy: CircuitState = {
    new CircuitState(
      inputPorts.clone(),
      outputPorts.clone(),
      registers.clone()
    )
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
    def showPorts(msg: String, m: Map[Port,ConcreteValue]): String = {
      m.keys.toSeq.sortBy(_.name).map { case key =>
        s"${key.name}=${m(key).value}"
      }.mkString(msg+prefix, separator, postfix)
    }
    def showRegisters(msg: String, m: Map[String,ConcreteValue]): String = {
      m.keys.toSeq.sorted.map { case key =>
        s"${key}=${m(key).value}"
      }.mkString(msg+prefix, separator, postfix)
    }
    s"""
       |CircuitState
       |${showPorts("Inputs", inputPorts.toMap)}
       |${showPorts("Outputs", outputPorts.toMap)}
       |${showRegisters("Registers", registers.toMap)}
     """.stripMargin
  }
}
