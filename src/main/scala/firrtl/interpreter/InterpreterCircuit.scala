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

/**
  * A convienence wrapper for a Circuit that provides methods for getting
  * port value maps used in the interpreter, and name to port mappings
  * Circuit must be LoFIRRTL
  *
  * @param circuit a LoFIRRTL circuit
  */
class InterpreterCircuit(val circuit: Circuit) {
  require(circuit.modules.length == 1)

  val module = circuit.modules.head

  val dependencyList = DependencyMapper(circuit.modules.head)

  def inputPortToValue = makePortToConcreteValueMap(INPUT)
  def outputPortToValue = makePortToConcreteValueMap(OUTPUT)

  val nameToPort = module.ports.map { port =>
    port.name -> port
  }.toMap

  val nameToRegister = new mutable.HashMap[String, Expression]

  def makeRegisterToConcreteValueMap: mutable.Map[Expression, ConcreteValue] = {
    val m = new mutable.HashMap[Expression, ConcreteValue]()
    val x = dependencyList.keys.map { case register =>
        register match {
          case WRef(name,tpe,RegKind(),_) =>
            m(register) = TypeInstanceFactory(tpe)
            nameToRegister(name) = register
          case _ => Unit
        }
    }
    m
  }

  def makePortToConcreteValueMap(direction: Direction) = {
    mutable.Map(module.ports.filter(_.direction == direction).map { port =>
      port -> TypeInstanceFactory(port.tpe)
    }: _*)
  }
}

object InterpreterCircuit {
  def apply(circuit: Circuit): InterpreterCircuit = {
    new InterpreterCircuit(circuit)
  }
}
