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
  def apply(circuit: Circuit): CircuitState = {
    require(circuit.modules.length == 1)
    val dependencyList = DependencyMapper(circuit.modules.head)
    new CircuitState(
      mutable.Map(
        circuit.modules.head.ports.filter(_.direction == INPUT).map { port =>
          port -> TypeInstanceFactory(port.tpe)
        }: _*
      ),
      mutable.Map(
        circuit.modules.head.ports.filter(_.direction == OUTPUT).map { port =>
          port -> TypeInstanceFactory(port.tpe)
        }: _*
      ),
      mutable.Map(
        dependencyList.keys.map { case key =>
          key -> TypeInstanceFactory(UIntType(IntWidth(1)))
        }.toSeq: _*
      ),
      dependencyList
    )
  }

  def apply(state: CircuitState): CircuitState = {
    state.copy
  }
}

case class CircuitState(
                    inputPorts: mutable.Map[Port, ConcreteValue],
                    outputPorts: mutable.Map[Port, ConcreteValue],
                    registers: mutable.Map[Expression, ConcreteValue],
                    dependencyList: Map[Expression, Expression]) {
  def copy: CircuitState = {
    new CircuitState(
      inputPorts.clone(),
      outputPorts.clone(),
      registers.clone(),
      dependencyList
    )
  }
}
