package firrtl.interpreter

import firrtl._

object CircuitState {
  def apply(circuit: Circuit): CircuitState = {
    require(circuit.modules.length == 1)
    val dependencyList = DependencyMapper(circuit.modules.head)
    new CircuitState(
      circuit.modules.head.ports.filter(_.direction == INPUT).map { port =>
        port -> TypeInstanceFactory(port.tpe)
      }.toMap,
      circuit.modules.head.ports.filter(_.direction == OUTPUT).map { port =>
        port -> TypeInstanceFactory(port.tpe)
      }.toMap,
      dependencyList.keys.map { case key => key -> TypeInstanceFactory(UIntType(IntWidth(1)))}.toMap,
      dependencyList
    )
  }

  def apply(state: CircuitState): CircuitState = {
    state.copy
  }
}

case class CircuitState(
                    inputPorts: Map[Port, TypeInstance],
                    outputPorts: Map[Port, TypeInstance],
                    registers: Map[Expression, TypeInstance],
                    dependencyList: Map[Expression, Expression]) {
  def copy: CircuitState = {
    new CircuitState(
      inputPorts.map { case (key, value) => key -> value.copy()},
      outputPorts.map { case (key, value) => key -> value.copy()},
      registers.map { case (key, value) => key -> value.copy()},
      dependencyList
    )
  }
}
