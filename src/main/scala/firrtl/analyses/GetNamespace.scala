// See LICENSE for license details.

package firrtl.analyses

import firrtl.annotations.NoTargetAnnotation
import firrtl.{CircuitForm, CircuitState, LowForm, Namespace, Transform}

case class ModuleNamespaceAnnotation(namespace: Namespace) extends NoTargetAnnotation

/** Create a namespace with this circuit
  *
  * namespace is used by RenameModules to get unique names
  */
class GetNamespace extends Transform {
  def inputForm: CircuitForm = LowForm
  def outputForm: CircuitForm = LowForm

  def execute(state: CircuitState): CircuitState = {
    val namespace = Namespace(state.circuit)
    state.copy(annotations = new ModuleNamespaceAnnotation(namespace) +: state.annotations)
  }
}
