// See LICENSE for license details.

package firrtl
package transforms

import firrtl.ir._
import firrtl.Mappers._
import firrtl.annotations._

import scala.collection.mutable

/** Apply Local Annotations to all nodes from the one found in annotations
  * @note Currently handle only registers
  * @note This pass must run before LowerTypes and RemoveReset
  */
class ApplyLocalAnnotations extends Transform {
  def inputForm = MidForm
  def outputForm = MidForm

  /**
    * run the transform
    * @param circuit the circuit
    * @param annotations all the annotations
    * @return annotated circuit
    */
  def run(circuit: Circuit, annotations: AnnotationSeq): Circuit = {

    val groups = annotations
      .collect{ case m : LocalAnnotations => m }
      .groupBy(_.target.serialize)

    val modulesByName = circuit.modules.collect { case module: firrtl.ir.Module =>  module.name -> module }.toMap

    /**
      * walk the module
      * add the annotation locally
      *
      * @param myModule module being searched for annotations
      */
    def processModule(myModule: DefModule): DefModule = {

      def processRegister(myRegister: DefRegister): DefRegister = {
        val fullNodeName = "~" + circuit.main + "|" + myModule.name + ">" + myRegister.name
        groups.get(fullNodeName) match {
          case Some(list : List[SingleTargetAnnotation[Target]]) => myRegister.copy(annos = myRegister.annos ::: list)
          case _ => myRegister
        }
      }

      def processStatements(statement: Statement): Statement = {
        statement match {
          case r : DefRegister => processRegister(r)
          case s               => s map processStatements
        }
      }
      myModule match {
        case module: firrtl.ir.Module =>
          module.copy(body = processStatements(module.body))
        case _ => myModule
      }
    }
    circuit map processModule
  }

  def execute(state: CircuitState): CircuitState = {
    val circuit = run(state.circuit, state.annotations)
    state.copy(annotations = state.annotations, circuit = circuit)
  }
}
