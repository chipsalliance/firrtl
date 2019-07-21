// See LICENSE for license details.

package firrtl.transforms

import firrtl._
import firrtl.ir._
import firrtl.annotations._

case class ClockEdgeAnnotation(register: ReferenceTarget, edge: Edge) extends SingleTargetAnnotation[ReferenceTarget] {
  override val target: ReferenceTarget = register

  override def duplicate(n: ReferenceTarget): Annotation = ClockEdgeAnnotation(register, edge: Edge)
}

class CompleteClockEdge extends Transform {
  override def inputForm: CircuitForm = MidForm

  override def outputForm: CircuitForm = MidForm

  def addEdge(annoMap: Map[String, Edge])(s: Statement): Statement = s.mapStmt {
    case r: DefRegister =>
      annoMap.get(r.name) match {
        case Some(e) => r.copy(edge = e)
        case None => r
      }
    case s: Statement => s.mapStmt(addEdge(annoMap))
  }

  def execute(state: CircuitState): CircuitState = state.copy(state.circuit.copy(modules = state.circuit.modules.map(_.mapStmt(addEdge(state.annotations.collect { case ClockEdgeAnnotation(t, e) => t.name -> e }.toMap)))))
}
