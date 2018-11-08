package firrtl.transforms

import firrtl.graph.{DiGraph, MutableDiGraph}
import firrtl.ir._
import firrtl._
import firrtl.{CircuitForm, CircuitState, LowForm, Transform}
import firrtl.Mappers._

import scala.collection.mutable

class ClockDomainAnalysis extends Transform {
  override def inputForm: CircuitForm = LowForm
  override def outputForm: CircuitForm = LowForm

  /** Plan.
    *
    * Per module, need pairs of register/memport with clock source
    *   Clock Source (per module):
    *     1. an input clock port
    *     2. a child's output clock port
    *     3. an asClock expression
    *   Clock Sink
    *    1. A register
    *    2. A mem port
    *
    * Per module, need paths
    *   Paths Source:
    *    1. A register output
    *    2. A mem port read data
    *    3. Input port
    *    4. child's output port
    *   Paths Sink:
    *    1. A register input, reset, init
    *    2. Any female part (except clock?) of an memport, like enable, address, write data, etc.
    *    3. Output port
    *    4. child's input port
    *
    * Walk bottom-to-top, per module build list of full paths
    * Walk bottom-to-top, per module build pairs of register/memport with (absolute) clock source
    *   Clock Source (absolute):
    *     1. module input clock port
    *     2. black box instance output clock port
    *     3. absolute path to asClock expression
    */
  override protected def execute(state: CircuitState): CircuitState = {
    state
  }


  /** Examples
    *
    * asClock(a), who owns a?
    * Path through an instance from register to register
    */
}



case class Component[T <: FirrtlNode](top: String, path: Seq[WDefInstance], ir: T) {
  val module: String = if(path.isEmpty) top else path.last.module
}

class SourceConnectivity private {
  private val diGraph = new MutableDiGraph[Component[Expression]]()

}

object SourceConnectivity {
  def apply(module: DefModule): SourceConnectivity = {
    val connectivity = new SourceConnectivity()

    def onStmt(s: Statement): Statement = s match {
      case w: DefWire =>
        val comps = expand(w.name, w.tpe)
        comps.map { e => connectivity.diGraph.addVertex(Component(module.name, Nil, e)) }
        w
    }

    module map onStmt
    connectivity
  }

  def expand(name: String, tpe: Type): Seq[Expression] = {
    val collection = new mutable.ArrayBuffer[Expression]()
    expand(collection)(WRef(name, tpe))
    collection
  }

  def expand(collection: mutable.ArrayBuffer[Expression])(e: Expression): Unit = {
    collection += e
    Utils.getKids(e) map expand(collection)
  }
}
