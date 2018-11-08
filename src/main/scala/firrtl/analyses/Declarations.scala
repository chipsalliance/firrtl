// See LICENSE for license details.

package firrtl.analyses

import firrtl._
import firrtl.ir._
import firrtl.Mappers._
import component._

import scala.collection.mutable


class Declarations {
  private val ir2component = mutable.HashMap[MemoizedHash[FirrtlNode], Component]()
  private val component2ir = mutable.HashMap[Component, MemoizedHash[FirrtlNode]]()
  private val references = mutable.HashMap[String, mutable.HashMap[String, Component]]()
  private val components = mutable.HashMap[Int, Component]()

  /** Check existence */
  def contains(c: Component) = component2ir.contains(c)
  def contains(tag: Int) = components.contains(tag)
  def contains(encapsulatingModule: String, name: String) = get(encapsulatingModule, name).isDefined

  /** Reference accessors */
  def get(encapsulatingModule: String, name: String): Option[Component] = references.get(encapsulatingModule).flatMap(_.get(name))
  def apply(encapsulatingModule: String, name: String): Component = references(encapsulatingModule)(name)

  /** Component accessors */
  def get(tag: Int): Option[Component] = components.get(tag)
  def apply(tag: Int): Component = components(tag)
  def get(ir: FirrtlNode): Option[Component] = ir2component.get(ir)
  def apply(ir: FirrtlNode): Component = ir2component(ir)

  /** Statement accessors */
  def getIR(encapsulatingModule: String, name: String): Option[FirrtlNode] = get(encapsulatingModule, name).flatMap(component2ir.get).map(_.t)
  def ir(encapsulatingModule: String, name: String): FirrtlNode = component2ir(apply(encapsulatingModule, name))
  def getIR(c: Component): Option[FirrtlNode] = component2ir.get(c).map(_.t)
  def ir(c: Component): FirrtlNode = component2ir(c)
  def getIR(tag: Int): Option[FirrtlNode] = get(tag).flatMap(component2ir.get).map(_.t)
  def ir(tag: Int): FirrtlNode = component2ir(apply(tag))
}

object Declarations {
  /**
    * TODO: optimize with annotations to not recompute
    * @param state
    * @return
    */
  def apply(state: CircuitState): Declarations = apply(state.circuit)
  def apply(circuit: Circuit): Declarations = {
    val declarations = new Declarations()
    circuit.modules.map(apply(declarations))
    declarations
  }
  def apply(module: DefModule): Declarations = {
    val declarations = new Declarations()
    apply(declarations)(module)
    declarations
  }
  private def apply(d: Declarations)(m: DefModule) = {
    // Initialize module's hashmaps
    d.references(m.name) = mutable.HashMap[String, Component]()
    def updateRef(name: String, ir: FirrtlNode): Unit = {
      val ref = Referable(name, m.name)
      d.references(m.name)(name) = ref
      d.component2ir(ref) = ir
      d.ir2component(ir) = ref
      d.components(ref.tag) = ref
    }
    def updateIrref(value: String, ir: FirrtlNode): Unit = {
      val irref = Irreferable(value, m.name)
      d.components(irref.tag) = irref
      d.component2ir(irref) = ir
      d.ir2component(ir) = irref
    }

    /** Walk statements */
    def onStmt(s: Statement): Statement = {
      s map onExpr match {
        case s: IsDeclaration => updateRef(s.name, s)
        case s: Print => updateIrref("print", s)
        case s: Stop => updateIrref("stop", s)
        case _ => s map onStmt
      }
      s // return unchanged statement
    }

    /** Walk expressions */
    def onExpr(e: Expression): Expression = e match {
      case _: WRef => e
      case p: DoPrim =>
        updateIrref(p.op.serialize, p)
        p map onExpr
      case p: Mux =>
        updateIrref("mux", p)
        p map onExpr
      case v: ValidIf =>
        updateIrref("validif", v)
        v map onExpr
      case _: WSubField| _: WSubIndex => e map onExpr
      case l: Literal =>
        updateIrref(l.value.toString, l)
        l
      case _ => e map onExpr
    }
    (m map onStmt).ports.foreach { p =>
      updateRef(p.name, p)
    }
  }
}


