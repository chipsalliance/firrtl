// See LICENSE for license details.

package firrtl.passes

import firrtl._
import firrtl.ir._
import firrtl.Mappers._
import firrtl.options.PreservesAll

import scala.collection.mutable

object ResolveKinds extends Pass with PreservesAll[Transform] {

  override def prerequisites = firrtl.stage.Forms.WorkingIR

  @deprecated("This API should never have been public", "1.3.1")
  type KindMap = collection.mutable.LinkedHashMap[String, Kind]

  @deprecated("This API should never have been public", "1.3.1")
  def find_port(kinds: KindMap)(p: Port): Port = findPort(kinds)(p)

  @deprecated("This API should never have been public", "1.3.1")
  def find_stmt(kinds: KindMap)(s: Statement): Statement = findStmt(kinds)(s)

  @deprecated("This API should never have been public", "1.3.1")
  def resolve_expr(kinds: KindMap)(e: Expression): Expression = onExpr(kinds)(e)

  @deprecated("This API should never have been public", "1.3.1")
  def resolve_stmt(kinds: KindMap)(s: Statement): Statement = onStmt(kinds)(s)

  private type NewKindMap = mutable.Map[String, Kind]

  private def findPort(kinds: NewKindMap)(p: Port): Port = {
    kinds(p.name) = PortKind
    p
  }

  private def findStmt(kinds: NewKindMap)(s: Statement): Statement = {
    s match {
      case sx: DefWire => kinds(sx.name) = WireKind
      case sx: DefNode => kinds(sx.name) = NodeKind
      case sx: DefRegister => kinds(sx.name) = RegKind
      case sx: WDefInstance => kinds(sx.name) = InstanceKind
      case sx: DefMemory => kinds(sx.name) = MemKind
      case _ =>
    }
    s.map(findStmt(kinds))
  }

  private def onExpr(kinds: NewKindMap)(e: Expression): Expression = e match {
    case ex: WRef => ex.copy(kind = kinds(ex.name))
    case _ => e.map(onExpr(kinds))
  }

  private def onStmt(kinds: NewKindMap)(s: Statement): Statement =
    s.map(onStmt(kinds)).map(onExpr(kinds))

  def resolve_kinds(m: DefModule): DefModule = {
    val kinds = new mutable.HashMap[String, Kind]
    m.map(findPort(kinds))
     .map(findStmt(kinds))
     .map(onStmt(kinds))
  }

  def run(c: Circuit): Circuit =
    c.copy(modules = c.modules.map(resolve_kinds))
}
