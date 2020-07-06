// See LICENSE for license details.

package firrtl.passes

import firrtl._
import firrtl.ir._
import firrtl.Mappers._
import firrtl.traversals.Foreachers._

object ResolveKinds extends Pass {

  override def prerequisites = firrtl.stage.Forms.WorkingIR

  override def invalidates(a: Transform) = false

  type KindMap = collection.mutable.HashMap[String, Kind]
  private type ExprCache = collection.mutable.HashMap[IdKey[Expression], Expression]

  private def find_port(kinds: KindMap)(p: Port): Unit = {
    kinds(p.name) = PortKind
  }

  def resolve_expr(kinds: KindMap, cache: ExprCache)(e: Expression): Expression = {
    cache.getOrElseUpdate(IdKey(e),
      e match {
      case ex: WRef => ex copy (kind = kinds(ex.name))
      case _ => e map resolve_expr(kinds, cache)
    })
  }

  def resolve_stmt(cache: ExprCache)(kinds: KindMap)(s: Statement): Statement = {
    s match {
      case sx: DefWire => kinds(sx.name) = WireKind
      case sx: DefNode => kinds(sx.name) = NodeKind
      case sx: DefRegister => kinds(sx.name) = RegKind
      case sx: WDefInstance => kinds(sx.name) = InstanceKind
      case sx: DefMemory => kinds(sx.name) = MemKind
      case _ =>
    }
    s.map(resolve_stmt(cache)(kinds))
     .map(resolve_expr(kinds, cache))
  }

  def resolve_kinds(m: DefModule): DefModule = {
    val kinds = new KindMap
    val cache = new ExprCache
    m.foreach(find_port(kinds))
    m.map(resolve_stmt(cache)(kinds))
  }

  def run(c: Circuit): Circuit =
    c copy (modules = c.modules map resolve_kinds)
}
