// See LICENSE for license details.

package firrtl.passes

import firrtl._
import firrtl.ir._
import firrtl.Mappers._

object ResolveFlows extends Pass {
  def resolve_e(g: Flow)(e: Expression): Expression = e match {
    case ex: WRef => ex copy (flow = g)
    case WSubField(exp, name, tpe, _) => WSubField(
      Utils.field_flip(exp.tpe, name) match {
        case Default => resolve_e(g)(exp)
        case Flip => resolve_e(Utils.swap(g))(exp)
      }, name, tpe, g)
    case WSubIndex(exp, value, tpe, _) =>
      WSubIndex(resolve_e(g)(exp), value, tpe, g)
    case WSubAccess(exp, index, tpe, _) =>
      WSubAccess(resolve_e(g)(exp), resolve_e(SourceFlow)(index), tpe, g)
    case _ => e map resolve_e(g)
  }

  def resolve_s(s: Statement): Statement = s match {
    //TODO(azidar): pretty sure don't need to do anything for Attach, but not positive...
    case IsInvalid(info, expr) =>
      IsInvalid(info, resolve_e(SinkFlow)(expr))
    case Connect(info, loc, expr) =>
      Connect(info, resolve_e(SinkFlow)(loc), resolve_e(SourceFlow)(expr))
    case PartialConnect(info, loc, expr) =>
      PartialConnect(info, resolve_e(SinkFlow)(loc), resolve_e(SourceFlow)(expr))
    case sx => sx map resolve_e(SourceFlow) map resolve_s
  }

  def resolve_flow(m: DefModule): DefModule = m map resolve_s

  def run(c: Circuit): Circuit =
    c copy (modules = c.modules map resolve_flow)
}

@deprecated("Use 'ResolveFlows'. This will be removed in 1.3", "1.2")
object ResolveGenders extends Pass {

  def run(c: Circuit): Circuit = ResolveFlows.run(c)

  def resolve_e(g: Gender)(e: Expression): Expression = ResolveFlows.resolve_e(g)(e)

  def resolve_s(s: Statement): Statement = ResolveFlows.resolve_s(s)

}
