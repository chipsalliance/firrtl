// See LICENSE for license details.

package firrtl.passes

import firrtl._
import firrtl.ir._
import firrtl.Mappers._
import firrtl.options.Dependency

object ResolveFlows extends Pass {

  override def prerequisites =
    Seq( Dependency(passes.ResolveKinds),
         Dependency(passes.InferTypes),
         Dependency(passes.Uniquify) ) ++ firrtl.stage.Forms.WorkingIR

  override def invalidates(a: Transform) = false

  private type ExprMap = collection.mutable.HashMap[IdKey[Expression], Expression]
  private type ExprCache = Map[Flow, ExprMap]

  def resolve_e(g: Flow, cache: ExprCache)(e: Expression): Expression = e match {
    case ex: WRef => ex copy (flow = g)
    case WSubField(exp, name, tpe, _) => WSubField(
      Utils.field_flip(exp.tpe, name) match {
        case Default => resolve_e(g, cache)(exp)
        case Flip => resolve_e(Utils.swap(g), cache)(exp)
      }, name, tpe, g)
    case WSubIndex(exp, value, tpe, _) =>
      WSubIndex(resolve_e(g, cache)(exp), value, tpe, g)
    case WSubAccess(exp, index, tpe, _) =>
      WSubAccess(resolve_e(g, cache)(exp), resolve_e(SourceFlow, cache)(index), tpe, g)
    case _ => e map resolve_e(g, cache)
  }

  def resolve_s(cache: ExprCache)(s: Statement): Statement = s match {
    //TODO(azidar): pretty sure don't need to do anything for Attach, but not positive...
    case IsInvalid(info, expr) =>
      IsInvalid(info, resolve_e(SinkFlow, cache)(expr))
    case Connect(info, loc, expr) =>
      Connect(info, resolve_e(SinkFlow, cache)(loc), resolve_e(SourceFlow, cache)(expr))
    case PartialConnect(info, loc, expr) =>
      PartialConnect(info, resolve_e(SinkFlow, cache)(loc), resolve_e(SourceFlow, cache)(expr))
    case sx => sx map resolve_e(SourceFlow, cache) map resolve_s(cache)
  }

  def resolve_flow(m: DefModule): DefModule = {
    val cache: ExprCache = Seq(SinkFlow -> new ExprMap, SourceFlow -> new ExprMap).toMap
    m map resolve_s(cache)
  }

  def run(c: Circuit): Circuit =
    c copy (modules = c.modules map resolve_flow)
}
