// See LICENSE for license details.

package firrtl.passes

import scala.collection.mutable
import firrtl.PrimOps._
import firrtl.ir._
import firrtl._
import firrtl.Mappers._
import firrtl.Utils.{sub_type, module_type, field_type, max, error}

/** Replaces IntervalType with SIntType, three AST walks:
  * 1) Align binary points
  *    - adds shift operators to primop args and connections
  *    - does not affect declaration- or inferred-types
  * 2) Replace declaration IntervalType's with SIntType's
  *    - for each declaration:
  *      a. remove non-zero binary points
  *      b. remove open bounds
  *      c. replace with SIntType
  * 3) Run InferTypes
  */
class RemoveIntervals extends Pass {
  def run(c: Circuit): Circuit = {
    val alignedCircuit = c map alignModuleBP
    val replacedCircuit = alignedCircuit map replaceModuleInterval
    InferTypes.run(replacedCircuit)
  }
  /* Replace interval types */
  private def replaceModuleInterval(m: DefModule): DefModule = m map replaceStmtInterval map replacePortInterval
  private def replaceStmtInterval(s: Statement): Statement = s map replaceTypeInterval map replaceStmtInterval map replaceExprInterval
  private def replaceExprInterval(e: Expression): Expression = e map replaceExprInterval match {
    //case DoPrim(AsInterval, args, consts, tpe) => DoPrim(AsSInt, args, Seq.empty, tpe)
    case DoPrim(BPShl, args, consts, tpe) => DoPrim(Shl, args, consts, tpe)
    case DoPrim(BPShr, args, consts, tpe) => DoPrim(Shr, args, consts, tpe)
    case other => other
  }
  private def replacePortInterval(p: Port): Port = p map replaceTypeInterval
  private def replaceTypeInterval(t: Type): Type = t match {
    case i@IntervalType(l: IsKnown, u: IsKnown, p: IntWidth) => SIntType(i.width)
    case i: IntervalType => sys.error(s"Shouldn't be here: $i")
    case v => v map replaceTypeInterval
  }

  /* Align interval binary points */
  private def alignModuleBP(m: DefModule): DefModule = m map alignStmtBP
  private def alignStmtBP(s: Statement): Statement = s map alignExpBP match {
    case c@Connect(info, loc, expr) => loc.tpe match {
      case IntervalType(_, _, p) => Connect(info, loc, fixBP(p)(expr))
      case _ => c
    }
    case c@PartialConnect(info, loc, expr) => loc.tpe match {
      case IntervalType(_, _, p) => PartialConnect(info, loc, fixBP(p)(expr))
      case _ => c
    }
    case other => other map alignStmtBP
  }
  private val opsToFix = Seq(Add, Sub, Lt, Leq, Gt, Geq, Eq, Neq) //Mul does not need to be fixed
  private def alignExpBP(e: Expression): Expression = e map alignExpBP match {
    case DoPrim(BPSet, Seq(arg), Seq(const), tpe: IntervalType) => fixBP(IntWidth(const))(arg)
    case DoPrim(o, args, consts, t) if opsToFix.contains(o) && (args.map(_.tpe).collect { case x: IntervalType => x }).size == args.size =>
      val minBP = 0 //TODO: change this if supporting negative binary points
      val maxBP = args.foldLeft(IntWidth(minBP): Width) { (maxBP, a) =>
        a.tpe match {
          case IntervalType(_, _, p) => maxBP max p
          case _ => sys.error("Shouldn't be here")
        }
      }
      DoPrim(o, args.map { a => fixBP(maxBP)(a) }, consts, t)
    case Mux(cond, tval, fval, t: IntervalType) => 
      val minBP = 0 //TODO: change this if supporting negative binary points
      val maxBP = Seq(tval, fval).foldLeft(IntWidth(minBP): Width) { (maxBP, a) =>
        a.tpe match {
          case IntervalType(_, _, p) => maxBP max p
          case _ => sys.error("Shouldn't be here")
        }
      }
      Mux(cond, fixBP(maxBP)(tval), fixBP(maxBP)(tval), t)
    case other => other
  }
  private def fixBP(p: Width)(e: Expression): Expression = (p, e.tpe) match {
    case (IntWidth(desired), IntervalType(l, u, IntWidth(current))) if desired == current => e
    case (IntWidth(desired), IntervalType(l, u, IntWidth(current))) if desired > current  =>
      DoPrim(BPShl, Seq(e), Seq(desired - current), IntervalType(l, u, IntWidth(desired)))
    case (IntWidth(desired), IntervalType(l, u, IntWidth(current))) if desired < current  =>
      DoPrim(BPShr, Seq(e), Seq(current - desired), IntervalType(l, u, IntWidth(desired)))
    case _ => sys.error("Shouldn't be here")
  }

}

// vim: set ts=4 sw=4 et:
