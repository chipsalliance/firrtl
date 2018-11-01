// See LICENSE for license details.

package firrtl.passes

import scala.collection.mutable
import firrtl.PrimOps._
import firrtl.ir._
import firrtl._
import firrtl.Mappers._
import firrtl.Utils.{error, field_type, getUIntWidth, max, module_type, sub_type}
import Implicits.{bigint2WInt, int2WInt}
import Implicits.{constraint2bound, constraint2width, width2constraint}
import firrtl.constraint.IsKnown

import scala.math.BigDecimal.RoundingMode._

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
    val wiredCircuit = alignedCircuit map makeWireModule
    val replacedCircuit = wiredCircuit map replaceModuleInterval
    InferTypes.run(replacedCircuit)
  }
  /* Replace interval types */
  private def replaceModuleInterval(m: DefModule): DefModule = m map replaceStmtInterval map replacePortInterval
  private def replaceStmtInterval(s: Statement): Statement = s map replaceTypeInterval map replaceStmtInterval map replaceExprInterval
  private def replaceExprInterval(e: Expression): Expression = e map replaceExprInterval match {
    case DoPrim(AsInterval, Seq(a1), _, tpe) => DoPrim(AsSInt, Seq(a1), Seq.empty, tpe)
    case DoPrim(BPShl, args, consts, tpe) => DoPrim(Shl, args, consts, tpe)
    case DoPrim(BPShr, args, consts, tpe) => DoPrim(Shr, args, consts, tpe)
    case DoPrim(Clip, Seq(a1, _), Nil, tpe: IntervalType) =>
      // Output interval (pre-calculated)
      val clipLo = tpe.minAdjusted
      val clipHi = tpe.maxAdjusted
      // Input interval
      val (inLow, inHigh) = a1.tpe match {
        case t2: IntervalType => (t2.minAdjusted, t2.maxAdjusted)
        case _ => sys.error("Shouldn't be here")
      }
      val gtOpt = clipHi >= inHigh
      val ltOpt = clipLo <= inLow
      (gtOpt, ltOpt) match {
        // input range within output range -> no optimization
        case (true, true)  => a1
        case (true, false) => Mux(Lt(a1, clipLo.S), clipLo.S, a1)
        case (false, true) => Mux(Gt(a1, clipHi.S), clipHi.S, a1)
        case _             => Mux(Gt(a1, clipHi.S), clipHi.S, Mux(Lt(a1, clipLo.S), clipLo.S, a1))
      }

    case DoPrim(Squeeze, Seq(a1, a2), Nil, tpe: IntervalType) =>
      // Using (conditional) reassign interval w/o adding mux
      val a2tpe = a2.tpe.asInstanceOf[IntervalType]
      val a1tpe = a1.tpe.asInstanceOf[IntervalType]
      val min2 = a2tpe.min * BigDecimal(BigInt(1) << a1tpe.point.get.toInt)
      // Conservative
      val minOpt2 = min2.setScale(0, FLOOR).toBigInt
      val max2 = a2tpe.max * BigDecimal(BigInt(1) << a1tpe.point.get.toInt)
      val maxOpt2 = max2.setScale(0, CEILING).toBigInt
      val w2 = Seq(minOpt2.bitLength, maxOpt2.bitLength).max + 1
      val w1 = Seq(a1tpe.minAdjusted.bitLength, a1tpe.maxAdjusted.bitLength).max + 1
      if (w1 < w2)
        a1
      else {
        val bits = DoPrim(Bits, Seq(a1), Seq(w2 - 1, 0), UIntType(IntWidth(w2)))
        DoPrim(AsSInt, Seq(bits), Seq.empty, SIntType(IntWidth(w2)))
      }
    case DoPrim(Wrap, Seq(a1, a2), Nil, tpe: IntervalType) => a2.tpe match {
      // If a2 type is SInt, wrap around width
      // TODO: (chick) add this back in after working through Chisel details
      // case SIntType(IntWidth(w)) => AsSInt(Bits(a1, w - 1, 0))
      // If a2 type is Interval wrap around range. If UInt, wrap around width
      case t: IntervalType => //| _: UIntType => // TODO
        // Need to match binary points before getting *adjusted!
        val (wrapLo, wrapHi) = t.copy(point = tpe.point) match {
          //case UIntType(IntWidth(w))     => (BigInt(0), BigInt((Math.pow(2, w.toDouble) - 1).toInt)) // TODO
          case t: IntervalType => (t.minAdjusted, t.maxAdjusted)
        }
        val (inLo, inHi) = a1.tpe match {
          case t2: IntervalType => (t2.minAdjusted, t2.maxAdjusted)
          case _ => sys.error("Shouldn't be here")
        }
        // If (max input) - (max wrap) + (min wrap) is less then (maxwrap), we can optimize when (max input > max wrap)
        val range = wrapHi - wrapLo
        val ltOpt = Add(a1, (range + 1).S)
        val gtOpt = Sub(a1, (range + 1).S)
        // [Angie]: This is dangerous. Would rather throw compilation error right now than allow "Rem" without the user explicitly including it.   
        // If x < wl
        // output: wh - (wl - x) + 1 AKA x + r + 1
        // worst case: wh - (wl - xl) + 1 = wl   
        // -> xl + wr + 1 = wl
        // If x > wh
        // output: wl + (x - wh) - 1 AKA x - r - 1
        // worst case: wl + (xh - wh) - 1 = wh
        // -> xh - wr - 1 = wh
        val default = Add(Rem(Sub(a1, wrapLo.S), Sub(wrapHi.S, wrapLo.S)), wrapLo.S)
        (wrapHi >= inHi, wrapLo <= inLo, (inHi - range - 1) <= wrapHi, (inLo + range + 1) >= wrapLo) match {
          case (true, true, _, _) =>
            // println("Clip Opt: Full")
            a1
          case (true, _, _, true) =>
            // println("Clip Opt: Lt")
            Mux(Lt(a1, wrapLo.S), ltOpt, a1)
          case (_, true, true, _) =>
            // println("Clip Opt: Gt")
            Mux(Gt(a1, wrapHi.S), gtOpt, a1)
          // Note: inHi - range - 1 = wrapHi can't be true when inLo + range + 1 = wrapLo (i.e. simultaneous extreme cases don't work)
          case (_, _, true, true) =>
            // println("Clip Opt: None")
            Mux(Gt(a1, wrapHi.S), gtOpt, Mux(Lt(a1, wrapLo.S), ltOpt, a1))
          case _                          => 
            println(
              s" wrapHi >= inHi: ${wrapHi >= inHi} ; " +
              s"wrapLo <= inLo: ${wrapLo <= inLo} ; " + 
              s"(inHi - range - 1) <= wrapHi: ${(inHi - range - 1) <= wrapHi} ; " + 
              s"(inLo + range + 1) >= wrapLo: ${(inLo + range + 1) >= wrapLo}"
            )                       
            sys.error(s"Wraps with remainder currently unsupported. in: $inLo , $inHi ; out: $wrapLo , $wrapHi") // default
        }
      case _ => sys.error("Shouldn't be here")
    }
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
  private val opsToFix = Seq(Add, Sub, Lt, Leq, Gt, Geq, Eq, Neq, Wrap, Clip) //Mul does not need to be fixed
  private def alignExpBP(e: Expression): Expression = e map alignExpBP match {
    case DoPrim(BPSet, Seq(arg), Seq(const), tpe: IntervalType) => fixBP(IntWidth(const))(arg)
    case DoPrim(o, args, consts, t) if opsToFix.contains(o) && (args.map(_.tpe).collect { case x: IntervalType => x }).size == args.size =>
      val maxBP = args.map(_.tpe).collect { case IntervalType(_, _, p) => p }.reduce(_ max _)
      DoPrim(o, args.map { a => fixBP(maxBP)(a) }, consts, t)
    case Mux(cond, tval, fval, t: IntervalType) =>
      val maxBP = Seq(tval, fval).map(_.tpe).collect { case IntervalType(_, _, p) => p }.reduce(_ max _)
      Mux(cond, fixBP(maxBP)(tval), fixBP(maxBP)(fval), t)
    case other => other
  }
  private def fixBP(p: Width)(e: Expression): Expression = (p, e.tpe) match {
    case (IntWidth(desired), IntervalType(l, u, IntWidth(current))) if desired == current => e
    case (IntWidth(desired), IntervalType(l, u, IntWidth(current))) if desired > current  =>
      DoPrim(BPShl, Seq(e), Seq(desired - current), IntervalType(l, u, IntWidth(desired)))
    case (IntWidth(desired), IntervalType(l, u, IntWidth(current))) if desired < current  =>
      DoPrim(BPShr, Seq(e), Seq(current - desired), IntervalType(l, u, IntWidth(desired)))
    case x => sys.error(s"Shouldn't be here: $x")
  }

  // TODO: (adam) better way to assign correct widths to wires?
  private def makeWireModule(m: DefModule): DefModule = m map makeWireStmt
  private def makeWireStmt(s: Statement): Statement = s match {
    case DefNode(info, name, value) => value.tpe match {
      case IntervalType(l, u, p) =>
        val newType = IntervalType(l, u, p)
        Block(Seq(DefWire(info, name, newType), Connect(info, WRef(name, newType, WireKind, FEMALE), value)))
      case other => s
    }
    case other => other map makeWireStmt
  }

}