// See LICENSE for license details.

package firrtl

import firrtl.ir._
import firrtl.Utils.{min, max, pow_minus_one}
import passes.{IsMul, IsAdd, IsNeg, IsPow, IsMax, IsMin, IsFloor, IsConstrainable}

import com.typesafe.scalalogging.LazyLogging
import Implicits.{constraint2bound, constraint2width, width2constraint}

/** Definitions and Utility functions for [[ir.PrimOp]]s */
object PrimOps extends LazyLogging {
  /** Addition */
  case object Add extends PrimOp { override def toString = "add" }
  /** Subtraction */
  case object Sub extends PrimOp { override def toString = "sub" }
  /** Multiplication */
  case object Mul extends PrimOp { override def toString = "mul" }
  /** Division */
  case object Div extends PrimOp { override def toString = "div" }
  /** Remainder */
  case object Rem extends PrimOp { override def toString = "rem" }
  /** Less Than */
  case object Lt extends PrimOp { override def toString = "lt" }
  /** Less Than Or Equal To */
  case object Leq extends PrimOp { override def toString = "leq" }
  /** Greater Than */
  case object Gt extends PrimOp { override def toString = "gt" }
  /** Greater Than Or Equal To */
  case object Geq extends PrimOp { override def toString = "geq" }
  /** Equal To */
  case object Eq extends PrimOp { override def toString = "eq" }
  /** Not Equal To */
  case object Neq extends PrimOp { override def toString = "neq" }
  /** Padding */
  case object Pad extends PrimOp { override def toString = "pad" }
  /** Static Shift Left */
  case object Shl extends PrimOp { override def toString = "shl" }
  /** Static Shift Right */
  case object Shr extends PrimOp { override def toString = "shr" }
  /** Dynamic Shift Left */
  case object Dshl extends PrimOp { override def toString = "dshl" }
  /** Dynamic Shift Right */
  case object Dshr extends PrimOp { override def toString = "dshr" }
  /** Arithmetic Convert to Signed */
  case object Cvt extends PrimOp { override def toString = "cvt" }
  /** Negate */
  case object Neg extends PrimOp { override def toString = "neg" }
  /** Bitwise Complement */
  case object Not extends PrimOp { override def toString = "not" }
  /** Bitwise And */
  case object And extends PrimOp { override def toString = "and" }
  /** Bitwise Or */
  case object Or extends PrimOp { override def toString = "or" }
  /** Bitwise Exclusive Or */
  case object Xor extends PrimOp { override def toString = "xor" }
  /** Bitwise And Reduce */
  case object Andr extends PrimOp { override def toString = "andr" }
  /** Bitwise Or Reduce */
  case object Orr extends PrimOp { override def toString = "orr" }
  /** Bitwise Exclusive Or Reduce */
  case object Xorr extends PrimOp { override def toString = "xorr" }
  /** Concatenate */
  case object Cat extends PrimOp { override def toString = "cat" }
  /** Bit Extraction */
  case object Bits extends PrimOp { override def toString = "bits" }
  /** Head */
  case object Head extends PrimOp { override def toString = "head" }
  /** Tail */
  case object Tail extends PrimOp { override def toString = "tail" }
  /** Shift Binary Point Left **/
  case object BPShl extends PrimOp { override def toString = "bpshl" }
  /** Shift Binary Point Right **/
  case object BPShr extends PrimOp { override def toString = "bpshr" }
  /** Set Binary Point **/
  case object BPSet extends PrimOp { override def toString = "bpset" }
  /** Interpret As UInt */
  case object AsUInt extends PrimOp { override def toString = "asUInt" }
  /** Interpret As SInt */
  case object AsSInt extends PrimOp { override def toString = "asSInt" }
  /** Interpret As Clock */
  case object AsClock extends PrimOp { override def toString = "asClock" }
  /** Interpret as Fixed Point **/
  case object AsFixedPoint extends PrimOp { override def toString = "asFixedPoint" }
  /** Interpret as Interval (closed lower bound, closed upper bound, binary point) **/
  case object AsInterval extends PrimOp { override def toString = "asInterval" }
  /** Wrap First Operand Around Range/Width of Second Operand **/
  case object Wrap extends PrimOp { override def toString = "wrap" }
  /** Clip First Operand At Range/Width of Second Operand **/
  case object Clip extends PrimOp { override def toString = "clip" }

  private lazy val builtinPrimOps: Seq[PrimOp] =
    Seq(Add, Sub, Mul, Div, Rem, Lt, Leq, Gt, Geq, Eq, Neq, Pad, AsUInt, AsSInt, AsInterval, AsClock, Shl, Shr,
        Dshl, Dshr, Neg, Cvt, Not, And, Or, Xor, Andr, Orr, Xorr, Cat, Bits, Head, Tail, AsFixedPoint, BPShl, BPShr, BPSet, Wrap, Clip)
  private lazy val strToPrimOp: Map[String, PrimOp] = builtinPrimOps.map { case op : PrimOp=> op.toString -> op }.toMap

  /** Seq of String representations of [[ir.PrimOp]]s */
  lazy val listing: Seq[String] = builtinPrimOps map (_.toString)
  /** Gets the corresponding [[ir.PrimOp]] from its String representation */
  def fromString(op: String): PrimOp = strToPrimOp(op)

  // Width Constraint Functions
  def PLUS(w1: Width, w2: Width): IsConstrainable = IsAdd(w1, w2)
  def MAX(w1: Width, w2: Width): IsConstrainable = IsMax(w1, w2)
  def MINUS(w1: Width, w2: Width): IsConstrainable = IsAdd(w1, IsNeg(w2))
  //def POW (w1:Width) : Width = w1 match {
  //  case IntWidth(i) => IntWidth(pow_minus_one(BigInt(2), i))
  //  case _ => ExpWidth(w1)
  //}
  def MIN(w1: Width, w2: Width): IsConstrainable = IsMin(w1, w2)

  def set_primop_type(e: DoPrim): DoPrim = {
    def t1 = e.args.head.tpe
    def t2 = e.args(1).tpe
    def t3 = e.args(2).tpe
    def w1 = getWidth(e.args.head.tpe)
    def w2 = getWidth(e.args(1).tpe)
    def p1 = t1 match {
      case FixedType(w, p) => p
      case IntervalType(min, max, p) => p
      case _ => sys.error(s"Cannot get binary point from $t1")
    }
    def p2 = t2 match {
      case FixedType(w, p) => p
      case IntervalType(min, max, p) => p
      case _ => sys.error(s"Cannot get binary point from $t1")
    } //Intentional
    def c1 = IntWidth(e.consts.head)
    def c2 = IntWidth(e.consts(1))
    def o1 = e.consts(0)
    def o2 = e.consts(1)
    def o3 = e.consts(2)
    e copy (tpe = e.op match {
      case Add => (t1, t2) match {
        case (_: UIntType, _: UIntType) => UIntType(IsAdd(IsMax(w1, w2), IntWidth(1)))
        case (_: SIntType, _: SIntType) => SIntType(IsAdd(IsMax(w1, w2), IntWidth(1)))
        case (_: FixedType, _: FixedType) => FixedType(IsAdd(IsAdd(IsMax(p1, p2), IsMax(IsAdd(w1, IsNeg(p1)), IsAdd(w2, IsNeg(p2)))), IntWidth(1)), IsMax(p1, p2))
        case (IntervalType(l1, u1, p1), IntervalType(l2, u2, p2)) => IntervalType(IsAdd(l1, l2), IsAdd(u1, u2), IsMax(p1, p2))
        case _ => UnknownType
      }
      case Sub => (t1, t2) match {
        case (_: UIntType, _: UIntType) => UIntType(IsAdd(IsMax(w1, w2), IntWidth(1)))
        case (_: SIntType, _: SIntType) => SIntType(IsAdd(IsMax(w1, w2), IntWidth(1)))
        case (_: FixedType, _: FixedType) => FixedType(IsAdd(IsAdd(IsMax(p1, p2),IsMax(IsAdd(w1, IsNeg(p1)), IsAdd(w2, IsNeg(p2)))),IntWidth(1)), IsMax(p1, p2))
        case (IntervalType(l1, u1, p1), IntervalType(l2, u2, p2)) => IntervalType(IsAdd(l1, IsNeg(u2)), IsAdd(u1, IsNeg(l2)), IsMax(p1, p2))
        case _ => UnknownType
      }
      case Mul => (t1, t2) match {
        case (_: UIntType, _: UIntType) => UIntType(IsAdd(w1, w2))
        case (_: SIntType, _: SIntType) => SIntType(IsAdd(w1, w2))
        case (_: FixedType, _: FixedType) => FixedType(IsAdd(w1, w2), IsAdd(p1, p2))
        case (IntervalType(l1, u1, p1), IntervalType(l2, u2, p2)) =>
          IntervalType(
            IsMin(IsMul(l1, l2), IsMul(l1, u2), IsMul(u1, l2), IsMul(u1, u2)),
            IsMax(IsMul(l1, l2), IsMul(l1, u2), IsMul(u1, l2), IsMul(u1, u2)),
            IsAdd(p1, p2)
          )
        case _ => UnknownType
      }
      case Div => (t1, t2) match {
        case (_: UIntType, _: UIntType) => UIntType(w1)
        case (_: SIntType, _: SIntType) => SIntType(IsAdd(w1, IntWidth(1)))
        case _ => UnknownType
      }
      case Rem => (t1, t2) match {
        case (_: UIntType, _: UIntType) => UIntType(MIN(w1, w2))
        case (_: SIntType, _: SIntType) => SIntType(MIN(w1, w2))
        case _ => UnknownType
      }
      case Lt => (t1, t2) match {
        case (_: UIntType, _: UIntType) => Utils.BoolType
        case (_: SIntType, _: SIntType) => Utils.BoolType
        case (_: FixedType, _: FixedType) => Utils.BoolType
        case (_: IntervalType, _: IntervalType) => Utils.BoolType
        case _ => UnknownType
      }
      case Leq => (t1, t2) match {
        case (_: UIntType, _: UIntType) => Utils.BoolType
        case (_: SIntType, _: SIntType) => Utils.BoolType
        case (_: FixedType, _: FixedType) => Utils.BoolType
        case (_: IntervalType, _: IntervalType) => Utils.BoolType
        case _ => UnknownType
      }
      case Gt => (t1, t2) match {
        case (_: UIntType, _: UIntType) => Utils.BoolType
        case (_: SIntType, _: SIntType) => Utils.BoolType
        case (_: FixedType, _: FixedType) => Utils.BoolType
        case (_: IntervalType, _: IntervalType) => Utils.BoolType
        case _ => UnknownType
      }
      case Geq => (t1, t2) match {
        case (_: UIntType, _: UIntType) => Utils.BoolType
        case (_: SIntType, _: SIntType) => Utils.BoolType
        case (_: FixedType, _: FixedType) => Utils.BoolType
        case (_: IntervalType, _: IntervalType) => Utils.BoolType
        case _ => UnknownType
      }
      case Eq => (t1, t2) match {
        case (_: UIntType, _: UIntType) => Utils.BoolType
        case (_: SIntType, _: SIntType) => Utils.BoolType
        case (_: FixedType, _: FixedType) => Utils.BoolType
        case (_: IntervalType, _: IntervalType) => Utils.BoolType
        case _ => UnknownType
      }
      case Neq => (t1, t2) match {
        case (_: UIntType, _: UIntType) => Utils.BoolType
        case (_: SIntType, _: SIntType) => Utils.BoolType
        case (_: FixedType, _: FixedType) => Utils.BoolType
        case (_: IntervalType, _: IntervalType) => Utils.BoolType
        case _ => UnknownType
      }
      case Pad => t1 match {
        case _: UIntType => UIntType(IsMax(w1, c1))
        case _: SIntType => SIntType(IsMax(w1, c1))
        case _: FixedType => FixedType(IsMax(w1, c1), p1)
        case _ => UnknownType
      }
      case AsUInt => t1 match {
        case _: UIntType => UIntType(w1)
        case _: SIntType => UIntType(w1)
        case _: FixedType => UIntType(w1)
        case ClockType => UIntType(IntWidth(1))
        case AnalogType(w) => UIntType(w1)
        case _: IntervalType => UIntType(w1)
        case _ => UnknownType
      }
      case AsSInt => t1 match {
        case _: UIntType => SIntType(w1)
        case _: SIntType => SIntType(w1)
        case _: FixedType => SIntType(w1)
        case ClockType => SIntType(IntWidth(1))
        case _: AnalogType => SIntType(w1)
        case _: IntervalType => SIntType(w1)
        case _ => UnknownType
      }
      case AsFixedPoint => t1 match {
        case _: UIntType => FixedType(w1, c1)
        case _: SIntType => FixedType(w1, c1)
        case _: FixedType => FixedType(w1, c1)
        case ClockType => FixedType(IntWidth(1), c1)
        case _: AnalogType => FixedType(w1, c1)
        case _: IntervalType => FixedType(w1, c1)
        case _ => UnknownType
      }
      case AsInterval => t1 match {
        case _: UIntType     => IntervalType(Closed(BigDecimal(o1)/BigDecimal(1 << o3.toInt)), Closed(BigDecimal(o2)/BigDecimal(1 << o3.toInt)), IntWidth(o3))
        case _: SIntType     => IntervalType(Closed(BigDecimal(o1)/BigDecimal(1 << o3.toInt)), Closed(BigDecimal(o2)/BigDecimal(1 << o3.toInt)), IntWidth(o3))
        case _: FixedType    => IntervalType(Closed(BigDecimal(o1)/BigDecimal(1 << o3.toInt)), Closed(BigDecimal(o2)/BigDecimal(1 << o3.toInt)), IntWidth(o3))
        case ClockType       => IntervalType(Closed(BigDecimal(o1)/BigDecimal(1 << o3.toInt)), Closed(BigDecimal(o2)/BigDecimal(1 << o3.toInt)), IntWidth(o3))
        case _: AnalogType   => IntervalType(Closed(BigDecimal(o1)/BigDecimal(1 << o3.toInt)), Closed(BigDecimal(o2)/BigDecimal(1 << o3.toInt)), IntWidth(o3))
        // Chisel shifts up and rounds first.
        case _: IntervalType => IntervalType(Closed(BigDecimal(o1)/BigDecimal(1 << o3.toInt)), Closed(BigDecimal(o2)/BigDecimal(1 << o3.toInt)), IntWidth(o3))
        case _ => UnknownType
      }
      case AsClock => t1 match {
        case _: UIntType => ClockType
        case _: SIntType => ClockType
        case ClockType => ClockType
        case _: AnalogType => ClockType
        case _: IntervalType => ClockType
        case _ => UnknownType
      }
      case Shl => t1 match {
        case _: UIntType => UIntType(IsAdd(w1, c1))
        case _: SIntType => SIntType(IsAdd(w1, c1))
        case _: FixedType => FixedType(IsAdd(w1,c1), p1)
        case IntervalType(l, u, p) => IntervalType(IsMul(l, Closed(BigDecimal(1 << o1.toInt))), IsMul(u, Closed(BigDecimal(1 << o1.toInt))), p)
        case _ => UnknownType
      }
      // Bit ops (not "math" friendly -- doesn't track precision)
      // aaa.bbb -> a.aab
      case Shr => t1 match {
        case _: UIntType => UIntType(IsMax(IsAdd(w1, IsNeg(c1)), IntWidth(1)))
        case _: SIntType => SIntType(IsMax(IsAdd(w1, IsNeg(c1)), IntWidth(1)))
        case _: FixedType => FixedType(IsMax(IsMax(IsAdd(w1, IsNeg(c1)), IntWidth(1)), p1), p1)
        case IntervalType(l, u, p) => 
          val shiftMul = Closed(BigDecimal(1) / BigDecimal(1 << o1.toInt))
          // BP is inferred at this point
          val bpRes = Closed(BigDecimal(1) / BigDecimal(1 << p.get.toInt))
          val bpResInv = Closed(BigDecimal(1 << p.get.toInt))
          val newL = IsMul(IsFloor(IsMul(IsMul(l, shiftMul), bpResInv)), bpRes)
          val newU = IsMul(IsFloor(IsMul(IsMul(u, shiftMul), bpResInv)), bpRes)
          // BP doesn't grow
          IntervalType(newL, newU, p)
        case _ => UnknownType
      }
      case Dshl => t1 match {
        case _: UIntType => UIntType(IsAdd(w1, IsAdd(IsPow(w2), Closed(-1))))
        case _: SIntType => SIntType(IsAdd(w1, IsAdd(IsPow(w2), Closed(-1))))
        case _: FixedType => FixedType(IsAdd(w1, IsAdd(IsPow(w2), Closed(-1))), p1)
        case IntervalType(l, u, p) => 
          val maxShiftAmt = IsAdd(IsPow(w2), Closed(-1))
          val shiftMul = IsPow(maxShiftAmt)
          // Magnitude matters! i.e. if l is negative, shifting by the largest amount makes the outcome more negative
          // whereas if l is positive, shifting by the largest amount makes the outcome more positive (in this case, the lower bound is the previous l)
          val newL = IsMin(l, IsMul(l, shiftMul))
          val newU = IsMax(u, IsMul(u, shiftMul))
          // BP doesn't grow
          IntervalType(newL, newU, p)
        case _ => UnknownType
      }
      case Dshr => t1 match {
        case _: UIntType => UIntType(w1)
        case _: SIntType => SIntType(w1)
        case _: FixedType => FixedType(w1, p1)
        // Decreasing magnitude -- don't need more bits
        case IntervalType(l, u, p) => IntervalType(l, u, p)
        case _ => UnknownType
      }
      case Cvt => t1 match {
        case _: UIntType => SIntType(IsAdd(w1, IntWidth(1)))
        case _: SIntType => SIntType(w1)
        case _ => UnknownType
      }
      case Neg => t1 match {
        case _: UIntType => SIntType(IsAdd(w1, IntWidth(1)))
        case _: SIntType => SIntType(IsAdd(w1, IntWidth(1)))
        case _ => UnknownType
      }
      case Not => t1 match {
        case _: UIntType => UIntType(w1)
        case _: SIntType => UIntType(w1)
        case _ => UnknownType
      }
      case And => (t1, t2) match {
        case (_: SIntType | _: UIntType, _: SIntType | _: UIntType) => UIntType(IsMax(w1, w2))
        case _ => UnknownType
      }
      case Or => (t1, t2) match {
        case (_: SIntType | _: UIntType, _: SIntType | _: UIntType) => UIntType(IsMax(w1, w2))
        case _ => UnknownType
      }
      case Xor => (t1, t2) match {
        case (_: SIntType | _: UIntType, _: SIntType | _: UIntType) => UIntType(IsMax(w1, w2))
        case _ => UnknownType
      }
      case Andr => t1 match {
        case (_: UIntType | _: SIntType) => Utils.BoolType
        case _ => UnknownType
      }
      case Orr => t1 match {
        case (_: UIntType | _: SIntType) => Utils.BoolType
        case _ => UnknownType
      }
      case Xorr => t1 match {
        case (_: UIntType | _: SIntType) => Utils.BoolType
        case _ => UnknownType
      }
      case Cat => (t1, t2) match {
        case (_: UIntType | _: SIntType | _: FixedType | _: IntervalType, _: UIntType | _: SIntType | _: FixedType | _: IntervalType) => UIntType(IsAdd(w1, w2))
        case (t1, t2) => UnknownType
      }
      case Bits => t1 match {
        case (_: UIntType | _: SIntType | _: FixedType | _: IntervalType) => UIntType(IsAdd(IsAdd(c1, IsNeg(c2)), IntWidth(1)))
        case _ => UnknownType
      }
      case Head => t1 match {
        case (_: UIntType | _: SIntType | _: FixedType | _: IntervalType) => UIntType(c1)
        case _ => UnknownType
      }
      case Tail => t1 match {
        case (_: UIntType | _: SIntType | _: FixedType | _: IntervalType) => UIntType(IsAdd(w1, IsNeg(c1)))
        case _ => UnknownType
      }
      // aaa.bbb -> aaa.bbb00
      case BPShl => t1 match {
        case _: FixedType => FixedType(IsAdd(w1,c1), IsAdd(p1, c1))
        // Keeps the same exact value, but adds more precision for the future i.e. aaa.bbb -> aaa.bbb00
        case IntervalType(l, u, p) => IntervalType(l, u, IsAdd(p, c1))
        case _ => UnknownType
      }
      // Decrease precision aaa.bbb -> aaa.b
      case BPShr => t1 match {
        case _: FixedType => FixedType(IsAdd(w1,IsNeg(c1)), IsAdd(p1, IsNeg(c1)))
        case IntervalType(l, u, p) => 
          val shiftMul = Closed(BigDecimal(1) / BigDecimal(1 << o1.toInt))
          // BP is inferred at this point
          // newBPRes is the only difference in calculating bpshr from shr
          // y = floor(x * 2^(-amt + bp)) gets rid of precision --> y * 2^(-bp + amt) 
          // without amt, same op as shr
          val newBPRes = Closed(BigDecimal(1 << o1.toInt) / BigDecimal(1 << p.get.toInt))
          val bpResInv = Closed(BigDecimal(1 << p.get.toInt))
          val newL = IsMul(IsFloor(IsMul(IsMul(l, shiftMul), bpResInv)), newBPRes)
          val newU = IsMul(IsFloor(IsMul(IsMul(u, shiftMul), bpResInv)), newBPRes)
          // BP doesn't grow
          IntervalType(newL, newU, IsAdd(p, IsNeg(c1)))
        case _ => UnknownType
      }
      // aaa.bbb -> aaa.bb
      // Expand or shrink precision to
      case BPSet => t1 match {
        case _: FixedType => FixedType(IsAdd(c1, IsAdd(w1, IsNeg(p1))), c1)
        case IntervalType(l, u, p) => 
          val newBPResInv = Closed(BigDecimal(1 << o1.toInt))
          val newBPRes = Closed(BigDecimal(1) / BigDecimal(1 << o1.toInt))
          val newL = IsMul(IsFloor(IsMul(l, newBPResInv)), newBPRes)
          val newU = IsMul(IsFloor(IsMul(u, newBPResInv)), newBPRes)
          IntervalType(newL, newU, c1)
        case _ => UnknownType
      }
      // = override range
      case Wrap => (t1, t2) match {
        case (IntervalType(l1, u1, p1), IntervalType(l2, u2, _)) if c1 != 2 => IntervalType(l2, u2, p1)
        // Conditionally reassign interval -- only if new bounds don't exceed previous bounds
        // TODO: (angie) -- maybe this should ride on clip instead?
        case (IntervalType(l1, u1, p1), IntervalType(l2, u2, _)) if c1 == 2 => IntervalType(IsMax(l1, l2), IsMin(u1, u2), p1)
        case (IntervalType(l1, u1, p1), _: SIntType) => IntervalType(IsNeg(IsPow(IsAdd(w2, Closed(-1)))), IsAdd(IsPow(IsAdd(w2, Closed(-1))), Closed(-1)), p1)
        case (IntervalType(l1, u1, p1), _: UIntType) => IntervalType(Closed(0), IsAdd(IsPow(w2), Closed(-1)), p1)
        case _ => UnknownType
      }
      case Clip => (t1, t2) match {
        case (IntervalType(l1, u1, p1), IntervalType(l2, u2, _)) => IntervalType(IsMax(l1, l2), IsMin(u1, u2), p1)
        case (IntervalType(l1, u1, p1), _: SIntType) => IntervalType(IsMax(IsNeg(IsPow(IsAdd(w2, Closed(-1)))), l1), IsMin(IsAdd(IsPow(IsAdd(w2, Closed(-1))), Closed(-1)), u1), p1)
        case (IntervalType(l1, u1, p1), _: UIntType) => IntervalType(IsMax(Closed(0), l1), IsMin(u1, IsAdd(IsPow(w2), Closed(-1))), p1)
        case _ => UnknownType
      }
    })
  }
}
