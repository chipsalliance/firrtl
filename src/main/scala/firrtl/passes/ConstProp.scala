/*
Copyright (c) 2014 - 2016 The Regents of the University of
California (Regents). All Rights Reserved.  Redistribution and use in
source and binary forms, with or without modification, are permitted
provided that the following conditions are met:
   * Redistributions of source code must retain the above
     copyright notice, this list of conditions and the following
     two paragraphs of disclaimer.
   * Redistributions in binary form must reproduce the above
     copyright notice, this list of conditions and the following
     two paragraphs of disclaimer in the documentation and/or other materials
     provided with the distribution.
   * Neither the name of the Regents nor the names of its contributors
     may be used to endorse or promote products derived from this
     software without specific prior written permission.
IN NO EVENT SHALL REGENTS BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS,
ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
REGENTS HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE. THE SOFTWARE AND ACCOMPANYING DOCUMENTATION, IF
ANY, PROVIDED HEREUNDER IS PROVIDED "AS IS". REGENTS HAS NO OBLIGATION
TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
MODIFICATIONS.
*/

package firrtl.passes

import firrtl._
import firrtl.Utils._
import firrtl.Mappers._

import annotation.tailrec

object ConstProp extends Pass {
  def name = "Constant Propagation"

  trait FoldLogicalOp {
    def fold(c1: UIntValue, c2: UIntValue): UIntValue
    def simplify(e: Expression, lhs: UIntValue, rhs: Expression): Expression

    def apply(e: DoPrim): Expression = (e.args(0), e.args(1)) match {
      case (lhs: UIntValue, rhs: UIntValue) => fold(lhs, rhs)
      case (lhs: UIntValue, rhs) => simplify(e, lhs, rhs)
      case (lhs, rhs: UIntValue) => simplify(e, rhs, lhs)
      case _ => e
    }
  }

  object FoldAND extends FoldLogicalOp {
    def fold(c1: UIntValue, c2: UIntValue) = UIntValue(c1.value & c2.value, c1.width max c2.width)
    def simplify(e: Expression, lhs: UIntValue, rhs: Expression) = lhs.width match {
      case IntWidth(w) if long_BANG(tpe(rhs)) == w =>
        if (lhs.value == 0) lhs // and(x, 0) => 0
        else if (lhs.value == (BigInt(1) << w.toInt) - 1) rhs // and(x, 1) => x
        else e
      case _ => e
    }
  }

  object FoldOR extends FoldLogicalOp {
    def fold(c1: UIntValue, c2: UIntValue) = UIntValue(c1.value | c2.value, c1.width max c2.width)
    def simplify(e: Expression, lhs: UIntValue, rhs: Expression) = lhs.width match {
      case IntWidth(w) if long_BANG(tpe(rhs)) == w =>
        if (lhs.value == 0) rhs // or(x, 0) => x
        else if (lhs.value == (BigInt(1) << w.toInt) - 1) lhs // or(x, 1) => 1
        else e
      case _ => e
    }
  }

  object FoldXOR extends FoldLogicalOp {
    def fold(c1: UIntValue, c2: UIntValue) = UIntValue(c1.value ^ c2.value, c1.width max c2.width)
    def simplify(e: Expression, lhs: UIntValue, rhs: Expression) = lhs.width match {
      case IntWidth(w) if long_BANG(tpe(rhs)) == w =>
        if (lhs.value == 0) rhs // xor(x, 0) => x
        else e
      case _ => e
    }
  }

  object FoldEqual extends FoldLogicalOp {
    def fold(c1: UIntValue, c2: UIntValue) = UIntValue(if (c1.value == c2.value) 1 else 0, IntWidth(1))
    def simplify(e: Expression, lhs: UIntValue, rhs: Expression) = lhs.width match {
      case IntWidth(w) if w == 1 && long_BANG(tpe(rhs)) == 1 =>
        if (lhs.value == 1) rhs // eq(x, 1) => x
        else e
      case _ => e
    }
  }

  object FoldNotEqual extends FoldLogicalOp {
    def fold(c1: UIntValue, c2: UIntValue) = UIntValue(if (c1.value != c2.value) 1 else 0, IntWidth(1))
    def simplify(e: Expression, lhs: UIntValue, rhs: Expression) = lhs.width match {
      case IntWidth(w) if w == 1 && long_BANG(tpe(rhs)) == w =>
        if (lhs.value == 0) rhs // neq(x, 0) => x
        else e
      case _ => e
    }
  }

  private def foldConcat(e: DoPrim) = (e.args(0), e.args(1)) match {
    case (UIntValue(xv, IntWidth(xw)), UIntValue(yv, IntWidth(yw))) => UIntValue(xv << yw.toInt | yv, IntWidth(xw + yw))
    case _ => e
  }

  private def foldShiftLeft(e: DoPrim) = e.consts(0).toInt match {
    case 0 => e.args(0)
    case x => e.args(0) match {
      case UIntValue(v, IntWidth(w)) => UIntValue(v << x, IntWidth(w + x))
      case SIntValue(v, IntWidth(w)) => SIntValue(v << x, IntWidth(w + x))
      case _ => e
    }
  }

  private def foldShiftRight(e: DoPrim) = e.consts(0).toInt match {
    case 0 => e.args(0)
    case x => e.args(0) match {
      // TODO when amount >= x.width, return a zero-width wire
      case UIntValue(v, IntWidth(w)) => UIntValue(v >> x, IntWidth((w - x) max 1))
      // take sign bit if shift amount is larger than arg width
      case SIntValue(v, IntWidth(w)) => SIntValue(v >> x, IntWidth((w - x) max 1))
      case _ => e
    }
  }

  private def foldComparison(e: DoPrim) = {
    def foldIfZeroedArg(x: Expression): Expression = {
      def isUInt(e: Expression): Boolean = tpe(e) match {
        case UIntType(_) => true
        case _ => false
      }
      def isZero(e: Expression) = e match {
          case UIntValue(value,_) => value == BigInt(0)
          case SIntValue(value,_) => value == BigInt(0)
          case _ => false
        }
      x match {
        case DoPrim(LESS_OP,      Seq(a,b),_,_) if(isUInt(a) && isZero(b)) => zero
        case DoPrim(LESS_EQ_OP,   Seq(a,b),_,_) if(isZero(a) && isUInt(b)) => one
        case DoPrim(GREATER_OP,   Seq(a,b),_,_) if(isZero(a) && isUInt(b)) => zero
        case DoPrim(GREATER_EQ_OP,Seq(a,b),_,_) if(isUInt(a) && isZero(b)) => one
        case e => e
      }
    }

    def foldIfOutsideRange(x: Expression): Expression = {
      //Note, only abides by a partial ordering
      case class Range(min: BigInt, max: BigInt) {
        def === (that: Range) =
          Seq(this.min, this.max, that.min, that.max)
            .sliding(2,1)
            .map(x => x(0) == x(1))
            .reduce(_ && _)
        def > (that: Range) = this.min > that.max
        def >= (that: Range) = this.min >= that.max
        def < (that: Range) = this.max < that.min
        def <= (that: Range) = this.max <= that.min
      }
      def range(e: Expression): Range = e match {
        case UIntValue(value, _) => Range(value, value)
        case SIntValue(value, _) => Range(value, value)
        case _ => tpe(e) match {
          case SIntType(IntWidth(width)) => Range(
            min = BigInt(0) - BigInt(2).pow(width.toInt - 1),
            max = BigInt(2).pow(width.toInt - 1) - BigInt(1)
          )
          case UIntType(IntWidth(width)) => Range(
            min = BigInt(0),
            max = BigInt(2).pow(width.toInt) - BigInt(1)
          )
        }
      }
      // Calculates an expression's range of values
      x match {
        case e: DoPrim => {
          def r0 = range(e.args(0))
          def r1 = range(e.args(1))
          e.op match {
            // Always true
            case LESS_OP       if (r0 < r1) => one
            case LESS_EQ_OP    if (r0 <= r1) => one
            case GREATER_OP    if (r0 > r1) => one
            case GREATER_EQ_OP if (r0 >= r1) => one
            // Always false
            case LESS_OP       if (r0 >= r1) => zero
            case LESS_EQ_OP    if (r0 > r1) => zero
            case GREATER_OP    if (r0 <= r1) => zero
            case GREATER_EQ_OP if (r0 < r1) => zero
            case _ => e
          }
        }
        case e => e
      }
    }
    foldIfZeroedArg(foldIfOutsideRange(e))
  }

  private def constPropPrim(e: DoPrim): Expression = e.op match {
    case SHIFT_LEFT_OP => foldShiftLeft(e)
    case SHIFT_RIGHT_OP => foldShiftRight(e)
    case CONCAT_OP => foldConcat(e)
    case AND_OP => FoldAND(e)
    case OR_OP => FoldOR(e)
    case XOR_OP => FoldXOR(e)
    case EQUAL_OP => FoldEqual(e)
    case NEQUAL_OP => FoldNotEqual(e)
    case LESS_OP|LESS_EQ_OP|GREATER_OP|GREATER_EQ_OP => foldComparison(e)
    case NOT_OP => e.args(0) match {
      case UIntValue(v, IntWidth(w)) => UIntValue(v ^ ((BigInt(1) << w.toInt) - 1), IntWidth(w))
      case _ => e
    }
    case BITS_SELECT_OP => e.args(0) match {
      case UIntValue(v, _) => {
        val hi = e.consts(0).toInt
        val lo = e.consts(1).toInt
        require(hi >= lo)
        UIntValue((v >> lo) & ((BigInt(1) << (hi - lo + 1)) - 1), widthBANG(tpe(e)))
      }
      case x if long_BANG(tpe(e)) == long_BANG(tpe(x)) => tpe(x) match {
        case t: UIntType => x
        case _ => DoPrim(AS_UINT_OP, Seq(x), Seq(), tpe(e))
      }
      case _ => e
    }
    case _ => e
  }

  private def constPropMuxCond(m: Mux) = {
    // Only propagate a value if its width matches the mux width
    def propagate(e: Expression, muxWidth: BigInt) = e match {
      case UIntValue(v, _) => UIntValue(v, IntWidth(muxWidth))
      case _ => tpe(e) match {
        case UIntType(IntWidth(w)) if muxWidth == w => e
        case _ => m
      }
    }
    (m.cond, m.tpe) match {
      case (UIntValue(c, _), UIntType(IntWidth(w))) => propagate(if (c == 1) m.tval else m.fval, w)
      case _ => m
    }
  }

  private def constPropMux(m: Mux): Expression = (m.tval, m.fval) match {
    case _ if m.tval == m.fval => m.tval
    case (t: UIntValue, f: UIntValue) =>
      if (t.value == 1 && f.value == 0 && long_BANG(m.tpe) == 1) m.cond
      else constPropMuxCond(m)
    case _ => constPropMuxCond(m)
  }

  private def constPropNodeRef(r: WRef, e: Expression) = e match {
    case _: UIntValue | _: SIntValue | _: WRef => e
    case _ => r
  }

  @tailrec
  private def constPropModule(m: InModule): InModule = {
    var nPropagated = 0L
    val nodeMap = collection.mutable.HashMap[String, Expression]()

    def constPropExpression(e: Expression): Expression = {
      val old = e map constPropExpression
      val propagated = old match {
        case p: DoPrim => constPropPrim(p)
        case m: Mux => constPropMux(m)
        case r: WRef if nodeMap contains r.name => constPropNodeRef(r, nodeMap(r.name))
        case x => x
      }
      if (old ne propagated)
        nPropagated += 1
      propagated
    }

    def constPropStmt(s: Stmt): Stmt = {
      s match {
        case x: DefNode => nodeMap(x.name) = x.value
        case _ =>
      }
      s map constPropStmt map constPropExpression
    }

    val res = InModule(m.info, m.name, m.ports, constPropStmt(m.body))
    if (nPropagated > 0) constPropModule(res) else res
  }

  def run(c: Circuit): Circuit = {
    val modulesx = c.modules.map {
      case m: ExModule => m
      case m: InModule => constPropModule(m)
    }
    Circuit(c.info, modulesx, c.main)
  }
}
