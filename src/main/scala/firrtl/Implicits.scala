// See LICENSE for license details.

package firrtl

import firrtl.ir._
import firrtl.passes.IsConstrainable

object Implicits {
  implicit def int2WInt(i: Int): WrappedInt = WrappedInt(BigInt(i))
  implicit def bigint2WInt(i: BigInt): WrappedInt = WrappedInt(i)
  implicit def constraint2bound(c: IsConstrainable): Bound = c match {
    case x: Bound => x
    case x => CalcBound(x)
  }
  implicit def constraint2width(c: IsConstrainable): Width = c.optimize() match {
    case Closed(x) if x.isWhole => IntWidth(x.toBigInt)
    case CalcWidth(Closed(x)) if x.isWhole => IntWidth(x.toBigInt)
    case x => CalcWidth(x)
  }
  implicit def width2constraint(w: Width): IsConstrainable = w match {
    case CalcWidth(x: IsConstrainable) => x
    case IntWidth(x) => Closed(BigDecimal(x))
    case UnknownWidth => UnknownBound
    case VarWidth(x) => VarBound(x)
  }
}
case class WrappedInt(value: BigInt) {
  def U: Expression = UIntLiteral(value, IntWidth(Utils.getUIntWidth(value)))
  def S: Expression = SIntLiteral(value, IntWidth(Utils.getSIntWidth(value)))
}
