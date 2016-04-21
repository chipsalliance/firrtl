package firrtl.interpreter

import firrtl.interpreter._
import firrtl._

/**
  * Created by chick on 4/21/16.
  */

case class TypeInstance(value: BigInt, min: BigInt, max: BigInt, signed: Boolean = false)

object TypeInstanceFactory {
  def apply(typ: Type): TypeInstance = {
    def getWidth(w: Width) = w match {
      case iw: IntWidth => math.pow(2.0, iw.width.toDouble).toInt
      case _ => throw new InterpreterException(s"unknown width for interpreter $typ")
    }
    typ match {
      case u: UIntType => TypeInstance(0, 0, getWidth(u.width))
      case s: SIntType =>
        val width = getWidth(s.width)
        TypeInstance(0, -width, width, signed = true)
      case c: ClockType => TypeInstance(0, 0, 1)
      case _ => throw new InterpreterException(s"Unsupported LoFIRRTL type for interperter $typ")
    }
  }
}
