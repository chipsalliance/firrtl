package firrtl.range

import firrtl.ir.{IntWidth, UIntType, SIntType, GroundType, ClockType, Type}

//case class Value(value: BigInt) {
//  //TODO(izraelevitz): think of a better way to do this
//  def - (that: Value): Value = Value(value - that.value)
//  def + (that: Value): Value = Value(value + that.value)
//  def * (that: Value): Value = Value(value * that.value)
//  def / (that: Value): Value = Value(value / that.value)
//  def % (that: Value): Value = Value(value % that.value)
//  def << (that: Value): Value =
//    if(Range.isTooBig(that.value)) throw new RangeException(s"shift left value $that is too big")
//    else Value(value << that.value.toInt)
//  def >> (that: Value): Value =
//    if(Range.isTooBig(that.value)) throw new RangeException(s"shift right value $that is too big")
//    else Value(value >> that.value.toInt)
//  def & (that: Value): Value = Value(value & that.value)
//  def | (that: Value): Value = Value(value | that.value)
//  def ^ (that: Value): Value = Value(value ^ that.value)
//  def < (that: Value): Boolean = value < that.value
//  def <= (that: Value): Boolean = value <= that.value
//  def > (that: Value): Boolean = value > that.value
//  def >= (that: Value): Boolean = value >= that.value
//  def min(that: Value): Value = Value(value.min(that.value))
//  def max(that: Value): Value = Value(value.max(that.value))
//}

class RangeException(message: String) extends Exception(message)

object Range {
  //implicit def ValueToBigInt(v: Value): BigInt = v.value
  implicit def WidthToBigInt(w: IntWidth): BigInt = w.width
  implicit def IntToBigInt(i: Int): BigInt = BigInt(i)

  def ceilLog2(value: BigInt): Int = value match {
    case v if(v == BigInt(0)) => 0
    case v if(v > BigInt(0)) => value.bitLength
    case v => 
      throw new RangeException(s"cannot take ceilLog2 of negative number $v")
  }

  def max(a: BigInt, b: BigInt): BigInt = if (a >= b) a else b

  def isTooBig(value: BigInt): Boolean = value.bitLength > 31

  def getWidth(minValue: BigInt, maxValue: BigInt): BigInt = {
    val minWidth = minValue match {
      case v if(v == 0) => 0
      case v if(v == -1) => 1
      case v if(v < -1) => ceilLog2(-v - 1) + 1 //e.g. v = -2 -> 1
      case v => throw new RangeException(s"minimum value $v cannot be positive");
    }
    
    val maxWidth = maxValue match {
      case v if(v == 0) => 0
      case v if(v > 0) => ceilLog2(v) + 1
      case v => throw new RangeException(s"maximum value $v cannot be negative")
    }
    
    math.max(minWidth,maxWidth)
  }

  def getUnsignedWidth(minValue: BigInt, maxValue: BigInt): BigInt = {
    if (minValue < 0) throw new RangeException(s"minimum value $minValue cannot be negative")
    max(BigInt(0), getWidth(minValue, maxValue) - 1)
  }

  def getMin(tpe: Type): BigInt = tpe match {
    case UIntType(_) => BigInt(0)
    case SIntType(IntWidth(width)) if(width == 0) => BigInt(0)
    case SIntType(IntWidth(width)) if(!isTooBig(width)) => -BigInt(2).pow(width.toInt - 1)  //TODO(izraelevitz): test isTooBig
    case SIntType(IntWidth(width)) => throw new RangeException(s"Width $width is too big!")
    case ClockType => throw new RangeException(s"Cannot get minimum value on Clock Type!") //TODO(izraelevitz): maybe not error here? taking conservative approach
    case _ => throw new RangeException(s"Type or Width unknown: $tpe")
  }
  
  def getMax(tpe: Type): BigInt = tpe match {
    case UIntType(IntWidth(width)) if(width == 0) => 0
    case UIntType(IntWidth(width)) if(!isTooBig(width)) => BigInt(2).pow(width.toInt) - 1 //TODO(izraelevitz): test isTooBig
    case SIntType(IntWidth(width)) if(width == 0) => 0
    case SIntType(IntWidth(width)) if(!isTooBig(width)) => BigInt(2).pow(width.toInt - 1) - 1 //TODO(izraelevitz): test isTooBig
    case UIntType(IntWidth(width)) => throw new RangeException(s"Width $width is too big!")
    case SIntType(IntWidth(width)) => throw new RangeException(s"Width $width is too big!")
    case ClockType => throw new RangeException(s"Cannot get maximum value on Clock Type!") //TODO(izraelevitz): maybe not error here? taking conservative approach
    case _ => throw new RangeException(s"Type or Width unknown: $tpe")
  }

  def getRange(tpe: Type): Tuple2[BigInt, BigInt] = (getMin(tpe), getMax(tpe))
}

// vim: set ts=4 sw=4 et:
