package firrtl.range

import firrtl.ir.{IntWidth, UIntType, SIntType, GroundType, ClockType}

case class Value(value: BigInt) {
  //TODO(izraelevitz): think of a better way to do this
  def - (that: Value): Value = Value(value - that.value)
  def + (that: Value): Value = Value(value + that.value)
  def * (that: Value): Value = Value(value * that.value)
  def / (that: Value): Value = Value(value / that.value)
  def % (that: Value): Value = Value(value % that.value)
  def << (that: Value): Value =
    if(Range.isTooBig(that.value)) throw new RangeException(s"shift left value $that is too big")
    else Value(value << that.value.toInt)
  def >> (that: Value): Value =
    if(Range.isTooBig(that.value)) throw new RangeException(s"shift right value $that is too big")
    else Value(value >> that.value.toInt)
  def & (that: Value): Value = Value(value & that.value)
  def | (that: Value): Value = Value(value | that.value)
  def ^ (that: Value): Value = Value(value ^ that.value)
  def < (that: Value): Boolean = value < that.value
  def <= (that: Value): Boolean = value <= that.value
  def > (that: Value): Boolean = value > that.value
  def >= (that: Value): Boolean = value >= that.value
}

class RangeException(message: String) extends Exception(message)

object Range {
  implicit def ValueToBigInt(v: Value): BigInt = v.value
  implicit def WidthToBigInt(w: IntWidth): BigInt = w.width
  implicit def IntToBigInt(i: Int): BigInt = BigInt(i)

  def ceilLog2(value: BigInt): Int = value match {
    case v if(v == BigInt(0)) => 0
    case v if(v > BigInt(0)) => value.bitLength
    case v => 
      throw new RangeException(s"cannot take ceilLog2 of negative number $v")
  }

  def isTooBig(value: BigInt): Boolean = value.bitLength > 31

  def getWidth(minValue: Value, maxValue: Value): IntWidth = {
    val minWidth = minValue.value match {
      case v if(v == 0) => 0
      case v if(v == -1) => 1
      case v if(v < -1) => ceilLog2(-v - 1) + 1 //e.g. v = -2 -> 1
      case v => throw new RangeException(s"minimum value $v cannot be positive");
    }
    
    val maxWidth = maxValue.value match {
      case v if(v == 0) => 0
      case v if(v > 0) => ceilLog2(v) + 1
      case v => throw new RangeException(s"maximum value $v cannot be negative")
    }
    
    IntWidth(math.max(minWidth,maxWidth))
  }

  def getMin(tpe: GroundType): Value = tpe match {
    case UIntType(_) => Value(0)
    case SIntType(IntWidth(width)) if(width == 0) => Value(0)
    case SIntType(IntWidth(width)) if(!isTooBig(width)) => Value(-BigInt(2).pow(width.toInt - 1))  //TODO(izraelevitz): test isTooBig
    case SIntType(IntWidth(width)) => throw new RangeException(s"Width $width is too big!")
    case ClockType => throw new RangeException(s"Cannot get minimum value on Clock Type!") //TODO(izraelevitz): maybe not error here? taking conservative approach
    case _ => throw new RangeException(s"Type or Width unknown: $tpe")
  }
  
  def getMax(tpe: GroundType): Value = tpe match {
    case UIntType(IntWidth(width)) if(width == 0) => Value(0)
    case UIntType(IntWidth(width)) if(!isTooBig(width)) => Value(BigInt(2).pow(width.toInt) - 1) //TODO(izraelevitz): test isTooBig
    case SIntType(IntWidth(width)) if(width == 0) => Value(0)
    case SIntType(IntWidth(width)) if(!isTooBig(width)) => Value(BigInt(2).pow(width.toInt - 1) - 1) //TODO(izraelevitz): test isTooBig
    case UIntType(IntWidth(width)) => throw new RangeException(s"Width $width is too big!")
    case SIntType(IntWidth(width)) => throw new RangeException(s"Width $width is too big!")
    case ClockType => throw new RangeException(s"Cannot get maximum value on Clock Type!") //TODO(izraelevitz): maybe not error here? taking conservative approach
    case _ => throw new RangeException(s"Type or Width unknown: $tpe")
  }

  def getRange(tpe: GroundType): Tuple2[Value, Value] = (getMin(tpe), getMax(tpe))
}

// vim: set ts=4 sw=4 et:
