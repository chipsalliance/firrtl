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
package firrtl.interpreter

import firrtl._

trait Concrete {
  val value : BigInt
  val width : Int
  val lowBitOffset = 0

  def +(that: Concrete): Concrete = {
    (this, that) match {
      case (ConcreteUInt(v1, w1), ConcreteUInt(v2, w2)) => ConcreteUInt(v1 + v2, w1.max(w2) + 1)
      case (ConcreteUInt(v1, w1), ConcreteSInt(v2, w2)) => ConcreteSInt(v1 + v2, w1.max(w2) + 1)
      case (ConcreteSInt(v1, w1), ConcreteUInt(v2, w2)) => ConcreteSInt(v1 + v2, w1.max(w2) + 1)
      case (ConcreteSInt(v1, w1), ConcreteSInt(v2, w2)) => ConcreteSInt(v1 + v2, w1.max(w2) + 1)
    }
  }
  def -(that: Concrete): Concrete = {
    (this, that) match {
      case (ConcreteUInt(v1, w1), ConcreteUInt(v2, w2)) => ConcreteSInt(v1 - v2, w1.max(w2) + 1)
      case (ConcreteUInt(v1, w1), ConcreteSInt(v2, w2)) => ConcreteSInt(v1 - v2, w1.max(w2) + 1)
      case (ConcreteSInt(v1, w1), ConcreteUInt(v2, w2)) => ConcreteSInt(v1 - v2, w1.max(w2) + 1)
      case (ConcreteSInt(v1, w1), ConcreteSInt(v2, w2)) => ConcreteSInt(v1 - v2, w1.max(w2) + 1)
    }
  }
  def *(that: Concrete): Concrete = {
    (this, that) match {
      case (ConcreteUInt(v1, w1), ConcreteUInt(v2, w2)) => ConcreteUInt(v1 * v2, w1 + w2)
      case (ConcreteUInt(v1, w1), ConcreteSInt(v2, w2)) => ConcreteSInt(v1 * v2, w1 + w2)
      case (ConcreteSInt(v1, w1), ConcreteUInt(v2, w2)) => ConcreteSInt(v1 * v2, w1 + w2)
      case (ConcreteSInt(v1, w1), ConcreteSInt(v2, w2)) => ConcreteSInt(v1 * v2, w1 + w2)
    }
  }
  def /(that: Concrete): Concrete = {
    if(that.value == BigInt(0)) throw new InterpreterException("divide by zero")
    (this, that) match {
      case (ConcreteUInt(v1, w1), ConcreteUInt(v2, w2)) => ConcreteUInt(v1 / v2, w1)
      case (ConcreteUInt(v1, w1), ConcreteSInt(v2, w2)) => ConcreteSInt(v1 / v2, w1+1)
      case (ConcreteSInt(v1, w1), ConcreteUInt(v2, w2)) => ConcreteSInt(v1 / v2, w1)
      case (ConcreteSInt(v1, w1), ConcreteSInt(v2, w2)) => ConcreteSInt(v1 / v2, w1+1)
    }
  }
  def %(that: Concrete): Concrete = {
    if(that.value == BigInt(0)) throw new InterpreterException("divide by zero")
    (this, that) match {
      case (ConcreteUInt(v1, w1), ConcreteUInt(v2, w2)) => ConcreteUInt(v1 % v2, w1.min(w2))
      case (ConcreteUInt(v1, w1), ConcreteSInt(v2, w2)) => ConcreteUInt(v1 % v2, w1.min(w2))
      case (ConcreteSInt(v1, w1), ConcreteUInt(v2, w2)) => ConcreteSInt(v1 % v2, w1.min(w2+1))
      case (ConcreteSInt(v1, w1), ConcreteSInt(v2, w2)) => ConcreteSInt(v1 % v2, w1.min(w2))
    }
  }
  // Comparison operators
  def <(that: Concrete):  ConcreteUInt = ConcreteUInt(boolToBigInt(this.value < that.value), 1)
  def <=(that: Concrete): ConcreteUInt = ConcreteUInt(boolToBigInt(this.value <= that.value), 1)
  def >(that: Concrete):  ConcreteUInt = ConcreteUInt(boolToBigInt(this.value > that.value), 1)
  def >=(that: Concrete): ConcreteUInt = ConcreteUInt(boolToBigInt(this.value >= that.value), 1)
  def ==(that: Concrete): ConcreteUInt = ConcreteUInt(boolToBigInt(this.value == that.value), 1)
  def !=(that: Concrete): ConcreteUInt = ConcreteUInt(boolToBigInt(this.value != that.value), 1)
  // Padding
  def pad(n: BigInt): Concrete = pad(n.toInt)
  def pad(n: Int): Concrete = this match {
    case ConcreteUInt(v, w) => ConcreteUInt(this.value, this.width.max(n))
    case ConcreteSInt(v, w) => ConcreteSInt(this.value, this.width.max(n))
  }
  // Casting     TODO: I don't think this is done right, need to look at top bit each way
  def asUInt: ConcreteUInt = ConcreteUInt(this.value, this.width)
  def asSInt: ConcreteSInt = {
    ConcreteSInt(this.value, this.width)
  }
  def asClock: ConcreteClock = ConcreteClock(boolToBigInt((this.value & BigInt(1)) > BigInt(0)))
  // Shifting
  def <<(that: Concrete): Concrete = that match {
    case ConcreteUInt(thisValue, _) =>
      assert(thisValue >= 0, s"ERROR:$this << $that ${that.value} must be >= 0")
      <<(that.value)
    case _ => throw new InterpreterException(s"Cannot shift $this << $that where $that is not a UInt parameter")
  }
  def <<(that: ConcreteUInt): Concrete = {
    val shift = that.value.toInt
    assert(that.value >= 0, s"ERROR:$this << $that ${that.value} must be >= 0")
    this match {
      case ConcreteUInt(thisValue, thisWidth) => ConcreteUInt(this.value << shift, thisWidth + shift)
      case ConcreteSInt(thisValue, thisWidth) => ConcreteSInt(this.value << shift, thisWidth + shift)
    }
  }
  def <<(that: BigInt): Concrete = <<(that.toInt)
  def <<(shift: Int): Concrete = {
    assert(shift >= 0, s"ERROR:$this << $shift $shift must be >= 0")
    this match {
      case ConcreteUInt(thisValue, thisWidth) => ConcreteUInt(this.value << shift, thisWidth + shift)
      case ConcreteSInt(thisValue, thisWidth) => ConcreteSInt(this.value << shift, thisWidth + shift)
    }
  }
  def >>(that: Concrete): Concrete = that match {
    case ConcreteUInt(thatValue, _) =>
      val shift = thatValue.toInt
      assert(shift >= 0, s"ERROR:$this >> $that ${that.value} must be >= 0")
      assert(shift < this.width, s"ERROR:$this >> $that ${that.value} must be > ${this.width}")
      ConcreteUInt(this.value >> shift, this.width)
    case _ => throw new InterpreterException(s"Cannot shift $this >> $that where $that is not a UInt parameter")
  }
  def >>(that: BigInt): Concrete = >>(that.toInt)
  def >>(shift: Int): Concrete = {
    assert(shift > 0, s"ERROR:$this >> $shift $shift must be >= 0")
    assert(shift < this.width, s"ERROR:$this >> $shift $shift must be >= 0")
    this match {
      case ConcreteUInt(thisValue, thisWidth) => ConcreteUInt(this.value >> shift, thisWidth - shift)
      case ConcreteSInt(thisValue, thisWidth) => ConcreteSInt(this.value >> shift, thisWidth - shift)
    }
  }
  // Signed
  def cvt: ConcreteSInt = this match {
    case ConcreteUInt(thisValue, thisWidth) => ConcreteSInt(thisValue, thisWidth+1)
    case ConcreteSInt(thisValue, thisWidth) => ConcreteSInt(thisValue, thisWidth)
  }
  def neg: ConcreteSInt = {
    //TODO: Is this right?
    ConcreteSInt(-value, width+1)
  }
  def not: ConcreteUInt = this match {
    case ConcreteUInt(v, _) =>
      var flipped = value
      for(bitIndex <- 0 until width) {
        flipped = flipped.flipBit(bitIndex)
      }
      ConcreteUInt(flipped, width)
    case ConcreteSInt(v, _) =>
      var flipped = value.abs
      for(bitIndex <- 0 until width-1) {
        flipped = flipped.flipBit(bitIndex)
      }
      if(v >= 0) flipped = flipped.setBit(width-1) // invert sign and stick at high end
      ConcreteUInt(flipped, width)
  }
  def &(that: Concrete): ConcreteUInt = ConcreteUInt(this.value & that.value, width.max(that.width))
  def |(that: Concrete): ConcreteUInt = ConcreteUInt(this.value | that.value, width.max(that.width))
  def ^(that: Concrete): ConcreteUInt = ConcreteUInt(this.value ^ that.value, width.max(that.width))
  def cat(that: Concrete): ConcreteUInt = {
    ConcreteUInt((this.value.abs << that.width) + that.value, this.width + that.width)
  }
  // extraction
  def getBits(hi: Int, lo: Int): BigInt = {
    val desiredNumberOfBits = (hi - lo) + 1
    val bottomRemoved = value >> lo
    val modulus = Big1 << desiredNumberOfBits
    val topRemoved = bottomRemoved % modulus
    topRemoved
  }
//  def bits(hi: BigInt, lo: BigInt): ConcreteUInt = bits(hi.toInt, lo.toInt)
  def bits(hi: BigInt, lo: BigInt): Concrete = {
  assert(lo >= Big0, s"Error:BIT_SELECT_OP($this, hi=$hi, lo=$lo) lo must be >= 0")
  assert(lo <= width, s"Error:BIT_SELECT_OP($this, hi=$hi, lo=$lo) lo must be < ${this.width}")
  assert(hi >= lo,   s"Error:BIT_SELECT_OP($this, hi=$hi, lo=$lo) hi must be >= $lo")
  assert(hi <= width, s"Error:BIT_SELECT_OP($this, hi=$hi, lo=$lo) hi must be < ${this.width}")
    val (high, low) = (hi.toInt, lo.toInt)
    this match {
      case ConcreteUInt(v, _) => ConcreteUInt(getBits(high, low), high - low + 1)
      case ConcreteSInt(v, _) => ConcreteSInt(getBits(high, low), high - low + 1)
    }
  }
  def head(n: BigInt): Concrete = {
    assert(n > 0, s"Error:HEAD_OP($this, n=$n) n must be >= 0")
    assert(n <= width, s"Error:HEAD_OP($this, n=$n) n must be <= ${this.width}")
    bits(width-1, width - n)
  }
  def tail(n: BigInt): ConcreteUInt = tail(n.toInt)
  def tail(n: Int): ConcreteUInt = {
    assert(n >= 0, s"Error:TAIL_OP($this, n=$n) n must be >= 0")
    assert(n < width, s"Error:TAIL_OP($this, n=$n) n must be < ${this.width}")
    if(n == 0) {
      ConcreteUInt(value, width)
    }
    else {
      val modulo = Big1 << (width - n)
      val modulus = value % modulo
      ConcreteUInt(modulus, width - n)
    }
  }

  def andReduce: Concrete = this match {
    case ConcreteUInt(v, w) =>
      ConcreteUInt(boolToBigInt((0 until w).map(i => v.testBit(i)).reduce(_&&_)), 1)
    case ConcreteSInt(v, w) =>
      ConcreteUInt(boolToBigInt((0 until w-1).map(i => v.testBit(i)).reduce(_&&_) && (w < Big0)), 1)
    case ConcreteClock(v) =>
      ConcreteUInt(boolToBigInt(v.testBit(0)), 1)
  }
  def orReduce: Concrete = this match {
    case ConcreteUInt(v, w) =>
      ConcreteUInt(boolToBigInt((0 until w).map(i => v.testBit(i)).reduce(_||_)), 1)
    case ConcreteSInt(v, w) =>
      ConcreteUInt(boolToBigInt((0 until w-1).map(i => v.testBit(i)).reduce(_||_) || (w < Big0)), 1)
    case ConcreteClock(v) =>
      ConcreteUInt(boolToBigInt(v.testBit(0)), 1)
  }
  def xorReduce: Concrete = this match {
    case ConcreteUInt(v, w) =>
      ConcreteUInt(boolToBigInt((0 until w).map(i => v.testBit(i)).reduce(_^_)), 1)
    case ConcreteSInt(v, w) =>
      ConcreteUInt(boolToBigInt((0 until w-1).map(i => v.testBit(i)).reduce(_^_) ^ (w < Big0)), 1)
    case ConcreteClock(v) =>
      ConcreteUInt(boolToBigInt(! v.testBit(0)), 1)
  }
  def forceWidth(newWidth: Int): Concrete
  def forceWidth(tpe: Type): Concrete
  def asBinaryString: String = {
    val bitString = value.abs.toString(2)

    this match {
      case ConcreteUInt(v, w) =>
        s"UInt<$width>${"0"*(width-bitString.length)}$bitString"
      case ConcreteSInt(v, w) =>
        s"UInt<$width>${if(v<0)"-" else "+"}${"0"*((width-1)-bitString.length)}$bitString"
    }
  }
  def poisoned: Boolean = false
}
object Concrete {
  def apply(u: UIntValue): ConcreteUInt = {
    ConcreteUInt(u.value, u.width.asInstanceOf[IntWidth].width.toInt)
  }
  def apply(u: SIntValue): ConcreteSInt = {
    ConcreteSInt(u.value, u.width.asInstanceOf[IntWidth].width.toInt)
  }
  def apply(tpe: Type, value: BigInt = Big0): Concrete = {
    tpe match {
      case UIntType(IntWidth(w))     => ConcreteUInt(value, w.toInt)
//      case UIntValue(v, IntWidth(w)) => ConcreteUInt(v, w.toInt)
      case SIntType(IntWidth(w))     => ConcreteSInt(value, w.toInt)
//      case SIntValue(v, IntWidth(w)) => ConcreteSInt(v, w.toInt)
      case ClockType()               => ConcreteClock(value)
    }
  }
  def randomUInt(width: Int): ConcreteUInt  = ConcreteUInt(randomBigInt(width), width)
  def randomSInt(width: Int): ConcreteSInt  = ConcreteSInt(randomBigInt(width), width)
  def randomClock():          ConcreteClock = ConcreteClock(randomBigInt(1))
}

/**
  * A runtime instance of a UInt
  * @param value the BigInt value of this UInt, must be non-negative
  * @param width the number of bits in this value, must be big enough to contain value
  */
case class ConcreteUInt(val value: BigInt, val width: Int) extends Concrete {
  if(width < 0) {
    throw new InterpreterException(s"error: ConcreteUInt($value, $width) bad width $width must be > 0")
  }
  val bitsRequired = requiredBits(value)
  if((width > 0) && (bitsRequired > width)) {
    throw new InterpreterException(s"error: ConcreteUInt($value, $width) bad width $width needs ${requiredBits(value.toInt)}")
  }
  def forceWidth(newWidth:Int): ConcreteUInt = {
    if(newWidth == width) this else ConcreteUInt(this.value, newWidth)
  }
  def forceWidth(tpe: Type): ConcreteUInt = forceWidth(typeToWidth(tpe))
}
/**
  * A runtime instance of a SInt
  * @param value the BigInt value of this UInt,
  * @param width the number of bits in this value, must be big enough to contain value plus 1 for sign bit
  */
case class ConcreteSInt(val value: BigInt, val width: Int) extends Concrete {
  if(width < 0) {
    throw new InterpreterException(s"error: ConcreteSInt($value, $width) bad width $width must be > 0")
  }
  val bitsRequired = requiredBits(value)
  if ((width > 0) && (bitsRequired > width)) {
    throw new InterpreterException(s"error: ConcreteSInt($value, $width) bad width $width needs ${requiredBits(value.toInt)}")
  }

  def forceWidth(newWidth: Int): ConcreteSInt = {
    if(newWidth == width) this else ConcreteSInt(this.value, newWidth)
  }
  def forceWidth(tpe: Type): ConcreteSInt = forceWidth(typeToWidth(tpe))
}
case class ConcreteClock(val value: BigInt) extends Concrete {
  val width = 1

  def forceWidth(width: Int): ConcreteClock = {
    if(width == 1) this
    else throw new InterpreterException(s"withWidth($width) not supported for $this")
  }
  def forceWidth(tpe: Type): ConcreteClock = forceWidth(typeToWidth(tpe))
}

case class PoisonedUInt(width: Int) extends Concrete {
  val value = Big0
  override def forceWidth(w: Int): PoisonedUInt = PoisonedUInt(w)
  def forceWidth(tpe: Type): PoisonedUInt = forceWidth(typeToWidth(tpe))
  override def poisoned: Boolean = true
}
case class PoisonedSInt(width: Int) extends Concrete {
  val value = Big0
  override def forceWidth(w: Int): PoisonedSInt = PoisonedSInt(w)
  def forceWidth(tpe: Type): PoisonedSInt = forceWidth(typeToWidth(tpe))
  override def poisoned: Boolean = true
}



