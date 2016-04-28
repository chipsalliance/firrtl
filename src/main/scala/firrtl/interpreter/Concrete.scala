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

import firrtl.{SIntValue, IntWidth, UIntValue}

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
      assert(thisValue > 0)
      <<(that.value)
    case _ => throw new InterpreterException(s"Cannot shift $this << $that where $that is not a UInt parameter")
  }
  def <<(that: ConcreteUInt): Concrete = {
    val shift = that.value.toInt
    assert(that.value > 0)
    this match {
      case ConcreteUInt(thisValue, thisWidth) => ConcreteUInt(this.value << shift, thisWidth + shift)
      case ConcreteSInt(thisValue, thisWidth) => ConcreteSInt(this.value << shift, thisWidth + shift)
    }
  }
  def <<(that: BigInt): Concrete = <<(that.toInt)
  def <<(shift: Int): Concrete = {
    assert(shift > 0)
    this match {
      case ConcreteUInt(thisValue, thisWidth) => ConcreteUInt(this.value << shift, thisWidth + shift)
      case ConcreteSInt(thisValue, thisWidth) => ConcreteSInt(this.value << shift, thisWidth + shift)
    }
  }
  def >>(that: Concrete): Concrete = that match {
    case ConcreteUInt(thatValue, _) =>
      val shift = thatValue.toInt
      assert(shift >= 0)
      assert(shift < this.width)
      ConcreteUInt(this.value >> shift, this.width)
    case _ => throw new InterpreterException(s"Cannot shift $this >> $that where $that is not a UInt parameter")
  }
  def >>(that: BigInt): Concrete = >>(that.toInt)
  def >>(shift: Int): Concrete = {
    assert(shift > 0)
    assert(shift < this.width)
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
  def not: ConcreteUInt = {
    ConcreteUInt(~value, width)
  }
  def &(that: Concrete): ConcreteUInt = ConcreteUInt(this.value & that.value, width.max(that.width))
  def |(that: Concrete): ConcreteUInt = ConcreteUInt(this.value | that.value, width.max(that.width))
  def ^(that: Concrete): ConcreteUInt = ConcreteUInt(this.value ^ that.value, width.max(that.width))
  def cat(that: Concrete): ConcreteUInt = {
    ConcreteUInt((this.value.abs << that.width) + that.value, this.width + that.width)
  }
  // extraction
  def bits(hi: BigInt, lo: BigInt): ConcreteUInt = apply(hi, lo)
  def bits(hi: Int, lo: Int): ConcreteUInt = apply(hi, lo)
  def apply(hi: BigInt, lo: BigInt): ConcreteUInt = apply(hi.toInt, lo.toInt)
  def apply(hi: Int, lo: Int): ConcreteUInt = {
    val finalWidth = (hi - lo) + 1
    val bottomRemoved = value >> (lo - lowBitOffset)
    val modulus = Big1 << finalWidth
    val topRemoved = bottomRemoved % modulus
    ConcreteUInt(topRemoved, finalWidth)
  }
  def head(n: BigInt): ConcreteUInt = head(n.toInt)
  def head(n: Int): ConcreteUInt = {
    assert(n > 0)
    assert(n <= width)
    bits(width + lowBitOffset, width - n + lowBitOffset)
  }
  def tail(n: BigInt): ConcreteUInt = tail(n.toInt)
  def tail(n: Int): ConcreteUInt = {
    assert(n > 0)
    assert(n <= width)
    bits(n, 1)
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
}
object Concrete {
  def apply(u: UIntValue): ConcreteUInt = {
    ConcreteUInt(u.value, u.width.asInstanceOf[IntWidth].width.toInt)
  }
  def apply(u: SIntValue): ConcreteSInt = {
    ConcreteSInt(u.value, u.width.asInstanceOf[IntWidth].width.toInt)
  }
}
// TODO: remove case classes, so that constructor can enforce widths
case class ConcreteUInt(val value: BigInt, val width: Int) extends Concrete {
  val bitsRequired = requiredBits(value)
  if((width > 0) && (bitsRequired > width)) {
    throw new InterpreterException(s"error: ConcreteUInt($value, $width) bad width $width needs ${requiredBits(value.toInt)}")
    assert(true)
  }
}
case class ConcreteSInt(val value: BigInt, val width: Int) extends Concrete
case class ConcreteClock(val value: BigInt) extends Concrete {
  val width = 1
}



