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
import firrtl.Utils._
import org.scalatest.{Matchers, FlatSpec}

/**
  * Created by chick on 4/25/16.
  */
class LoFirrtlExpressionEvaluatorSpec extends FlatSpec with Matchers {
  behavior of "Primitive ops"

  val input =
    """circuit Test :
      |  module Test :
      |    input clk : Clock
      |    input a : UInt<1>
      |    output c : UInt<2>
      |    reg w : UInt<1>, clk
      |    w <= a
      |    c <= w
    """.stripMargin

  val interpreter = FirrtlTerp(input)
  val evaluator = new LoFirrtlExpressionEvaluator(Iterable.empty, interpreter.interpreterCircuit.dependencyGraph, interpreter.sourceState)
  val random = util.Random

  val baseWidth = 4

  def makeSignedBigInt = {
    BigInt(baseWidth, random) * (if(random.nextBoolean()) 1 else -1)
  }
  they should "return types correctly" in {
    val w1 = IntWidth(4)
    val w2 = IntWidth(4)
    val i1 = evaluator.makeUIntValue(1, w1)
    val s1 = evaluator.makeSIntValue(2, w2)
    val outWidth = IntWidth(4)
    val outWidthType = UIntType(outWidth)

    evaluator.mathPrimitive(MUL_OP, Seq(i1, i1), outWidthType ).isInstanceOf[UIntValue] should be (true)
    evaluator.mathPrimitive(MUL_OP, Seq(i1, s1), outWidthType ).isInstanceOf[SIntValue] should be (true)
    evaluator.mathPrimitive(MUL_OP, Seq(s1, i1), outWidthType ).isInstanceOf[SIntValue] should be (true)
    evaluator.mathPrimitive(MUL_OP, Seq(s1, s1), outWidthType ).isInstanceOf[SIntValue] should be (true)

  }
  they should "evaluate multiply UIntValues correctly" in {
    for(i <- 0 to 10) {
      val w1 = IntWidth(BigInt(baseWidth, random))
      val w2 = IntWidth(BigInt(baseWidth, random))
      val i1 = evaluator.makeUIntValue(BigInt(baseWidth, random), w1)
      val i2 = evaluator.makeUIntValue(BigInt(baseWidth, random), w2)
      val outWidth = IntWidth(BigInt(baseWidth, random))
      val outWidthType = UIntType(outWidth)

      val out = evaluator.mathPrimitive(MUL_OP, Seq(i1, i2), outWidthType )

      println(s"$i1 * $i2 => $out")
      out.value should be (evaluator.mask(i1.value * i2.value, outWidth.width))
    }
  }
  they should "evaluate multiply SIntValues correctly" in {

    for(i <- 0 to 10) {
      val w1 = IntWidth(BigInt(baseWidth, random))
      val w2 = IntWidth(BigInt(baseWidth, random))
      val i1 = evaluator.makeUIntValue(makeSignedBigInt, w1)
      val i2 = evaluator.makeUIntValue(makeSignedBigInt, w2)
      val outWidth = IntWidth(BigInt(baseWidth, random))
      val outWidthType = UIntType(outWidth)

      val out = evaluator.mathPrimitive(MUL_OP, Seq(i1, i2), outWidthType )

      println(s"$i1 * $i2 => $out")
      out.value should be (evaluator.mask(i1.value * i2.value, outWidth.width))
    }
  }

  behavior of "mask operation"

  it should "return sensible values" in {
    val num: BigInt = BigInt("1080825922752")
    val bits: BigInt = 31
    val pow: BigInt = BigInt("2147483648")
    evaluator.mask(num, bits) should be(num % pow)

    var power: BigInt = 2
    for(maskSize <- 0 to 100) {
      for (samples <- 0 to 10) {
        val number = BigInt(maskSize + 10, random)
        val masked = evaluator.mask(number, maskSize)
//        println(s"mask $maskSize sample number $samples number $number power $power masked $masked calc ${number % power} ")
        masked should be(number % power)
      }
      if(maskSize > 0 ) power <<= 1
    }
  }

  behavior of "SHIFT_RIGHT_OP"

  it should "throw assertions when parameter is zero" in {
    intercept[AssertionError] {
      evaluator.bitOps(SHIFT_RIGHT_OP, Seq(UIntValue(1, IntWidth(3))), Seq(0), UIntType(IntWidth(3)))
    }
    intercept[AssertionError] {
      evaluator.bitOps(SHIFT_RIGHT_OP, Seq(UIntValue(1, IntWidth(3))), Seq(4), UIntType(IntWidth(3)))
    }
    intercept[AssertionError] {
      evaluator.bitOps(SHIFT_RIGHT_OP, Seq(UIntValue(1, IntWidth(33))), Seq(34), UIntType(IntWidth(3)))
    }
  }
  it should "shift bits n bits to the right" in {
    for(i <- 3 to 100) {
      for(arg <- 1 until i) {
        val num = BigInt("1"*i, 2)
        val target = UIntValue(evaluator.shiftLeft(num, arg), IntWidth(i + arg))

        val result = evaluator.bitOps(SHIFT_RIGHT_OP, Seq(target), Seq(arg), UIntType(IntWidth(i)))
        //        println(s"num $num arg $arg, target $target result $result")
        result.value should be (num)
      }
      for(arg <- 1 until i) {
        val num = BigInt("1"*i, 2)
        val target = SIntValue(evaluator.shiftLeft(num, arg), IntWidth(i + arg + 1))

        val result = evaluator.bitOps(SHIFT_RIGHT_OP, Seq(target), Seq(arg), SIntType(IntWidth(i+1)))
        //        println(s"num $num arg $arg, target $target result $result")
        result.value should be (num)
      }
      for(arg <- 1 until i) {
        val num = -BigInt("1"*i, 2)
        val target = SIntValue(evaluator.shiftLeft(num, arg), IntWidth(i + arg + 1))

        val result = evaluator.bitOps(SHIFT_RIGHT_OP, Seq(target), Seq(arg), SIntType(IntWidth(i+1)))
        //        println(s"num $num arg $arg, target $target result $result")
        result.value should be (num)
      }
    }
  }

  behavior of "SHIFT_LEFT_OP"

  it should "throw assertions when parameter is zero" in {
    intercept[AssertionError] {
      evaluator.bitOps(SHIFT_LEFT_OP, Seq(UIntValue(1, IntWidth(3))), Seq(0), UIntType(IntWidth(3)))
    }
  }
  it should "shift bits n bits to the left" in {
    for(i <- 3 to 100) {
      for(arg <- 1 until 50) {
        val num = BigInt("1"*i, 2)
        val target = UIntValue(num, IntWidth(i))
        val testTarget = evaluator.shiftLeft(num, arg)

        val result = evaluator.bitOps(SHIFT_LEFT_OP, Seq(target), Seq(arg), UIntType(IntWidth(i+arg)))
        //        println(s"num $num arg $arg, target $target result $result")
        result.value should be (testTarget)
        result.widthAsBigInt should be (i+arg)
      }
      for(arg <- 1 until 50) {
        val num = BigInt("1"*i, 2)
        val target = SIntValue(num, IntWidth(i))
        val testTarget = evaluator.shiftLeft(num, arg)

        val result = evaluator.bitOps(SHIFT_LEFT_OP, Seq(target), Seq(arg), SIntType(IntWidth(i+arg)))
        //        println(s"num $num arg $arg, target $target result $result")
        result.value should be (testTarget)
        result.widthAsBigInt should be (i+arg)
      }
    }
  }

  behavior of "DYN_SHIFT_LEFT_OP"

  it should "throw assertions when parameter is zero" in {
    intercept[AssertionError] {
      evaluator.dynamicBitOps(DYN_SHIFT_LEFT_OP,
        Seq(UIntValue(1, IntWidth(3)), UIntValue(0, IntWidth(2))), Seq(), UIntType(IntWidth(3)))
    }
    intercept[AssertionError] {
      evaluator.dynamicBitOps(DYN_SHIFT_LEFT_OP,
        Seq(UIntValue(1, IntWidth(3)), SIntValue(1, IntWidth(2))), Seq(), UIntType(IntWidth(3)))
    }
  }
  it should "shift bits n bits to the left" in {
    for(i <- 3 to 100) {
      for(arg <- 1 until 50) {
        val num = BigInt("1"*i, 2)
        val target = UIntValue(num, IntWidth(i))
        val testTarget = evaluator.shiftLeft(num, arg)
        val shiftValue = evaluator.makeUIntValue(arg, IntWidth(req_num_bits(arg)))

        val result = evaluator.dynamicBitOps(DYN_SHIFT_LEFT_OP, Seq(target, shiftValue), Seq(arg), UIntType(IntWidth(i+arg)))
        //        println(s"num $num arg $arg, target $target result $result")
        result.value should be (testTarget)
        result.widthAsBigInt should be (i+arg)
      }
      for(arg <- 1 until 50) {
        val num = BigInt("1"*i, 2)
        val target = SIntValue(num, IntWidth(i))
        val testTarget = evaluator.shiftLeft(num, arg)
        val shiftValue = evaluator.makeUIntValue(arg, IntWidth(req_num_bits(arg)))

        val result = evaluator.dynamicBitOps(DYN_SHIFT_LEFT_OP, Seq(target, shiftValue), Seq(0), SIntType(IntWidth(i+arg)))
        //        println(s"num $num arg $arg, target $target result $result")
        result.value should be (testTarget)
        result.widthAsBigInt should be (i+arg)
      }
    }
  }

  behavior of "DYN_SHIFT_RIGHT_OP"

  it should "throw assertions when parameter is zero" in {
    intercept[AssertionError] {
      evaluator.dynamicBitOps(DYN_SHIFT_RIGHT_OP,
        Seq(UIntValue(1, IntWidth(3)), UIntValue(4, IntWidth(2))), Seq(), UIntType(IntWidth(3)))
    }
    intercept[AssertionError] {
      evaluator.dynamicBitOps(DYN_SHIFT_RIGHT_OP,
        Seq(UIntValue(1, IntWidth(3)), SIntValue(1, IntWidth(2))), Seq(), UIntType(IntWidth(3)))
    }
  }
  it should "shift bits n bits to the right" in {
//    evaluator.setVerbose(true)
    for(i <- 3 to 100) {
      for(arg <- 1 until i) {
        val num = BigInt("1"*i, 2)
        val target = UIntValue(evaluator.shiftLeft(num, arg), IntWidth(i + arg))
        val shiftValue = evaluator.makeUIntValue(arg, IntWidth(req_num_bits(arg)))

        val result = evaluator.dynamicBitOps(DYN_SHIFT_RIGHT_OP, Seq(target, shiftValue), Seq(), UIntType(IntWidth(i)))
        //        println(s"num $num arg $arg, target $target result $result")
        result.value should be (num)
      }
      for(arg <- 1 until i) {
        val num = BigInt("1"*i, 2)
        val target = SIntValue(evaluator.shiftLeft(num, arg), IntWidth(i + arg + 1))
        val shiftValue = evaluator.makeUIntValue(arg, IntWidth(req_num_bits(arg)))

        val result = evaluator.dynamicBitOps(DYN_SHIFT_RIGHT_OP, Seq(target, shiftValue), Seq(), SIntType(IntWidth(i+1)))
        //        println(s"num $num arg $arg, target $target result $result")
        result.value should be (num)
      }
      for(arg <- 1 until i) {
        val num = -BigInt("1"*i, 2)
        val target = SIntValue(evaluator.shiftLeft(num, arg), IntWidth(i + arg + 1))
        val shiftValue = evaluator.makeUIntValue(arg, IntWidth(req_num_bits(arg)))

        val result = evaluator.dynamicBitOps(DYN_SHIFT_RIGHT_OP, Seq(target, shiftValue), Seq(), SIntType(IntWidth(i+1)))
        //        println(s"num $num arg $arg, target $target result $result")
        result.value should be (num)
      }
    }
  }

  behavior of "HEAD_OP"

  it should "throw assertions when parameter is zero or not less than width of target" in {
    intercept[AssertionError] {
      evaluator.bitOps(HEAD_OP, Seq(UIntValue(1, IntWidth(3))), Seq(0), UIntType(IntWidth(3)))
    }
    intercept[AssertionError] {
      evaluator.bitOps(HEAD_OP, Seq(UIntValue(1, IntWidth(3))), Seq(4), UIntType(IntWidth(3)))
    }
    intercept[AssertionError] {
      evaluator.bitOps(HEAD_OP, Seq(UIntValue(1, IntWidth(33))), Seq(34), UIntType(IntWidth(3)))
    }
  }
  it should "shift n bits at top of number over the width - n bits to the right" in {
    for(i <- 3 to 100) {
      for(arg <- 1 until i) {
        val num = BigInt("1"*i, 2)
        val target = UIntValue(evaluator.shiftLeft(num, arg), IntWidth(i + arg))

        val result = evaluator.bitOps(HEAD_OP, Seq(target), Seq(i), UIntType(IntWidth(i)))
        //        println(s"num $num arg $arg, target $target result $result")
        result.value should be (num)
      }
    }
  }

  behavior of "tail"

  it should "throw assertions when parameter is zero or not less than width of target" in {
    intercept[AssertionError] {
      evaluator.bitOps(TAIL_OP, Seq(UIntValue(1, IntWidth(3))), Seq(0), UIntType(IntWidth(3)))
    }
    intercept[AssertionError] {
      evaluator.bitOps(TAIL_OP, Seq(UIntValue(1, IntWidth(3))), Seq(3), UIntType(IntWidth(3)))
    }
    intercept[AssertionError] {
      evaluator.bitOps(TAIL_OP, Seq(UIntValue(1, IntWidth(33))), Seq(34), UIntType(IntWidth(3)))
    }
  }
  it should "remove top n bits of a number" in {
    evaluator.bitOps(TAIL_OP, Seq(UIntValue(1, IntWidth(3))), Seq(1), UIntType(IntWidth(3))) should be (UIntValue(1,IntWidth(3)))

    for(i <- 3 to 100) {
      for(arg <- 1 until i) {
        val num = BigInt("1"*i, 2)
        val mask = BigInt("1"*(i-arg), 2)
        val target = UIntValue(num, IntWidth(i))
        val result = evaluator.bitOps(TAIL_OP, Seq(target), Seq(arg), UIntType(IntWidth(i)))
        //        println(s"num $num arg $arg, result $result")
        result.value should be (mask)
      }
    }
  }
}
