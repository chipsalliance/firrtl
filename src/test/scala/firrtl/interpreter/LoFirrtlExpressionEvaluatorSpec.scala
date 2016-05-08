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
import TestUtils._
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
  val evaluator = new LoFirrtlExpressionEvaluator(Iterable.empty, interpreter.dependencyGraph, interpreter.circuitState)
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

    evaluator.mathPrimitive(MUL_OP, Seq(i1, i1), outWidthType ).isInstanceOf[ConcreteUInt] should be (true)
    evaluator.mathPrimitive(MUL_OP, Seq(i1, s1), outWidthType ).isInstanceOf[ConcreteSInt] should be (true)
    evaluator.mathPrimitive(MUL_OP, Seq(s1, i1), outWidthType ).isInstanceOf[ConcreteSInt] should be (true)
    evaluator.mathPrimitive(MUL_OP, Seq(s1, s1), outWidthType ).isInstanceOf[ConcreteSInt] should be (true)

  }
  they should "evaluate multiply UIntValues correctly" in {
    for(i <- 0 to 10) {
      val w1 = IntWidth(BigInt(baseWidth, random).abs)
      val w2 = IntWidth(BigInt(baseWidth, random).abs)
      val i1 = evaluator.makeUIntValue(BigInt(baseWidth-1, random).abs, w1)
      val i2 = evaluator.makeUIntValue(BigInt(baseWidth-1, random).abs, w2)
      val outWidth = IntWidth(BigInt(baseWidth, random))
      val outWidthType = UIntType(outWidth)

      val out = evaluator.mathPrimitive(MUL_OP, Seq(i1, i2), outWidthType )

      println(s"$i1 * $i2 => $out $outWidth")
      out.value should be (i1.value * i2.value)
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

//      println(s"$i1 * $i2 => $out")
      out.value should be (i1.value * i2.value)
    }
  }

  behavior of "requiredBits"

  it should "return the right amount" in {
    requiredBits(BigInt("1"*29, 2)) should be (29)
    for( width <- 1 to 100) {
      val num = Big1 << (width - 1)
      val computed = requiredBits(num)
      computed should be (width)

      val maxNum = BigInt("1"*width, 2)
      val maxComputed = requiredBits(maxNum)
//      println(s"width $width computed $maxComputed num $maxNum")
      maxComputed should be (width)
    }
  }

  behavior of "mask operation"

  it should "return sensible values" in {
    val num: BigInt = BigInt("1080825922752")
    val bits: BigInt = 31
    val pow: BigInt = BigInt("2147483648")
    evaluator.mask(num, bits) should be(num % pow)

    var power: BigInt = 2
    for(maskSize <- 1 to 100) {
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
    def testShiftOp(width: Int, shift: Int): Unit = {
      val num = BigInt("1"*width, 2)
      val shiftedNum = num << shift
      val target = UIntValue(shiftedNum, IntWidth(width + shift))
      requiredBits(num) should be (width)
      requiredBits(shiftedNum) should be (width + shift)

//      println(s"width $width => num $num arg $shift, target $target result NA")
      val result = evaluator.bitOps(SHIFT_RIGHT_OP, Seq(target), Seq(shift), UIntType(IntWidth(width)))
//      println(s"width $width => num $num arg $shift, target $target result $result")
      result.value should be (num)
    }
    testShiftOp(29, 1)

    for(i <- 3 to 40) {
      for(arg <- 1 until i) {
        testShiftOp(i, arg)
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

  it should "throw assertions when parameter is  > zero" in {
    intercept[AssertionError] {
      evaluator.bitOps(SHIFT_LEFT_OP, Seq(UIntValue(1, IntWidth(3))), Seq(-1), UIntType(IntWidth(3)))
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
        result.width should be (i+arg)
      }
      for(arg <- 1 until 50) {
        val num = BigInt("1"*i, 2)
        val target = SIntValue(num, IntWidth(i))
        val testTarget = evaluator.shiftLeft(num, arg)

        val result = evaluator.bitOps(SHIFT_LEFT_OP, Seq(target), Seq(arg), SIntType(IntWidth(i+arg)))
        //        println(s"num $num arg $arg, target $target result $result")
        result.value should be (testTarget)
        result.width should be (i+arg)
      }
    }
  }

  behavior of "DYN_SHIFT_LEFT_OP"

  it should "throw assertions when parameter is > zero" in {
    intercept[InterpreterException] {
      evaluator.dynamicBitOps(DYN_SHIFT_LEFT_OP,
        Seq(UIntValue(1, IntWidth(3)), SIntValue(-1, IntWidth(2))), Seq(), UIntType(IntWidth(3)))
    }
    intercept[InterpreterException] {
      evaluator.dynamicBitOps(DYN_SHIFT_LEFT_OP,
        Seq(UIntValue(1, IntWidth(3)), SIntValue(1, IntWidth(2))), Seq(), UIntType(IntWidth(3)))
    }
  }
  it should "shift bits n bits to the left" in {
    def testShiftOp(width: Int, shift: Int): Unit = {
      val num = BigInt("1"*width, 2)
      val shiftedNum = num << shift
      val target = UIntValue(shiftedNum, IntWidth(width + shift))
      requiredBits(num) should be (width)
      requiredBits(shiftedNum) should be (width + shift)

//      println(s"width $width => num $num arg $shift, target $target result NA")
      val result = evaluator.bitOps(SHIFT_RIGHT_OP, Seq(target), Seq(shift), UIntType(IntWidth(width)))
//      println(s"width $width => num $num arg $shift, target $target result $result")
      result.value should be (num)
    }
    testShiftOp(29, 1)

    for(i <- 3 to 40) {
      for(arg <- 1 until i) {
        testShiftOp(i, arg)
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
    for(i <- 3 to 100) {
      for(arg <- 1 until 50) {
        val num = BigInt("1"*i, 2)
        val target = UIntValue(num, IntWidth(i))
        val testTarget = evaluator.shiftLeft(num, arg)
        val shiftValue = evaluator.makeUIntValue(arg, IntWidth(req_num_bits(arg)))

        val result = evaluator.dynamicBitOps(DYN_SHIFT_LEFT_OP, Seq(target, shiftValue), Seq(), UIntType(IntWidth(i+arg)))
        //        println(s"num $num arg $arg, target $target result $result")
        result.value should be (testTarget)
        result.width should be (i+arg)
      }
      for(arg <- 1 until 50) {
        val num = BigInt("1"*i, 2)
        val target = SIntValue(num, IntWidth(i))
        val testTarget = evaluator.shiftLeft(num, arg)
        val shiftValue = evaluator.makeUIntValue(arg, IntWidth(req_num_bits(arg)))

        val result = evaluator.dynamicBitOps(DYN_SHIFT_LEFT_OP, Seq(target, shiftValue), Seq(), SIntType(IntWidth(i+arg)))
        //        println(s"num $num arg $arg, target $target result $result")
        result.value should be (testTarget)
        result.width should be (i+arg)
      }
    }
  }

  behavior of "DYN_SHIFT_RIGHT_OP"

  it should "throw assertions when parameter is zero" in {
    intercept[InterpreterException] {
      evaluator.dynamicBitOps(DYN_SHIFT_RIGHT_OP,
        Seq(UIntValue(1, IntWidth(3)), UIntValue(4, IntWidth(2))), Seq(), UIntType(IntWidth(3)))
    }
    intercept[InterpreterException] {
      evaluator.dynamicBitOps(DYN_SHIFT_RIGHT_OP,
        Seq(UIntValue(1, IntWidth(3)), SIntValue(1, IntWidth(2))), Seq(), UIntType(IntWidth(3)))
    }
  }
  it should "shift bits n bits to the right" in {
    def testShiftOp(width: Int, shift: Int): Unit = {
      val num = BigInt("1"*width, 2)
      val shiftedNum = num >> shift

      val target = UIntValue(num, IntWidth(width))
      val shiftUInt = UIntValue(shift, IntWidth(requiredBits(shift)))

      requiredBits(num) should be (width)
      requiredBits(shiftedNum) should be (width - shift)

      val result = evaluator.dynamicBitOps(DYN_SHIFT_RIGHT_OP, Seq(target, shiftUInt), Seq(), UIntType(IntWidth(width)))
//      println(s"width $width => num $num arg $shift, target $target result $result")
      result.value should be (shiftedNum)
    }

    for(i <- 3 to 20) {
      for(arg <- 1 until i) {
        testShiftOp(i, arg)
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
    for(i <- 1 to 100) {
      for(arg <- 1 until i) {
        val num = BigInt("1"*i, 2)
        val target = UIntValue(evaluator.shiftLeft(num, arg), IntWidth(i + arg))

//        println(s"i $i num $num arg $arg, target $target result NA")
        val result = evaluator.bitOps(HEAD_OP, Seq(target), Seq(i), UIntType(IntWidth(i)))
        result.value should be (num)
      }
    }
  }

  behavior of "tail"

  it should "throw assertions when parameter is zero or not less than width of target" in {
    intercept[AssertionError] {
      evaluator.bitOps(TAIL_OP, Seq(UIntValue(1, IntWidth(3))), Seq(-1), UIntType(IntWidth(3)))
    }
    intercept[AssertionError] {
      evaluator.bitOps(TAIL_OP, Seq(UIntValue(1, IntWidth(33))), Seq(34), UIntType(IntWidth(3)))
    }
  }
  it should "remove top n bits of a number" in {
    for(i <- IntWidthTestValuesGenerator(1, TestUtils.MaxWidth)) {
      for(arg <- IntWidthTestValuesGenerator(0, i-1)) {
        val num  = allOnes(i)
        val mask = allOnes(i-arg)

        val target = UIntValue(num, IntWidth(i))
        val result = evaluator.bitOps(TAIL_OP, Seq(target), Seq(arg), UIntType(IntWidth(i-arg)))
//        println(s"num $num arg $arg, result $result")
        result.value should be (mask)
      }
    }
  }
}
