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

import org.scalatest.{Matchers, FlatSpec}

/**
  * Created by chick on 4/27/16.
  */
class ConcreteSpec extends FlatSpec with Matchers {
  val maxWidth = 100

  behavior of "random BigInt generation"

  it should "not create numbers with wider than specified width" in {
    for(i <- 20 to 100) {
      for( trails <- 0 to 1000) {
        val x = randomBigInt(i)
//        println(s"$i $x ${x.bitLength}")
        x.bitLength should be <= i
      }
    }
  }

  behavior of "creating concrete values"

  it should "work up to a reasonable size" in {
    for(i <- 20 to 100) {
      for( trails <- 0 to 1000) {
        val x = randomBigInt(i)
//        println(s"$i $x ${x.bitLength}")

        val ci = ConcreteUInt(x, i)
        ci.isInstanceOf[ConcreteUInt] should be (true)
        ci.width should be <= i
      }
    }
  }

  behavior of "concrete addition"

  it should "return proper type under mixed addition" in {
    val (i1, i2, s1, s2) = (randU, randU, randS, randS)

    (i1 + i2).isInstanceOf[ConcreteUInt] should be (true)
    (i1 + s2).isInstanceOf[ConcreteSInt] should be (true)
    (s1 + i2).isInstanceOf[ConcreteSInt] should be (true)
    (s1 + s2).isInstanceOf[ConcreteSInt] should be (true)

    (i1 + i2).width should be (i1.width.max(i2.width)+1)
    (i1 + s2).width should be (i1.width.max(s2.width)+1)
    (s1 + i2).width should be (s1.width.max(i2.width)+1)
    (s1 + s2).width should be (s1.width.max(s2.width)+1)
  }

  it should "return obvious additions under large range of numbers" in {
    for (trials <- 0 to 10000) {
      val (width1, width2) = (random.nextInt(maxWidth), random.nextInt(maxWidth))
      val (num1, num2) = (randomBigInt(width1), randomBigInt(width2))
      val (cu1, cu2) = (ConcreteUInt(num1, width1), ConcreteUInt(num2, width2))
      val sum = cu1 + cu2

      sum.value should be (cu1.value + cu2.value)
    }
  }

  behavior of "concrete subtraction"

  it should "return proper type under mixed subtraction" in {
    val (i1, i2, s1, s2) = (randU, randU, randS, randS)

    (i1 - i2).isInstanceOf[ConcreteSInt] should be (true)
    (i1 - s1).isInstanceOf[ConcreteSInt] should be (true)
    (s1 - i2).isInstanceOf[ConcreteSInt] should be (true)
    (s1 - s2).isInstanceOf[ConcreteSInt] should be (true)

    (i1 - i2).width should be (i1.width.max(i2.width)+1)
    (i1 - s2).width should be (i1.width.max(s2.width)+1)
    (s1 - i2).width should be (s1.width.max(i2.width)+1)
    (s1 - s2).width should be (s1.width.max(s2.width)+1)
  }

  it should "return obvious subtractions under large range of numbers" in {
    for (trials <- 0 to 10000) {
      val (cu1, cu2) = (randC, randC)
      val sum = cu1 - cu2

      sum.value should be (cu1.value - cu2.value)
    }
  }

  behavior of "concrete multiplication"

  it should "return proper type under mixed multiplication" in {
    val (i1, i2, s1, s2) = (randU, randU, randS, randS)

    (i1 * i2).isInstanceOf[ConcreteUInt] should be (true)
    (i1 * s1).isInstanceOf[ConcreteSInt] should be (true)
    (s1 * i2).isInstanceOf[ConcreteSInt] should be (true)
    (s1 * s2).isInstanceOf[ConcreteSInt] should be (true)

    (i1 * i2).width should be (i1.width + i2.width)
    (i1 * s2).width should be (i1.width + s2.width)
    (s1 * i2).width should be (s1.width + i2.width)
    (s1 * s2).width should be (s1.width + s2.width)
  }

  it should "return obvious multiplications under large range of numbers" in {
    for (trials <- 0 to 10000) {
      val (cu1, cu2) = (randC, randC)
      val sum = cu1 * cu2

      sum.value should be (cu1.value * cu2.value)
    }
  }

  behavior of "concrete division"

  it should "return proper type under mixed division" in {
    val (i1, i2, s1, s2) = (randU, randU, randS, randS)

    (i1 / i2).isInstanceOf[ConcreteUInt] should be (true)
    (i1 / s1).isInstanceOf[ConcreteSInt] should be (true)
    (s1 / i2).isInstanceOf[ConcreteSInt] should be (true)
    (s1 / s2).isInstanceOf[ConcreteSInt] should be (true)

    (i1 / i2).width should be (i1.width)
    (i1 / s2).width should be (i1.width+1)
    (s1 / i2).width should be (s1.width)
    (s1 / s2).width should be (s1.width+1)
  }

  it should "return obvious divisions under large range of numbers" in {
    for (trials <- 0 to 10000) {
      val (cu1, cu2) = (randC, randC)

      if(cu2.value != Big0 ) {
        val sum = cu1 / cu2
        sum.value should be (cu1.value / cu2.value)
      }
    }
  }

  behavior of "concrete modulus"

  it should "return proper type under mixed modulus" in {
    val (i1, i2, s1, s2) = (randU, randU, randS, randS)

    (i1 % i2).isInstanceOf[ConcreteUInt] should be (true)
    (i1 % s1).isInstanceOf[ConcreteUInt] should be (true)
    (s1 % i2).isInstanceOf[ConcreteSInt] should be (true)
    (s1 % s2).isInstanceOf[ConcreteSInt] should be (true)

    (i1 % i2).width should be (i1.width.min(i2.width))
    (i1 % s2).width should be (i1.width.min(s2.width))
    (s1 % i2).width should be (s1.width.min(i2.width+1))
    (s1 % s2).width should be (s1.width.min(s2.width))
  }

  it should "return obvious modulus under large range of numbers" in {
    for (trials <- 0 to 10000) {
      val (cu1, cu2) = (randC, randC)

      if(cu2.value != Big0 ) {
        val sum = cu1 % cu2
        sum.value should be (cu1.value % cu2.value)
      }
    }
  }

  behavior of "bits"

  it should "allows arbitrary selection of bits" in {
    def testBits(width: Int, lo: Int, hi: Int) {
      val num = BigInt("1" * width, 2)
      val uint = ConcreteUInt(num, width)

      val subUint = uint.bits(hi, lo)
      println(s"width $width lo $lo hi $hi uint $uint => $subUint")

      val size = hi - lo + 1
      subUint.value should be (BigInt("1" * size, 2))
      subUint.width should be (size)
    }

    testBits(32, 1, 32)

    for(width <- 1 to maxWidth) {
      for(lo <- 0 until width) {
        for(hi <- lo until width) {
          testBits(width, lo, hi)
        }
      }
    }
  }

  behavior of "head"

  it should "allows arbitrary selection of top n bits" in {
    def testBits(width: Int, n: Int) {
      val num = BigInt("1" * width, 2)
      val uint = ConcreteUInt(num, width)

      val subUint = uint.head(n)
      println(s"width $width n $n uint $uint => $subUint")

      subUint.value should be (BigInt("1" * n, 2))
      subUint.width should be (n)
    }

    for(width <- 1 to maxWidth) {
      for(n <- 1 to width) {
        testBits(width, n)
      }
    }
  }

  behavior of "tail"

  it should "allows arbitrary selection of top n bits" in {
    def testBits(width: Int, n: Int) {
      val num = BigInt("1" * width, 2)
      val uint = ConcreteUInt(num, width)

      val subUint = uint.tail(n)
      println(s"width $width n $n uint $uint => $subUint")

      subUint.value should be (BigInt("1" * n, 2))
      subUint.width should be (n)
    }

    for(width <- 1 to maxWidth) {
      for(n <- 1 to width) {
        testBits(width, n)
      }
    }
  }

  behavior of "dyn shift left"

  it should "work for wide range of values" in {
    def testShiftOp(width: Int, shift: Int): Unit = {
      val num = BigInt("1"*width, 2)
      val shiftedNum = num << shift

      val target = ConcreteUInt(num, width)
      val shiftArg = ConcreteUInt(shift, requiredBits(shift))

      requiredBits(num) should be (width)
      requiredBits(shiftedNum) should be (width + shift)

      val result = target << shiftArg
      // println(s"width $width => num $num arg $shift, target $target result $result")
      result.value should be (shiftedNum)
    }
    testShiftOp(29, 1)

    for(i <- 3 to maxWidth) {
      for(arg <- 1 until i) {
        testShiftOp(i, arg)
      }
    }

  }

  def randC: Concrete = {
    if(random.nextBoolean()) randU else randS
  }
  def randU: ConcreteUInt = {
    val randomWidth = random.nextInt(maxWidth)
    ConcreteUInt(BigInt(randomWidth, random), randomWidth)
  }
  def randS: ConcreteSInt = {
    val randomWidth = random.nextInt(maxWidth)
    ConcreteSInt(BigInt(randomWidth, random) * (if(random.nextBoolean()) 1 else -1), randomWidth)
  }
}
