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

package firrtlTests

import org.scalatest.Matchers
import java.io.{StringWriter,Writer}
import firrtl._
import firrtl.Parser.IgnoreInfo
import firrtl.passes._

// Tests the following cases for constant propagation:
//   1) Unsigned integers are always greater than or
//        equal to zero
//   2) Values are always smaller than a number greater
//        than their maximum value
//   3) Values are always greater than a number smaller
//        than their minimum value
class ConstantPropagationSpec extends FirrtlFlatSpec {
  val passes = Seq(
      ToWorkingIR,
      ResolveKinds,
      InferTypes,
      ResolveGenders,
      InferWidths,
      ConstProp)
  def parse(input: String): Circuit = Parser.parse(input.split("\n").toIterator, IgnoreInfo)
  private def exec (input: String) = {
    passes.foldLeft(parse(input)) {
      (c: Circuit, p: Pass) => p.run(c)
    }.serialize
  }
   // =============================
   "The rule x >= 0 " should " always be true if x is a UInt" in {
      val input =
"""circuit Top :
  module Top :
    input x : UInt<5>
    output y : UInt<1>
    y <= geq(x, UInt(0))
"""
      val check =
"""circuit Top :
  module Top :
    input x : UInt<5>
    output y : UInt<1>
    y <= UInt<1>("h1")
"""
      (parse(exec(input))) should be (parse(check))
   }

   // =============================
   "The rule x < 0 " should " never be true if x is a UInt" in {
      val input =
"""circuit Top :
  module Top :
    input x : UInt<5>
    output y : UInt<1>
    y <= lt(x, UInt(0))
"""
      val check =
"""circuit Top :
  module Top :
    input x : UInt<5>
    output y : UInt<1>
    y <= UInt<1>(0)
"""
      (parse(exec(input))) should be (parse(check))
   }

   // =============================
   "The rule 0 <= x " should " always be true if x is a UInt" in {
      val input =
"""circuit Top :
  module Top :
    input x : UInt<5>
    output y : UInt<1>
    y <= leq(UInt(0),x)
"""
      val check =
"""circuit Top :
  module Top :
    input x : UInt<5>
    output y : UInt<1>
    y <= UInt<1>(1)
"""
      (parse(exec(input))) should be (parse(check))
   }

   // =============================
   "The rule 0 > x " should " never be true if x is a UInt" in {
      val input =
"""circuit Top :
  module Top :
    input x : UInt<5>
    output y : UInt<1>
    y <= gt(UInt(0),x)
"""
      val check =
"""circuit Top :
  module Top :
    input x : UInt<5>
    output y : UInt<1>
    y <= UInt<1>(0)
"""
      (parse(exec(input))) should be (parse(check))
   }

   // =============================
   "The rule 1 < 3 " should " always be true" in {
      val input =
"""circuit Top :
  module Top :
    input x : UInt<5>
    output y : UInt<1>
    y <= lt(UInt(0),UInt(3))
"""
      val check =
"""circuit Top :
  module Top :
    input x : UInt<5>
    output y : UInt<1>
    y <= UInt<1>(1)
"""
      (parse(exec(input))) should be (parse(check))
   }

   // =============================
   "The rule x < 8 " should " always be true if x only has 3 bits" in {
      val input =
"""circuit Top :
  module Top :
    input x : UInt<3>
    output y : UInt<1>
    y <= lt(x,UInt(8))
"""
      val check =
"""circuit Top :
  module Top :
    input x : UInt<3>
    output y : UInt<1>
    y <= UInt<1>(1)
"""
      (parse(exec(input))) should be (parse(check))
   }

   // =============================
   "The rule x <= 7 " should " always be true if x only has 3 bits" in {
      val input =
"""circuit Top :
  module Top :
    input x : UInt<3>
    output y : UInt<1>
    y <= leq(x,UInt(7))
"""
      val check =
"""circuit Top :
  module Top :
    input x : UInt<3>
    output y : UInt<1>
    y <= UInt<1>(1)
"""
      (parse(exec(input))) should be (parse(check))
   }

   // =============================
   "The rule 8 > x" should " always be true if x only has 3 bits" in {
      val input =
"""circuit Top :
  module Top :
    input x : UInt<3>
    output y : UInt<1>
    y <= gt(UInt(8),x)
"""
      val check =
"""circuit Top :
  module Top :
    input x : UInt<3>
    output y : UInt<1>
    y <= UInt<1>(1)
"""
      (parse(exec(input))) should be (parse(check))
   }

   // =============================
   "The rule 7 >= x" should " always be true if x only has 3 bits" in {
      val input =
"""circuit Top :
  module Top :
    input x : UInt<3>
    output y : UInt<1>
    y <= geq(UInt(7),x)
"""
      val check =
"""circuit Top :
  module Top :
    input x : UInt<3>
    output y : UInt<1>
    y <= UInt<1>(1)
"""
      (parse(exec(input))) should be (parse(check))
   }

   // =============================
   "The rule 10 == 10" should " always be true" in {
      val input =
"""circuit Top :
  module Top :
    input x : UInt<3>
    output y : UInt<1>
    y <= eq(UInt(10),UInt(10))
"""
      val check =
"""circuit Top :
  module Top :
    input x : UInt<3>
    output y : UInt<1>
    y <= UInt<1>(1)
"""
      (parse(exec(input))) should be (parse(check))
   }

   // =============================
   "The rule x == z " should " not be true even if they have the same number of bits" in {
      val input =
"""circuit Top :
  module Top :
    input x : UInt<3>
    input z : UInt<3>
    output y : UInt<1>
    y <= eq(x,z)
"""
      val check =
"""circuit Top :
  module Top :
    input x : UInt<3>
    input z : UInt<3>
    output y : UInt<1>
    y <= eq(x,z)
"""
      (parse(exec(input))) should be (parse(check))
   }

   // =============================
   "The rule 10 != 10 " should " always be false" in {
      val input =
"""circuit Top :
  module Top :
    output y : UInt<1>
    y <= neq(UInt(10),UInt(10))
"""
      val check =
"""circuit Top :
  module Top :
    output y : UInt<1>
    y <= UInt(0)
"""
      (parse(exec(input))) should be (parse(check))
   }
   // =============================
   "The rule 1 >= 3 " should " always be false" in {
      val input =
"""circuit Top :
  module Top :
    input x : UInt<5>
    output y : UInt<1>
    y <= geq(UInt(1),UInt(3))
"""
      val check =
"""circuit Top :
  module Top :
    input x : UInt<5>
    output y : UInt<1>
    y <= UInt<1>(0)
"""
      (parse(exec(input))) should be (parse(check))
   }

   // =============================
   "The rule x >= 8 " should " never be true if x only has 3 bits" in {
      val input =
"""circuit Top :
  module Top :
    input x : UInt<3>
    output y : UInt<1>
    y <= geq(x,UInt(8))
"""
      val check =
"""circuit Top :
  module Top :
    input x : UInt<3>
    output y : UInt<1>
    y <= UInt<1>(0)
"""
      (parse(exec(input))) should be (parse(check))
   }

   // =============================
   "The rule x > 7 " should " never be true if x only has 3 bits" in {
      val input =
"""circuit Top :
  module Top :
    input x : UInt<3>
    output y : UInt<1>
    y <= gt(x,UInt(7))
"""
      val check =
"""circuit Top :
  module Top :
    input x : UInt<3>
    output y : UInt<1>
    y <= UInt<1>(0)
"""
      (parse(exec(input))) should be (parse(check))
   }

   // =============================
   "The rule 8 <= x" should " never be true if x only has 3 bits" in {
      val input =
"""circuit Top :
  module Top :
    input x : UInt<3>
    output y : UInt<1>
    y <= leq(UInt(8),x)
"""
      val check =
"""circuit Top :
  module Top :
    input x : UInt<3>
    output y : UInt<1>
    y <= UInt<1>(0)
"""
      (parse(exec(input))) should be (parse(check))
   }

   // =============================
   "The rule 7 < x" should " never be true if x only has 3 bits" in {
      val input =
"""circuit Top :
  module Top :
    input x : UInt<3>
    output y : UInt<1>
    y <= lt(UInt(7),x)
"""
      val check =
"""circuit Top :
  module Top :
    input x : UInt<3>
    output y : UInt<1>
    y <= UInt<1>(0)
"""
      (parse(exec(input))) should be (parse(check))
   }
}
