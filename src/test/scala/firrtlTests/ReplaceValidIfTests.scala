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

import util.Random
import java.io.StringWriter

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import firrtl.{Parser,Circuit}
import firrtl.Annotations.{
  Named,
  CircuitName,
  ModuleName,
  ComponentName,
  TransID,
  Annotation,
  AnnotationMap
}
import firrtl.passes.{
  PassExceptions,
  ReplaceValidIf,
  ReplaceValidIfAnnotation,
  RBehavior,
  DBehavior
}


/**
 * Tests ReplaceValidIf Transform
 */
class ReplaceValidIfTests extends LowTransformSpec {
   val tID = TransID(0)
   val transform = new ReplaceValidIf(tID)
   "Validif" should "be replaced with default value" in {
      val input =
         """circuit Top :
           |  module Top :
           |    input p : UInt<1>
           |    input a : UInt<32>
           |    output b : UInt<32>
           |    b <= validif(p, a)""".stripMargin
      val check =
         """circuit Top :
           |  module Top :
           |    input p : UInt<1>
           |    input a : UInt<32>
           |    output b : UInt<32>
           |    skip
           |    b <= a""".stripMargin
      val writer = new StringWriter()
      val aMap = new AnnotationMap(Seq(ReplaceValidIfAnnotation(DBehavior, CircuitName("Top"), tID)))
      execute(writer, aMap, input, check)
   }

   "UIntType Validif" should "be replaced with muxing a random value" in {
      val input =
         """circuit Top :
           |  module Top :
           |    input p : UInt<1>
           |    input a : UInt<32>
           |    output b : UInt<32>
           |    b <= validif(p, a)""".stripMargin
      val check =
         """circuit Top :
           |  module Top :
           |    input p : UInt<1>
           |    input a : UInt<32>
           |    output b : UInt<32>
           |    skip
           |    b <= mux(p, a, UInt("hd69fd5b8"))""".stripMargin
      val writer = new StringWriter()
      val aMap = new AnnotationMap(Seq(ReplaceValidIfAnnotation(RBehavior(new Random(100)), CircuitName("Top"), tID)))
      execute(writer, aMap, input, check)
   }

   "SIntType Validif" should "be replaced with muxing a random value" in {
      val input =
         """circuit Top :
           |  module Top :
           |    input p : UInt<1>
           |    input a : SInt<32>
           |    output b : SInt<32>
           |    b <= validif(p, a)""".stripMargin
      val check =
         """circuit Top :
           |  module Top :
           |    input p : UInt<1>
           |    input a : SInt<32>
           |    output b : SInt<32>
           |    skip
           |    b <= mux(p, a, SInt<32>("hd69fd5b8"))""".stripMargin
      val writer = new StringWriter()
      val aMap = new AnnotationMap(Seq(ReplaceValidIfAnnotation(RBehavior(new Random(100)), CircuitName("Top"), tID)))
      execute(writer, aMap, input, check)
   }
}

