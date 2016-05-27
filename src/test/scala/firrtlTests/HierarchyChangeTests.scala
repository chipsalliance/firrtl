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

import java.io.StringWriter

import scala.collection.mutable

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import firrtl.{Parser,Circuit}
import firrtl.passes.PassExceptions
import firrtl.Annotations.{
   Named,
   CircuitName,
   ModuleName,
   ComponentName,
   TransID,
   Annotation,
   AnnotationMap
}
import firrtl.passes.{HierarchyChange, HierarchyAnnotation, InstModulePath, InstModulePair}
//HierarchyChange({
//   val from = InstModulePath(
//      InstModulePair("top","Top"),
//      InstModulePair("uncore","Uncore"),
//      InstModulePair("htif","Htif"))
//   val to = InstModulePath(
//      InstModulePair("top","Top"),
//      InstModulePair("htif","Htif_star"))
//   val oldModRenames = HashMap[String,String]()
//   oldModRenames("Uncore") = "Uncore_star"
//   val newModRenames = HashMap[String,String]()
//   val portName = "htif_port"
//   new HierarchyAnnotation(from,to,oldModRenames,newModRenames,portName)
//}),


/**
 * Tests hierarchy change transformation
 */
class HierarchyChangeTests extends HighTransformSpec {
   val tID = TransID(0)
   val transform = new HierarchyChange(tID)
   "The module C" should "be moved to Top/c:C" in {
      val input =
         """circuit Top :
           |  module Top :
           |    input a : UInt<32>
           |    output b : UInt<32>
           |    inst old0 of Old0
           |    old0.a <= a
           |    b <= old0.b
           |  module Old0 :
           |    input a : UInt<32>
           |    output b : UInt<32>
           |    inst c of C
           |    c.a <= a
           |    b <= c.b
           |  module C :
           |    input a : UInt<32>
           |    output b : UInt<32>
           |    b <= a
           |""".stripMargin
      val check =
         """circuit Top :
           |  module Top :
           |    input a : UInt<32>
           |    output b : UInt<32>
           |    inst c_moved of C_star
           |    inst old0 of Old0_star
           |    old0.a <= a
           |    b <= old0.b
           |    old0.c_port <= c_moved
           |  module C :
           |    input a : UInt<32>
           |    output b : UInt<32>
           |    b <= a
           |  module Old0 :
           |    input a : UInt<32>
           |    output b : UInt<32>
           |    inst c of C
           |    c.a <= a
           |    b <= c.b
           |  module Old0_star :
           |    input a : UInt<32>
           |    output b : UInt<32>
           |    input c_port : { flip a : UInt<32>, b : UInt<32>}
           |    wire c : { flip a : UInt<32>, b : UInt<32>}
           |    c.a <= a
           |    b <= c.b
           |    c <= c_port
           |  module C_star :
           |    input a : UInt<32>
           |    output b : UInt<32>
           |    b <= a""".stripMargin
      val from = new InstModulePath(Seq(
         InstModulePair("top","Top"),
         InstModulePair("old0","Old0"),
         InstModulePair("c","C")))
      val to = new InstModulePath(Seq(
         InstModulePair("top","Top"),
         InstModulePair("c_moved","C_star")))
      val oldModRenames = Map("Old0" -> "Old0_star")
      val newModRenames = Map[String,String]()
      val portName = "c_port"
      val ann = new HierarchyAnnotation(CircuitName("Top"), tID, from, to, oldModRenames,newModRenames, portName)
      val aMap = new AnnotationMap(Seq(ann))
      val writer = new StringWriter()
      execute(writer, aMap, input, check)
   }
}



