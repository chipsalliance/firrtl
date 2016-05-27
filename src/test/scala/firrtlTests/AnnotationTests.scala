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

import java.io.{Writer, StringWriter}

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner

import firrtl.{Parser, Circuit}
import firrtl.{
   ResolveAndCheck,
   RenameMap,
   Compiler,
   CompilerResult,
   VerilogCompiler
}
import firrtl.Annotations.{
   TransID,
   Named,
   CircuitName,
   ModuleName,
   ComponentName,
   AnnotationException,
   Annotation,
   Strict,
   Rigid,
   Firm,
   Loose,
   Sticky,
   Insistent,
   Fickle,
   Unstable,
   AnnotationMap
}

/**
 * An example methodology for testing Firrtl annotations.
 */
trait AnnotationSpec extends LowTransformSpec {
  // Dummy transform
  def transform = new ResolveAndCheck()

  // Check if Annotation Exception is thrown
  override def failingexecute(writer: Writer, annotations: AnnotationMap, input: String) = {
    intercept[AnnotationException] {
      compile(parse(input), annotations, writer)
    }
  }
  def execute(writer: Writer, annotations: AnnotationMap, input: String, check: Annotation) = {
    val cr = compile(parse(input), annotations, writer)
    (cr.annotationMap.annotations.head) should be (check)
  }
}


/**
 * Tests for Annotation Permissibility and Tenacity
 *
 * WARNING(izraelevitz): Not a complete suite of tests, requires the LowerTypes
 * pass and ConstProp pass to correctly populate its RenameMap before Strict, Rigid, Firm,
 * Unstable, Fickle, and Insistent can be tested.
 */
class AnnotationTests extends AnnotationSpec with Matchers {
  def getAMap (a: Annotation): AnnotationMap = new AnnotationMap(Seq(a))
  val tID = TransID(1)
  val input =
    """circuit Top :
       |  module Top :
       |    input a : UInt<1>[2]
       |    input b : UInt<1>
       |    node c = b""".stripMargin
  val mName = ModuleName("Top", CircuitName("Top"))
  val aName = ComponentName("a", mName)
  val bName = ComponentName("b", mName)
  val cName = ComponentName("c", mName)

  "Loose and Sticky annotation on a node" should "pass through" in {
    case class TestAnnotation(target: Named, tID: TransID) extends Annotation with Loose with Sticky {
      def duplicate(to: Named) = this.copy(target=to)
    }
    val w = new StringWriter()
    val ta = TestAnnotation(cName, tID)
    execute(w, getAMap(ta), input, ta)
  }
}
