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

import com.typesafe.scalalogging.LazyLogging
import java.io.{StringWriter,Writer}
import org.scalatest.{FlatSpec, Matchers}
import firrtl.{Parser,Circuit,FIRRTLEmitter}
import firrtl.Parser.IgnoreInfo
import firrtl.passes.{Pass, PassExceptions}
import firrtl.{
   Transform,
   TransformResult,
   SimpleRun,
   Chisel3ToHighFirrtl,
   IRToWorkingIR,
   ResolveAndCheck,
   HighFirrtlToMiddleFirrtl,
   MiddleFirrtlToLowFirrtl,
   EmitFirrtl,
   Compiler
}
import firrtl.Annotations.AnnotationMap


// An example methodology for testing Firrtl Passes
// Spec class should extend this class
abstract class SimpleTransformSpec extends FlatSpec with Matchers with Compiler with LazyLogging {
   // Utility function
   def parse(s: String): Circuit = Parser.parse(s.split("\n").toIterator, infoMode = IgnoreInfo)

   // Executes the test. Call in tests.
   def execute(writer: Writer, annotations: AnnotationMap, input: String, check: String) = {
      compile(parse(input), annotations, writer)
      println(writer.toString)
      println(check)
      //logger.debug(writer.toString)
      //logger.debug(check)
      (parse(writer.toString)) should be (parse(check))
   }
   // Executes the test, should throw an error
   def failingexecute(writer: Writer, annotations: AnnotationMap, input: String): Exception = {
      intercept[PassExceptions] {
         compile(parse(input), annotations, writer)
      }
   }
}

trait LowTransformSpec extends SimpleTransformSpec {
   def transform: Transform
   def transforms (writer: Writer) = Seq(
      new Chisel3ToHighFirrtl(),
      new IRToWorkingIR(),
      new ResolveAndCheck(),
      new HighFirrtlToMiddleFirrtl(),
      new MiddleFirrtlToLowFirrtl(),
      new ResolveAndCheck(),
      transform,
      new EmitFirrtl(writer)
   )
}

trait MiddleTransformSpec extends SimpleTransformSpec {
   def transform: Transform
   def transforms (writer: Writer) = Seq(
      new Chisel3ToHighFirrtl(),
      new IRToWorkingIR(),
      new ResolveAndCheck(),
      new HighFirrtlToMiddleFirrtl(),
      new ResolveAndCheck(),
      transform,
      new EmitFirrtl(writer)
   )
}

trait HighTransformSpec extends SimpleTransformSpec {
   def transform: Transform
   def transforms (writer: Writer) = Seq(
      new Chisel3ToHighFirrtl(),
      new IRToWorkingIR(),
      new ResolveAndCheck(),
      transform,
      new EmitFirrtl(writer)
   )
}
