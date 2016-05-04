package firrtlTests

import java.io.{StringWriter,Writer}
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.junit.JUnitRunner
import firrtl.{Parser,Circuit,FIRRTLEmitter}
import firrtl.passes.{Pass, PassExceptions}
import firrtl.{
   Transform,
   CircuitAnnotation,
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


// An example methodology for testing Firrtl Passes
// Spec class should extend this class
abstract class SimpleTransformSpec extends FlatSpec with Matchers with Compiler {
   // Utility function
   def parse(s: String): Circuit = Parser.parse("",s.split("\n").toIterator,useInfo = false)

   // Executes the test. Call in tests.
   def execute(writer: Writer, annotations: Seq[CircuitAnnotation], input: String, check: String) = {
      compile(parse(input), annotations, writer)
      println(writer.toString)
      println(check)
      (parse(writer.toString)) should be (parse(check))
   }
   // Executes the test, should throw an error
   def failingexecute(writer: Writer, annotations: Seq[CircuitAnnotation], input: String) = {
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
