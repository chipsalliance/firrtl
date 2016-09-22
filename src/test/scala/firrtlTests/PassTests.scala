package firrtlTests

import com.typesafe.scalalogging.LazyLogging
import java.io.{StringWriter,Writer}
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.junit.JUnitRunner
import firrtl.{Parser,FIRRTLEmitter}
import firrtl.ir.Circuit
import firrtl.Parser.IgnoreInfo
import firrtl.passes.{Pass, PassExceptions}
import firrtl.{
   Transformation,
   TransformationResult,
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
   def parse(s: String): TransformationResult = TransformationResult.createTR(s.split("\n").toIterator, infoMode = IgnoreInfo)

   // Executes the test. Call in tests.
   def execute(writer: Writer, annotations: AnnotationMap, input: String, check: String) = {
      compile(parse(input), writer)
      logger.debug(writer.toString)
      logger.debug(check)
      (parse(writer.toString)) should be (parse(check))
   }
   // Executes the test, should throw an error
   def failingexecute(writer: Writer, annotations: AnnotationMap, input: String): Exception = {
      intercept[PassExceptions] {
         compile(parse(input), writer)
      }
   }
}

trait LowTransformSpec extends SimpleTransformSpec {
   def transform: Transformation
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
   def transform: Transformation
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
   def transform: Transformation
   def transforms (writer: Writer) = Seq(
      new Chisel3ToHighFirrtl(),
      new IRToWorkingIR(),
      new ResolveAndCheck(),
      transform,
      new EmitFirrtl(writer)
   )
}
