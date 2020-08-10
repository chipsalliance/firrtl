// See LICENSE for license details.

package firrtl.testutils

import org.scalatest.flatspec.AnyFlatSpec
import firrtl.ir.Circuit
import firrtl.options.{Dependency, IdentityLike}
import firrtl.passes.{PassExceptions, RemoveEmpty}
import firrtl.stage.Forms
import firrtl._
import firrtl.annotations._
import firrtl.stage.TransformManager.TransformDependency
import logger._
import org.scalatest.flatspec.AnyFlatSpec



class VerilogTransformSpec extends LeanTransformSpec(Seq(Dependency[firrtl.VerilogEmitter]))
class LowFirrtlTransformSpec extends LeanTransformSpec(Seq(Dependency[firrtl.LowFirrtlEmitter]))

class LeanTransformSpec(protected val transforms: Seq[TransformDependency]) extends AnyFlatSpec with FirrtlMatchers with LazyLogging {
   private val compiler = new firrtl.stage.transforms.Compiler(transforms)

   protected def compile(src: String): CircuitState = compile(src, Seq())
   protected def compile(src: String, annos: AnnotationSeq): CircuitState = compile(firrtl.Parser.parse(src), annos)
   protected def compile(c: ir.Circuit): CircuitState = compile(c, Seq())
   protected def compile(c: ir.Circuit, annos: AnnotationSeq): CircuitState =
      compiler.transform(CircuitState(c, annos))
   protected def execute(input: String, check: String): CircuitState = execute(input, check ,Seq())
   protected def execute(input: String, check: String, annotations: Seq[Annotation]): CircuitState = {
      val finalState = compiler.transform(CircuitState(parse(input), annotations))
      val actual = RemoveEmpty.run(parse(finalState.getEmittedCircuit.value)).serialize
      val expected = parse(check).serialize
      logger.debug(actual)
      logger.debug(expected)
      (actual) should be (expected)
      finalState
   }
}

trait MakeCompiler {
   protected def makeVerilogCompiler(transforms: Seq[TransformDependency] = Seq()) =
      new firrtl.stage.transforms.Compiler(Seq(Dependency[firrtl.VerilogEmitter]) ++ transforms)
   protected def makeMinimumVerilogCompiler(transforms: Seq[TransformDependency] = Seq()) =
      new firrtl.stage.transforms.Compiler(Seq(Dependency[firrtl.MinimumVerilogEmitter]) ++ transforms)
   protected def makeLowFirrtlCompiler(transforms: Seq[TransformDependency] = Seq()) =
      new firrtl.stage.transforms.Compiler(Seq(Dependency[firrtl.LowFirrtlEmitter]) ++ transforms)
}


// An example methodology for testing Firrtl Passes
// Spec class should extend this class
@deprecated("Use LeanTransformSpec instead!", "FIRRTL 1.4")
abstract class SimpleTransformSpec extends AnyFlatSpec with FirrtlMatchers with LazyLogging {
   protected def emitter: Dependency[Emitter]
   private lazy val compiler = new firrtl.stage.transforms.Compiler(Seq(emitter))

   // Utility function
   def squash(c: Circuit): Circuit = RemoveEmpty.run(c)

   // Executes the test. Call in tests.
   // annotations cannot have default value because scalatest trait Suite has a default value
   def execute(input: String, check: String, annotations: Seq[Annotation]): CircuitState = {
      val finalState = compiler.transform(CircuitState(parse(input), annotations))
      val actual = RemoveEmpty.run(parse(finalState.getEmittedCircuit.value)).serialize
      val expected = parse(check).serialize
      logger.debug(actual)
      logger.debug(expected)
      (actual) should be (expected)
      finalState
   }

   def executeWithAnnos(input: String, check: String, annotations: Seq[Annotation],
     checkAnnotations: Seq[Annotation]): CircuitState = {
      val finalState = compiler.transform(CircuitState(parse(input), annotations))
      val actual = RemoveEmpty.run(parse(finalState.getEmittedCircuit.value)).serialize
      val expected = parse(check).serialize
      logger.debug(actual)
      logger.debug(expected)
      (actual) should be (expected)

      annotations.foreach { anno =>
        logger.debug(anno.serialize)
      }

      finalState.annotations.toSeq.foreach { anno =>
        logger.debug(anno.serialize)
      }
      checkAnnotations.foreach { check =>
        (finalState.annotations.toSeq) should contain (check)
      }
      finalState
   }
   // Executes the test, should throw an error
   // No default to be consistent with execute
   def failingexecute(input: String, annotations: Seq[Annotation]): Exception = {
      intercept[PassExceptions] {
         compiler.transform(CircuitState(parse(input), annotations))
      }
   }

   protected def compile(state: CircuitState): CircuitState = compiler.transform(state)
}

/** Transform that re-runs resolve and check transforms as late as possible, but before any emitters. */
object ReRunResolveAndCheck extends Transform with DependencyAPIMigration with IdentityLike[CircuitState] {

  override val optionalPrerequisites = Forms.LowFormOptimized
  override val optionalPrerequisiteOf = Forms.ChirrtlEmitters

  override def invalidates(a: Transform) = {
    val resolveAndCheck = Forms.Resolved.toSet -- Forms.WorkingIR
    resolveAndCheck.contains(Dependency.fromTransform(a))
  }

  override def execute(a: CircuitState) = transform(a)

}

trait LowTransformSpec extends SimpleTransformSpec {
   protected override def emitter = Dependency[LowFirrtlEmitter]
}

trait MiddleTransformSpec extends SimpleTransformSpec {
   protected override def emitter = Dependency[MiddleFirrtlEmitter]
}

trait HighTransformSpec extends SimpleTransformSpec {
   protected override def emitter = Dependency[HighFirrtlEmitter]
}
