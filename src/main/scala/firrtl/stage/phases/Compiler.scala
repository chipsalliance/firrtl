// See LICENSE for license details.

package firrtl.stage.phases

import firrtl.{AnnotationSeq, ChirrtlForm, CircuitState, Transform, seqToAnnoSeq}
import firrtl.options.{Dependency, Phase, PhasePrerequisiteException, Translator}
import firrtl.stage.{FirrtlCircuitAnnotation, Forms, RunFirrtlTransformAnnotation}
import firrtl.stage.TransformManager.TransformDependency

import scala.collection.mutable

/** An encoding of the information necessary to run the FIRRTL compiler once */
private [stage] case class CompilerRun(
  stateIn: CircuitState,
  stateOut: Option[CircuitState],
  transforms: Seq[TransformDependency] )

/** An encoding of possible defaults for a [[CompilerRun]] */
private [stage] case class Defaults(
  annotations: AnnotationSeq = Seq.empty,
  transforms: Seq[TransformDependency] = Seq.empty)

/** Runs the FIRRTL compilers on an [[AnnotationSeq]]. If the input [[AnnotationSeq]] contains more than one circuit
  * (i.e., more than one [[firrtl.stage.FirrtlCircuitAnnotation FirrtlCircuitAnnotation]]), then annotations will be
  * broken up and each run will be executed in parallel.
  *
  * The [[AnnotationSeq]] will be chunked up into compiler runs using the following algorithm. All annotations that
  * occur before the first [[firrtl.stage.FirrtlCircuitAnnotation FirrtlCircuitAnnotation]] are treated as global
  * annotations that apply to all circuits. Annotations after a circuit are only associated with their closest
  * preceeding circuit. E.g., for the following annotations (where A, B, and C are some annotations):
  *
  *    A(a), FirrtlCircuitAnnotation(x), B, FirrtlCircuitAnnotation(y), A(b), C, FirrtlCircuitAnnotation(z)
  *
  * Then this will result in three compiler runs:
  *   - FirrtlCircuitAnnotation(x): A(a), B
  *   - FirrtlCircuitAnnotation(y): A(a), A(b), C
  *   - FirrtlCircuitAnnotation(z): A(a)
  *
  * A(a) is a default, global annotation. B binds to FirrtlCircuitAnnotation(x). A(a), A(b), and C bind to
  * FirrtlCircuitAnnotation(y). Note: A(b) ''may'' overwrite A(a) if this is a CompilerAnnotation.
  * FirrtlCircuitAnnotation(z) has no annotations, so it only gets the default A(a).
  */
class Compiler extends Phase with Translator[AnnotationSeq, Seq[CompilerRun]] {

  override def prerequisites =
    Seq(Dependency[AddDefaults],
        Dependency[AddImplicitEmitter],
        Dependency[Checks],
        Dependency[AddCircuit],
        Dependency[AddImplicitOutputFile])

  override def optionalPrerequisiteOf = Seq(Dependency[WriteEmitted])

  override def invalidates(a: Phase) = false

  /** Convert an [[AnnotationSeq]] into a sequence of compiler runs. */
  protected def aToB(a: AnnotationSeq): Seq[CompilerRun] = {
    var foundFirstCircuit = false
    val c = mutable.ArrayBuffer.empty[CompilerRun]
    a.foldLeft(Defaults()){
      case (d, FirrtlCircuitAnnotation(circuit)) =>
        foundFirstCircuit = true
        CompilerRun(CircuitState(circuit, ChirrtlForm, d.annotations, None), None, d.transforms) +=: c
        d
      case (d, a) if foundFirstCircuit => a match {
        case RunFirrtlTransformAnnotation(transform) =>
          c(0) = c(0).copy(transforms = Dependency.fromTransform(transform) +: c(0).transforms)
          d
        case annotation =>
          val state = c(0).stateIn
          c(0) = c(0).copy(stateIn = state.copy(annotations = annotation +: state.annotations))
          d
      }
      case (d, a) if !foundFirstCircuit => a match {
        case RunFirrtlTransformAnnotation(transform) => d.copy(transforms = Dependency.fromTransform(transform) +: d.transforms)
        case annotation => d.copy(annotations = annotation +: d.annotations)
      }
    }
    c.toSeq
  }

  /** Expand compiler output back into an [[AnnotationSeq]]. Annotations used in the construction of the compiler run are
    * removed ([[CompilerAnnotation]]s and [[RunFirrtlTransformAnnotation]]s).
    */
  protected def bToA(b: Seq[CompilerRun]): AnnotationSeq =
    b.flatMap( bb => FirrtlCircuitAnnotation(bb.stateOut.get.circuit) +: bb.stateOut.get.annotations )

  /** Run the FIRRTL compiler some number of times. If more than one run is specified, a parallel collection will be
    * used.
    */
  protected def internalTransform(b: Seq[CompilerRun]): Seq[CompilerRun] = {
    def f(c: CompilerRun): CompilerRun = {
      val tm = new firrtl.stage.transforms.Compiler(c.transforms)
      /* Transform order is lazily evaluated. Force it here to remove its resolution time from actual compilation. */
      val (timeResolveDependencies, _) = firrtl.Utils.time { tm.flattenedTransformOrder }
      logger.info(f"Computed transform order in: $timeResolveDependencies%.1f ms")
      /* Show the determined transform order */
      logger.info("Determined Transform order that will be executed:\n" + tm.prettyPrint("  "))
      /* Run all determined transforms tracking how long everything takes to run */
      val (timeExecute, annotationsOut) = firrtl.Utils.time { tm.transform(c.stateIn) }
      logger.info(f"Total FIRRTL Compile Time: $timeExecute%.1f ms")
      c.copy(stateOut = Some(annotationsOut))
    }

    if (b.size <= 1) { b.map(f) } else {
      collection.parallel.immutable.ParVector(b :_*).par.map(f).seq
    }
  }
}
