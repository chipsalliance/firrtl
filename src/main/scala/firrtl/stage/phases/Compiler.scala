// See LICENSE for license details.

package firrtl.stage.phases

import firrtl.{AnnotationSeq, ChirrtlForm, CircuitState, Compiler => FirrtlCompiler, Transform, seqToAnnoSeq}
import firrtl.options.{Phase, PhasePrerequisiteException, Translator}
import firrtl.stage.{CompilerAnnotation, FirrtlCircuitAnnotation,
  RunFirrtlTransformAnnotation}

import scala.collection.mutable

/** An encoding of the information necessary to run the FIRRTL compiler once */
private [stage] case class CompilerRun(
  stateIn: CircuitState,
  stateOut: Option[CircuitState],
  transforms: Seq[Transform],
  compiler: Option[FirrtlCompiler] )

/** An encoding of possible defaults for a [[CompilerRun]] */
private [stage] case class Defaults(
  annotations: AnnotationSeq = Seq.empty,
  transforms: Seq[Transform] = Seq.empty,
  compiler: Option[FirrtlCompiler] = None)

/** Runs the FIRRTL compilers on an [[AnnotationSeq]]. If the input [[AnnotationSeq]] contains more than one circuit
  * (i.e., more than one [[FirrtlCircuitAnnotation]]), then annotations will be broken up and each run will be executed
  * in parallel.
  *
  * The [[AnnotationSeq]] will be chunked up into compiler runs using the following algorithm. All annotations that
  * occur before the first [[FirrtlCircuitAnnotation]] are treated as global annotations that apply to all circuits.
  * Annotations after a circuit are only associated with their closest preceeding circuit. E.g., for the following
  * annotations (where A, B, and C are some annotations):
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

  /** Convert an [[AnnotationSeq]] into a sequence of compiler runs. */
  protected def aToB(a: AnnotationSeq): Seq[CompilerRun] = {
    var foundFirstCircuit = false
    val c = mutable.ArrayBuffer.empty[CompilerRun]
    a.foldLeft(Defaults()){
      case (d, FirrtlCircuitAnnotation(circuit)) =>
        foundFirstCircuit = true
        CompilerRun(CircuitState(circuit, ChirrtlForm, d.annotations, None), None, d.transforms, d.compiler) +=: c
        d
      case (d, a) if foundFirstCircuit => a match {
        case RunFirrtlTransformAnnotation(transform) =>
          c(0) = c(0).copy(transforms = transform +: c(0).transforms)
          d
        case CompilerAnnotation(compiler) =>
          c(0) = c(0).copy(compiler = Some(compiler))
          d
        case annotation =>
          val state = c(0).stateIn
          c(0) = c(0).copy(stateIn = state.copy(annotations = annotation +: state.annotations))
          d
      }
      case (d, a) if !foundFirstCircuit => a match {
        case RunFirrtlTransformAnnotation(transform) => d.copy(transforms = transform +: d.transforms)
        case CompilerAnnotation(compiler) => d.copy(compiler = Some(compiler))
        case annotation => d.copy(annotations = annotation +: d.annotations)
      }
    }
    c
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
      val statex = c
        .compiler
        .getOrElse { throw new PhasePrerequisiteException("No compiler specified!") }
        .compile(c.stateIn, c.transforms.reverse)
      c.copy(stateOut = Some(statex))
    }

    if (b.size <= 1) { b.map(f)         }
    else             { b.par.map(f).seq }
  }

}
