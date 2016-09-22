// See LICENSE for license details.

package firrtl

import firrtl._
import firrtl.ir._
import firrtl.passes.Pass
import firrtl.Annotations.AnnotationMap
import java.io.Writer

// A simple pass manager designed to facilitate the needs of "standard" FIRRTL
// flows: the conversion of Chisel's output to Verilog.  Users that want to
// produce Verilog from Chisel in slightly custom ways (for example, the
// emission of SRAM-related metadata to feed into Berkeley's various VLSI
// flows).
abstract class ReferencePassManager extends firrtl.Compiler {
  private def convertToTransforms(ors: Seq[Either[Pass, Transform]]): Seq[Transform] = {
    class SinglePassTransform(pass: Pass) extends Transform with SimpleRun {
      def execute(circuit: Circuit, annotationMap: AnnotationMap): TransformResult =
        run(circuit, Seq(pass))
    }

    def convertToTransform(either: Either[Pass, Transform]): Transform =
      either match {
        case Right(t) => t
        case Left(p)  => new SinglePassTransform(p)
      }

    ors.map( or => convertToTransform(or) )
  }

  // This is some sugar to users don't need to actually know ther's a tagged
  // union in here to allow them to transparently provide either Pass or
  // Transform instances to the pass manager.
  implicit def l2Either[L, R](l: L): Either[L, R] = Left(l)
  implicit def r2Either[L, R](r: R): Either[L, R] = Right(r)

  // These functions are designed to be overridden by users.  They return a
  // sequence of transformations that will be applied at various FIRRTL levels.
  // A transform that operates at a FIRRTL level is expected to both produce
  // and consume FIRRTL at that level.
  protected def operateHigh():   Seq[Either[Pass, Transform]] = Seq()
  protected def operateMiddle(): Seq[Either[Pass, Transform]] = Seq()
  protected def operateLow():    Seq[Either[Pass, Transform]] = Seq()

  // Since the set of top-level annotations required to make a pass function
  // are specific to the pass, pass managers have to provide their own set of
  // top-level annotations.
  def annotations(): AnnotationMap = new AnnotationMap(Seq())

  // The pass manager exists to define what passes should be used to compile
  // FIRRTL to Verilog, this function defines that set.  This is final, if you
  // want to change what passes are run then override the methods listed above.
  final def transforms(writer: Writer): Seq[Transform] =
    Seq(
      new Chisel3ToHighFirrtl(),
      new IRToWorkingIR(),
      new ResolveAndCheck()
    ) ++ convertToTransforms(operateHigh()) ++
    Seq(
      new HighFirrtlToMiddleFirrtl()
    ) ++ convertToTransforms(operateMiddle()) ++
    Seq(
      new MiddleFirrtlToLowFirrtl()
    ) ++ convertToTransforms(operateLow()) ++
    Seq(
      new EmitVerilogFromLowFirrtl(writer)
    )
}
