// See LICENSE for license details.

package firrtlTests.options.phases

import org.scalatest.{FlatSpec, Matchers}

import java.io.File

import firrtl.{AnnotationSeq, EmittedFirrtlCircuitAnnotation, EmittedFirrtlCircuit}
import firrtl.annotations.{DeletedAnnotation, NoTargetAnnotation}
import firrtl.options.{OutputAnnotationFileAnnotation, InputAnnotationFileAnnotation}
import firrtl.options.phases.{GetIncludes, WriteOutputAnnotations}
import firrtl.stage.FirrtlFileAnnotation

class WriteOutputAnnotationsSpec extends FlatSpec with Matchers with firrtlTests.Utils {

  val dir = "test_run_dir/WriteOutputAnnotationSpec"

  /** Check if the annotations contained by a [[File]] and the same, and in the same order, as a reference
    * [[AnnotationSeq]]. This uses [[GetIncludes]] as that already knows how to read annotation files.
    * @param f a file to read
    * @param a the expected annotations
    */
  private def fileContainsAnnotations(f: File, a: AnnotationSeq): Unit = {
    info(s"output file '$f' exists")
    f should (exist)

    info(s"reading '$f' works")
    val read = GetIncludes
      .transform(Seq(InputAnnotationFileAnnotation(f.toString)))
      .filterNot{
        case a @ DeletedAnnotation(_, _: InputAnnotationFileAnnotation) => true
        case _                                                          => false }

    info(s"annotations in file are expected size")
    read.size should be (a.size)

    read
      .zip(a)
      .foreach{ case (read, expected) =>
        info(s"$read matches")
        read should be (expected) }

    f.delete()
  }

  behavior of WriteOutputAnnotations.getClass.getName

  it should "write annotations to a file" in {
    val file = new File(dir + "/should-write-annotations-to-a-file.anno.json")
    val annotations = Seq( OutputAnnotationFileAnnotation(file.toString),
                           WriteOutputAnnotationsSpec.FooAnnotation,
                           WriteOutputAnnotationsSpec.BarAnnotation(0),
                           WriteOutputAnnotationsSpec.BarAnnotation(1) )
    val out = WriteOutputAnnotations.transform(annotations)

    info("annotations are unmodified")
    out.toSeq should be (annotations)

    fileContainsAnnotations(file, annotations)
  }

  it should "do nothing if no output annotation file is specified" in {
    val annotations = Seq( WriteOutputAnnotationsSpec.FooAnnotation,
                           WriteOutputAnnotationsSpec.BarAnnotation(0),
                           WriteOutputAnnotationsSpec.BarAnnotation(1) )

    val out = catchWrites { WriteOutputAnnotations.transform(annotations) } match {
      case Right(a) =>
        info("no file writes occurred")
        a
      case Left(a) =>
        fail(s"No file writes expected, but a write to '$a' ocurred!")
    }

    info("annotations are unmodified")
    out.toSeq should be (annotations)
  }

}

private object WriteOutputAnnotationsSpec {
  case object FooAnnotation extends NoTargetAnnotation
  case class BarAnnotation(x: Int) extends NoTargetAnnotation
}
