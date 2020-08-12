// See LICENSE for license details.

package firrtlTests.options.phases


import java.io.File

import firrtl.AnnotationSeq
import firrtl.annotations.{DeletedAnnotation, NoTargetAnnotation}
import firrtl.options.{
  CustomFileEmission,
  InputAnnotationFileAnnotation,
  OutputAnnotationFileAnnotation,
  Phase,
  PhaseException,
  StageOptions,
  TargetDirAnnotation,
  WriteDeletedAnnotation}
import firrtl.options.Viewer.view
import firrtl.options.phases.{GetIncludes, WriteOutputAnnotations}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class WriteOutputAnnotationsSpec extends AnyFlatSpec with Matchers with firrtl.testutils.Utils {

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
    val read = (new GetIncludes)
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

  class Fixture { val phase: Phase = new WriteOutputAnnotations }

  behavior of classOf[WriteOutputAnnotations].toString

  it should "write annotations to a file (excluding DeletedAnnotations)" in new Fixture {
    val file = new File(dir + "/should-write-annotations-to-a-file.anno.json")
    val annotations = Seq( OutputAnnotationFileAnnotation(file.toString),
                           WriteOutputAnnotationsSpec.FooAnnotation,
                           WriteOutputAnnotationsSpec.BarAnnotation(0),
                           WriteOutputAnnotationsSpec.BarAnnotation(1),
                           DeletedAnnotation("foo", WriteOutputAnnotationsSpec.FooAnnotation) )
    val expected = annotations.filter {
      case a: DeletedAnnotation => false
      case a => true
    }
    val out = phase.transform(annotations)

    info("annotations are unmodified")
    out.toSeq should be (annotations)

    fileContainsAnnotations(file, expected)
  }

  it should "include DeletedAnnotations if a WriteDeletedAnnotation is present" in new Fixture {
    val file = new File(dir + "should-include-deleted.anno.json")
    val annotations = Seq( OutputAnnotationFileAnnotation(file.toString),
                           WriteOutputAnnotationsSpec.FooAnnotation,
                           WriteOutputAnnotationsSpec.BarAnnotation(0),
                           WriteOutputAnnotationsSpec.BarAnnotation(1),
                           DeletedAnnotation("foo", WriteOutputAnnotationsSpec.FooAnnotation),
                           WriteDeletedAnnotation )
    val out = phase.transform(annotations)

    info("annotations are unmodified")
    out.toSeq should be (annotations)

    fileContainsAnnotations(file, annotations)
  }

  it should "do nothing if no output annotation file is specified" in new Fixture {
    val annotations = Seq( WriteOutputAnnotationsSpec.FooAnnotation,
                           WriteOutputAnnotationsSpec.BarAnnotation(0),
                           WriteOutputAnnotationsSpec.BarAnnotation(1) )

    val out = catchWrites { phase.transform(annotations) } match {
      case Right(a) =>
        info("no file writes occurred")
        a
      case Left(a) =>
        fail(s"No file writes expected, but a write to '$a' ocurred!")
    }

    info("annotations are unmodified")
    out.toSeq should be (annotations)
  }

  it should "write CustomFileEmission annotations" in new Fixture {
    val file = new File("write-CustomFileEmission-annotations.anno.json")
    val annotations = Seq( TargetDirAnnotation(dir),
                           OutputAnnotationFileAnnotation(file.toString),
                           WriteOutputAnnotationsSpec.Custom("hello!") )
    val serializedFileName = view[StageOptions](annotations).getBuildFileName("Custom", Some(".Emission"))
    val expected = annotations.map {
      case _: WriteOutputAnnotationsSpec.Custom => WriteOutputAnnotationsSpec.Replacement(serializedFileName)
      case a => a
    }

    val out = phase.transform(annotations)

    info("annotations are unmodified")
    out.toSeq should be (annotations)

    fileContainsAnnotations(new File(dir, file.toString), expected)

    info(s"file '$serializedFileName' exists")
    new File(serializedFileName) should (exist)
  }

  it should "error if multiple annotations try to write to the same file" in new Fixture {
    val file = new File("write-CustomFileEmission-annotations-error.anno.json")
    val annotations = Seq( TargetDirAnnotation(dir),
                           OutputAnnotationFileAnnotation(file.toString),
                           WriteOutputAnnotationsSpec.Custom("foo"),
                           WriteOutputAnnotationsSpec.Custom("bar") )
    intercept[PhaseException] {
      phase.transform(annotations)
    }.getMessage should startWith ("Multiple CustomFileEmission annotations")
  }

}

private object WriteOutputAnnotationsSpec {

  case object FooAnnotation extends NoTargetAnnotation

  case class BarAnnotation(x: Int) extends NoTargetAnnotation

  case class Custom(value: String) extends NoTargetAnnotation with CustomFileEmission {

    override protected def baseFileName(a: AnnotationSeq): String = "Custom"

    override protected def suffix: Option[String] = Some(".Emission")

    override def getBytes: Iterable[Byte] = value.getBytes

    override def replacements(file: File): AnnotationSeq = Seq(Replacement(file.toString))

  }

  case class Replacement(file: String) extends NoTargetAnnotation

}
