// See LICENSE for license details

package firrtl.options

import firrtl.AnnotationSeq
import firrtl.annotations.{Annotation, NoTargetAnnotation}
import firrtl.options.Viewer.view

import java.io.File

import scopt.OptionParser

sealed trait StageOption { this: Annotation => }

/** An annotation that should not be serialized automatically [[phases.WriteOutputAnnotations WriteOutputAnnotations]].
  * This usually means that this is an annotation that is used only internally to a [[Stage]].
  */
trait Unserializable { this: Annotation => }

/** Mix-in that lets an [[firrtl.annotations.Annotation Annotation]] serialize itself to a file separate from the output
  * annotation file.
  *
  * This can be used as a mechanism to serialize an [[firrtl.options.Unserializable Unserializable]] annotation or to
  * write ancillary collateral used by downstream tooling, e.g., a TCL script or an FPGA constraints file. Any
  * annotations using this mix-in will be serialized by the [[firrtl.options.phases.WriteOutputAnnotations
  * WriteOutputAnnotations]] phase. This is one of the last phases common to all [[firrtl.options.Stage Stages]] and
  * should not have to be called/included manually.
  *
  * Note: from the perspective of transforms generating annotations that mix-in this trait, the serialized files are not
  * expected to be available to downstream transforms. Communication of information between transforms must occur
  * through the annotations that will eventually be serialized to files.
  */
trait CustomFileEmission { this: Annotation =>

  /** Output filename where serialized content will be written */
  protected def baseFileName: String

  /** Optional suffix of the output file */
  protected def suffix: Option[String]

  /** A method that can convert this annotation to bytes that will be written to a file.
    *
    * If you only need to serialize a string, you can use the `getBytes` method:
    * {{{
    *  def toBytes: Option[Iterable[Byte]] = Some(myString.getBytes)
    * }}}
    */
  def toBytes: Option[Iterable[Byte]]

  /** Optionally, a sequence of annotations that will replace this annotation in the output annotation file.
    *
    * A non-None implementation of this method is a mechanism for telling a downstream [[firrtl.options.Stage Stage]]
    * how to deserialize the information that was serialized to a separate file. For example, if a FIRRTL circuit is
    * serialized to a separate file, this method could include an input file annotation that a later stage can use to
    * read the serialized FIRRTL circuit back in.
    */
  def replacements(file: File): Option[AnnotationSeq]

  /** Method that returns the filename where this annotation will be serialized.
    *
    * Users are not normally expected to override this method. Instead, changes to the default behavior can be handled
    * by overriding the baseFileName and suffix methods. However, if the filename cannot be statically known and is a
    * function of the content being serialized, then users may need to override this, e.g., if the filename should
    * include the top module of a FIRRTL circuit.
    *
    * @param annotations the annotations at the time of serialization
    */
  def filename(annotations: AnnotationSeq): File = {
    val name = view[StageOptions](annotations).getBuildFileName(baseFileName, suffix)
    new File(name)
  }

}

/** Holds the name of the target directory
  *  - set with `-td/--target-dir`
  *  - if unset, a [[TargetDirAnnotation]] will be generated with the
  * @param value target directory name
  */
case class TargetDirAnnotation(directory: String = ".") extends NoTargetAnnotation with StageOption

object TargetDirAnnotation extends HasShellOptions {

  val options = Seq(
    new ShellOption[String](
      longOption = "target-dir",
      toAnnotationSeq = (a: String) => Seq(TargetDirAnnotation(a)),
      helpText = "Work directory (default: '.')",
      shortOption = Some("td"),
      helpValueName = Some("<directory>") ) )

}

/** Additional arguments
  *  - set with any trailing option on the command line
  * @param value one [[scala.Predef.String String]] argument
  */
case class ProgramArgsAnnotation(arg: String) extends NoTargetAnnotation with StageOption

object ProgramArgsAnnotation {

  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.arg[String]("<arg>...")
    .unbounded()
    .optional()
    .action( (x, c) => ProgramArgsAnnotation(x) +: c )
    .text("optional unbounded args")
}

/** Holds a filename containing one or more [[annotations.Annotation]] to be read
  *  - this is not stored in [[FirrtlExecutionOptions]]
  *  - set with `-faf/--annotation-file`
  * @param value input annotation filename
  */
case class InputAnnotationFileAnnotation(file: String) extends NoTargetAnnotation with StageOption

object InputAnnotationFileAnnotation extends HasShellOptions {

  val options = Seq(
    new ShellOption[String](
      longOption = "annotation-file",
      toAnnotationSeq = (a: String) => Seq(InputAnnotationFileAnnotation(a)),
      helpText = "An input annotation file",
      shortOption = Some("faf"),
      helpValueName = Some("<file>") ) )

}

/** An explicit output _annotation_ file to write to
  *  - set with `-foaf/--output-annotation-file`
  * @param value output annotation filename
  */
case class OutputAnnotationFileAnnotation(file: String) extends NoTargetAnnotation with StageOption

object OutputAnnotationFileAnnotation extends HasShellOptions {

  val options = Seq(
    new ShellOption[String](
      longOption = "output-annotation-file",
      toAnnotationSeq = (a: String) => Seq(OutputAnnotationFileAnnotation(a)),
      helpText = "An output annotation file",
      shortOption = Some("foaf"),
      helpValueName = Some("<file>") ) )

}

/** If this [[firrtl.annotations.Annotation Annotation]] exists in an [[firrtl.AnnotationSeq AnnotationSeq]], then a
  * [[firrtl.options.phases.WriteOutputAnnotations WriteOutputAnnotations]] will include
  * [[firrtl.annotations.DeletedAnnotation DeletedAnnotation]]s when it writes to a file.
  *  - set with '--write-deleted'
  */
case object WriteDeletedAnnotation extends NoTargetAnnotation with StageOption with HasShellOptions {

  val options = Seq(
    new ShellOption[Unit](
      longOption = "write-deleted",
      toAnnotationSeq = (_: Unit) => Seq(WriteDeletedAnnotation),
      helpText = "Include deleted annotations in the output annotation file" ) )

}
