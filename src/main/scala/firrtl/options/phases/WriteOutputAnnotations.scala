// See LICENSE for license details.

package firrtl.options.phases

import firrtl.AnnotationSeq
import firrtl.annotations.{DeletedAnnotation, JsonProtocol}
import firrtl.options.{Dependency, HowToSerialize, Phase, StageOptions, Unserializable, Viewer}

import java.io.PrintWriter

/** [[firrtl.options.Phase Phase]] that writes an [[AnnotationSeq]] to a file. A file is written if and only if a
  * [[StageOptions]] view has a non-empty [[StageOptions.annotationFileOut annotationFileOut]].
  */
class WriteOutputAnnotations extends Phase {

  override def prerequisites =
    Seq( Dependency[GetIncludes],
         Dependency[ConvertLegacyAnnotations],
         Dependency[AddDefaults],
         Dependency[Checks] )

  override def optionalPrerequisiteOf = Seq.empty

  override def invalidates(a: Phase) = false

  /** Write the input [[AnnotationSeq]] to a fie. */
  def transform(annotations: AnnotationSeq): AnnotationSeq = {
    val sopts = Viewer[StageOptions].view(annotations)
    val serializable: AnnotationSeq = annotations.flatMap{
      case _: Unserializable    => None
      case a: DeletedAnnotation => if (sopts.writeDeleted) { Some(a) } else { None }
      case a: HowToSerialize    =>
        val filename = a.filename(annotations)
        a.howToSerialize.map { str =>
          val pw = new PrintWriter(filename)
          pw.write(str)
          pw.close()
        }
        a.howToResume(filename) match {
          case Some(a) => a
          case None => None
        }
      case a => Some(a)
    }

    sopts.annotationFileOut match {
      case None =>
      case Some(file) =>
        val pw = new PrintWriter(sopts.getBuildFileName(file, Some(".anno.json")))
        pw.write(JsonProtocol.serialize(serializable))
        pw.close()
    }

    annotations
  }

}
