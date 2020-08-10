// See LICENSE for license details.

package firrtl.options.phases

import firrtl.AnnotationSeq
import firrtl.annotations.{DeletedAnnotation, JsonProtocol}
import firrtl.options.{CustomFileEmission, Dependency, Phase, StageOptions, Unserializable, Viewer}

import java.io.{BufferedWriter, FileWriter, PrintWriter}

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
    val serializable: AnnotationSeq = annotations.toSeq.flatMap {
      case _: Unserializable     => None
      case a: DeletedAnnotation  => if (sopts.writeDeleted) { Some(a) } else { None }
      case a: CustomFileEmission =>
        val filename = a.filename(annotations)
        a.toBytes.map { str =>
          val w = new BufferedWriter(new FileWriter(filename))
          str.foreach( w.write(_) )
          w.close()
        }
        a.replacements(filename)
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
