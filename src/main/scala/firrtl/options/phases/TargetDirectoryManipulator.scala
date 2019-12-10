// See LICENSE for license details.

package firrtl.options.phases

import firrtl.AnnotationSeq
import firrtl.annotations.NoTargetAnnotation
import firrtl.options.{Phase, StageOptions, TargetDirAnnotation}
import firrtl.options.Viewer._

import java.io.File

private[options] sealed trait TargetDirectoryManipulatorAnnotation extends NoTargetAnnotation

private[options] case class TargetDirectoryEnterAnnotation(dir: String) extends TargetDirectoryManipulatorAnnotation

private[options] case object TargetDirectoryExitAnnotation extends TargetDirectoryManipulatorAnnotation

class TargetDirectoryManipulator extends Phase {

  def transform(annotations: AnnotationSeq): AnnotationSeq = {
    val targetDir = new File(view[StageOptions](annotations).targetDir)
    val targetDirx = annotations.foldLeft(targetDir){ (dir, anno) =>
      anno match {
        case TargetDirectoryEnterAnnotation(dir) => new File(targetDir, dir)
        case TargetDirectoryExitAnnotation       => dir.getParentFile
        case _                                   => dir
      }
    }
    annotations.flatMap {
      case _: TargetDirectoryEnterAnnotation => None
      case TargetDirectoryExitAnnotation     => None
      case _: TargetDirAnnotation            => Some(TargetDirAnnotation(targetDirx.toString))
      case a                                 => Some(a)
    }
  }

}

class TargetDirectoryExit extends Phase {

  def transform(a: AnnotationSeq) = TargetDirectoryExitAnnotation +: a

}
