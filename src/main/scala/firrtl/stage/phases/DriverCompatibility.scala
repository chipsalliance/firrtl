// See LICENSE for license details.

package firrtl.stage.phases

import firrtl.stage._

import firrtl.{AnnotationSeq, EmitAllModulesAnnotation, EmitCircuitAnnotation, Parser}
import firrtl.annotations.NoTargetAnnotation
import firrtl.proto.FromProto
import firrtl.options.{InputAnnotationFileAnnotation, Phase, StageOptions, StageUtils}
import firrtl.options.Viewer

import java.io.File

/** Provides compatibility methods to replicate deprecated [[Driver]] semantics.
  *
  * At a high level, the [[Driver]] tries extremely hard to figure out what the user meant and to enable them to not be
  * explicit with command line options. As an example, the `--top-name` option is not used for any FIRRTL top module
  * determination, but to find a FIRRTL file by that name and/or an annotation file by that name. This mode of file
  * discovery is only used if no explicit FIRRTL file/source/circuit and/or annotation file is given. Going further, the
  * `--top-name` argument is implicitly specified by the `main` of an input circuit if not explicit and can be used to
  * derive an annotation file. Summarily, the [[firrtl.options.Phase Phase]]s provided by this enable this type of
  * resolution.
  *
  * '''Only use these methods if you are intending to replicate old [[Driver]] semantics for a good reason.'''
  * Otherwise, opt for more explicit specification by the user.
  */
object DriverCompatibility {
  /** Holds the name of the top (main) module in an input circuit
    * @param value top module name
    */
  case class TopNameAnnotation(topName: String) extends NoTargetAnnotation

  /** Indicates that the implicit emitter, derived from a [[CompilerAnnotation]] should be an [[EmitAllModulesAnnotation]]
    * as opposed to an [[EmitCircuitAnnotation]].
    */
  private [firrtl] case object EmitOneFilePerModuleAnnotation extends NoTargetAnnotation

  /** Indicates that FIRRTL was called using the [[Driver]] compatibility mode
    *
    * If we're in compatibility mode, error handling and reporting may change (e.g., usage messages for the
    * [[FirrtlStage]] should not be reported). This enables communication to downstream annotation consumers that this was
    * created using the [[Driver]] compatibility layer.
    * @todo This is an interesting API and could be expanded into some kind of "stack trace" annotation or "stage history"
    * annotation. There may be some utility in tracking the path that a set of annotations took through the compiler.
    */
  private [phases] case object DriverCompatibilityAnnotation extends NoTargetAnnotation

  /** Determine the top name using the following precedence (highest to lowest):
    *  - Explicitly from a [[TopNameAnnotation]]
    *  - Implicitly from the top module ([[firrtl.ir.Circuit.main main]]) of a [[FirrtlCircuitAnnotation]]
    *  - Implicitly from the top module ([[firrtl.ir.Circuit.main main]]) of a [[FirrtlSourceAnnotation]]
    *  - Implicitly from the top module ([[firrtl.ir.Circuit.main main]]) of a [[FirrtlFileAnnotation]]
    *
    * @param annotations annotations to extract topName from
    * @return the top module ''if it can be determined''
    */
  private def topName(annotations: AnnotationSeq): Option[String] =
    annotations.collectFirst{ case TopNameAnnotation(n) => n }.orElse(
      annotations.collectFirst{ case FirrtlCircuitAnnotation(c) => c.main }.orElse(
        annotations.collectFirst{ case FirrtlSourceAnnotation(s) => Parser.parse(s).main }.orElse(
          annotations.collectFirst{ case FirrtlFileAnnotation(f) =>
            FirrtlStageUtils.getFileExtension(f) match {
              case ProtoBufFile => FromProto.fromFile(f).main
              case FirrtlFile   => Parser.parse(io.Source.fromFile(f).getLines().mkString("\n")).main } } )))

  /** Determine the target directory with the following precedence (highest to lowest):
    *  - Explicitly from the user-specified [[firrtl.options.TargetDirAnnotation TargetDirAnnotation]]
    *  - Implicitly from the default of [[firrtl.options.StageOptions.targetDir StageOptions.targetDir]]
    *
    * @param annotations input annotations to extract targetDir from
    * @return the target directory
    */
  private def targetDir(annotations: AnnotationSeq): String = Viewer.view[StageOptions](annotations).targetDir

  /** Add an implicit annotation file derived from the determined top name of the circuit if no
    * [[firrtl.options.InputAnnotationFileAnnotation InputAnnotationFileAnnotation]] is present.
    *
    * The implicit annotation file is determined through the following complicated semantics:
    *   - If an [[InputAnnotationFileAnnotation]] already exists, then nothing is modified
    *   - If the derived topName (the `main` in a [[firrtl.ir.Circuit Circuit]]) is ''discernable'' (see below) and a
    *     file called `topName.anno` (exactly, not `topName.anno.json`) exists, then this will add an
    *     [[InputAnnotationFileAnnotation]] using that `topName.anno`
    *   - If any of this doesn't work, then the the [[AnnotationSeq]] is unmodified
    *
    * The precedence for determining the `topName` is the following (first one wins):
    *   - The `topName` in a [[TopNameAnnotation]]
    *   - The `main` [[FirrtlCircuitAnnotation]]
    *   - The `main` in a parsed [[FirrtlSourceAnnotation]]
    *   - The `main` in the first [[FirrtlFileAnnotation]] using either ProtoBuf or parsing as determined by file
    *     extension
    *
    * @param annos input annotations
    * @return output annotations
    */
  private [firrtl] object AddImplicitAnnotationFile extends Phase {

    /** Try to add an [[InputAnnotationFileAnnotation]] implicitly specified by an [[AnnotationSeq]]. */
    def transform(annotations: AnnotationSeq): AnnotationSeq = {
      val annotationsx: AnnotationSeq = annotations
        .collectFirst{ case a: InputAnnotationFileAnnotation => a } match {
          case Some(_) => annotations
          case None => topName(annotations) match {
            case Some(n) =>
              val filename = targetDir(annotations) + "/" + n + ".anno"
              if (new File(filename).exists) {
                StageUtils.dramaticWarning(
                  s"Implicit reading of the annotation file is deprecated! Use an explict --annotation-file argument.")
                annotations :+ InputAnnotationFileAnnotation(filename)
              } else {
                annotations
              }
            case None => annotations
          } }

      DriverCompatibilityAnnotation +: annotationsx
    }

  }

  /** Add a [[FirrtlFileAnnotation]] if no annotation that explictly defines a circuit exists.
    *
    * This takes the option with the following precedence:
    *  - If an annotation subclassing [[CircuitOption]] exists, do nothing
    *  - If a [[TopNameAnnotation]] exists, use that to derive a [[FirrtlFileAnnotation]] and append it
    *  - Do nothing
    *
    * In the case of (3) above, this [[AnnotationSeq]] is likely insufficient for FIRRTL to work with (no circuit was
    * passed). However, instead of catching this here, we rely on [[Checks]] to validate the annotations.
    *
    * @param annotations input annotations
    * @return
    */
  private [firrtl] object AddImplicitFirrtlFile extends Phase {

    /** Try to add a [[FirrtlFileAnnotation]] implicitly specified by an [[AnnotationSeq]]. */
    def transform(annotations: AnnotationSeq): AnnotationSeq = {
      val circuit = annotations.collectFirst { case a @ (_: CircuitOption | _: FirrtlCircuitAnnotation) => a }
      val main = annotations.collectFirst { case a: TopNameAnnotation => a.topName }

      val annotationsx: AnnotationSeq = if (circuit.nonEmpty) {
        annotations
      } else if (main.nonEmpty) {
        StageUtils.dramaticWarning(
          s"Implicit reading of the input file is deprecated! Use an explict --input-file argument.")
        FirrtlFileAnnotation(Viewer.view[StageOptions](annotations).getBuildFileName(s"${main.get}.fir")) +: annotations
      } else {
        annotations
      }

      DriverCompatibilityAnnotation +: annotationsx
    }
  }

  /** Adds an [[EmitAnnotation]] for each [[CompilerAnnotation]].
    *
    * If an [[EmitOneFilePerModuleAnnotation]] exists, then this will add an [[EmitAllModulesAnnotation]]. Otherwise,
    * this adds an [[EmitCircuitAnnotation]]. This replicates old behavior where specifying a compiler automatically
    * meant that an emitter would also run.
    */
  object AddImplicitEmitter extends Phase {

    /** Add one [[EmitAnnotation]] foreach [[CompilerAnnotation]]. */
    def transform(annotations: AnnotationSeq): AnnotationSeq = {
      val splitModules = annotations.collectFirst{ case a: EmitOneFilePerModuleAnnotation.type => a }.isDefined

      val annotationsx = annotations.flatMap {
        case a @ CompilerAnnotation(c) =>
          if (splitModules) { Seq(a, EmitAllModulesAnnotation(c.emitter.getClass)) }
          else              { Seq(a, EmitCircuitAnnotation   (c.emitter.getClass)) }
        case a => Seq(a)
      }

      DriverCompatibilityAnnotation +: annotationsx
    }

  }

  /** Adds an [[OutputFileAnnotation]] derived from a [[TopNameAnnotation]] if no [[OutputFileAnnotation]] already
    * exists. If no [[TopNameAnnotation]] exists, then no [[OutputFileAnnotation]] is added.
    */
  object AddImplicitOutputFile extends Phase {

    /** Add an [[OutputFileAnnotation]] derived from a [[TopNameAnnotation]] if needed. */
    def transform(annotations: AnnotationSeq): AnnotationSeq = {
      val hasOutputFile = annotations
        .collectFirst{ case a @(_: EmitOneFilePerModuleAnnotation.type | _: OutputFileAnnotation) => a }
        .isDefined
      val top = topName(annotations)

      val annotationsx: AnnotationSeq =
        if (!hasOutputFile && top.isDefined) {
          OutputFileAnnotation(top.get) +: annotations
        } else {
          annotations
        }

      DriverCompatibilityAnnotation +: annotationsx
    }

  }

}
