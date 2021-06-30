// SPDX-License-Identifier: Apache-2.0

package firrtl
package transforms

import firrtl.ir._
import firrtl.Mappers._
import firrtl.options.Dependency
import firrtl.Utils.BoolType
import firrtl.annotations.Annotation
import scala.collection.mutable.Buffer
import firrtl.annotations.MemoryFileInlineAnnotation
import firrtl.passes.PassException
import firrtl.annotations.ReferenceTarget
import firrtl.annotations._

object DedupAnnotationsTransform {

  final class DifferingModuleAnnotationsException private (msg: String) extends PassException(msg)
  object DifferingModuleAnnotationsException {
    def apply(left: ReferenceTarget, right: ReferenceTarget): DifferingModuleAnnotationsException = {
      val msg = s"${left.serialize} and ${right.serialize} have differing module binaries"
      new DifferingModuleAnnotationsException(msg)
    }
  }

  private def annotationBelongsInGroup(annotationIn: Annotation, group: Seq[Annotation]): Boolean = {
    // Assume that all existing annotations in the group are 'equal' to each other (targets refer to same module),
    // and the condition to add annotations to the group is only the result of this function.

    // Then the newly added annotation simply needs to be compared to the head

    group.headOption match {
      case None => true // Base case: empty partition
      case Some(anno) => {
        (annotationIn, anno) match {
          // TODO: All of this can be folded into the annotation types
          // May also apply to generic annotations: add *something* to the Annotation trait class
          // that would look like left.compare(right), which would return true if both non-local
          // annotations will deduplicate into the same local annotation
          case (left: MemoryRandomInitAnnotation, right: MemoryRandomInitAnnotation) =>
            left.target.encapsulatingModule.equals(right.target.encapsulatingModule) &&
              left.target.ref.equals(right.target.ref)
          case (left: MemoryScalarInitAnnotation, right: MemoryScalarInitAnnotation) =>
            val comparison = left.target.encapsulatingModule.equals(right.target.encapsulatingModule) &&
              left.target.ref.equals(right.target.ref)

            if (comparison && !left.value.equals(right.value))
              // Error - deduplicated mem modules contain different scalar init values.
              throw DifferingModuleAnnotationsException(left.target, right.target)

            comparison
          case (left: MemoryArrayInitAnnotation, right: MemoryArrayInitAnnotation) =>
            val comparison = left.target.encapsulatingModule.equals(right.target.encapsulatingModule) &&
              left.target.ref.equals(right.target.ref)

            if (comparison && !left.values.equals(right.values))
              // Error - deduplicated mem modules contain different array init values.
              throw DifferingModuleAnnotationsException(left.target, right.target)

            comparison
          case (left: MemoryFileInlineAnnotation, right: MemoryFileInlineAnnotation) =>
            val comparison = left.target.encapsulatingModule.equals(right.target.encapsulatingModule) &&
              left.target.ref.equals(right.target.ref)

            if (comparison && !left.hexOrBinary.equals(right.hexOrBinary))
              // Error - deduplicated modules contain different binaries (should be the same).
              throw DifferingModuleAnnotationsException(left.target, right.target)

            comparison
          case (_, _) => false
        }
      }
    }
  }

  def dedupAnnotations(annotations: Seq[Annotation]): Seq[Annotation] = {
    // Used to check all 'similar' annotations for deduplication. If two annotations
    // would deduplicate to each other (have the same encapsulating module, and the same reference target),
    // but otherwise have differing contents, then this would result in an error.
    var annotationCheckGroups: Seq[Seq[Annotation]] = Seq(Seq())

    // For every annotation, search for an appropriate group to append it to.
    annotations.map(annotation => {
      // If we previously added an annotation to the empty group, append a new empty group to the list
      if (annotationCheckGroups.tail.nonEmpty) annotationCheckGroups = annotationCheckGroups :+ Seq()

      // Append the annotation to the first matching group (this can be the empty group itself)
      annotationCheckGroups = annotationCheckGroups.map(group =>
        if (annotationBelongsInGroup(annotation, group)) group :+ annotation else group
      )

      // If the above statement executes with no issues, then we can safely dedup this annotation
      annotation match {
        // TODO: Like the comparison code, this can likely be folded into the Annotation type
        // as something like annotation.dedup
        case memoryAnnotation: MemoryFileInlineAnnotation =>
          memoryAnnotation.duplicate(
            ReferenceTarget(
              memoryAnnotation.target.circuit,
              memoryAnnotation.target.encapsulatingModule,
              Seq(),
              memoryAnnotation.target.ref,
              memoryAnnotation.target.component
            )
          )
        case other => other
      }
    })
  }
}

/** Deduplicates memory annotations
  */
class DedupAnnotationsTransform extends Transform with DependencyAPIMigration {

  override def prerequisites = firrtl.stage.Forms.LowForm

  override def optionalPrerequisites = Nil

  override def optionalPrerequisiteOf = firrtl.stage.Forms.BackendEmitters

  override def invalidates(a: Transform) = false

  def execute(state: CircuitState): CircuitState = CircuitState(
    state.circuit,
    state.form,
    DedupAnnotationsTransform.dedupAnnotations(state.annotations.underlying),
    state.renames
  )
}
