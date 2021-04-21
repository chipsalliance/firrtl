// SPDX-License-Identifier: Apache-2.0

package firrtl
package passes
package memlib

import firrtl.stage.Forms

class DumpMemoryAnnotations extends Transform with DependencyAPIMigration {

  override def prerequisites = Forms.MidForm
  override def optionalPrerequisites = Seq.empty
  override def optionalPrerequisiteOf = Forms.MidEmitters
  override def invalidates(a: Transform) = false

  def execute(state: CircuitState): CircuitState = {
    state.copy(annotations = state.annotations.flatMap {
      // convert and remove AnnotatedMemoriesCollectorAnnotation to CustomFileEmission
      case a: AnnotatedMemoriesCollectorAnnotation =>
        state.annotations.collect {
          // convert ReplSeqMemAnnotation to configurations.
          case ReplSeqMemAnnotation(_, outputConfig) => MemLibOutConfigFileAnnotation(outputConfig, a.annotatedMemories)
          // todo convert xxx to verilogs here.
        }
      // remove ReplSeqMemAnnotation
      case _: ReplSeqMemAnnotation => Nil
      case a => Seq(a)
    })
  }
}
