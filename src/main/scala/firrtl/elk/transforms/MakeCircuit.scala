// SPDX-License-Identifier: Apache-2.0

package firrtl.elk.transforms

import firrtl.elk.ElkTopModuleAnnotation
import firrtl.options.{Dependency, TargetDirAnnotation}
import firrtl.stage.Forms
import firrtl.{passes, CircuitState, DependencyAPIMigration, Transform}

import java.io.File
import scala.collection.mutable

class MakeCircuit extends Transform with DependencyAPIMigration {

  override def prerequisites = Forms.LowFormOptimized ++ Seq(
    Dependency(passes.RemoveEmpty)
  )

  override def optionalPrerequisites = Seq.empty

  override def optionalPrerequisiteOf = Seq.empty

  override def invalidates(a: Transform) = false

  /**
    * Creates a series of elk graphs starting with the startModule and continuing
    * through all descendant sub-modules.
    * @param state the state to be diagrammed
    * @return
    */

  override def execute(state: CircuitState): CircuitState = {

    val startModuleName = state.annotations.collectFirst {
      case ElkTopModuleAnnotation(moduleName) => moduleName
    }.getOrElse(state.circuit.main)

    var targetDir = state.annotations.collectFirst { case TargetDirAnnotation(dir) => dir }.get.stripSuffix("/")
    if (targetDir != ".") {
      val p = new File(targetDir)
      if (!p.exists()) {
        p.mkdirs()
      }
      targetDir = p.getAbsolutePath
    }

    val queue = new mutable.Queue[String]()
    val modulesSeen = new mutable.HashSet[String]()

    queue += startModuleName // set top level of diagram tree

    while (queue.nonEmpty) {
      val moduleName = queue.dequeue()
      if (!modulesSeen.contains(moduleName)) {

        val updatedAnnotations = {
          state.annotations.filterNot { x =>
            x.isInstanceOf[ElkTopModuleAnnotation]
          } :+ ElkTopModuleAnnotation(moduleName)
        }
        val stateToDiagram = CircuitState(state.circuit, state.form, updatedAnnotations)

        val transform = new MakeModule(targetDir)
        transform.execute(stateToDiagram)

        queue ++= transform.subModulesFound.map(module => module.name)
      }
      modulesSeen += moduleName
    }

    state
  }
}
