// See LICENSE for license details.

package firrtl.annotations.transforms

import firrtl.Mappers._
import firrtl.annotations.TargetToken.{Instance, OfModule}
import firrtl.annotations.analysis.DuplicationHelper
import firrtl.annotations.{Annotation, CompleteTarget, IsMember, Target}
import firrtl.ir._
import firrtl.{CircuitForm, CircuitState, HighForm, MidForm, RenameMap, Transform, WDefInstance}

import scala.collection.mutable


/** Group of targets that should become local targets
  * @param targets
  */
case class ResolvePaths(targets: Seq[CompleteTarget]) extends Annotation {
  override def update(renames: RenameMap): Seq[Annotation] = {
    val newTargets = targets.flatMap(t => renames.get(t).getOrElse(Seq(t)))
    Seq(ResolvePaths(newTargets))
  }
}

/** For a set of non-local targets, modify the instance/module hierarchy of the circuit such that
  * the paths in each non-local target can be removed
  *
  * In other words, if targeting a specific instance of a module, duplicate that module with a unique name
  * and instantiate the new module instead.
  *
  * Consumes [[ResolvePaths]]
  *
  * E.g. for non-local target A/b:B/c:C/d, rename the following
  * A/b:B/c:C/d -> C_/d
  * A/b:B/c:C -> B_/c:C_
  * A/b:B -> A/b:B_
  * B/x -> (B/x, B_/x) // where x is any reference in B
  * C/x -> (C/x, C_/x) // where x is any reference in C
  */
class EliminateTargetPaths extends Transform {

  def inputForm: CircuitForm = HighForm

  def outputForm: CircuitForm = HighForm

  /** Returns a modified circuit and [[RenameMap]] containing the associated target remapping
    * @param cir
    * @param targets
    * @return
    */
  def run(cir: Circuit, targets: Seq[IsMember]): (Circuit, RenameMap) = {

    val dupMap = DuplicationHelper(cir.modules.map(_.name).toSet)

    // For each target, record its path and calculate the necessary modifications to circuit
    targets.foreach { t => dupMap.expandHierarchy(t) }

    // Records original list of used ofModules
    val oldUsedOfModules = mutable.HashSet[String]()
    oldUsedOfModules += cir.main

    // Records new list of used ofModules
    val newUsedOfModules = mutable.HashSet[String]()
    newUsedOfModules += cir.main

    // Contains new list of module declarations
    val duplicatedModuleList = mutable.ArrayBuffer[DefModule]()

    /** Replaces old ofModules with new ofModules by calling dupMap methods
      *
      * Updates oldUsedOfModules, newUsedOfModules
      *
      * @param originalModule Original name of this module
      * @param newModule New name of this module
      * @param s
      * @return
      */
    def onStmt(originalModule: String, newModule: String)(s: Statement): Statement = s match {
      case DefInstance(info, name, module) =>
        val ofModule = dupMap.getNewOfModule(originalModule, newModule, Instance(name), OfModule(module)).value
        newUsedOfModules += ofModule
        oldUsedOfModules += module
        DefInstance(info, name, ofModule)
      case WDefInstance(info, name, module, tpe) =>
        val ofModule = dupMap.getNewOfModule(originalModule, newModule, Instance(name), OfModule(module)).value
        newUsedOfModules += ofModule
        oldUsedOfModules += module
        WDefInstance(info, name, ofModule, tpe)
      case other => other map onStmt(originalModule, newModule)
    }


    // Foreach module, calculate the unique names of its duplicates
    // Then, update the ofModules of instances that it encapsulates
    cir.modules.foreach { m =>
      dupMap.getDuplicates(m.name).foreach { newName =>
        val newM = m match {
          case e: ExtModule => e.copy(name = newName)
          case o: Module => o.copy(name = newName, body = onStmt(m.name, newName)(o.body))
        }
        duplicatedModuleList += newM
      }
    }

    // Calculate the final module list
    // A module is in the final list if:
    // 1) it is a module that is instantiated (new or old)
    // 2) it is an old module that was not instantiated and is still not instantiated
    val finalModuleList = duplicatedModuleList.filter(m =>
      newUsedOfModules.contains(m.name) || (!newUsedOfModules.contains(m.name) && !oldUsedOfModules.contains(m.name))
    )

    // Records how targets have been renamed
    val renameMap = RenameMap()

    // Foreach target, calculate the pathless version
    // Only rename targets that are instantiated
    targets.foreach { t =>
      val newTs = dupMap.makePathless(t).filter(c => newUsedOfModules.contains(c.moduleOpt.get))
      renameMap.rename(t, newTs)
    }

    // Return modified circuit and associated renameMap
    (cir.copy(modules = finalModuleList), renameMap)
  }

  override protected def execute(state: CircuitState): CircuitState = {

    val annotations = state.annotations.collect { case a: ResolvePaths => a }

    // Collect targets that are not local
    val targets = annotations.flatMap(_.targets.collect { case x: IsMember if !x.isLocal => x })

    val (newCircuit, renameMap) = run(state.circuit, targets)

    state.copy(circuit = newCircuit, renames = Some(renameMap))
  }
}
