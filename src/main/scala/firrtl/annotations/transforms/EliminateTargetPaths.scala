// See LICENSE for license details.

package firrtl.annotations.transforms

import firrtl.Mappers._
import firrtl.annotations.TargetToken.{Instance, OfModule}
import firrtl.annotations.analysis.DuplicationHelper
import firrtl.annotations.{Annotation, Target}
import firrtl.ir._
import firrtl.{CircuitForm, CircuitState, HighForm, MidForm, RenameMap, Transform, WDefInstance}

import scala.collection.mutable


case class ResolvePaths(targets: Seq[Target]) extends Annotation {
  override def update(renames: RenameMap): Seq[Annotation] = {
    val newTargets = targets.flatMap(t => renames.get(t).getOrElse(Seq(t)))
    Seq(ResolvePaths(newTargets))
  }
}

class EliminateTargetPaths extends Transform {
  def inputForm: CircuitForm = HighForm
  def outputForm: CircuitForm = HighForm

  def run(cir: Circuit, targets: Seq[Target]): (Circuit, RenameMap) = {

    val dupMap = DuplicationHelper(cir.modules.map(_.name).toSet)

    targets.foreach { t => dupMap.expandHierarchy(t) }


    val usedOfModules = mutable.HashSet[String]()
    usedOfModules += cir.main
    val oldUsedOfModules = mutable.HashSet[String]()
    oldUsedOfModules += cir.main
    val duplicatedModuleList = mutable.ArrayBuffer[DefModule]()

    def onStmt(originalModule: String, newModule: String)(s: Statement): Statement = s match {
      case DefInstance(info, name, module) =>
        val ofModule = dupMap.getNewOfModule(originalModule, newModule, Instance(name), OfModule(module)).value
        usedOfModules += ofModule
        oldUsedOfModules += module
        DefInstance(info, name, ofModule)
      case WDefInstance(info, name, module, tpe) =>
        val ofModule = dupMap.getNewOfModule(originalModule, newModule, Instance(name), OfModule(module)).value
        usedOfModules += ofModule
        oldUsedOfModules += module
        WDefInstance(info, name, ofModule, tpe)
      case other => other map onStmt(originalModule, newModule)
    }


    cir.modules.foreach { m =>
      dupMap.getDuplicates(m.name).foreach { newName =>
        val newM = m match {
          case e: ExtModule => e.copy(name = newName)
          case o: Module => o.copy(name = newName, body = onStmt(m.name, newName)(o.body))
        }
        duplicatedModuleList += newM
      }
    }


    val finalModuleList = duplicatedModuleList.filter(m =>
      usedOfModules.contains(m.name) || (!usedOfModules.contains(m.name) && !oldUsedOfModules.contains(m.name))
    )
    val renameMap = RenameMap()

    targets.foreach { t =>
      val newTs = dupMap.makePathless(t).filter(c => usedOfModules.contains(c.moduleOpt.get))
      renameMap.rename(t, newTs)
    }


    (cir.copy(modules = finalModuleList), renameMap)
  }

  override protected def execute(state: CircuitState): CircuitState = {

    val annotations = state.annotations.collect { case a: ResolvePaths => a }

    val targets = annotations.flatMap(_.targets)

    val (newCircuit, renameMap) = run(state.circuit, targets)

    state.copy(circuit = newCircuit, renames = Some(renameMap))

  }
}
