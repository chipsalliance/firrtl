package firrtl.annotations.transforms

import firrtl.Mappers._
import firrtl.annotations.SubComponent.{Instance, OfModule}
import firrtl.annotations.analysis.DuplicationHelper
import firrtl.annotations.{Annotation, Component, SubComponent}
import firrtl.ir._
import firrtl.{CircuitState, MidForm, RenameMap, Transform, WDefInstance}

import scala.collection.mutable


trait AutoResolution {
  this: Annotation =>
  def targets: Seq[Component]
}


class EliminateComponentPaths extends Transform {
  def inputForm = MidForm
  def outputForm = MidForm

  def run(cir: Circuit, targets: Seq[Component]): (Circuit, RenameMap) = {

    val dupMap = DuplicationHelper()

    targets.foreach { t => dupMap.expandHierarchy(t) }


    val usedOfModules = mutable.HashSet[String]()
    usedOfModules += cir.main
    val duplicatedModuleList = mutable.ArrayBuffer[DefModule]()

    def onStmt(originalModule: String, newModule: String)(s: Statement): Statement = s match {
      case DefInstance(info, name, module) =>
        val ofModule = dupMap.getNewOfModule(originalModule, newModule, Instance(name), OfModule(module)).value
        usedOfModules += ofModule
        DefInstance(info, name, ofModule)
      case WDefInstance(info, name, module, tpe) =>
        val ofModule = dupMap.getNewOfModule(originalModule, newModule, Instance(name), OfModule(module)).value
        usedOfModules += ofModule
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


    val finalModuleList = duplicatedModuleList.filter(m => usedOfModules.contains(m.name))
    val renameMap = RenameMap()

    targets.foreach { t =>
      val newTs = dupMap.rewriteWithoutPaths(t).filter(c => usedOfModules.contains(c.module.get))
      renameMap.rename(t, newTs)
    }


    (cir.copy(modules = finalModuleList), renameMap)
  }

  override protected def execute(state: CircuitState): CircuitState = {
    import SubComponent._

    val annotations = state.annotations.collect { case a: AutoResolution => a }

    val targets = annotations.flatMap(_.targets)

    val (newCircuit, renameMap) = run(state.circuit, targets)

    state.copy(circuit = newCircuit, renames = Some(renameMap))

    //val circuit = state.circuit
    //renameMap.setCircuit(circuit.main)

    //Should first resolve instance refs, Map(originalTargets -> Seq[nonHierarchicalTargets])
    // Could be a seq if top.x1.foo and X.bar are both targets, X.bar will point to X.bar and X_.bar
    // Maps (Original -> (Duped -> Instance, Duped))


    //Then, should resolve clock, init, regs, etc. Map(nonHierarchicalTargets -> Seq[finalTarget])



    /*
    def resolveBase(comp: Component, index: Int): Seq[Component] = {
      val refs = comp.reference.slice(index, comp.reference.length)
      def module: String = {
        refs.reverse.collectFirst{ case OfModule(m) => m }.getOrElse(comp.module.get)
      }
      refs match {
        case Nil => Seq(comp)
        case Instance(i) :: OfModule(m) :: tail if instanceMap(module)(i).module == m =>
          resolveBase(comp, index + 2)
        case Ref(r) :: Nil if declaresMap(module).contains(r) =>
          Seq(comp)
        case Regs :: Nil =>
          registerMap(module).values.map { dr =>
            val pre = Component(comp.circuit, comp.module, comp.reference.dropRight(1), comp.tag)
            pre.ref(dr.name)
          }.toSeq
        case other => Nil
      }
    }

    def resolveFields(reference: Seq[SubComponent]): Seq[Component] = ???

    def resolve(component: Component): Seq[Component] = component match {
      case Component(Some(`circuitName`), Some(mod), reference, _) if moduleMap.keySet.contains(mod) =>
        resolveBase(component, 0)
      case _ => Nil
    }
    */


  }
}
