// See LICENSE for license details.

package firrtl.annotations.analysis

import firrtl.annotations._
import firrtl.annotations.TargetToken.{Instance, OfModule, Ref}
import firrtl.Utils.throwInternalError

import scala.collection.mutable

/**
  * Used by [[firrtl.annotations.transforms.EliminateTargetPaths]] to eliminate target paths
  * Calculates needed modifications to a circuit's module/instance hierarchy
  */
case class DuplicationHelper(existingModules: Set[String]) {
  type InstanceOfModuleMap = mutable.HashMap[Instance, OfModule]
  type ModuleHasInstanceOfModuleMap = mutable.HashMap[String, InstanceOfModuleMap]
  type DupMap = mutable.HashMap[String, ModuleHasInstanceOfModuleMap]
  private val dupMap = new DupMap()

  /**
    * Updates internal state to calculate instance hierarchy modifications so t's tokens in an instance can be
    * expressed as a tokens in a module (e.g. uniquify/duplicate the instance path in t's tokens)
    * @param t An instance-resolved component
    */
  def expandHierarchy(t: IsMember): Unit = {
    val path = if(t.isInstanceOf[IsModule]) t.asInstanceOf[IsModule].asPath else t.path
    path.reverse.tails.map { p => p.reverse }.toSeq.foreach { p =>
      duplicate(t.module, p)
    }
  }

  /**
    * Updates dupMap with how original module names map to new duplicated module names
    * @param top Root module of a component
    * @param path Path down instance hierarchy of a component
    */
  private def duplicate(top: String, path: Seq[(Instance, OfModule)]): Unit = {
    val (originalModule, instance, ofModule) = path.size match {
      case 0 => return
      case 1 => (top, path.head._1, path.head._2)
      case _ => (path(path.length - 2)._2.value, path.last._1, path.last._2)
    }
    val originalModuleToDupedModule = dupMap.getOrElseUpdate(originalModule, new ModuleHasInstanceOfModuleMap())
    val dupedModule = getModuleName(top, path.dropRight(1))
    val dupedModuleToInstances = originalModuleToDupedModule.getOrElseUpdate(dupedModule, new InstanceOfModuleMap())
    val dupedInstanceModule = getModuleName(top, path)
    dupedModuleToInstances += ((instance, OfModule(dupedInstanceModule)))

    val originalInstanceModuleToDupedModule = dupMap.getOrElseUpdate(ofModule.value, new ModuleHasInstanceOfModuleMap())
    originalInstanceModuleToDupedModule.getOrElseUpdate(dupedInstanceModule, new InstanceOfModuleMap())
  }

  /**
    * Deterministic name-creation of a duplicated module
    * @param top
    * @param path
    * @return
    */
  def getModuleName(top: String, path: Seq[(Instance, OfModule)]): String = {
    if(path.isEmpty) top else {
      val bestName = path.last._2.value + "___" + top + "_" + path.map { case (i, m) => i.value }.mkString("_")
      var idx = ""
      var counter = 0
      while(existingModules.contains(bestName + idx)) {
        counter += 1
        idx = "_" + counter.toString
      }
      bestName + idx
    }
  }

  /**
    * Return the duplicated module (formerly originalOfModule) instantiated by instance in newModule (formerly originalModule)
    * @param originalModule original encapsulating module
    * @param newModule new name of encapsulating module
    * @param instance instance name being declared in encapsulating module
    * @param originalOfModule original module being instantiated in originalModule
    * @return
    */
  def getNewOfModule(originalModule: String, newModule: String, instance: Instance, originalOfModule: OfModule): OfModule = {
    dupMap.get(originalModule) match {
      case None => // No duplication, can return originalOfModule
        originalOfModule
      case Some(newDupedModules) =>
        newDupedModules.get(newModule) match {
          case None if newModule != originalModule => throwInternalError("BAD")
          case None => // No duplication, can return originalOfModule
            originalOfModule
          case Some(newDupedModule) =>
            newDupedModule.get(instance) match {
              case None => // Not duped, can return originalOfModule
                originalOfModule
              case Some(newOfModule) =>
                newOfModule
            }
        }
    }
  }

  /**
    * Returns the names of this module's duplicated (including the original name)
    * @param module
    * @return
    */
  def getDuplicates(module: String): Set[String] = {
    dupMap.get(module).map(_.keys.toSet[String]).getOrElse(Set.empty[String]) ++ Set(module)
  }

  /**
    * Rewrites t with new module/instance hierarchy calculated after repeated calls to [[expandHierarchy]]
    * @param t A target
    * @return t rewritten, is a seq because if the t.module has been duplicated, it must now refer to multiple modules
    */
  def makePathless(t: IsMember): Seq[IsMember] = {
    val top = t.module
    val path = if(t.isInstanceOf[IsModule]) t.asInstanceOf[IsModule].asPath else t.path
    val newTops = getDuplicates(top)
    newTops.map { newTop =>
      val newPath = mutable.ArrayBuffer[TargetToken]()
      path.foldLeft((top, newTop)) { case ((originalModule, newModule), (instance, ofModule)) =>
        val newOfModule = getNewOfModule(originalModule, newModule, instance, ofModule)
        newPath ++= Seq(instance, newOfModule)
        (ofModule.value, newOfModule.value)
      }
      val module = if(newPath.nonEmpty) newPath.last.value.toString else newTop
      t.notPath match {
        case Nil => ModuleTarget(t.circuit, module)
        case Instance(i) :: OfModule(m) :: Nil => ModuleTarget(t.circuit, module)
        case Ref(r) :: components => ReferenceTarget(t.circuit, module, Nil, r, components)
      }

      //t.toGenericTarget.copy(moduleOpt = Some(module), tokens = t.notPath).complete
    }.toSeq
  }
}

