// See LICENSE for license details.

package firrtl.annotations.analysis

import firrtl.annotations.{Target, TargetToken}
import firrtl.annotations.TargetToken.{Instance, OfModule}
import firrtl.Utils.throwInternalError

import scala.collection.mutable

/**
  * Used by [[firrtl.annotations.transforms.EliminateComponentPaths]] to eliminate component paths
  * Calculates needed modifications to a circuit's module/instance hierarchy
  */
case class DuplicationHelper(existingModules: Set[String]) {
  type InstanceOfModuleMap = mutable.HashMap[Instance, OfModule]
  type ModuleHasInstanceOfModuleMap = mutable.HashMap[String, InstanceOfModuleMap]
  type DupMap = mutable.HashMap[String, ModuleHasInstanceOfModuleMap]
  private val dupMap = new DupMap()

  /**
    * Updates internal state to calculate instance hierarchy modifications so c's reference in an instance can be
    * expressed as a reference in a module (e.g. uniquify/duplicate the instance path in c's reference)
    * @param c An instance-resolved component
    */
  def expandHierarchy(c: Target): Unit = {
    require(c.circuit.isDefined)
    require(c.module.isDefined)
    c.growingPath.foreach { p =>
      duplicate(c.module.get, p)
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
    * Rewrites c with new module/instance hierarchy calculated after repeated calls to [[expandHierarchy]]
    * @param c A component
    * @return c rewritten, is a seq because if the c.module has been duplicated, it must now refer to multiple modules
    */
  def makePathless(c: Target): Seq[Target] = {
    val top = c.module.get
    val path = c.path
    val newTops = getDuplicates(top)
    newTops.map { newTop =>
      val newPath = mutable.ArrayBuffer[TargetToken]()
      path.foldLeft((top, newTop)) { case ((originalModule, newModule), (instance, ofModule)) =>
        val newOfModule = getNewOfModule(originalModule, newModule, instance, ofModule)
        newPath ++= Seq(instance, newOfModule)
        (ofModule.value, newOfModule.value)
      }
      val module = if(newPath.nonEmpty) newPath.last.value.toString else newTop
      c.copy(module = Some(module), reference = c.notPath)
    }.toSeq
  }
}

