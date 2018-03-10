// See LICENSE for license details.

package firrtl
package transforms

import firrtl.ir._
import firrtl.Mappers._
import firrtl.analyses.InstanceGraph
import firrtl.annotations._
import firrtl.passes.InferTypes

// Datastructures
import scala.collection.mutable


/** A component, e.g. register etc. Must be declared only once under the TopAnnotation
  */
case class NoDedupAnnotation(target: ModuleName) extends SingleTargetAnnotation[ModuleName] {
  def duplicate(n: ModuleName) = NoDedupAnnotation(n)
}

/** Only use on legal Firrtl.
  *
  * Specifically, the restriction of instance loops must have been checked, or else this pass can
  *  infinitely recurse
  */
class DedupModules extends Transform {
  def inputForm: CircuitForm = HighForm
  def outputForm: CircuitForm = HighForm

  /**
    * Deduplicate a Circuit
    * @param state Input Firrtl AST
    * @return A transformed Firrtl AST
    */
  def execute(state: CircuitState): CircuitState = {
    val noDedups = state.annotations.collect { case NoDedupAnnotation(ModuleName(m, c)) => m }
    val (newC, renameMap) = run(state.circuit, noDedups)
    state.copy(circuit = newC, renames = Some(renameMap))
  }

  /**
    * Deduplicates a circuit, and records renaming
    * @param c Circuit to dedup
    * @param noDedups Modules not to dedup
    * @return Deduped Circuit and corresponding RenameMap
    */
  def run(c: Circuit, noDedups: Seq[String]): (Circuit, RenameMap) = {
    // Map of module name to module
    val moduleMap = c.modules.map(m => m.name -> m).toMap

    // Order of modules, from leaf to top
    val moduleOrder = new InstanceGraph(c).moduleOrder.map(_.name).reverse

    // RenameMap
    val renameMap = RenameMap()
    renameMap.setCircuit(c.main)

    // Maps module name to corresponding dedup module
    val dedupMap = DedupModules.deduplicate(moduleOrder, moduleMap, noDedups.toSet, renameMap)

    // Use old module list to preserve ordering
    val dedupedModules = c.modules.map(m => dedupMap(m.name)).distinct

    val cname = CircuitName(c.main)
    renameMap.addMap(dedupMap.map { case (from, to) =>
      logger.debug(s"[Dedup] $from -> $to")
      ModuleName(from, cname) -> List(ModuleName(to.name, cname))
    })

    (InferTypes.run(c.copy(modules = dedupedModules)), renameMap)
  }
}

/**
  * Utility functions for [[DedupModules]]
  */
object DedupModules {
  /**
    * Change's a module's internal signal names, types, infos, and modules.
    * Populates renameMap.
    * @param rename Function to rename a signal. Called on declaration and references.
    * @param retype Function to retype a signal. Called on declaration, references, and subfields
    * @param reinfo Function to re-info a statement
    * @param renameModule Function to rename an instance's module
    * @param renameMap Updates renameMap to keep track of signal name changes
    * @param module Module to change internals
    * @return Changed Module
    */
  def changeInternals(rename: String=>String,
                      retype: String=>Type=>Type,
                      reinfo: Info=>Info,
                      renameModule: String=>String,
                      renameMap: RenameMap
                     )(module: DefModule): DefModule = {
    def onPort(p: Port): Port = Port(reinfo(p.info), rename(p.name), p.direction, retype(p.name)(p.tpe))
    def onExp(e: Expression): Expression = e match {
      case WRef(n, t, k, g) =>
        val finalExpr = WRef(rename(n), retype(n)(t), k, g)
        renameMap.rename(n, finalExpr.name)
        finalExpr
      case WSubField(expr, n, tpe, kind) =>
        val fieldIndex = expr.tpe.asInstanceOf[BundleType].fields.indexWhere(f => f.name == n)
        val newExpr = onExp(expr)
        val newField = newExpr.tpe.asInstanceOf[BundleType].fields(fieldIndex)
        val finalExpr = WSubField(newExpr, newField.name, newField.tpe, kind)
        renameMap.rename(e.serialize, finalExpr.serialize)
        finalExpr
      case other => other map onExp
    }
    def onStmt(s: Statement): Statement = s match {
      case WDefInstance(i, n, m, t) =>
        val w = WDefInstance(reinfo(i), rename(n), renameModule(m), retype(n)(t))
        renameMap.rename(n, w.name)
        w
      case DefInstance(i, n, m) =>
        val d = DefInstance(reinfo(i), rename(n), renameModule(m))
        renameMap.rename(n, d.name)
        d
      case h: IsDeclaration => h map rename map retype(h.name) map onExp map reinfo
      case other => other map reinfo map onExp map onStmt
    }
    renameMap.setModule(module.name)
    val finalModule = module match {
      case m: Module => m map onPort map onStmt
      case other => other
    }
    renameMap.setModule("")
    finalModule
  }

  /**
    * Turns a module into a name-agnostic module
    * @param module module to change
    * @param renameMap RenameMap to record signal name changes
    * @return name-agnostic module
    */
  def agnostify(module: DefModule, renameMap: RenameMap): DefModule = {
    val namespace = Namespace()
    val nameMap = mutable.HashMap[String, String]()
    val typeMap = mutable.HashMap[String, Type]()
    def rename(name: String): String = {
      if(nameMap.contains(name)) nameMap(name) else {
        val newName = namespace.newTemp
        nameMap(name) = newName
        newName
      }
    }
    def retype(name: String)(tpe: Type): Type = {
      if(typeMap.contains(name)) typeMap(name) else {
        def onType(tpe: Type): Type = tpe map onType match {
          case BundleType(fields) => BundleType(fields.map(f => Field(rename(f.name), f.flip, f.tpe)))
          case other => other
        }
        val newType = onType(tpe)
        typeMap(name) = newType
        newType
      }
    }
    changeInternals(rename, retype, {i: Info => NoInfo}, {n: String => n}, renameMap)(module)
  }

  /** Dedup a module's instances based on dedup map
    *
    * Will fixes up module if deduped instance's ports are differently named
    *
    * @param module Module who's instances will be deduped
    * @param dedupMap Map of module name to deduped module
    * @return fixed up module deduped instances
    */
  def dedupInstances(module: DefModule, dedupMap: Map[String, DefModule], renameMap: RenameMap): DefModule = {

    // If black box, return it (it has no instances)
    if(module.isInstanceOf[ExtModule]) return module


    // Get all instances to know what to rename in the module
    val instances = mutable.Set[WDefInstance]()
    InstanceGraph.collectInstances(instances)(module.asInstanceOf[Module].body)
    val instanceModuleMap = instances.map(i => i.name -> i.module).toMap
    val moduleNames = instances.map(_.module)

    // Define rename functions
    def renameModule(name: String): String = dedupMap(name).name
    val typeMap = mutable.HashMap[String, Type]()
    def retype(name: String)(tpe: Type): Type = {
      if(typeMap.contains(name)) typeMap(name) else {
        if (instanceModuleMap.contains(name)) {
          val newType = Utils.module_type(dedupMap(instanceModuleMap(name)))
          typeMap(name) = newType
          newType
        } else tpe
      }
    }

    // Change module internals
    changeInternals({n => n}, retype, {i => i}, renameModule, renameMap)(module)
  }

  /**
    * Deduplicate
    * @param moduleOrder order from leaf to top of modules
    * @param moduleMap maps original module name to original module
    * @param noDedups list of modules to not dedup
    * @return Map of original Module name -> Deduped Module
    */
  def deduplicate(moduleOrder: Seq[String],
               moduleMap: Map[String, DefModule],
               noDedups: Set[String],
               renameMap: RenameMap): Map[String, DefModule] = {

    // Maps a module's tag to its deduplicated module's name
    val tag2duplicate = mutable.HashMap.empty[String, String]

    // Maps a module's name to its deduplicate module
    val dedupMap = mutable.HashMap.empty[String, DefModule]

    // Build dedupMap
    moduleOrder.foreach { moduleName =>
      // Get original module
      val originalModule = moduleMap(moduleName)

      // Replace instance references to new deduped modules
      val fixedModule = DedupModules.dedupInstances(originalModule, dedupMap.toMap, renameMap)

      if(noDedups.contains(fixedModule.name)) {
        // Don't dedup. Set dedup module to be the same as fixed module
        dedupMap(fixedModule.name) = fixedModule
      } else { // Try to dedup

        // Build name-agnostic module
        val agnosticRenameMap = RenameMap()
        val agnosticModule = DedupModules.agnostify(fixedModule, agnosticRenameMap)

        // Build tag
        val tag = agnosticModule match {
          case Module(i, n, ps, b) =>
            ps.map(_.serialize).mkString + b.serialize
          case ExtModule(i, n, ps, dn, p) =>
            ps.map(_.serialize).mkString + dn + p.map(_.serialize).mkString
        }

        // Check deduplication
        if(tag2duplicate.contains(tag)) {
          // Set fixedModule's dedup to be the tag's dedup
          dedupMap(fixedModule.name) = dedupMap(tag2duplicate(tag))
        } else {
          // Set fixedModule's dedup to be itself
          tag2duplicate(tag) = fixedModule.name
          dedupMap(fixedModule.name) = fixedModule
        }
      }
    }

    // Return dedupMap
    dedupMap.toMap
  }

}
