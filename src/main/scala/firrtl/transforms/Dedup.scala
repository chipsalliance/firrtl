// See LICENSE for license details.

package firrtl
package transforms

import firrtl.ir._
import firrtl.Mappers._
import firrtl.analyses.InstanceGraph
import firrtl.annotations.SubComponent.{Instance, OfModule, Ref}
import firrtl.annotations._
import firrtl.annotations.transforms.AutoResolution
import firrtl.passes.{InferTypes, MemPortUtils}
import firrtl.Utils.throwInternalError

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
    val (newC, renameMap) = run(state.circuit, noDedups, state.annotations.collect { case a: AutoResolution => a})
    state.copy(circuit = newC, renames = Some(renameMap))
  }

  /**
    * Deduplicates a circuit, and records renaming
    * @param c Circuit to dedup
    * @param noDedups Modules not to dedup
    * @return Deduped Circuit and corresponding RenameMap
    */
  def run(c: Circuit, noDedups: Seq[String], annos: Seq[AutoResolution]): (Circuit, RenameMap) = {

    // RenameMap
    val renameMap = RenameMap()
    renameMap.setCircuit(c.main)

    // Maps module name to corresponding dedup module
    val dedupMap = DedupModules.deduplicate(c, noDedups.toSet, annos, renameMap)

    /*
    Time to think about how dedup works
    Example 1:
    A_ -> A
    Annos: A_, A
    Should not dedup

    Depends on Annotation type. If A_ and A share the exact same annotation, then is it ok to dedup?
    Wire(A_/a, A_/b)
    Wire(A/a, A/b) ok

    Wire(A_/a, A_/b:B_/c)
    Wire(A/a, A/b:B/c) ok
    * */

    // Use old module list to preserve ordering
    val dedupedModules = c.modules.map(m => dedupMap(m.name)).distinct

    val cname = CircuitName(c.main)
    val map = dedupMap.map { case (from, to) =>
        logger.debug(s"[Dedup] $from -> ${to.name}")
        ModuleName(from, cname) -> List(ModuleName(to.name, cname))
      }
    renameMap.addMap(map.map{ case (k: ModuleName, v: List[ModuleName]) => Component.convertNamed2Component(k) -> v.map(Component.convertNamed2Component)})

    (InferTypes.run(c.copy(modules = dedupedModules)), renameMap)
  }
}

/**
  * Utility functions for [[DedupModules]]
  */
object DedupModules {
  /**
    * Change's a module's internal signal names, types, infos, and modules.
    * @param rename Function to rename a signal. Called on declaration and references.
    * @param retype Function to retype a signal. Called on declaration, references, and subfields
    * @param reinfo Function to re-info a statement
    * @param renameOfModule Function to rename an instance's module
    * @param module Module to change internals
    * @return Changed Module
    */
  def changeInternals(rename: String=>String,
                      retype: String=>Type=>Type,
                      reinfo: Info=>Info,
                      renameOfModule: (String, String)=>String
                     )(module: DefModule): DefModule = {
    def onPort(p: Port): Port = Port(reinfo(p.info), rename(p.name), p.direction, retype(p.name)(p.tpe))
    def onExp(e: Expression): Expression = e match {
      case WRef(n, t, k, g) => WRef(rename(n), retype(n)(t), k, g)
      case WSubField(expr, n, tpe, kind) =>
        val fieldIndex = expr.tpe.asInstanceOf[BundleType].fields.indexWhere(f => f.name == n)
        val newExpr = onExp(expr)
        val newField = newExpr.tpe.asInstanceOf[BundleType].fields(fieldIndex)
        val finalExpr = WSubField(newExpr, newField.name, newField.tpe, kind)
        //TODO: renameMap.rename(e.serialize, finalExpr.serialize)
        finalExpr
      case other => other map onExp
    }
    def onStmt(s: Statement): Statement = s match {
      case WDefInstance(i, n, m, t) =>
        val newmod = renameOfModule(n, m)
        WDefInstance(reinfo(i), rename(n), newmod, retype(n)(t))
      case DefInstance(i, n, m) => DefInstance(reinfo(i), rename(n), renameOfModule(n, m))
      case d: DefMemory =>
        val oldType = MemPortUtils.memType(d)
        val newType = retype(d.name)(oldType)
        val index = oldType
          .asInstanceOf[BundleType].fields.headOption
          .map(_.tpe.asInstanceOf[BundleType].fields.indexWhere(
            {
              case Field("data" | "wdata" | "rdata", _, _) => true
              case _ => false
            }))
        val newDataType = index match {
          case Some(i) =>
            //If index nonempty, then there exists a port
            newType.asInstanceOf[BundleType].fields.head.tpe.asInstanceOf[BundleType].fields(i).tpe
          case None =>
            //If index is empty, this mem has no ports, and so we don't need to record the dataType
            // Thus, call retype with an illegal name, so we can retype the memory's datatype, but not
            // associate it with the type of the memory (as the memory type is different than the datatype)
            retype(d.name + ";&*^$")(d.dataType)
        }
        d.copy(dataType = newDataType) map rename map reinfo
      case h: IsDeclaration => h map rename map retype(h.name) map onExp map reinfo
      case other => other map reinfo map onExp map onStmt
    }
    module map onPort map onStmt
  }

  def uniquifyField(ref: String, depth: Int, field: String): String = ref+depth+field

  /**
    * Turns a module into a name-agnostic module
    * @param module module to change
    * @return name-agnostic module
    */
  def agnostify(top: Component,
                module: DefModule,
                renameMap: RenameMap
               ): DefModule = {


    val namespace = Namespace()
    val typeMap = mutable.HashMap[String, Type]()

    renameMap.setCircuit(top.circuit.get)
    renameMap.setModule(module.name)

    def rename(name: String): String = {
      val ret = renameMap.get(top.module(module.name).ref(name))
      ret match {
        case Some(Seq(Component(_, _, Seq(Ref(x))))) => x
        case None =>
          val newName = namespace.newTemp
          renameMap.rename(name, newName)
          newName
        case other => throwInternalError(other.toString)
      }
    }

    def retype(name: String)(tpe: Type): Type = {
      if (typeMap.contains(name)) typeMap(name) else {
        def onType(depth: Int)(tpe: Type): Type = tpe map onType(depth+1) match {
          //TODO bugfix: ref.data.data and ref.datax.data will not rename to the right tags, even if they should be
          case BundleType(fields) => BundleType(fields.map(f => Field(rename(uniquifyField(name, depth, f.name)), f.flip, f.tpe)))
          case other => other
        }
        val newType = onType(0)(tpe)
        typeMap(name) = newType
        newType
      }
    }

    def reOfModule(instance: String, ofModule: String): String = {
      renameMap.get(top.module(ofModule)) match {
        case Some(Seq(Component(_, Some(ofModuleTag), Nil))) => ofModuleTag
        case None => ofModule
        case other => throwInternalError(other.toString)
      }
    }

    val renamedModule = changeInternals(rename, retype, {i: Info => NoInfo}, reOfModule)(module)
    renamedModule
  }

  /** Dedup a module's instances based on dedup map
    *
    * Will fixes up module if deduped instance's ports are differently named
    *
    * @param moduleName Module name who's instances will be deduped
    * @param moduleMap Map of module name to its original module
    * @param name2name Map of module name to the module deduping it. Not mutated in this function.
    * @param renameMap Will be modified to keep track of renames in this function
    * @return fixed up module deduped instances
    */
  def dedupInstances(top: Component, originalModule: String, moduleMap: Map[String, DefModule], name2name: Map[String, String], renameMap: RenameMap): DefModule = {
    val module = moduleMap(originalModule)

    // If black box, return it (it has no instances)
    if (module.isInstanceOf[ExtModule]) return module


    // Get all instances to know what to rename in the module
    val instances = mutable.Set[WDefInstance]()
    InstanceGraph.collectInstances(instances)(module.asInstanceOf[Module].body)
    val instanceModuleMap = instances.map(i => i.name -> i.module).toMap
    val moduleNames = instances.map(_.module)

    def getNewModule(old: String): DefModule = {
      moduleMap(name2name(old))
    }
    // Define rename functions
    def renameOfModule(instance: String, ofModule: String): String = {
      val newOfModule = name2name(ofModule)
      renameMap.rename(top.module(originalModule).inst(instance).of(ofModule),top.module(originalModule).inst(instance).of(newOfModule))
      newOfModule
    }
    val typeMap = mutable.HashMap[String, Type]()
    def retype(name: String)(tpe: Type): Type = {
      if (typeMap.contains(name)) typeMap(name) else {
        if (instanceModuleMap.contains(name)) {
          val newType = Utils.module_type(getNewModule(instanceModuleMap(name)))
          typeMap(name) = newType
          getAffectedExpressions(WRef(name, tpe)).zip(getAffectedExpressions(WRef(name, newType))).foreach {
            case (old, nuu) => renameMap.rename(old.serialize, nuu.serialize)
          }
          newType
        } else tpe
      }
    }

    renameMap.setModule(module.name)
    // Change module internals
    changeInternals({n => n}, retype, {i => i}, renameOfModule)(module)
  }

  def buildRTLTags(tag2all: mutable.HashMap[String, mutable.HashSet[String]])
                  (top: Component,
                   moduleLinearization: Seq[DefModule],
                   noDedups: Set[String],
                   annotations: Seq[AutoResolution],
                   renameMap: RenameMap): Unit = {
    // Build tag2all, name2tag, name2sourceTags

    val module2Annotations = mutable.HashMap.empty[String, mutable.HashSet[AutoResolution]]
    annotations.foreach { a =>
      a.targets.foreach { t =>
        val annos = module2Annotations.getOrElseUpdate(t.module.get, mutable.HashSet.empty[AutoResolution])
        annos += a
      }
    }

    val agnosticRename = RenameMap()

    moduleLinearization.foreach { originalModule =>

      // Replace instance references to new deduped modules
      val dontcare = RenameMap()
      dontcare.setCircuit("dontcare")
      //val fixedModule = DedupModules.dedupInstances(originalModule, tag2module, name2tag, name2module, dontcare)


      if (noDedups.contains(originalModule.name)) {
        // Don't dedup. Set dedup module to be the same as fixed module
        tag2all(originalModule.name) = mutable.HashSet(originalModule.name)
        //templateModules += originalModule.name
      } else { // Try to dedup

        // Build name-agnostic module
        val agnosticModule = DedupModules.agnostify(top, originalModule, agnosticRename)
        agnosticRename.rename(top.module(originalModule.name), top.module("thisModule"))
        val agnosticAnnos = module2Annotations.getOrElse(originalModule.name, mutable.HashSet.empty[AutoResolution]).map(_.update(agnosticRename))
        agnosticRename.delete(top.module(originalModule.name))

        // Build tag
        val builder = new mutable.ArrayBuffer[Any]()
        agnosticModule.ports.foreach {
          builder ++= _.serialize
        }

        builder ++= agnosticAnnos

        agnosticModule match {
          case Module(i, n, ps, b) =>
            builder ++= b.serialize
          case ExtModule(i, n, ps, dn, p) =>
            builder ++= dn
            p.foreach {
              builder ++= _.serialize
            }
        }
        val tag = builder.hashCode().toString

        // Match old module name to its tag
        agnosticRename.rename(top.module(originalModule.name), top.module(tag))
        renameMap.rename(top.module(originalModule.name), top.module(tag))

        // Set tag's module to be the first matching module
        val all = tag2all.getOrElseUpdate(tag, mutable.HashSet.empty[String])
        all += originalModule.name
      }
    }
  }

  /**
    * Deduplicate
    * @param circuit Circuit
    * @param noDedups list of modules to not dedup
    * @param renameMap rename map to populate when deduping
    * @return Map of original Module name -> Deduped Module
    */
  def deduplicate(circuit: Circuit,
                  noDedups: Set[String],
                  annotations: Seq[AutoResolution],
                  renameMap: RenameMap): Map[String, DefModule] = {


    val multiTargetMultiModules =
      annotations.collect {
        case a if a.targets.nonEmpty && !a.targets.forall(_.module.get == a.targets.head.module.get) => a.targets.map(_.module.get).toSet
      }.foldLeft(Set.empty[String])((set, m) => set ++ m)

    val allNoDedups = noDedups ++ multiTargetMultiModules

    val moduleMap = circuit.modules.map(m => m.name -> m).toMap
    val moduleLinearization = new InstanceGraph(circuit).moduleOrder.map(_.name).reverse.map(moduleMap(_))

    // Maps a tag to all matching module names
    val tag2all = mutable.HashMap.empty[String, mutable.HashSet[String]]

    val tagMap = RenameMap()

    val top = Component(Some(circuit.main), None, Nil)

    buildRTLTags(tag2all)(top, moduleLinearization, allNoDedups, annotations, tagMap)

    // Set tag2name to be the best dedup module name
    val moduleIndex = circuit.modules.zipWithIndex.map{case (m, i) => m.name -> i}.toMap
    def order(l: String, r: String): String = if (moduleIndex(l) < moduleIndex(r)) l else r

    // Maps a module's tag to its deduplicated module
    val tag2name = mutable.HashMap.empty[String, String]
    tag2all.foreach { case (tag, all) => tag2name(tag) = all.reduce(order)}

    // Create map from original to dedup name
    val name2name = moduleMap.keysIterator.map{ originalModule =>
      tagMap.get(top.module(originalModule)) match {
        case Some(Seq(Component(_, Some(tag), Nil))) => originalModule -> tag2name(tag)
        case None => originalModule -> originalModule
        case other => throwInternalError(other.toString)
      }
    }.toMap

    // Build Remap for modules with deduped module references
    val dedupedName2module = tag2name.map({ case (tag, name) => name -> DedupModules.dedupInstances(top, name, moduleMap, name2name, renameMap) })

    // Build map from original name to corresponding deduped module
    val name2module = tag2all.flatMap({ case (tag, names) => names.map(n => n -> dedupedName2module(tag2name(tag))) })

    name2module.toMap
  }

  def getAffectedExpressions(root: Expression): Seq[Expression] = {
    val all = mutable.ArrayBuffer[Expression]()

    def onExp(expr: Expression): Unit = {
      expr.tpe match {
        case _: GroundType =>
        case b: BundleType => b.fields.foreach { f => onExp(WSubField(expr, f.name, f.tpe)) }
        case v: VectorType => (0 until v.size).foreach { i => onExp(WSubIndex(expr, i, v.tpe, UNKNOWNGENDER)) }
      }
      all += expr
    }

    onExp(root)
    all
  }
}
