// See LICENSE for license details.

package firrtl

import annotations._
import firrtl.RenameMap.{CircularRenameException, IllegalRenameException}
import firrtl.annotations.TargetToken.{Field, Index, Instance, OfModule}

import scala.collection.mutable

object RenameMap {
  @deprecated("Use create with CompleteTarget instead, this will be removed in 1.3", "1.2")
  def apply(map: collection.Map[Named, Seq[Named]]): RenameMap = {
    val rm = new RenameMap
    rm.addMap(map)
    rm
  }

  def create(map: collection.Map[CompleteTarget, Seq[CompleteTarget]]): RenameMap = {
    val rm = new RenameMap
    rm.recordAll(map)
    rm
  }

  def apply(): RenameMap = new RenameMap

  abstract class RenameTargetException(reason: String) extends Exception(reason)
  case class IllegalRenameException(reason: String) extends RenameTargetException(reason)
  case class CircularRenameException(reason: String) extends RenameTargetException(reason)
}

/** Map old names to new names
  *
  * Transforms that modify names should return a [[RenameMap]] with the [[CircuitState]]
  * These are mutable datastructures for convenience
  */
// TODO This should probably be refactored into immutable and mutable versions
final class RenameMap private () {

  /** Record that the from [[CircuitTarget]] is renamed to another [[CircuitTarget]]
    * @param from
    * @param to
    */
  def record(from: CircuitTarget, to: CircuitTarget): Unit = completeRename(from, Seq(to))

  /** Record that the from [[CircuitTarget]] is renamed to another sequence of [[CircuitTarget]]s
    * @param from
    * @param tos
    */
  def record(from: CircuitTarget, tos: Seq[CircuitTarget]): Unit = completeRename(from, tos)

  /** Record that the from [[IsMember]] is renamed to another [[IsMember]]
    * @param from
    * @param to
    */
  def record(from: IsMember, to: IsMember): Unit = completeRename(from, Seq(to))

  /** Record that the from [[IsMember]] is renamed to another sequence of [[IsMember]]s
    * @param from
    * @param tos
    */
  def record(from: IsMember, tos: Seq[IsMember]): Unit = completeRename(from, tos)

  /** Records that the keys in map are also renamed to their corresponding value seqs.
    * Only ([[CircuitTarget]] -> Seq[ [[CircuitTarget]] ]) and ([[IsMember]] -> Seq[ [[IsMember]] ]) key/value allowed
    * @param map
    */
  def recordAll(map: collection.Map[CompleteTarget, Seq[CompleteTarget]]): Unit =
    map.foreach{
      case (from: IsComponent, tos: Seq[IsMember]) => completeRename(from, tos)
      case (from: IsModule, tos: Seq[IsMember]) => completeRename(from, tos)
      case (from: CircuitTarget, tos: Seq[CircuitTarget]) => completeRename(from, tos)
      case other => Utils.throwInternalError(s"Illegal rename: ${other._1} -> ${other._2}")
    }

  /** Records that a [[CompleteTarget]] is deleted
    * @param name
    */
  def delete(name: CompleteTarget): Unit = underlying(name) = Seq.empty

  /** Renames a [[CompleteTarget]]
    * @param t target to rename
    * @return renamed targets
    */
  def apply(t: CompleteTarget): Seq[CompleteTarget] = completeGet(t).getOrElse(Seq(t))

  /** Get renames of a [[CircuitTarget]]
    * @param key Target referencing the original circuit
    * @return Optionally return sequence of targets that key remaps to
    */
  def get(key: CompleteTarget): Option[Seq[CompleteTarget]] = completeGet(key)

  /** Get renames of a [[CircuitTarget]]
    * @param key Target referencing the original circuit
    * @return Optionally return sequence of targets that key remaps to
    */
  def get(key: CircuitTarget): Option[Seq[CircuitTarget]] = completeGet(key).map( _.map { case x: CircuitTarget => x } )

  /** Get renames of a [[IsMember]]
    * @param key Target referencing the original member of the circuit
    * @return Optionally return sequence of targets that key remaps to
    */
  def get(key: IsMember): Option[Seq[IsMember]] = completeGet(key).map { _.map { case x: IsMember => x } }


  /** Create new [[RenameMap]] that merges this and renameMap
    * @param renameMap
    * @return
    */
  def ++ (renameMap: RenameMap): RenameMap = RenameMap(underlying ++ renameMap.getUnderlying)

  /** Returns the underlying map of rename information
    * @return
    */
  def getUnderlying: collection.Map[CompleteTarget, Seq[CompleteTarget]] = underlying

  /** @return Whether this [[RenameMap]] has collected any changes */
  def hasChanges: Boolean = underlying.nonEmpty

  def getReverseRenameMap: RenameMap = {
    val reverseMap = mutable.HashMap[CompleteTarget, Seq[CompleteTarget]]()
    underlying.keysIterator.foreach{ key =>
      apply(key).foreach { v =>
        reverseMap(v) = key +: reverseMap.getOrElse(v, Nil)
      }
    }
    RenameMap.create(reverseMap)
  }

  def keys: Iterator[CompleteTarget] = underlying.keysIterator

  /** Serialize the underlying remapping of keys to new targets
    * @return
    */
  def serialize: String = underlying.map { case (k, v) =>
    k.serialize + "=>" + v.map(_.serialize).mkString(", ")
  }.mkString("\n")

  /** Maps old names to new names. New names could still require renaming parts of their name
    * Old names must refer to existing names in the old circuit
    */
  private val underlying = mutable.HashMap[CompleteTarget, Seq[CompleteTarget]]()

  /** Records which local InstanceTargets will require modification.
    * Used to reduce time to rename nonlocal targets who's path does not require renaming
    */
  private val sensitivity = mutable.HashSet[IsComponent]()

  /** Caches results of recursiveGet. Is cleared any time a new rename target is added
    */
  private val getCache = mutable.HashMap[CompleteTarget, Seq[CompleteTarget]]()

  /** Updates [[sensitivity]]
    * @param from original target
    * @param to new target
    */
  private def recordSensitivity(from: CompleteTarget, to: CompleteTarget): Unit = {
    (from, to) match {
      case (f: IsMember, t: IsMember) =>
        val fromSet = f.pathAsTargets.toSet
        val toSet = t.pathAsTargets
        sensitivity ++= (fromSet -- toSet)
        sensitivity ++= (fromSet.map(_.asReference) -- toSet.map(_.asReference))
      case other =>
    }
  }

  /** Get renames of a [[CompleteTarget]]
    * @param key Target referencing the original circuit
    * @return Optionally return sequence of targets that key remaps to
    */
  private def completeGet(key: CompleteTarget): Option[Seq[CompleteTarget]] = {
    val errors = mutable.ArrayBuffer[String]()
    val ret = if(hasChanges) {
      val ret = recursiveGet(errors)(key)
      if(errors.nonEmpty) { throw IllegalRenameException(errors.mkString("\n")) }
      if(ret.size == 1 && ret.head == key) { None } else { Some(ret) }
    } else { None }
    ret
  }


  /** Checks for renames of only the component portion of a [[ReferenceTarget]]
    * NOTE: assumes key has been normalized
    * @param errors Used to record illegal renames
    * @param key Target to rename
    * @return Renamed targets
    */
  private def componentGet(errors: mutable.ArrayBuffer[String])(key: ReferenceTarget): Option[Seq[CompleteTarget]] = {
    def traverseTokens(key: ReferenceTarget): Option[Seq[CompleteTarget]] = {
      if (underlying.contains(key)) {
        Some(underlying(key))
      } else {
        key match {
          case t: ReferenceTarget if t.component.nonEmpty =>
            val last = t.component.last
            val parent = t.copy(component = t.component.dropRight(1))
            traverseTokens(parent).map(_.flatMap { x =>
              (x, last) match {
                case (t2: ReferenceTarget, Field(f)) => Some(t2.field(f))
                case (t2: ReferenceTarget, Index(i)) => Some(t2.index(i))
                case other =>
                  errors += s"1Illegal rename: ${key.targetParent} cannot be renamed to ${other._1} - must rename $key directly"
                  None
              }
            })
          case t: ReferenceTarget => None
        }
      }
    }

    def traverseHierarchy(key: ReferenceTarget): Option[Seq[CompleteTarget]] = {
      val tokenRenamed = traverseTokens(key)
      if (tokenRenamed.nonEmpty) {
        tokenRenamed
      } else {
        key match {
          case t: ReferenceTarget if t.isLocal => None
          case t: ReferenceTarget =>
            val encapsulatingInstance = t.path.head._1.value
            val stripped = t.stripHierarchy(1)
            traverseHierarchy(stripped).map(_.map {
              _.addHierarchy(t.moduleOpt.get, encapsulatingInstance)
            })
        }
      }
    }

    traverseHierarchy(key)
  }

  /** Checks for renames of only the module portion of a [[IsModule]]
    * @param errors Used to record illegal renames
    * @param key Target to rename
    * @return Renamed targets
    */
  private def instanceGet(errors: mutable.ArrayBuffer[String])(key: InstanceTarget): Seq[IsModule] = {
    def traverseLeft(key: IsModule): Option[Seq[IsModule]] = {
      val getOpt = key match {
        case t: InstanceTarget if underlying.contains(t) => underlying.get(t)
        case t: InstanceTarget => underlying.get(t.asReference)
        case t: ModuleTarget => underlying.get(t)
      }

      if (getOpt.nonEmpty) {
        getOpt.map(_.flatMap {
          case isMod: IsModule => Some(isMod)
          case ref @ ReferenceTarget(c, m, p, r, Nil) =>
            val ofMod = key match {
              case t: InstanceTarget => t.ofModule
              case t: ModuleTarget => key match {
                case i: InstanceTarget => i.ofModule
                case m: ModuleTarget =>
                  errors += s"Instance as reference: $ref cannot be renamed to a non-instance $m"
                  t.module
              }
            }
            Some(InstanceTarget(c, m, p, r, ofMod))
          case other =>
            errors += s"IsModule: $key cannot be renamed to non-IsModule $other"
            None
        })
      } else {
        key match {
          case t: ModuleTarget => None
          case t: InstanceTarget if t.isLocal =>
            traverseLeft(ModuleTarget(t.circuit, t.ofModule)).map(_.map {
              _.addHierarchy(t.moduleOpt.get, t.instance)
            })
          case t: InstanceTarget =>
            val (Instance(outerInst), OfModule(outerMod)) = t.path.head
            val stripped = t.copy(path = t.path.tail, module = outerMod)
            traverseLeft(stripped).map(_.map {
              _.addHierarchy(t.moduleOpt.get, outerInst)
            })
        }
      }
    }

    def traverseRight(key: InstanceTarget): Seq[IsModule] = {
      val findLeft = traverseLeft(key)
      if (findLeft.nonEmpty) {
        findLeft.get
      } else {
        key match {
          case t: InstanceTarget if t.isLocal =>
            traverseLeft(t.targetParent).map(_.map {
              _.instOf(t.ofModule, t.instance)
            }).getOrElse(Seq(key))
          case t: InstanceTarget =>
            val (Instance(i), OfModule(m)) = t.path.last
            val parent = t.copy(path = t.path.dropRight(1), instance = i, ofModule = m)
            traverseRight(parent).map(_.instOf(t.instance, t.ofModule))
        }
      }
    }

    traverseRight(key)
  }

  private def circuitGet(errors: mutable.ArrayBuffer[String])(key: CircuitTarget): Seq[CircuitTarget] = {
    underlying.get(key).map(_.flatMap {
      case c: CircuitTarget => Some(c)
      case other =>
        errors += s"Illegal rename: $key cannot be renamed to non-circuit target: $other"
        None
    }).getOrElse(Seq(key))
  }

  private def moduleGet(errors: mutable.ArrayBuffer[String])(key: ModuleTarget): Seq[IsModule] = {
    underlying.get(key).map(_.flatMap {
      case mod: IsModule => Some(mod)
      case ReferenceTarget(c, m, p, r, Nil) => Some(InstanceTarget(c, m, p, r, key.module))
      case other =>
        errors += s"Illegal rename: $key cannot be renamed to non-module target: $other"
        None
    }).getOrElse(Seq(key))
  }

  // scalastyle:off
  // This function requires a large cyclomatic complexity, and is best naturally expressed as a large function
  /** Recursively renames a target so the returned targets are complete renamed
    * @param set Used to detect circular renames
    * @param errors Used to record illegal renames
    * @param key Target to rename
    * @return Renamed targets
    */
  private def recursiveGet(errors: mutable.ArrayBuffer[String])(key: CompleteTarget): Seq[CompleteTarget] = {
    if(getCache.contains(key)) {
      getCache(key)
    } else {
      val getter = recursiveGet(errors)(_)

      // rename just the first level
      val topRename = key match {
        case t: CircuitTarget => circuitGet(errors)(t)
        case t: ModuleTarget => moduleGet(errors)(t)
        case t: InstanceTarget => instanceGet(errors)(t)
        case ref: ReferenceTarget if ref.isLocal => componentGet(errors)(ref).getOrElse(Seq(ref))
        case ref @ ReferenceTarget(c, m, p, r, t) =>
          val (Instance(inst), OfModule(ofMod)) = p.last
          componentGet(errors)(ref).getOrElse {
            val parent = InstanceTarget(c, m, p.dropRight(1), inst, ofMod)
            instanceGet(errors)(parent).map(ref.setPathTarget(_))
          }
      }

      // rename the next level
      val midRename = topRename.flatMap {
        case t: CircuitTarget => Seq(t)
        case t: ModuleTarget =>
          circuitGet(errors)(CircuitTarget(t.circuit)).map {
            case CircuitTarget(c) => t.copy(circuit = c)
          }
        case t: ReferenceTarget =>
          val renamedPath = t.path.map { pair =>
            val pathMod = ModuleTarget(t.circuit, pair._2.value)
            moduleGet(errors)(pathMod) match {
              case Seq(ModuleTarget(c, m)) if c == t.circuit => pair.copy(_2 = OfModule(m))
              case other =>
                errors += s"Illegal rename: ofModule of ${pathMod} is renamed to $other - must rename $t directly."
                pair
            }
          }
          moduleGet(errors)(ModuleTarget(t.circuit, t.module)).map { mod =>
              val newPath = mod match {
                case m: ModuleTarget => renamedPath
                case i: InstanceTarget => i.path ++ ((Instance(i.instance), OfModule(i.ofModule)) +: renamedPath)
              }
              t.copy(circuit = mod.circuit, module = mod.module, path = newPath)
          }
        case t: InstanceTarget =>
          val renamedPath = t.asPath.flatMap { pair =>
            val pathMod = ModuleTarget(t.circuit, pair._2.value)
            moduleGet(errors)(pathMod) match {
              case Seq(isMod: IsModule) if isMod.circuit == t.circuit =>
                pair.copy(_2 = OfModule(isMod.module)) +: isMod.path
              case other =>
                errors += s"Illegal rename: ofModule of ${pathMod} is renamed to $other - must rename $t directly."
                Seq(pair)
            }
          }
          moduleGet(errors)(ModuleTarget(t.circuit, t.module)).map { mod =>
              val newPath = mod match {
                case m: ModuleTarget => renamedPath.dropRight(1)
                case i: InstanceTarget => i.path ++ ((Instance(i.instance), OfModule(i.ofModule)) +: renamedPath.dropRight(1))
              }
              val (Instance(newInst), OfModule(newOfMod)) = renamedPath.last
              t.copy(circuit = mod.circuit,
                module = mod.module,
                path = newPath,
                instance = newInst,
                ofModule = newOfMod)
          }
      }

      val botRename = midRename.flatMap {
        case t: CircuitTarget => Seq(t)
        case t: ModuleTarget => Seq(t)
        case t: IsComponent =>
          circuitGet(errors)(CircuitTarget(t.circuit)).map {
            case CircuitTarget(c) =>
              t match {
                case ref: ReferenceTarget => ref.copy(circuit = c)
                case inst: InstanceTarget => inst.copy(circuit = c)
              }
          }
      }

      // Cache result
      getCache(key) = botRename
      botRename
    }
  }
  // scalastyle:on

  /** Fully renames from to tos
    * @param from
    * @param tos
    */
  private def completeRename(from: CompleteTarget, tos: Seq[CompleteTarget]): Unit = {
    def check(from: CompleteTarget, to: CompleteTarget)(t: CompleteTarget): Unit = {
      require(from != t, s"Cannot record $from to $to, as it is a circular constraint")
      t match {
        case _: CircuitTarget =>
        case other: IsMember => check(from, to)(other.targetParent)
      }
    }
    tos.foreach { to => if(from != to) check(from, to)(to) }
    (from, tos) match {
      case (x, Seq(y)) if x == y =>
      case _ =>
        tos.foreach{recordSensitivity(from, _)}
        val existing = underlying.getOrElse(from, Vector.empty)
        val updated = existing ++ tos
        underlying(from) = updated
        getCache.clear()
    }
  }

  /* DEPRECATED ACCESSOR/SETTOR METHODS WITH [[Named]] */

  @deprecated("Use record with CircuitTarget instead, this will be removed in 1.3", "1.2")
  def rename(from: Named, to: Named): Unit = rename(from, Seq(to))

  @deprecated("Use record with IsMember instead, this will be removed in 1.3", "1.2")
  def rename(from: Named, tos: Seq[Named]): Unit = recordAll(Map(from.toTarget -> tos.map(_.toTarget)))

  @deprecated("Use record with IsMember instead, this will be removed in 1.3", "1.2")
  def rename(from: ComponentName, to: ComponentName): Unit = record(from, to)

  @deprecated("Use record with IsMember instead, this will be removed in 1.3", "1.2")
  def rename(from: ComponentName, tos: Seq[ComponentName]): Unit = record(from, tos.map(_.toTarget))

  @deprecated("Use delete with CircuitTarget instead, this will be removed in 1.3", "1.2")
  def delete(name: CircuitName): Unit = underlying(name) = Seq.empty

  @deprecated("Use delete with IsMember instead, this will be removed in 1.3", "1.2")
  def delete(name: ModuleName): Unit = underlying(name) = Seq.empty

  @deprecated("Use delete with IsMember instead, this will be removed in 1.3", "1.2")
  def delete(name: ComponentName): Unit = underlying(name) = Seq.empty

  @deprecated("Use recordAll with CompleteTarget instead, this will be removed in 1.3", "1.2")
  def addMap(map: collection.Map[Named, Seq[Named]]): Unit =
    recordAll(map.map { case (key, values) => (Target.convertNamed2Target(key), values.map(Target.convertNamed2Target)) })

  @deprecated("Use get with CircuitTarget instead, this will be removed in 1.3", "1.2")
  def get(key: CircuitName): Option[Seq[CircuitName]] = {
    get(Target.convertCircuitName2CircuitTarget(key)).map(_.collect{ case c: CircuitTarget => c.toNamed })
  }

  @deprecated("Use get with IsMember instead, this will be removed in 1.3", "1.2")
  def get(key: ModuleName): Option[Seq[ModuleName]] = {
    get(Target.convertModuleName2ModuleTarget(key)).map(_.collect{ case m: ModuleTarget => m.toNamed })
  }

  @deprecated("Use get with IsMember instead, this will be removed in 1.3", "1.2")
  def get(key: ComponentName): Option[Seq[ComponentName]] = {
    get(Target.convertComponentName2ReferenceTarget(key)).map(_.collect{ case c: IsComponent => c.toNamed })
  }

  @deprecated("Use get with IsMember instead, this will be removed in 1.3", "1.2")
  def get(key: Named): Option[Seq[Named]] = key match {
    case t: CompleteTarget => get(t)
    case other => get(key.toTarget).map(_.collect{ case c: IsComponent => c.toNamed })
  }


  // Mutable helpers - APIs that set these are deprecated!
  private var circuitName: String = ""
  private var moduleName: String = ""

  /** Sets mutable state to record current module we are visiting
    * @param module
    */
  @deprecated("Use typesafe rename defs instead, this will be removed in 1.3", "1.2")
  def setModule(module: String): Unit = moduleName = module

  /** Sets mutable state to record current circuit we are visiting
    * @param circuit
    */
  @deprecated("Use typesafe rename defs instead, this will be removed in 1.3", "1.2")
  def setCircuit(circuit: String): Unit = circuitName = circuit

  /** Records how a reference maps to a new reference
    * @param from
    * @param to
    */
  @deprecated("Use typesafe rename defs instead, this will be removed in 1.3", "1.2")
  def rename(from: String, to: String): Unit = rename(from, Seq(to))

  /** Records how a reference maps to a new reference
    * The reference's root module and circuit are determined by whomever called setModule or setCircuit last
    * @param from
    * @param tos
    */
  @deprecated("Use typesafe rename defs instead, this will be removed in 1.3", "1.2")
  def rename(from: String, tos: Seq[String]): Unit = {
    val mn = ModuleName(moduleName, CircuitName(circuitName))
    val fromName = ComponentName(from, mn).toTarget
    val tosName = tos map { to => ComponentName(to, mn).toTarget }
    record(fromName, tosName)
  }

  /** Records named reference is deleted
    * The reference's root module and circuit are determined by whomever called setModule or setCircuit last
    * @param name
    */
  @deprecated("Use typesafe rename defs instead, this will be removed in 1.3", "1.2")
  def delete(name: String): Unit = {
    Target(Some(circuitName), Some(moduleName), AnnotationUtils.toSubComponents(name)).getComplete match {
      case Some(t: CircuitTarget) => delete(t)
      case Some(m: IsMember) => delete(m)
      case other =>
    }
  }

  /** Records that references in names are all deleted
    * The reference's root module and circuit are determined by whomever called setModule or setCircuit last
    * @param names
    */
  @deprecated("Use typesafe rename defs instead, this will be removed in 1.3", "1.2")
  def delete(names: Seq[String]): Unit = names.foreach(delete(_))
}


