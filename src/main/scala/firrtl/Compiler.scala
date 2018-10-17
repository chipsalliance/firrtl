// See LICENSE for license details.

package firrtl

import logger._
import java.io.Writer

import annotations._
import firrtl.RenameMap.{CircularRenameException, IllegalRenameException}

import scala.collection.mutable
import firrtl.annotations._
import firrtl.ir.{Circuit, Expression}
import firrtl.Utils.{error, throwInternalError}
import firrtl.annotations.TargetToken
import firrtl.annotations.TargetToken.{Field, Index}
import firrtl.annotations.transforms.{EliminateTargetPaths, ResolvePaths}

object RenameMap {
  def apply(map: collection.Map[CompleteTarget, Seq[CompleteTarget]]): RenameMap = {
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
      case other => throwInternalError(s"Illegal rename: ${other._1} -> ${other._2}")
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
    RenameMap(reverseMap)
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
      val ret = recursiveGet(mutable.LinkedHashSet.empty[CompleteTarget], errors)(key)
      if(errors.nonEmpty) { throw IllegalRenameException(errors.mkString("\n")) }
      if(ret.size == 1 && ret.head == key) { None } else { Some(ret) }
    } else { None }
    ret
  }

  // scalastyle:off
  // This function requires a large cyclomatic complexity, and is best naturally expressed as a large function
  /** Recursively renames a target so the returned targets are complete renamed
    * @param set Used to detect circular renames
    * @param errors Used to record illegal renames
    * @param key Target to rename
    * @return Renamed targets
    */
  private def recursiveGet(set: mutable.LinkedHashSet[CompleteTarget],
                           errors: mutable.ArrayBuffer[String]
                          )(key: CompleteTarget): Seq[CompleteTarget] = {
    if(getCache.contains(key)) {
      getCache(key)
    } else {
      // First, check if whole key is remapped
      // Note that remapped could hold stale parent targets that require renaming
      val remapped = underlying.getOrElse(key, Seq(key))

      // If we've seen this key before in recursive calls to parentTargets, then we know a circular renaming
      // mapping has occurred, and no legal name exists
      if(set.contains(key) && !key.isInstanceOf[CircuitTarget]) {
        throw CircularRenameException(s"Illegal rename: circular renaming is illegal - ${set.mkString(" -> ")}")
      }

      // Add key to set to detect circular renaming
      set += key

      // Curry recursiveGet for cleaner syntax below
      val getter = recursiveGet(set, errors)(_)

      // For each remapped key, call recursiveGet on their parentTargets
      val ret = remapped.flatMap {

        // If t is a CircuitTarget, return it because it has no parent target
        case t: CircuitTarget => Seq(t)

        // If t is a ModuleTarget, try to rename parent target, then update t's parent
        case t: ModuleTarget => getter(t.targetParent).map {
          case CircuitTarget(c) => ModuleTarget(c, t.module)
        }

        /** If t is an InstanceTarget (has a path) but has no references:
          * 1) Check whether the instance has been renamed (asReference)
          * 2) Check whether the ofModule of the instance has been renamed (only 1:1 renaming is ok)
          */
        case t: InstanceTarget =>
          getter(t.asReference).map {
            case t2:InstanceTarget => t2
            case t2@ReferenceTarget(c, m, p, r, Nil) =>
              val t3 = InstanceTarget(c, m, p, r, t.ofModule)
              val ofModuleTarget = t3.ofModuleTarget
              getter(ofModuleTarget) match {
                case Seq(ModuleTarget(newCircuit, newOf)) if newCircuit == t3.circuit => t3.copy(ofModule = newOf)
                case other =>
                  errors += s"Illegal rename: ofModule of $t is renamed to $other - must rename $t directly."
                  t
              }
            case other =>
              errors += s"Illegal rename: $t has new instance reference $other"
              t
          }

        /** If t is a ReferenceTarget:
          * 1) Check parentTarget to tokens
          * 2) Check ReferenceTarget with one layer stripped from its path hierarchy (i.e. a new root module)
          */
        case t: ReferenceTarget =>
          val ret: Seq[CompleteTarget] = if(t.component.nonEmpty) {
            val last = t.component.last
            getter(t.targetParent).map{ x =>
              (x, last) match {
                case (t2: ReferenceTarget, Field(f)) => t2.field(f)
                case (t2: ReferenceTarget, Index(i)) => t2.index(i)
                case other =>
                  errors += s"Illegal rename: ${t.targetParent} cannot be renamed to ${other._1} - must rename $t directly"
                  t
              }
            }
          } else {
            val pathTargets = sensitivity.empty ++ (t.pathAsTargets ++ t.pathAsTargets.map(_.asReference))
            if(t.pathAsTargets.nonEmpty && sensitivity.intersect(pathTargets).isEmpty) Seq(t) else {
              getter(t.pathTarget).map {
                case newPath: IsModule => t.setPathTarget(newPath)
                case other =>
                  errors += s"Illegal rename: path ${t.pathTarget} of $t cannot be renamed to $other - must rename $t directly"
                  t
              }
            }
          }
          ret.flatMap {
            case y: IsComponent if !y.isLocal =>
              val encapsulatingInstance = y.path.head._1.value
              getter(y.stripHierarchy(1)).map {
                _.addHierarchy(y.moduleOpt.get, encapsulatingInstance)
              }
            case other => Seq(other)
          }
      }

      // Remove key from set as visiting the same key twice is ok, as long as its not during the same recursive call
      set -= key

      // Cache result
      getCache(key) = ret

      // Return result
      ret

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
        val existing = underlying.getOrElse(from, Seq.empty)
        val updated = existing ++ tos
        underlying(from) = updated
        getCache.clear()
    }
  }

  /* DEPRECATED ACCESSOR/SETTOR METHODS WITH [[Named]] */

  @deprecated("Use record with CircuitTarget instead, this will be removed in 1.3", "1.2")
  def rename(from: CircuitName, to: CircuitName): Unit = record(from, to)

  @deprecated("Use record with IsMember instead, this will be removed in 1.3", "1.2")
  def rename(from: ModuleName, to: ModuleName): Unit = record(from, to)

  @deprecated("Use record with IsMember instead, this will be removed in 1.3", "1.2")
  def rename(from: ModuleName, tos: Seq[ModuleName]): Unit = record(from, tos.map(_.toTarget))

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

/** Container of all annotations for a Firrtl compiler */
class AnnotationSeq private (private[firrtl] val underlying: List[Annotation]) {
  def toSeq: Seq[Annotation] = underlying.toSeq
}
object AnnotationSeq {
  def apply(xs: Seq[Annotation]): AnnotationSeq = new AnnotationSeq(xs.toList)
}

/** Current State of the Circuit
  *
  * @constructor Creates a CircuitState object
  * @param circuit The current state of the Firrtl AST
  * @param form The current form of the circuit
  * @param annotations The current collection of [[firrtl.annotations.Annotation Annotation]]
  * @param renames A map of [[firrtl.annotations.Named Named]] things that have been renamed.
  *   Generally only a return value from [[Transform]]s
  */
case class CircuitState(
    circuit: Circuit,
    form: CircuitForm,
    annotations: AnnotationSeq,
    renames: Option[RenameMap]) {

  /** Helper for getting just an emitted circuit */
  def emittedCircuitOption: Option[EmittedCircuit] =
    emittedComponents collectFirst { case x: EmittedCircuit => x }
  /** Helper for getting an [[EmittedCircuit]] when it is known to exist */
  def getEmittedCircuit: EmittedCircuit = emittedCircuitOption match {
    case Some(emittedCircuit) => emittedCircuit
    case None =>
      throw new FIRRTLException(s"No EmittedCircuit found! Did you delete any annotations?\n$deletedAnnotations")
  }

  /** Helper function for extracting emitted components from annotations */
  def emittedComponents: Seq[EmittedComponent] =
    annotations.collect { case emitted: EmittedAnnotation[_] => emitted.value }
  def deletedAnnotations: Seq[Annotation] =
    annotations.collect { case anno: DeletedAnnotation => anno }

  /** Returns a new CircuitState with all targets being resolved.
    * Paths through instances are replaced with a uniquified final target
    * Includes modifying the circuit and annotations
    * @param targets
    * @return
    */
  def resolvePaths(targets: Seq[CompleteTarget]): CircuitState = {
    val newCS = new EliminateTargetPaths().runTransform(this.copy(annotations = ResolvePaths(targets) +: annotations ))
    newCS.copy(form = form)
  }

  /** Returns a new CircuitState with the targets of every annotation of a type in annoClasses
    * @param annoClasses
    * @return
    */
  def resolvePathsOf(annoClasses: Class[_]*): CircuitState = {
    val targets = getAnnotationsOf(annoClasses:_*).flatMap(_.getTargets)
    if(targets.nonEmpty) resolvePaths(targets.flatMap{_.getComplete}) else this
  }

  /** Returns all annotations which are of a class in annoClasses
    * @param annoClasses
    * @return
    */
  def getAnnotationsOf(annoClasses: Class[_]*): AnnotationSeq = {
    annotations.collect { case a if annoClasses.contains(a.getClass) => a }
  }
}

object CircuitState {
  def apply(circuit: Circuit, form: CircuitForm): CircuitState = apply(circuit, form, Seq())
  def apply(circuit: Circuit, form: CircuitForm, annotations: AnnotationSeq): CircuitState =
    new CircuitState(circuit, form, annotations, None)
}

/** Current form of the Firrtl Circuit
  *
  * Form is a measure of addition restrictions on the legality of a Firrtl
  * circuit.  There is a notion of "highness" and "lowness" implemented in the
  * compiler by extending scala.math.Ordered. "Lower" forms add additional
  * restrictions compared to "higher" forms. This means that "higher" forms are
  * strictly supersets of the "lower" forms. Thus, that any transform that
  * operates on [[HighForm]] can also operate on [[MidForm]] or [[LowForm]]
  */
sealed abstract class CircuitForm(private val value: Int) extends Ordered[CircuitForm] {
  // Note that value is used only to allow comparisons
  def compare(that: CircuitForm): Int = this.value - that.value
}

// scalastyle:off magic.number
// These magic numbers give an ordering to CircuitForm
/** Chirrtl Form
  *
  * The form of the circuit emitted by Chisel. Not a true Firrtl form.
  * Includes cmem, smem, and mport IR nodes which enable declaring memories
  * separately form their ports. A "Higher" form than [[HighForm]]
  *
  * See [[CDefMemory]] and [[CDefMPort]]
  */
final case object ChirrtlForm extends CircuitForm(value = 3)
/** High Form
  *
  * As detailed in the Firrtl specification
  * [[https://github.com/ucb-bar/firrtl/blob/master/spec/spec.pdf]]
  *
  * Also see [[firrtl.ir]]
  */
final case object HighForm extends CircuitForm(2)
/** Middle Form
  *
  * A "lower" form than [[HighForm]] with the following restrictions:
  *  - All widths must be explicit
  *  - All whens must be removed
  *  - There can only be a single connection to any element
  */
final case object MidForm extends CircuitForm(1)
/** Low Form
  *
  * The "lowest" form. In addition to the restrictions in [[MidForm]]:
  *  - All aggregate types (vector/bundle) must have been removed
  *  - All implicit truncations must be made explicit
  */
final case object LowForm extends CircuitForm(0)
/** Unknown Form
  *
  * Often passes may modify a circuit (e.g. InferTypes), but return
  * a circuit in the same form it was given.
  *
  * For this use case, use UnknownForm. It cannot be compared against other
  * forms.
  *
  * TODO(azidar): Replace with PreviousForm, which more explicitly encodes
  * this requirement.
  */
final case object UnknownForm extends CircuitForm(-1) {
  override def compare(that: CircuitForm): Int = { sys.error("Illegal to compare UnknownForm"); 0 }
}
// scalastyle:on magic.number

/** The basic unit of operating on a Firrtl AST */
abstract class Transform extends LazyLogging {
  /** A convenience function useful for debugging and error messages */
  def name: String = this.getClass.getSimpleName
  /** The [[firrtl.CircuitForm]] that this transform requires to operate on */
  def inputForm: CircuitForm
  /** The [[firrtl.CircuitForm]] that this transform outputs */
  def outputForm: CircuitForm
  /** Perform the transform, encode renaming with RenameMap, and can
    *   delete annotations
    * Called by [[runTransform]].
    *
    * @param state Input Firrtl AST
    * @return A transformed Firrtl AST
    */
  protected def execute(state: CircuitState): CircuitState

  /** Convenience method to get annotations relevant to this Transform
    *
    * @param state The [[CircuitState]] form which to extract annotations
    * @return A collection of annotations
    */
  @deprecated("Just collect the actual Annotation types the transform wants", "1.1")
  final def getMyAnnotations(state: CircuitState): Seq[Annotation] = {
    val msg = "getMyAnnotations is deprecated, use collect and match on concrete types"
    Driver.dramaticWarning(msg)
    state.annotations.collect { case a: LegacyAnnotation if a.transform == this.getClass => a }
  }

  /** Executes before any transform's execute method
    * @param state
    * @return
    */
  def prepare(state: CircuitState): CircuitState = state

  /** Perform the transform and update annotations.
    *
    * @param state Input Firrtl AST
    * @return A transformed Firrtl AST
    */
  final def runTransform(state: CircuitState): CircuitState = {
    logger.info(s"======== Starting Transform $name ========")

    val (timeMillis, result) = Utils.time { execute(prepare(state)) }

    logger.info(s"""----------------------------${"-" * name.size}---------\n""")
    logger.info(f"Time: $timeMillis%.1f ms")

    val remappedAnnotations = propagateAnnotations(state.annotations, result.annotations, result.renames)

    logger.info(s"Form: ${result.form}")
    logger.debug(s"Annotations:")
    remappedAnnotations.foreach { a =>
      logger.debug(a.serialize)
    }
    logger.trace(s"Circuit:\n${result.circuit.serialize}")
    logger.info(s"======== Finished Transform $name ========\n")
    CircuitState(result.circuit, result.form, remappedAnnotations, None)
  }

  /** Propagate annotations and update their names.
    *
    * @param inAnno input AnnotationSeq
    * @param resAnno result AnnotationSeq
    * @param renameOpt result RenameMap
    * @return the updated annotations
    */
  final private def propagateAnnotations(
      inAnno: AnnotationSeq,
      resAnno: AnnotationSeq,
      renameOpt: Option[RenameMap]): AnnotationSeq = {
    val newAnnotations = {
      val inSet = mutable.LinkedHashSet() ++ inAnno
      val resSet = mutable.LinkedHashSet() ++ resAnno
      val deleted = (inSet -- resSet).map {
        case DeletedAnnotation(xFormName, delAnno) => DeletedAnnotation(s"$xFormName+$name", delAnno)
        case anno => DeletedAnnotation(name, anno)
      }
      val created = resSet -- inSet
      val unchanged = resSet & inSet
      (deleted ++ created ++ unchanged)
    }

    // For each annotation, rename all annotations.
    val renames = renameOpt.getOrElse(RenameMap())
    val remapped2original = mutable.LinkedHashMap[Annotation, mutable.LinkedHashSet[Annotation]]()
    val keysOfNote = mutable.LinkedHashSet[Annotation]()
    val finalAnnotations = newAnnotations.flatMap { anno =>
      val remappedAnnos = anno.update(renames)
      remappedAnnos.foreach { remapped =>
        val set = remapped2original.getOrElseUpdate(remapped, mutable.LinkedHashSet.empty[Annotation])
        set += anno
        if(set.size > 1) keysOfNote += remapped
      }
      remappedAnnos
    }.toSeq
    keysOfNote.foreach { key =>
      logger.debug(s"""The following original annotations are renamed to the same new annotation.""")
      logger.debug(s"""Original Annotations:\n  ${remapped2original(key).mkString("\n  ")}""")
      logger.debug(s"""New Annotation:\n  $key""")
    }
    finalAnnotations
  }
}

trait SeqTransformBased {
  def transforms: Seq[Transform]
  protected def runTransforms(state: CircuitState): CircuitState =
    transforms.foldLeft(state) { (in, xform) => xform.runTransform(in) }
}

/** For transformations that are simply a sequence of transforms */
abstract class SeqTransform extends Transform with SeqTransformBased {
  def execute(state: CircuitState): CircuitState = {
    /*
    require(state.form <= inputForm,
      s"[$name]: Input form must be lower or equal to $inputForm. Got ${state.form}")
    */
    val ret = runTransforms(state)
    CircuitState(ret.circuit, outputForm, ret.annotations, ret.renames)
  }
}

/** Extend for transforms that require resolved targets in their annotations
  * Ensures all targets in annotations of a class in annotationClasses are resolved before the execute method
  */
trait ResolvedAnnotationPaths {
  this: Transform =>

  val annotationClasses: Traversable[Class[_]]

  override def prepare(state: CircuitState): CircuitState = {
    state.resolvePathsOf(annotationClasses.toSeq:_*)
  }
}

/** Defines old API for Emission. Deprecated */
trait Emitter extends Transform {
  @deprecated("Use emission annotations instead", "firrtl 1.0")
  def emit(state: CircuitState, writer: Writer): Unit
}

object CompilerUtils extends LazyLogging {
  /** Generates a sequence of [[Transform]]s to lower a Firrtl circuit
    *
    * @param inputForm [[CircuitForm]] to lower from
    * @param outputForm [[CircuitForm]] to lower to
    * @return Sequence of transforms that will lower if outputForm is lower than inputForm
    */
  def getLoweringTransforms(inputForm: CircuitForm, outputForm: CircuitForm): Seq[Transform] = {
    // If outputForm is equal-to or higher than inputForm, nothing to lower
    if (outputForm >= inputForm) {
      Seq.empty
    } else {
      inputForm match {
        case ChirrtlForm =>
          Seq(new ChirrtlToHighFirrtl) ++ getLoweringTransforms(HighForm, outputForm)
        case HighForm =>
          Seq(new IRToWorkingIR, new ResolveAndCheck, new transforms.DedupModules,
              new HighFirrtlToMiddleFirrtl) ++ getLoweringTransforms(MidForm, outputForm)
        case MidForm => Seq(new MiddleFirrtlToLowFirrtl) ++ getLoweringTransforms(LowForm, outputForm)
        case LowForm => throwInternalError("getLoweringTransforms - LowForm") // should be caught by if above
        case UnknownForm => throwInternalError("getLoweringTransforms - UnknownForm") // should be caught by if above
      }
    }
  }

  /** Merge a Seq of lowering transforms with custom transforms
    *
    * Custom Transforms are inserted based on their [[Transform.inputForm]] and
    * [[Transform.outputForm]]. Custom transforms are inserted in order at the
    * last location in the Seq of transforms where previous.outputForm ==
    * customTransform.inputForm. If a customTransform outputs a higher form
    * than input, [[getLoweringTransforms]] is used to relower the circuit.
    *
    * @example
    *   {{{
    *     // Let Transforms be represented by CircuitForm => CircuitForm
    *     val A = HighForm => MidForm
    *     val B = MidForm => LowForm
    *     val lowering = List(A, B) // Assume these transforms are used by getLoweringTransforms
    *     // Some custom transforms
    *     val C = LowForm => LowForm
    *     val D = MidForm => MidForm
    *     val E = LowForm => HighForm
    *     // All of the following comparisons are true
    *     mergeTransforms(lowering, List(C)) == List(A, B, C)
    *     mergeTransforms(lowering, List(D)) == List(A, D, B)
    *     mergeTransforms(lowering, List(E)) == List(A, B, E, A, B)
    *     mergeTransforms(lowering, List(C, E)) == List(A, B, C, E, A, B)
    *     mergeTransforms(lowering, List(E, C)) == List(A, B, E, A, B, C)
    *     // Notice that in the following, custom transform order is NOT preserved (see note)
    *     mergeTransforms(lowering, List(C, D)) == List(A, D, B, C)
    *   }}}
    *
    * @note Order will be preserved for custom transforms so long as the
    * inputForm of a latter transforms is equal to or lower than the outputForm
    * of the previous transform.
    */
  def mergeTransforms(lowering: Seq[Transform], custom: Seq[Transform]): Seq[Transform] = {
    custom.foldLeft(lowering) { case (transforms, xform) =>
      val index = transforms lastIndexWhere (_.outputForm == xform.inputForm)
      assert(index >= 0 || xform.inputForm == ChirrtlForm, // If ChirrtlForm just put at front
        s"No transform in $lowering has outputForm ${xform.inputForm} as required by $xform")
      val (front, back) = transforms.splitAt(index + 1) // +1 because we want to be AFTER index
      front ++ List(xform) ++ getLoweringTransforms(xform.outputForm, xform.inputForm) ++ back
    }
  }

}

trait Compiler extends LazyLogging {
  def emitter: Emitter

  /** The sequence of transforms this compiler will execute
    * @note The inputForm of a given transform must be higher than or equal to the ouputForm of the
    *       preceding transform. See [[CircuitForm]]
    */
  def transforms: Seq[Transform]

  // Similar to (input|output)Form on [[Transform]] but derived from this Compiler's transforms
  def inputForm: CircuitForm = transforms.head.inputForm
  def outputForm: CircuitForm = transforms.last.outputForm

  private def transformsLegal(xforms: Seq[Transform]): Boolean =
    if (xforms.size < 2) {
      true
    } else {
      xforms.sliding(2, 1)
            .map { case Seq(p, n) => n.inputForm >= p.outputForm }
            .reduce(_ && _)
    }

  assert(transformsLegal(transforms),
    "Illegal Compiler, each transform must be able to accept the output of the previous transform!")

  /** Perform compilation
    *
    * @param state The Firrtl AST to compile
    * @param writer The java.io.Writer where the output of compilation will be emitted
    * @param customTransforms Any custom [[Transform]]s that will be inserted
    *   into the compilation process by [[CompilerUtils.mergeTransforms]]
    */
  @deprecated("Please use compileAndEmit or other compile method instead", "firrtl 1.0")
  def compile(state: CircuitState,
              writer: Writer,
              customTransforms: Seq[Transform] = Seq.empty): CircuitState = {
    val finalState = compileAndEmit(state, customTransforms)
    writer.write(finalState.getEmittedCircuit.value)
    finalState
  }

  /** Perform compilation and emit the whole Circuit
    *
    * This is intended as a convenience method wrapping up Annotation creation for the common case.
    * It creates a [[EmitCircuitAnnotation]] that will be consumed by this Transform's emitter. The
    * [[EmittedCircuit]] can be extracted from the returned [[CircuitState]] via
    * [[CircuitState.emittedCircuitOption]]
    *
    * @param state The Firrtl AST to compile
    * @param customTransforms Any custom [[Transform]]s that will be inserted
    *   into the compilation process by [[CompilerUtils.mergeTransforms]]
    * @return result of compilation with emitted circuit annotated
    */
  def compileAndEmit(state: CircuitState,
                     customTransforms: Seq[Transform] = Seq.empty): CircuitState = {
    val emitAnno = EmitCircuitAnnotation(emitter.getClass)
    compile(state.copy(annotations = emitAnno +: state.annotations), customTransforms)
  }

  /** Perform compilation
    *
    * Emission will only be performed if [[EmitAnnotation]]s are present
    *
    * @param state The Firrtl AST to compile
    * @param customTransforms Any custom [[Transform]]s that will be inserted into the compilation
    *   process by [[CompilerUtils.mergeTransforms]]
    * @return result of compilation
    */
  def compile(state: CircuitState, customTransforms: Seq[Transform]): CircuitState = {
    val allTransforms = CompilerUtils.mergeTransforms(transforms, customTransforms) :+ emitter

    val (timeMillis, finalState) = Utils.time {
      allTransforms.foldLeft(state) { (in, xform) => xform.runTransform(in) }
    }

    logger.error(f"Total FIRRTL Compile Time: $timeMillis%.1f ms")

    finalState
  }

}

