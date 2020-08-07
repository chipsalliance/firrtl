// See LICENSE for license details.

package firrtl

import logger._
import java.io.Writer

import scala.collection.mutable
import scala.util.Try
import scala.util.control.NonFatal

import firrtl.annotations._
import firrtl.ir.Circuit
import firrtl.Utils.throwInternalError
import firrtl.annotations.transforms.{EliminateTargetPaths, ResolvePaths}
import firrtl.options.{DependencyAPI, Dependency, StageUtils, TransformLike}
import firrtl.stage.Forms

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
      throw new FirrtlInternalException(s"No EmittedCircuit found! Did you delete any annotations?\n$deletedAnnotations")
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
  def resolvePaths(targets: Seq[CompleteTarget]): CircuitState = targets match {
    case Nil => this
    case _ =>
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
  def apply(circuit: Circuit, annotations: AnnotationSeq): CircuitState =
    new CircuitState(circuit, UnknownForm, annotations, None)
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
@deprecated(
  "Mix-in the DependencyAPIMigration trait into your Transform and specify its Dependency API dependencies. See: https://bit.ly/2Voppre",
  "FIRRTL 1.3")
sealed abstract class CircuitForm(private val value: Int) extends Ordered[CircuitForm] {
  // Note that value is used only to allow comparisons
  def compare(that: CircuitForm): Int = this.value - that.value

  /** Defines a suffix to use if this form is written to a file */
  def outputSuffix: String
}

// These magic numbers give an ordering to CircuitForm
/** Chirrtl Form
  *
  * The form of the circuit emitted by Chisel. Not a true Firrtl form.
  * Includes cmem, smem, and mport IR nodes which enable declaring memories
  * separately form their ports. A "Higher" form than [[HighForm]]
  *
  * See [[CDefMemory]] and [[CDefMPort]]
  */
@deprecated(
  "Mix-in the DependencyAPIMigration trait into your Transform and specify its Dependency API dependencies. See: https://bit.ly/2Voppre",
  "FIRRTL 1.3")
final case object ChirrtlForm extends CircuitForm(value = 3) {
  val outputSuffix: String = ".fir"
}

/** High Form
  *
  * As detailed in the Firrtl specification
  * [[https://github.com/ucb-bar/firrtl/blob/master/spec/spec.pdf]]
  *
  * Also see [[firrtl.ir]]
  */
@deprecated(
  "Mix-in the DependencyAPIMigration trait into your Transform and specify its Dependency API dependencies. See: https://bit.ly/2Voppre",
  "FIRRTL 1.3")
final case object HighForm extends CircuitForm(2) {
  val outputSuffix: String = ".hi.fir"
}

/** Middle Form
  *
  * A "lower" form than [[HighForm]] with the following restrictions:
  *  - All widths must be explicit
  *  - All whens must be removed
  *  - There can only be a single connection to any element
  */
@deprecated(
  "Mix-in the DependencyAPIMigration trait into your Transform and specify its Dependency API dependencies. See: https://bit.ly/2Voppre",
  "FIRRTL 1.3")
final case object MidForm extends CircuitForm(1) {
  val outputSuffix: String = ".mid.fir"
}

/** Low Form
  *
  * The "lowest" form. In addition to the restrictions in [[MidForm]]:
  *  - All aggregate types (vector/bundle) must have been removed
  *  - All implicit truncations must be made explicit
  */
@deprecated(
  "Mix-in the DependencyAPIMigration trait into your Transform and specify its Dependency API dependencies. See: https://bit.ly/2Voppre",
  "FIRRTL 1.3")
final case object LowForm extends CircuitForm(0) {
  val outputSuffix: String = ".lo.fir"
}

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
@deprecated(
  "Mix-in the DependencyAPIMigration trait into your Transform and specify its Dependency API dependencies. See: https://bit.ly/2Voppre",
  "FIRRTL 1.3")
final case object UnknownForm extends CircuitForm(-1) {
  override def compare(that: CircuitForm): Int = { sys.error("Illegal to compare UnknownForm"); 0 }

  val outputSuffix: String = ".unknown.fir"
}

// Internal utilities to keep code DRY, not a clean interface
private[firrtl] object Transform {

  // Run transform with logging
  def runTransform(name: String, mk: => CircuitState, logger: Logger): CircuitState = {
    logger.info(s"======== Starting Transform $name ========")

    val (timeMillis, result) = Utils.time(mk)

    logger.info(s"""----------------------------${"-" * name.size}---------\n""")
    logger.info(f"Time: $timeMillis%.1f ms")

    result
  }

  def remapAnnotations(name: String, before: CircuitState, after: CircuitState, logger: Logger): CircuitState = {
    val remappedAnnotations = propagateAnnotations(name, logger, before.annotations, after.annotations, after.renames)

    logger.info(s"Form: ${after.form}")
    logger.trace(s"Annotations:")
    logger.trace {
      JsonProtocol.serializeTry(remappedAnnotations).recoverWith {
        case NonFatal(e) =>
          val msg = s"Exception thrown during Annotation serialization:\n  " +
                    e.toString.replaceAll("\n", "\n  ")
          Try(msg)
      }.get
    }

    logger.trace(s"Circuit:\n${after.circuit.serialize}")
    logger.info(s"======== Finished Transform $name ========\n")

    CircuitState(after.circuit, after.form, remappedAnnotations, None)
  }

  /** Propagate annotations and update their names.
    *
    * @param inAnno input AnnotationSeq
    * @param resAnno result AnnotationSeq
    * @param renameOpt result RenameMap
    * @return the updated annotations
    */
  def propagateAnnotations(
      name: String,
      logger: Logger,
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

/** The basic unit of operating on a Firrtl AST */
trait Transform extends TransformLike[CircuitState] with DependencyAPI[Transform] {

  /** A convenience function useful for debugging and error messages */
  def name: String = this.getClass.getName

  /** The [[firrtl.CircuitForm]] that this transform requires to operate on */
  @deprecated(
    "Use Dependency API methods for equivalent functionality. See: https://bit.ly/2Voppre",
    "FIRRTL 1.3")
  def inputForm: CircuitForm

  /** The [[firrtl.CircuitForm]] that this transform outputs */
  @deprecated(
    "Use Dependency API methods for equivalent functionality. See: https://bit.ly/2Voppre",
    "FIRRTL 1.3")
  def outputForm: CircuitForm

  /** Perform the transform, encode renaming with RenameMap, and can
    *   delete annotations
    * Called by [[runTransform]].
    *
    * @param state Input Firrtl AST
    * @return A transformed Firrtl AST
    */
  protected def execute(state: CircuitState): CircuitState

  def transform(state: CircuitState): CircuitState = execute(state)

  import firrtl.{ChirrtlForm => C, HighForm => H, MidForm => M, LowForm => L, UnknownForm => U}

  override def prerequisites: Seq[Dependency[Transform]] = inputForm match {
    case C => Nil
    case H => Forms.Deduped
    case M => Forms.MidForm
    case L => Forms.LowForm
    case U => Nil
  }

  override def optionalPrerequisites: Seq[Dependency[Transform]] = inputForm match {
    case L => Forms.LowFormOptimized ++ Forms.AssertsRemoved
    case _ => Seq.empty
  }

  private lazy val fullCompilerSet = new mutable.LinkedHashSet[Dependency[Transform]] ++ Forms.VerilogOptimized

  override def optionalPrerequisiteOf: Seq[Dependency[Transform]] = {
    val lowEmitters = Dependency[LowFirrtlEmitter] :: Dependency[VerilogEmitter] :: Dependency[MinimumVerilogEmitter] ::
      Dependency[SystemVerilogEmitter] :: Nil

    val emitters = inputForm match {
      case C => Dependency[ChirrtlEmitter]      :: Dependency[HighFirrtlEmitter]   :: Dependency[MiddleFirrtlEmitter] :: lowEmitters
      case H => Dependency[HighFirrtlEmitter]   :: Dependency[MiddleFirrtlEmitter] :: lowEmitters
      case M => Dependency[MiddleFirrtlEmitter] :: lowEmitters
      case L => lowEmitters
      case U => Nil
    }

    val selfDep = Dependency.fromTransform(this)

    inputForm match {
      case C => (fullCompilerSet                           ++ emitters - selfDep).toSeq
      case H => (fullCompilerSet -- Forms.Deduped          ++ emitters - selfDep).toSeq
      case M => (fullCompilerSet -- Forms.MidForm          ++ emitters - selfDep).toSeq
      case L => (fullCompilerSet -- Forms.LowFormOptimized ++ emitters - selfDep).toSeq
      case U => Nil
    }
  }

  private lazy val highOutputInvalidates = fullCompilerSet -- Forms.MinimalHighForm
  private lazy val midOutputInvalidates = fullCompilerSet -- Forms.MidForm

  override def invalidates(a: Transform): Boolean = {
    (inputForm, outputForm) match {
      case (U, _) | (_, U)  => true  // invalidate everything
      case (i, o) if i >= o => false // invalidate nothing
      case (_, C)           => true  // invalidate everything
      case (_, H)           => highOutputInvalidates(Dependency.fromTransform(a))
      case (_, M)           => midOutputInvalidates(Dependency.fromTransform(a))
      case (_, L)           => false // invalidate nothing
    }
  }

  /** Convenience method to get annotations relevant to this Transform
    *
    * @param state The [[CircuitState]] form which to extract annotations
    * @return A collection of annotations
    */
  @deprecated("Just collect the actual Annotation types the transform wants", "1.1")
  final def getMyAnnotations(state: CircuitState): Seq[Annotation] = {
    val msg = "getMyAnnotations is deprecated, use collect and match on concrete types"
    StageUtils.dramaticWarning(msg)
    state.annotations.collect { case a: LegacyAnnotation if a.transform == this.getClass => a }
  }

  /** Executes before any transform's execute method
    * @param state
    * @return
    */
  private[firrtl] def prepare(state: CircuitState): CircuitState = state

  /** Perform the transform and update annotations.
    *
    * @param state Input Firrtl AST
    * @return A transformed Firrtl AST
    */
  final def runTransform(state: CircuitState): CircuitState = {
    val result = Transform.runTransform(name, execute(prepare(state)), logger)
    Transform.remapAnnotations(name, state, result, logger)
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

  override def invalidates(a: Transform) = false

  /** An output suffix to use if the output of this [[Emitter]] was written to a file */
  def outputSuffix: String
}