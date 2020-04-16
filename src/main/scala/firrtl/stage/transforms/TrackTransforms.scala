// See LICENSE for license details.

package firrtl.stage.transforms

import firrtl.{AnnotationSeq, CircuitState, Transform}
import firrtl.annotations.NoTargetAnnotation
import firrtl.options.{Dependency, DependencyManagerException}
import firrtl.stage.CircuitPhase

case class TransformHistoryAnnotation(history: Seq[CircuitPhase], state: Set[CircuitPhase]) extends NoTargetAnnotation {

  def add(transform: CircuitPhase,
          invalidates: (CircuitPhase) => Boolean = (a: CircuitPhase) => false): TransformHistoryAnnotation =
    this.copy(
      history = transform +: this.history,
      state = (this.state + transform).filterNot(invalidates)
    )

}

object TransformHistoryAnnotation {

  def apply(transform: CircuitPhase): TransformHistoryAnnotation = TransformHistoryAnnotation(
    history = Seq(transform),
    state = Set(transform)
  )

}

class TrackTransforms(val underlying: CircuitPhase) extends CircuitPhase with WrappedTransform {

  private def updateState(annotations: AnnotationSeq): AnnotationSeq = {
    var foundAnnotation = false
    val annotationsx = annotations.map {
      case x: TransformHistoryAnnotation =>
        foundAnnotation = true
        x.add(trueUnderlying)
      case x => x
    }
    if (!foundAnnotation) {
      TransformHistoryAnnotation(trueUnderlying) +: annotationsx
    } else {
      annotationsx
    }
  }

  override def transform(c: CircuitState): CircuitState = {
    val state = c.annotations
      .collectFirst{ case TransformHistoryAnnotation(_, state) => state }
      .getOrElse(Set.empty[CircuitPhase])
      .map(Dependency.fromTransform(_))

    if (!trueUnderlying.prerequisites.toSet.subsetOf(state)) {
      throw new DependencyManagerException(
        s"""|Tried to execute Transform '$trueUnderlying' for which run-time prerequisites were not satisfied:
            |  state: ${state.mkString("\n    -", "\n    -", "")}
            |  prerequisites: ${trueUnderlying.prerequisites.mkString("\n    -", "\n    -", "")}""".stripMargin)
    }

    val out = underlying.transform(c)
    out.copy(annotations = updateState(out.annotations))
  }

}

object TrackTransforms {

  def apply(a: CircuitPhase) = new TrackTransforms(a)

}
