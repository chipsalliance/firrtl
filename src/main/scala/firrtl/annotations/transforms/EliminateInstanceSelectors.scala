package firrtl.annotations.transforms

import firrtl.analyses.IRLookup
import firrtl.{CircuitForm, CircuitState, MidForm, Transform}

/**
  * Resolves [[firrtl.annotations.Component]] instance selectors into concrete instance references
  */
class EliminateInstanceSelectors extends Transform {
  override def inputForm: CircuitForm = MidForm
  override def outputForm: CircuitForm = MidForm

  override protected def execute(state: CircuitState): CircuitState = {
    val irLookup = IRLookup(state)

    val annotations = state.annotations.collect { case a: AutoResolution => a }

    val targets = annotations.flatMap(_.targets)

    targets.foreach { t =>
      require(t.isLegal)
      require(t.isComplete)
    }

    targets.foreach { t =>
      irLookup
    }

    state
  }

}
