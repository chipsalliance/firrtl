package fix

import firrtl.{Transform, CircuitState, LowForm, UnknownForm}

class NoOverrides extends Transform {
  override def inputForm = LowForm
  override def outputForm = LowForm
  override def execute(state: CircuitState): CircuitState = state
}

class HasOverrides extends Transform {
  override def inputForm = LowForm
  override def outputForm = LowForm
  override def execute(state: CircuitState): CircuitState = state
}

class HasOtherModifiers extends Transform {
  override final def inputForm = LowForm
  override final def outputForm = LowForm
  override def execute(state: CircuitState): CircuitState = state
}

class HasAllModifiers extends Transform {
  override final def inputForm = LowForm
  final override def outputForm = LowForm
  override def execute(state: CircuitState): CircuitState = state
}

class HasVals extends Transform {
  override val inputForm = LowForm
  override val outputForm = LowForm
  override def execute(state: CircuitState): CircuitState = state
}

object ObjTransform extends Transform {
  override val inputForm = LowForm
  override val outputForm = LowForm
  override def execute(state: CircuitState): CircuitState = state
}

abstract class SubClass extends Transform {
  override def execute(state: CircuitState): CircuitState = state
}
class SubSubClass extends SubClass {
  override val inputForm = LowForm
  override val outputForm = LowForm
  val otherVal = 10
  def otherDef = 10
}
