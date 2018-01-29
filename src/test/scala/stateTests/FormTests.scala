package stateTests

import firrtl._
import firrtl.form._
import org.scalatest._
import org.scalatest.prop._

class FormTests extends FlatSpec with Matchers {
  behavior of "CircuitForm"

  it should "update properly" in {
    CircuitForm(IR).update(CircuitForm(NotForm(IR))) should be(CircuitForm(NotForm(IR)))
    CircuitForm(IR, WIR).update(CircuitForm(NotForm(IR), WIR)) should be(CircuitForm(NotForm(IR), WIR))
    CircuitForm(Resolved).update(CircuitForm(NotForm(Genders), NotForm(Types), NotForm(Kinds), NotForm(Widths))) should be(CircuitForm(WIR, NotForm(IR), NotForm(Types), NotForm(Genders), NotForm(Kinds), NotForm(Widths)))
    CircuitForm(Genders).update(UnknownForm) should be(CircuitForm(Genders))
  }

  it should "contains properly" in {
    CircuitForm(IR).contains(CircuitForm(NotForm(IR))) should be(false)
    CircuitForm(IR).contains(CircuitForm(IR)) should be(true)
    CircuitForm(IR, CircuitForm(Types, Widths)).contains(CircuitForm(IR, Types)) should be(true)
  }

  it should "NotForm base elements properly" in {
    NotForm(NotForm(IR)).featureElements should be(Set(IR))
    NotForm(NotForm(IR), WIR).featureElements should be(Set(IR, NotForm(WIR)))
    CircuitForm(NotForm(NotForm(IR), WIR)).featureElements should be(Set(IR, NotForm(WIR)))
  }

  it should "Have nice descriptions" in {
    println(LowForm.description)
  }
}
