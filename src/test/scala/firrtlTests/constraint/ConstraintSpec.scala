package firrtlTests.constraint

import org.scalatest.{FlatSpec, Matchers}
import firrtl.passes.{IsMax, IsMin, VarCon, IsAdd, IsMul, IsNeg, IsKnown}
import firrtl.ir.{Closed}

class ConstraintSpec extends FlatSpec with Matchers {

  behavior of "Constraints"

  "IsConstraints" should "reduce properly" in {
    IsMin(Closed(0), Closed(1)).optimize() should be (Closed(0))
    IsMin(Closed(-1), Closed(1)).optimize() should be (Closed(-1))
    IsMax(Closed(-1), Closed(1)).optimize() should be (Closed(1))
    IsNeg(IsMul(Closed(-1), Closed(-2))).optimize() should be (Closed(-2))
    IsMin(IsMul(Closed(1), VarCon("a")), Closed(2)).optimize() should be (IsMin(Closed(2), IsMul(Closed(1), VarCon("a"))))
  }
  "IsMul" should "not reduce with min/max" in {
    IsMul(VarCon("a"), IsMax(VarCon("x"), VarCon("y"))).optimize() should be (IsMul(VarCon("a"), IsMax(VarCon("x"), VarCon("y"))))
    IsMul(VarCon("a"), IsMin(VarCon("x"), VarCon("y"))).optimize() should be (IsMul(VarCon("a"), IsMin(VarCon("x"), VarCon("y"))))
  }
}

