package firrtlTests.constraint

import firrtl.constraint._
import org.scalatest.{FlatSpec, Matchers}
import firrtl.ir.Closed

class InequalitySpec extends FlatSpec with Matchers {

  behavior of "Constraints"

  "IsConstraints" should "reduce properly" in {
    IsMin(Closed(0), Closed(1)) should be (Closed(0))
    IsMin(Closed(-1), Closed(1)) should be (Closed(-1))
    IsMax(Closed(-1), Closed(1)) should be (Closed(1))
    IsNeg(IsMul(Closed(-1), Closed(-2))) should be (Closed(-2))
    val x = IsMin(IsMul(Closed(1), VarCon("a")), Closed(2))
    x.getChildren.toSet should be (IsMin(Closed(2), IsMul(Closed(1), VarCon("a"))).getChildren.toSet)
  }
  "IsMul" should "not reduce with min/max" in {
    val isMax = IsMax(VarCon("x"), VarCon("y"))
    val isMin = IsMin(VarCon("x"), VarCon("y"))
    val a = VarCon("a")
    IsMul(a, isMax).getChildren should be (Seq(isMax, a))
    IsMul(a, isMin).getChildren should be (Seq(isMin, a))

    IsMul(Closed(0), VarCon("x")) should be (Closed(0))
  }
}

