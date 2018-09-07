package firrtlTests.annotationTests

import firrtl.annotations.{ModuleTarget, Target}
import firrtl.annotations.TargetToken._
import firrtlTests.FirrtlPropSpec

class TargetSpec extends FirrtlPropSpec {
  def check(comp: Target): Unit = {
    val named = Target.convertTarget2Named(comp)
    println(named)
    val comp2 = Target.convertNamed2Target(named)
    assert(comp == comp2)
  }
  property("Should convert to/from Named") {
    check(Target(Some("Top"), None, Nil))
    check(Target(Some("Top"), Some("Top"), Nil))
    check(Target(Some("Top"), Some("Other"), Nil))
    val r1 = Seq(Ref("r1"), Field("I"))
    val r2 = Seq(Ref("r2"), Index(0))
    check(Target(Some("Top"), Some("Top"), r1))
    check(Target(Some("Top"), Some("Top"), r2))
  }
  property("Should enable creating from API") {
    val top = ModuleTarget("Top","Top")
    val x_reg0_data = top.instOf("x", "X").ref("reg0").field("data")
    top.instOf("x", "x")
    top.ref("y")
    println(x_reg0_data)
  }
}

