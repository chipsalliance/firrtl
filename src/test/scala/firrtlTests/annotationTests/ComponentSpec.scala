package firrtlTests.annotationTests

import firrtl.annotations.Component
import firrtl.annotations.SubComponent._
import firrtlTests.FirrtlPropSpec

class ComponentSpec extends FirrtlPropSpec {
  def check(comp: Component): Unit = {
    val named = Component.convertComponent2Named(comp)
    println(named)
    val comp2 = Component.convertNamed2Component(named)
    assert(comp == comp2)
  }
  property("Should convert to/from Named") {
    check(Component(Some("Top"), None, Nil, None))
    check(Component(Some("Top"), Some("Top"), Nil, None))
    check(Component(Some("Top"), Some("Other"), Nil, None))
    val r1 = Seq(Ref("r1"), Field("I"))
    val r2 = Seq(Ref("r2"), Index(0))
    check(Component(Some("Top"), Some("Top"), r1, None))
    check(Component(Some("Top"), Some("Top"), r2, None))
  }
  property("Should enable creating from API") {
    val top = Component(Some("Top"), Some("Top"), Nil, None)
    val x_reg0_data = top.inst("x").of("X").ref("reg0").field("data")
    top.inst("x").inst("x")
    top.ref("y")
    println(x_reg0_data)
  }
}

