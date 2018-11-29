// See LICENSE for license details.

package firrtlTests.transforms

import firrtl.{ChirrtlForm, CircuitState}
import firrtl.transforms.{ClockSource, FindClockSources, GroupAnnotation, GroupComponents}
import firrtl.annotations._
import firrtlTests.MiddleAnnotationSpec


class FindClockSourcesSpec extends MiddleAnnotationSpec {
  def execute(input: String, checks: Seq[Annotation], notChecks: Seq[Annotation], annotations: Seq[Annotation]): Unit = {
    val cr = compile(CircuitState(parse(input), ChirrtlForm, annotations), Seq(new FindClockSources()))
    checks.foreach { c =>
      cr.annotations.toSeq should contain (c)
    }
    notChecks.foreach { c =>
      cr.annotations.toSeq shouldNot contain (c)
    }
  }
  "test" should "do stuff" in {
    val input =
      """circuit Test:
        |  module Test :
        |    input in : UInt<8>
        |    input clk: Clock
        |    output out : UInt<8>
        |    inst a1 of A
        |    a1.clk <= asClock(UInt(1))
        |    inst a2 of A
        |    a2.clk <= clk
        |    inst b1 of B
        |    b1.clkin <= UInt(1)
        |    inst b2 of B
        |    b2.clkin <= UInt(1)
        |    inst c1 of C
        |    c1.clk <= clk
        |    inst c2 of C
        |    c2.clk <= clk
        |    a1.in <= in
        |    a2.in <= a1.out
        |    b1.in <= a2.out
        |    b2.in <= b1.out
        |    c1.in <= b2.out
        |    c2.in <= c1.out
        |    out <= c2.out
        |  module A :
        |    input in: UInt<8>
        |    input clk: Clock
        |    output out: UInt<8>
        |    reg r : UInt<8>, clk
        |    r <= in
        |    out <= r
        |  module B :
        |    input in: UInt<8>
        |    input clkin: UInt<1>
        |    output out: UInt<8>
        |    reg r : UInt<8>, asClock(clkin)
        |    r <= in
        |    out <= r
        |  module C :
        |    input in: UInt<8>
        |    input clk: Clock
        |    output out: UInt<8>
        |    inst clkdiv of CLKDIV
        |    clkdiv.clk <= clk
        |    reg r : UInt<8>, clkdiv.clk_2
        |    r <= in
        |    out <= r
        |  extmodule CLKDIV:
        |    input clk: Clock
        |    output clk_2: Clock
        |""".stripMargin


    //TODO(azidar): check this
    execute(input, Nil, Nil, Nil)
  }

  "All clock source types" should "be collected" in {
    val input =
      """circuit Test:
        |  module Test :
        |    input in : UInt<8>
        |    input clk: Clock
        |    output out0 : UInt<8>
        |    output out1 : UInt<8>
        |    output out2 : UInt<8>
        |    reg r0: UInt<8>, clk
        |    reg r1: UInt<8>, asClock(bits(in, 1, 0))
        |    inst clkdiv of CLKDIV
        |    clkdiv.clk <= clk
        |    reg r2: UInt<8>, clkdiv.clkOut
        |
        |    r0 <= in
        |    r1 <= in
        |    r2 <= in
        |
        |    out0 <= r0
        |    out1 <= r1
        |    out2 <= r2
        |  extmodule CLKDIV:
        |    input clk: Clock
        |    output clkOut: Clock
        |""".stripMargin

    val C = CircuitTarget("Test")
    val Test = C.module("Test")
    val out = Test.ref("out")
    val clockSources = Seq(
      ClockSource(Seq(Test.ref("out0")), Test.ref("clk"), None),
      ClockSource(Seq(Test.ref("out1")), Test, Some("asClock$0")),
      ClockSource(Seq(Test.ref("out2")), Test.instOf("clkdiv", "CLKDIV").ref("clkOut"), None)
    )

    execute(input, clockSources, Nil, Nil)
  }

  "Clock source search" should "not pass through registers" in {
    val input =
      """circuit Test:
        |  module Test :
        |    input in : UInt<8>
        |    input clk: Clock
        |    output out0 : UInt<8>
        |    reg r0: UInt<8>, clk
        |    reg r1: UInt<8>, asClock(bits(in, 1, 0))
        |
        |    r1 <= in
        |    r0 <= r1
        |    out0 <= r0
        |""".stripMargin

    val C = CircuitTarget("Test")
    val Test = C.module("Test")
    val out = Test.ref("out")
    val clockSources = Seq( ClockSource(Seq(Test.ref("out0")), Test.ref("clk"), None) )
    val notClockSources = Seq( ClockSource(Seq(Test.ref("out0")), Test, Some("asClock$0")) )

    execute(input, clockSources, notClockSources, Nil)
  }

  "Clock source search" should "go through child instances" in {
    val input =
      """circuit Test:
        |  module Test :
        |    input in : UInt<8>
        |    input clk: Clock
        |    output out0 : UInt<8>
        |    inst cm of ClockModule
        |    cm.clk <= clk
        |    reg r0: UInt<8>, cm.clkOut
        |    r0 <= in
        |    out0 <= r0
        |  module ClockModule:
        |    input clk: Clock
        |    output clkOut: Clock
        |    clkOut <= clk
        |""".stripMargin

    val C = CircuitTarget("Test")
    val Test = C.module("Test")
    val out = Test.ref("out")
    val clockSources = Seq( ClockSource(Seq(Test.ref("out0")), Test.ref("clk"), None) )

    execute(input, clockSources, Nil, Nil)
  }

  "Clock source search" should "go through parent instances" in {
    val input =
      """circuit Test:
        |  module Test :
        |    input in : UInt<8>
        |    input clk: Clock
        |    output out0 : UInt<8>
        |    inst cm of ClockModule
        |    cm.clk <= clk
        |    reg r0: UInt<8>, cm.clkOut
        |    r0 <= in
        |    out0 <= r0
        |  module ClockModule:
        |    input clk: Clock
        |    output clkOut: Clock
        |    clkOut <= clk
        |""".stripMargin

    val C = CircuitTarget("Test")
    val Test = C.module("Test")
    val out = Test.ref("out")
    val clockSources = Seq( ClockSource(Seq(Test.ref("out0")), Test.ref("clk"), None) )

    execute(input, clockSources, Nil, Nil)
  }

  "Clocks of aggregate registers" should "still work" in {
    val input =
      """circuit Test:
        |  module Test :
        |    input in : UInt<8>
        |    input clk: Clock
        |    output out0 : UInt<8>
        |    output out1 : UInt<8>
        |    output out2 : UInt<8>
        |    reg r0: {f: UInt<8>, v: UInt<8>[2]}, clk
        |    r0.f <= in
        |    r0.v[0] <= in
        |    r0.v[1] <= in
        |    out0 <= r0.f
        |    out1 <= r0.v[0]
        |    out2 <= r0.v[1]
        |  module ClockModule:
        |    input clk: Clock
        |    output clkOut: Clock
        |    clkOut <= clk
        |""".stripMargin

    val C = CircuitTarget("Test")
    val Test = C.module("Test")
    val out = Test.ref("out")
    val clockSources = Seq( ClockSource(Seq(
      Test.ref("out0"),
      Test.ref("out1"),
      Test.ref("out2")), Test.ref("clk"), None) )

    execute(input, clockSources, Nil, Nil)
  }

  "Clocks of aggregate wires" should "still work" in {
    val input =
      """circuit Test:
        |  module Test :
        |    input in : UInt<8>
        |    input clk0: Clock
        |    input clk1: Clock
        |    output out0 : UInt<8>
        |    output out1 : UInt<8>
        |    reg r0: UInt<8>, clk0
        |    reg r1: UInt<8>, clk1
        |    r0 <= in
        |    r1 <= in
        |    wire x: {f0: UInt<8>, f1: UInt<8>}
        |    x.f0 <= r0
        |    x.f1 <= r1
        |    out0 <= x.f0
        |    out1 <= x.f1
        |""".stripMargin

    val C = CircuitTarget("Test")
    val Test = C.module("Test")
    val out = Test.ref("out")
    val clockSources = Seq(
      ClockSource(Seq( Test.ref("out0")), Test.ref("clk0"), None),
      ClockSource(Seq( Test.ref("out1")), Test.ref("clk1"), None)
    )

    execute(input, clockSources, Nil, Nil)
  }

  "Outputs of aggregate wires" should "still work" in {
    val input =
      """circuit Test:
        |  module Test :
        |    input in : UInt<8>
        |    input clk0: Clock
        |    input clk1: Clock
        |    output out : {f0: UInt<8>, f1: UInt<8>}
        |    reg r0: UInt<8>, clk0
        |    reg r1: UInt<8>, clk1
        |    r0 <= in
        |    r1 <= in
        |    out.f0 <= r0
        |    out.f1 <= r1
        |""".stripMargin

    val C = CircuitTarget("Test")
    val Test = C.module("Test")
    val out = Test.ref("out")
    val clockSources = Seq(
      ClockSource(Seq( Test.ref("out").field("f0")), Test.ref("clk0"), None),
      ClockSource(Seq( Test.ref("out").field("f1")), Test.ref("clk1"), None)
    )

    execute(input, clockSources, Nil, Nil)
  }


  "Multiple clock sources" should "be detected" in {
    val input =
      """circuit Test:
        |  module Test :
        |    input in : UInt<8>
        |    input clk0: Clock
        |    input clk1: Clock
        |    output out : UInt<8>
        |    reg r0: UInt<8>, clk0
        |    reg r1: UInt<8>, clk1
        |    out <= and(r0, r1)
        |""".stripMargin

    val C = CircuitTarget("Test")
    val Test = C.module("Test")
    val out = Test.ref("out")
    val clockSources = Seq(
      ClockSource(Seq( Test.ref("out")), Test.ref("clk0"), None),
      ClockSource(Seq( Test.ref("out")), Test.ref("clk1"), None)
    )

    execute(input, clockSources, Nil, Nil)
  }

  // Memories
  // Check renaming module target of asClock target
  // Check cache works of topological sorting of modules for all signals
}

