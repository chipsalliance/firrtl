// See LICENSE for license details.

package firrtlTests.transforms

import firrtl.annotations.TargetToken.Field
import firrtl.{ChirrtlForm, CircuitState}
import firrtl.transforms._
import firrtl.annotations._
import firrtlTests.MiddleAnnotationSpec


trait MemStuff {
  def commonFields: Seq[String] = Seq("clk", "en", "addr")
  def readerTargets(rt: ReferenceTarget): Seq[ReferenceTarget] = {
    (commonFields ++ Seq("data")).map(rt.field)
  }
  def writerTargets(rt: ReferenceTarget): Seq[ReferenceTarget] = {
    (commonFields ++ Seq("data", "mask")).map(rt.field)
  }
  def readwriterTargets(rt: ReferenceTarget): Seq[ReferenceTarget] = {
    (commonFields ++ Seq("wdata", "wmask", "wmode", "rdata")).map(rt.field)
  }
  def makeInput(readLatency: Int) =
    s"""circuit Test:
      |  module Test :
      |    input in : UInt<8>
      |    input clk: Clock[3]
      |    input dataClk: Clock
      |    input mode: UInt<1>
      |    output out : UInt<8>[2]
      |    mem m:
      |      data-type => UInt<8>
      |      reader => r
      |      writer => w
      |      readwriter => rw
      |      depth => 2
      |      write-latency => 1
      |      read-latency => ${readLatency}
      |
      |    reg addr: UInt<1>, dataClk
      |    reg en: UInt<1>, dataClk
      |    reg indata: UInt<8>, dataClk
      |
      |    m.r.clk <= clk[0]
      |    m.r.en <= en
      |    m.r.addr <= addr
      |    out[0] <= m.r.data
      |
      |    m.w.clk <= clk[1]
      |    m.w.en <= en
      |    m.w.addr <= addr
      |    m.w.data <= indata
      |    m.w.mask <= en
      |
      |    m.rw.clk <= clk[2]
      |    m.rw.en <= en
      |    m.rw.addr <= addr
      |    m.rw.wdata <= indata
      |    m.rw.wmask <= en
      |    m.rw.wmode <= en
      |    out[1] <= m.rw.rdata
      |""".stripMargin

  val C = CircuitTarget("Test")
  val Test = C.module("Test")
  val Mem = Test.ref("m")
  val Reader = Mem.field("r")
  val Writer = Mem.field("w")
  val Readwriter = Mem.field("rw")
  val allSignals = readerTargets(Reader) ++ writerTargets(Writer) ++ readwriterTargets(Readwriter)
}

class FindClockSourcesSpec extends MiddleAnnotationSpec with MemStuff {
  def execute(input: String, annotations: Seq[Annotation], check: ClockSources, notCheck: Option[ClockSources]): Unit = {
    val cr = compile(CircuitState(parse(input), ChirrtlForm, annotations), Seq(new FindClockSources()))
    val signalToClocks = cr.annotations.flatMap {
      case c: ClockSources => c.signalToClocks
      case _ => Nil
    }.toMap
    check.signalToClocks.foreach { c =>
      signalToClocks should contain(c)
    }
    if(notCheck.nonEmpty) {
      notCheck.get.signalToClocks.foreach { c =>
        signalToClocks shouldNot contain(c)
      }
    }
  }

  def execute(input: String, annotations: Seq[Annotation], checks: Seq[Annotation], notChecks: Seq[Annotation]): Unit = {
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
      ClockSources(Map(
        Test.ref("out0") -> Set((Test.ref("clk"), None)),
        Test.ref("out1") -> Set((Test, Some("asClock$0"))),
        Test.ref("out2") -> Set((Test.instOf("clkdiv", "CLKDIV").ref("clkOut"), None))
      ))
    )

    execute(input, Seq(GetClockSources(Seq(Test))), clockSources, Nil)
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
    val clockSources = Seq(ClockSources(Map(Test.ref("out0") -> Set((Test.ref("clk"), None)))))
    val notClockSources = Seq(ClockSources(Map(Test.ref("out0") -> Set((Test, Some("asClock$0"))))))

    execute(input, Seq(GetClockSources(Seq(Test))), clockSources, notClockSources)
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
    val clockSources = Seq(ClockSources(Map(Test.ref("out0") -> Set((Test.ref("clk"), None)))))

    execute(input, Seq(GetClockSources(Seq(Test))), clockSources, Nil)
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
    val clockSources = Seq(ClockSources(Map(Test.ref("out0") -> Set((Test.ref("clk"), None)))))

    execute(input, Seq(GetClockSources(Seq(Test))), clockSources, Nil)
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
    val clockSources = Seq(ClockSources(Map(
      Test.ref("out0") -> Set((Test.ref("clk"), None)),
      Test.ref("out1") -> Set((Test.ref("clk"), None)),
      Test.ref("out2") -> Set((Test.ref("clk"), None))
    )))

    execute(input, Seq(GetClockSources(Seq(Test))), clockSources, Nil)
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
    val clockSources = Seq(ClockSources(Map(
      Test.ref("out0") -> Set((Test.ref("clk0"), None)),
      Test.ref("out1") -> Set((Test.ref("clk1"), None))
    )))

    execute(input, Seq(GetClockSources(Seq(Test))), clockSources, Nil)
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
    val clockSources = Seq(ClockSources(Map(
      Test.ref("out").field("f0") -> Set((Test.ref("clk0"), None)),
      Test.ref("out").field("f1") -> Set((Test.ref("clk1"), None))
    )))

    execute(input, Seq(GetClockSources(Seq(Test))), clockSources, Nil)
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
    val clockSources = Seq(ClockSources(Map(
      Test.ref("out") -> Set((Test.ref("clk0"), None), (Test.ref("clk1"), None))
    )))

    execute(input, Seq(GetClockSources(Seq(Test))), clockSources, Nil)
  }

  "Female mem port fields" should "just follow back to source" in {
    val input = makeInput(0)

    // Solutions
    val clockSources: Map[ReferenceTarget, Set[(IsMember, Option[String])]] =
      Map(
        Reader.field("clk") -> Set((Test.ref("clk").index(0).asInstanceOf[IsMember], Option.empty[String])),
        Writer.field("clk") -> Set((Test.ref("clk").index(1).asInstanceOf[IsMember], Option.empty[String])),
        Readwriter.field("clk") -> Set((Test.ref("clk").index(2).asInstanceOf[IsMember], Option.empty[String]))
      ) ++ allSignals.collect {
        case sig if sig.tokens.last != Field("clk") =>
          sig -> Set((Test.ref("dataClk").asInstanceOf[IsMember], Option.empty[String]))
      }

    execute(
      input,
      Seq(GetClockSources(allSignals)),
      ClockSources(clockSources),
      None
    )
  }

  "Data read from sequential memories" should "return clock domain of read/readwrite's enable and address" in {
    val input = makeInput(1)

    // Solutions
    val clockSources: Map[ReferenceTarget, Set[(IsMember, Option[String])]] =
      Map(
        Reader.field("data") -> Set((Test.ref("clk").index(0).asInstanceOf[IsMember], Option.empty[String])),
        Readwriter.field("rdata") -> Set((Test.ref("clk").index(2).asInstanceOf[IsMember], Option.empty[String]))
      )

    execute(input, Seq(GetClockSources(allSignals)), ClockSources(clockSources), None )

  }

  "Constant signals" should "be represented as such" in {
    val input =
      """circuit Test:
        |  module Test :
        |    output out: UInt<8>
        |    out <= UInt(1)
        |""".stripMargin

    val C = CircuitTarget("Test")
    val Test = C.module("Test")
    val out = Test.ref("out")
    val clockSources = Seq(ClockSources(Map(
      Test.ref("out") -> Set()
    )))

    execute(input, Seq(GetClockSources(Seq(out))), clockSources, Nil)
  }

  "Signals combinationally linked to inputs" should "be correctly worked" in {
    val input =
      """circuit Test:
        |  module Test :
        |    input clk: Clock
        |    output out: UInt<8>
        |    reg r: UInt<8>, clk
        |    inst child of Child
        |    child.in <= r
        |    out <= child.out
        |  module Child :
        |    input in: UInt<8>
        |    output out: UInt<8>
        |    out <= in
        |""".stripMargin

    val C = CircuitTarget("Test")
    val Test = C.module("Test")
    val Child = C.module("Child")
    val out = Test.ref("out")
    val clockSources = ClockSources(Map(
      Child.ref("out") -> Set((Child.ref("in"), None)),
      Test.ref("out") -> Set((Test.ref("clk"), None))
    ))
    execute(input, Seq(GetClockSources(Seq(Child, Test))), clockSources, None)
  }

  "Caching of topological sorting of modules" should "work" in {
    def mkChild(n: Int): String =
      s"""  module Child${n} :
         |    input in: UInt<8>
         |    output out: UInt<8>
         |    inst c1 of Child${n+1}
         |    inst c2 of Child${n+1}
         |    c1.in <= in
         |    c2.in <= c1.out
         |    out <= c2.out
       """.stripMargin
    def mkLeaf(n: Int): String =
      s"""  module Child${n} :
         |    input in: UInt<8>
         |    output out: UInt<8>
         |    out <= in
       """.stripMargin

    (2 until 13 by 2).foreach { n =>
      val input = new StringBuilder()
      input ++=
        """circuit Child0:
          |""".stripMargin
      println(s"Depth of $n:")
      (0 until n).foreach { i => input ++= mkChild(i); input ++= "\n" }
      input ++= mkLeaf(n)

      val C = CircuitTarget("Child0")
      val Child0 = C.module("Child0")
      val clockSources = ClockSources(Map(
        Child0.ref("out") -> Set((Child0.ref("in"), None))
      ))
      print(s"\tWith caching: ")
      val (timeWithCaching, dc2) = firrtl.Utils.time {
        execute(input.toString(), Seq(GetClockSources((0 to n).map(i => C.module(s"Child${i}")))), clockSources, None)
      }
      //println(s"\tWith caching: $timeWithCaching")
      print(s"\tNo caching: ")
      val (timeNoCaching, dc) = firrtl.Utils.time{
        execute(input.toString(), Seq(GetClockSources(Seq(Child0.ref("out")))), clockSources, None)
      }
      //println(s"\tNo caching: $timeNoCaching")
    }
  }



  // Check bundled nodes
  // Check cache works of topological sorting of modules for all signals
  // Check all IRLookup functions
}

