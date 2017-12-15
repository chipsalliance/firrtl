// See LICENSE for license details.

package firrtlTests

import java.io._
import org.scalatest._
import org.scalatest.prop._
import firrtl._
import firrtl.ir.Circuit
import firrtl.passes._
import firrtl.transforms._
import firrtl.Parser.IgnoreInfo

class UnitTests extends FirrtlFlatSpec {
  private def executeTest(input: String, expected: Seq[String], transforms: Seq[Transform]) = {
    val c = transforms.foldLeft(CircuitState(parse(input), UnknownForm)) {
      (c: CircuitState, t: Transform) => t.runTransform(c)
    }.circuit

    val lines = c.serialize.split("\n") map normalized

    expected foreach { e =>
      lines should contain(e)
    }
  }

  "Pull muxes" should "not be exponential in runtime" in {
    val passes = Seq(
      ToWorkingIR,
      CheckHighForm,
      ResolveKinds,
      InferTypes,
      CheckTypes,
      PullMuxes)
    val input =
      """circuit Unit :
        |  module Unit :
        |    input _2: UInt<1>
        |    output x: UInt<32>
        |    x <= cat(_2, cat(_2, cat(_2, cat(_2, cat(_2, cat(_2, cat(_2, cat(_2, cat(_2, cat(_2, cat(_2, cat(_2, cat(_2, cat(_2, cat(_2, cat(_2, cat(_2, cat(_2, cat(_2, cat(_2, cat(_2, cat(_2, cat(_2, cat(_2, cat(_2, cat(_2, cat(_2, cat(   _2, cat(_2, cat(_2, cat(_2, _2)))))))))))))))))))))))))))))))""".stripMargin
    passes.foldLeft(parse(input)) {
      (c: Circuit, p: Pass) => p.run(c)
    }
  }

  "Connecting bundles of different types" should "throw an exception" in {
    val passes = Seq(
      ToWorkingIR,
      CheckHighForm,
      ResolveKinds,
      InferTypes,
      CheckTypes)
    val input =
      """circuit Unit :
        |  module Unit :
        |    input y: {a : UInt<1>}
        |    output x: {a : UInt<1>, b : UInt<1>}
        |    x <= y""".stripMargin
    intercept[CheckTypes.InvalidConnect] {
      passes.foldLeft(parse(input)) {
        (c: Circuit, p: Pass) => p.run(c)
      }
    }
  }

  "Initializing a register with a different type" should "throw an exception" in {
    val passes = Seq(
      ToWorkingIR,
      CheckHighForm,
      ResolveKinds,
      InferTypes,
      CheckTypes)
    val input =
     """circuit Unit :
       |  module Unit :
       |    input clock : Clock
       |    input reset : UInt<1>
       |    wire x : { valid : UInt<1> }
       |    reg y : { valid : UInt<1>, bits : UInt<3> }, clock with :
       |      reset => (reset, x)""".stripMargin
    intercept[CheckTypes.InvalidRegInit] {
      passes.foldLeft(parse(input)) {
        (c: Circuit, p: Pass) => p.run(c)
      }
    }
  }

  "Partial connection two bundle types whose relative flips don't match but leaf node directions do" should "connect correctly" in {
    val passes = Seq(
      ToWorkingIR,
      CheckHighForm,
      ResolveKinds,
      InferTypes,
      CheckTypes,
      ExpandConnects)
    val input =
     """circuit Unit :
       |  module Unit :
       |    wire x : { flip a: { b: UInt<32> } }
       |    wire y : { a: { flip b: UInt<32> } }
       |    x <- y""".stripMargin
    val check =
     """circuit Unit :
       |  module Unit :
       |    wire x : { flip a: { b: UInt<32> } }
       |    wire y : { a: { flip b: UInt<32> } }
       |    y.a.b <= x.a.b""".stripMargin
    val c_result = passes.foldLeft(parse(input)) {
      (c: Circuit, p: Pass) => p.run(c)
    }
    val writer = new StringWriter()
    (new HighFirrtlEmitter).emit(CircuitState(c_result, HighForm), writer)
    (parse(writer.toString())) should be (parse(check))
  }

  val splitExpTestCode =
     """
       |circuit Unit :
       |  module Unit :
       |    input a : UInt<1>
       |    input b : UInt<2>
       |    input c : UInt<2>
       |    output out : UInt<1>
       |    out <= bits(mux(a, b, c), 0, 0)
       |""".stripMargin

  "Emitting a nested expression" should "throw an exception" in {
    val passes = Seq(
      ToWorkingIR,
      InferTypes,
      ResolveKinds)
    intercept[PassException] {
      val c = Parser.parse(splitExpTestCode.split("\n").toIterator)
      val c2 = passes.foldLeft(c)((c, p) => p run c)
      val writer = new StringWriter()
      (new VerilogEmitter).emit(CircuitState(c2, LowForm), writer)
    }
  }

  "After splitting, emitting a nested expression" should "compile" in {
    val passes = Seq(
      ToWorkingIR,
      SplitExpressions,
      InferTypes)
    val c = Parser.parse(splitExpTestCode.split("\n").toIterator)
    val c2 = passes.foldLeft(c)((c, p) => p run c)
      val writer = new StringWriter()
      (new VerilogEmitter).emit(CircuitState(c2, LowForm), writer)
  }

  "Simple compound expressions" should "be split" in {
    val passes = Seq(
      ToWorkingIR,
      ResolveKinds,
      InferTypes,
      ResolveGenders,
      InferWidths,
      SplitExpressions
    )
    val input =
      """circuit Top :
         |  module Top :
         |    input a : UInt<32>
         |    input b : UInt<32>
         |    input d : UInt<32>
         |    output c : UInt<1>
         |    c <= geq(add(a, b),d)""".stripMargin
    val check = Seq(
      "node _GEN_0 = add(a, b)",
      "c <= geq(_GEN_0, d)"
    )
    executeTest(input, check, passes)
  }

  "Smaller widths" should "be explicitly padded" in {
    val passes = Seq(
      ToWorkingIR,
      ResolveKinds,
      InferTypes,
      ResolveGenders,
      InferWidths,
      PadWidths
    )
    val input =
      """circuit Top :
         |  module Top :
         |    input a : UInt<32>
         |    input b : UInt<20>
         |    input pred : UInt<1>
         |    output c : UInt<32>
         |    c <= mux(pred,a,b)""".stripMargin
     val check = Seq("c <= mux(pred, a, pad(b, 32))")
     executeTest(input, check, passes)
  }
  "Indexes into sub-accesses" should "be dealt with" in {
    val passes = Seq(
      ToWorkingIR,
      ResolveKinds,
      InferTypes,
      ResolveGenders,
      InferWidths,
      PullMuxes,
      ExpandConnects,
      RemoveAccesses,
      new ConstantPropagation
    )
    val input =
      """circuit AssignViaDeref : 
         |  module AssignViaDeref : 
         |    input clock : Clock
         |    input reset : UInt<1>
         |    output io : {a : UInt<8>, sel : UInt<1>}
         |
         |    io is invalid
         |    reg table : {a : UInt<8>}[2], clock
         |    reg otherTable : {a : UInt<8>}[2], clock
         |    otherTable[table[UInt<1>("h01")].a].a <= UInt<1>("h00")""".stripMargin
     //TODO(azidar): I realize this is brittle, but unfortunately there
     //  isn't a better way to test this pass
     val check = Seq(
       """wire _GEN_0 : { a : UInt<8>}""",
       """_GEN_0.a <= table[0].a""",
       """when UInt<1>("h1") :""",
       """_GEN_0.a <= table[1].a""",
       """wire _GEN_1 : UInt<8>""",
       """when eq(UInt<1>("h0"), _GEN_0.a) :""",
       """otherTable[0].a <= _GEN_1""",
       """when eq(UInt<1>("h1"), _GEN_0.a) :""",
       """otherTable[1].a <= _GEN_1""",
       """_GEN_1 <= UInt<1>("h0")"""
     )
     executeTest(input, check, passes)
  }

  "Oversized bit select" should "throw an exception" in {
    val passes = Seq(
      ToWorkingIR,
      ResolveKinds,
      InferTypes,
      ResolveGenders,
      InferWidths,
      CheckWidths)
    val input =
      """circuit Unit :
        |  module Unit :
        |    node x = bits(UInt(1), 100, 0)""".stripMargin
    intercept[CheckWidths.BitsWidthException] {
      passes.foldLeft(Parser.parse(input.split("\n").toIterator)) {
        (c: Circuit, p: Pass) => p.run(c)
      }
    }
  }

  "Oversized head select" should "throw an exception" in {
    val passes = Seq(
      ToWorkingIR,
      ResolveKinds,
      InferTypes,
      ResolveGenders,
      InferWidths,
      CheckWidths)
    val input =
      """circuit Unit :
        |  module Unit :
        |    node x = head(UInt(1), 100)""".stripMargin
    intercept[CheckWidths.HeadWidthException] {
      passes.foldLeft(Parser.parse(input.split("\n").toIterator)) {
        (c: Circuit, p: Pass) => p.run(c)
      }
    }
  }

  "Oversized tail select" should "throw an exception" in {
    val passes = Seq(
      ToWorkingIR,
      ResolveKinds,
      InferTypes,
      ResolveGenders,
      InferWidths,
      CheckWidths)
    val input =
      """circuit Unit :
        |  module Unit :
        |    node x = tail(UInt(1), 100)""".stripMargin
    intercept[CheckWidths.TailWidthException] {
      passes.foldLeft(Parser.parse(input.split("\n").toIterator)) {
        (c: Circuit, p: Pass) => p.run(c)
      }
    }
  }

  "Partial connecting incompatable types" should "throw an exception" in {
    val passes = Seq(
      ToWorkingIR,
      ResolveKinds,
      InferTypes,
      CheckTypes)
    val input =
      """circuit Unit :
        |  module Unit :
        |    input foo : { bar : UInt<32> }
        |    output bar : UInt<32>
        |    bar <- foo
        |""".stripMargin
    intercept[PassException] {
      passes.foldLeft(Parser.parse(input.split("\n").toIterator)) {
        (c: Circuit, p: Pass) => p.run(c)
      }
    }
  }

  "Conditional conection of clocks" should "throw an exception" in {
    val input =
      """circuit Unit :
        |  module Unit :
        |    input clock1 : Clock
        |    input clock2 : Clock
        |    input sel : UInt<1>
        |    output clock3 : Clock
        |    clock3 <= clock1
        |    when sel :
        |      clock3 <= clock2
        |""".stripMargin
    intercept[PassExceptions] { // Both MuxClock and InvalidConnect are thrown
      compileToVerilog(input)
    }
  }

  "Parsing SInts" should "work" in {
    val passes = Seq()
    val input =
      """circuit Unit :
        |  module Unit :
        |    node posSInt = SInt(13)
        |    node negSInt = SInt(-13)
        |    node posSIntString = SInt("h0d")
        |    node posSIntString2 = SInt("h+d")
        |    node posSIntString3 = SInt("hd")
        |    node negSIntString = SInt("h-d")
        |    node negSIntStringWidth = SInt<5>("h-d")
        |    node neg2 = SInt("h-2")
        |    node pos2 = SInt("h2")
        |    node neg1 = SInt("h-1")
        |""".stripMargin
    val expected = Seq(
      """node posSInt = SInt<5>("hd")""",
      """node negSInt = SInt<5>("h-d")""",
      """node posSIntString = SInt<5>("hd")""",
      """node posSIntString2 = SInt<5>("hd")""",
      """node posSIntString3 = SInt<5>("hd")""",
      """node negSIntString = SInt<5>("h-d")""",
      """node negSIntStringWidth = SInt<5>("h-d")""",
      """node neg2 = SInt<2>("h-2")""",
      """node pos2 = SInt<3>("h2")""",
      """node neg1 = SInt<1>("h-1")"""
    )
    executeTest(input, expected, passes)
  }
  "Verilog SInts" should "work" in {
    val passes = Seq()
    val input =
      """circuit Unit :
        |  module Unit :
        |    output posSInt : SInt
        |    output negSInt : SInt
        |    posSInt <= SInt(13)
        |    negSInt <= SInt(-13)
        |""".stripMargin
    val expected = Seq(
      """assign posSInt = 5'shd;""",
      """assign negSInt = -5'shd;"""
    )
    val out = compileToVerilog(input)
    val lines = out.split("\n") map normalized
    expected foreach { e =>
      lines should contain(e)
    }
  }
}
