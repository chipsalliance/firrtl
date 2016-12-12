// See LICENSE for license details.

package firrtlTests

import org.scalatest.Matchers
import java.io.{StringWriter,Writer}
import firrtl.ir.Circuit
import firrtl._
import firrtl.Parser.IgnoreInfo
import firrtl.Parser
import firrtl.passes._

class ZeroWidthTests extends FirrtlFlatSpec {
  val passes = Seq(
      ToWorkingIR,
      ResolveKinds,
      InferTypes,
      ResolveGenders,
      InferWidths,
      ZeroWidth)
  private def exec (input: String) = {
    passes.foldLeft(parse(input)) {
      (c: Circuit, p: Pass) => p.run(c)
    }.serialize
  }
   // =============================
  "Zero width port" should " be deleted" in {
    val input =
      """circuit Top :
        |  module Top :
        |    input y : UInt<0>
        |    output x : UInt<1>
        |    x <= y""".stripMargin
    val check =
      """circuit Top :
         |  module Top :
         |    output x : UInt<1>
         |    x <= UInt<1>(0)""".stripMargin
      (parse(exec(input))) should be (parse(check))
  }
  "Add of <0> and <2> " should " be <3>" in {
    val input =
      """circuit Top :
        |  module Top :
        |    input y : UInt<0>
        |    output x : UInt
        |    x <= add(y, UInt<2>(2))""".stripMargin
    val check =
      """circuit Top :
        |  module Top :
        |    output x : UInt<3>
        |    x <= add(UInt<1>(0), UInt<2>(2))""".stripMargin
      (parse(exec(input)).serialize) should be (parse(check).serialize)
  }
  "Mux on <0>" should "constprop" in {
    val input =
      """circuit Top :
        |  module Top :
        |    input y : UInt<0>
        |    output x : UInt
        |    x <= mux(y, UInt<2>(2), UInt<2>(1))""".stripMargin
    val check =
      """circuit Top :
        |  module Top :
        |    output x : UInt<2>
        |    x <= UInt<2>(1)""".stripMargin
      (parse(exec(input)).serialize) should be (parse(check).serialize)
  }
}

class ZeroWidthVerilog extends FirrtlFlatSpec {
  private def executeTest(input: String, expected: Seq[String], compiler: Compiler) = {
    val writer = new StringWriter()
    compiler.compile(CircuitState(parse(input), ChirrtlForm), writer)
    val lines = writer.toString().split("\n") map normalized
    println(writer.toString())
    expected foreach { e =>
      lines should contain(e)
    }
  }
  "Circuit" should "accept zero width wires" in {
    val compiler = new VerilogCompiler
    val input =
      """circuit Top : 
         |  module Top : 
         |    input y: UInt<0>
         |    output x: UInt<3>
         |    x <= y""".stripMargin
    val check = 
      """module Top(
        |  output  [2:0] x
        |);
        |  assign x = 3'h0;
        |endmodule
        |""".stripMargin.split("\n") map normalized
    executeTest(input, check, compiler)
  }
}
