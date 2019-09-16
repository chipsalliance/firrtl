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
  val transforms = Seq(
      ToWorkingIR,
      ResolveKinds,
      InferTypes,
      ResolveFlows,
      new InferWidths,
      ZeroWidth)
  private def exec (input: String) = {
    val circuit = parse(input)
    transforms.foldLeft(CircuitState(circuit, UnknownForm)) {
      (c: CircuitState, p: Transform) => p.runTransform(c)
    }.circuit.serialize
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
  "Add of <0> and <2> " should " put in zero" in {
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
  "Mux on <0>" should "put in zero" in {
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
        |    x <= mux(UInt<1>(0), UInt<2>(2), UInt<2>(1))""".stripMargin
      (parse(exec(input)).serialize) should be (parse(check).serialize)
  }
  "Bundle with field of <0>" should "get deleted" in {
    val input =
      """circuit Top :
        |  module Top :
        |    input y : { a: UInt<0> }
        |    output x : { a: UInt<0>, b: UInt<1>}
        |    x.a <= y.a""".stripMargin
    val check =
      """circuit Top :
        |  module Top :
        |    output x : { b: UInt<1> }
        |    skip""".stripMargin
      (parse(exec(input)).serialize) should be (parse(check).serialize)
  }
  "Vector with type of <0>" should "get deleted" in {
    val input =
      """circuit Top :
        |  module Top :
        |    input y : UInt<0>[10]
        |    output x : UInt<0>[10]
        |    x <= y""".stripMargin
    val check =
      """circuit Top :
        |  module Top :
        |    skip""".stripMargin
      (parse(exec(input)).serialize) should be (parse(check).serialize)
  }
  "Node with <0>" should "be removed" in {
    val input =
      """circuit Top :
        |  module Top :
        |    input y: UInt<0>
        |    node x = y""".stripMargin
    val check =
      """circuit Top :
        |  module Top :
        |    skip""".stripMargin
      (parse(exec(input)).serialize) should be (parse(check).serialize)
  }
  "IsInvalid on <0>" should "be deleted" in {
    val input =
      """circuit Top :
        |  module Top :
        |    output y: UInt<0>
        |    y is invalid""".stripMargin
    val check =
      """circuit Top :
        |  module Top :
        |    skip""".stripMargin
      (parse(exec(input)).serialize) should be (parse(check).serialize)
  }
  "Expression in node with type <0>" should "be replaced by UInt<1>(0)" in {
    val input =
      """circuit Top :
        |  module Top :
        |    input x: UInt<1>
        |    input y: UInt<0>
        |    node z = add(x, y)""".stripMargin
    val check =
      """circuit Top :
        |  module Top :
        |    input x: UInt<1>
        |    node z = add(x, UInt<1>(0))""".stripMargin
      (parse(exec(input)).serialize) should be (parse(check).serialize)
  }
  "Expression in cat with type <0>" should "be removed" in {
    val input =
      """circuit Top :
        |  module Top :
        |    input x: UInt<1>
        |    input y: UInt<0>
        |    node z = cat(x, y)""".stripMargin
    val check =
      """circuit Top :
        |  module Top :
        |    input x: UInt<1>
        |    node z = x""".stripMargin
      (parse(exec(input)).serialize) should be (parse(check).serialize)
  }
  "Nested cats with type <0>" should "be removed" in {
    val input =
      """circuit Top :
        |  module Top :
        |    input x: UInt<0>
        |    input y: UInt<0>
        |    input z: UInt<0>
        |    node a = cat(cat(x, y), z)""".stripMargin
    val check =
      """circuit Top :
        |  module Top :
        |    skip""".stripMargin
      (parse(exec(input)).serialize) should be (parse(check).serialize)
  }
  "Nested cats where one has type <0>" should "be unaffected" in {
    val input =
      """circuit Top :
        |  module Top :
        |    input x: UInt<1>
        |    input y: UInt<0>
        |    input z: UInt<1>
        |    node a = cat(cat(x, y), z)""".stripMargin
    val check =
      """circuit Top :
        |  module Top :
        |    input x: UInt<1>
        |    input z: UInt<1>
        |    node a = cat(x, z)""".stripMargin
      (parse(exec(input)).serialize) should be (parse(check).serialize)
  }
  "Stop with type <0>" should "be replaced with UInt(0)" in {
    val input =
      """circuit Top :
        |  module Top :
        |    input clk: Clock
        |    input x: UInt<1>
        |    input y: UInt<0>
        |    input z: UInt<1>
        |    stop(clk, y, 1)""".stripMargin
    val check =
      """circuit Top :
        |  module Top :
        |    input clk: Clock
        |    input x: UInt<1>
        |    input z: UInt<1>
        |    stop(clk, UInt(0), 1)""".stripMargin
      (parse(exec(input)).serialize) should be (parse(check).serialize)
  }
  "Print with type <0>" should "be replaced with UInt(0)" in {
    val input =
      """circuit Top :
        |  module Top :
        |    input clk: Clock
        |    input x: UInt<1>
        |    input y: UInt<0>
        |    input z: UInt<1>
        |    printf(clk, UInt(1), "%d %d %d\n", x, y, z)""".stripMargin
    val check =
      """circuit Top :
        |  module Top :
        |    input clk: Clock
        |    input x: UInt<1>
        |    input z: UInt<1>
        |    printf(clk, UInt(1), "%d %d %d\n", x, UInt(0), z)""".stripMargin
      (parse(exec(input)).serialize) should be (parse(check).serialize)
  }
}

class ZeroWidthVerilog extends FirrtlFlatSpec {
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
