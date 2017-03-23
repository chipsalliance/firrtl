// See LICENSE for license details.

package firrtlTests

import org.scalatest.Matchers
import java.io.{StringWriter,Writer}
import firrtl.ir.Circuit
import firrtl._
import firrtl.Parser.IgnoreInfo
import firrtl.Parser
import firrtl.passes._

class DCETests extends FirrtlFlatSpec {
  val passes = Seq(
      ToWorkingIR,
      ResolveKinds,
      InferTypes,
      ResolveGenders,
      InferWidths,
      DeadCodeElimination)
  private def exec (input: String) = {
    passes.foldLeft(parse(input)) {
      (c: Circuit, p: Pass) => p.run(c)
    }.serialize
  }
   // =============================
  "Unread wire" should "be deleted" in {
    val input =
      """circuit Top :
        |  module Top :
        |    input y : UInt<1>
        |    wire x: UInt<1>
        |    x <= y""".stripMargin
    val check =
      """circuit Top :
         |  module Top :
         |    input y : UInt<1>
         |    skip
         |    skip""".stripMargin
      (parse(exec(input))) should be (parse(check))
  }
  "Unread register" should "be deleted" in {
    val input =
      """circuit Top :
        |  module Top :
        |    input y: UInt<1>
        |    input clk: Clock
        |    reg x: UInt<1>, clk
        |    x <= y""".stripMargin
    val check =
      """circuit Top :
         |  module Top :
         |    input y : UInt<1>
         |    input clk: Clock
         |    skip
         |    skip""".stripMargin
      (parse(exec(input))) should be (parse(check))
  }
  "Unread node" should "be deleted" in {
    val input =
      """circuit Top :
        |  module Top :
        |    input y: UInt<1>
        |    node x = add(y, y)""".stripMargin
    val check =
      """circuit Top :
         |  module Top :
         |    input y : UInt<1>
         |    skip""".stripMargin
      (parse(exec(input))) should be (parse(check))
  }
  "Chain of unread nodes" should "be deleted" in {
    val input =
      """circuit Top :
        |  module Top :
        |    input y: UInt<1>
        |    node x = y
        |    node z = x""".stripMargin
    val check =
      """circuit Top :
         |  module Top :
         |    input y : UInt<1>
         |    skip
         |    skip""".stripMargin
      (parse(exec(input))) should be (parse(check))
  }
  "Chain of unread wires" should "be deleted" in {
    val input =
      """circuit Top :
        |  module Top :
        |    input y: UInt<1>
        |    wire x: UInt<1>
        |    x <= y
        |    wire z: UInt<1>
        |    z <= x""".stripMargin
    val check =
      """circuit Top :
         |  module Top :
         |    input y : UInt<1>
         |    skip
         |    skip
         |    skip
         |    skip""".stripMargin
      (parse(exec(input))) should be (parse(check))
  }
  "Read register" should "not be deleted" in {
    val input =
      """circuit Top :
        |  module Top :
        |    input y: UInt<1>
        |    input clk: Clock
        |    output z: UInt<1>
        |    reg r: UInt<1>, clk
        |    r <= y
        |    z <= r""".stripMargin
    val check =
      """circuit Top :
        |  module Top :
        |    input y: UInt<1>
        |    input clk: Clock
        |    output z: UInt<1>
        |    reg r: UInt<1>, clk with: (reset => (UInt<1>("h0"), r))
        |    r <= y
        |    z <= r""".stripMargin
      (parse(exec(input))) should be (parse(check))
  }
}
