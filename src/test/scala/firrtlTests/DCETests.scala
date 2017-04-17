// See LICENSE for license details.

package firrtlTests

import firrtl.ir.Circuit
import firrtl._
import firrtl.passes._
import firrtl.transforms._

class DCETests extends FirrtlFlatSpec {
  //val transforms = Seq(
  //    ToWorkingIR,
  //    ResolveKinds,
  //    InferTypes,
  //    ResolveGenders,
  //    InferWidths,
  //    new DeadCodeElimination)
  //private def exec(input: String): String =
  //  passes.foldLeft(parse(input)) { (c: Circuit, p: Pass) => p.run(c) }.serialize

  "Unread wire" should "be deleted" in {
    val input =
      """circuit Top :
        |  module Top :
        |    input x : UInt<1>
        |    output z : UInt<1>
        |    wire a : UInt<1>
        |    z <= x
        |    a <= x""".stripMargin
    val check =
      """circuit Top :
        |  module Top :
        |    input x : UInt<1>
        |    output z : UInt<1>
        |    skip
        |    z <= x
        |    skip""".stripMargin.split("\n").map(normalized(_))
    //(parse(exec(input))) should be (parse(check))
    executeTest(input, check, new VerilogCompiler)
  }
  //"Unread register" should "be deleted" in {
  //  val input =
  //    """circuit Top :
  //      |  module Top :
  //      |    input clk : Clock
  //      |    input x : UInt<1>
  //      |    output z : UInt<1>
  //      |    reg a : UInt<1>, clk
  //      |    a <= x
  //      |    z <= or(x, asUInt(clk))""".stripMargin
  //  val check =
  //    """circuit Top :
  //      |  module Top :
  //      |    input clk : Clock
  //      |    input x : UInt<1>
  //      |    output z : UInt<1>
  //      |    skip
  //      |    skip
  //      |    z <= or(x, asUInt(clk))""".stripMargin
  //  (parse(exec(input))) should be (parse(check))
  //}
  //"Unread node" should "be deleted" in {
  //  val input =
  //    """circuit Top :
  //      |  module Top :
  //      |    input x : UInt<1>
  //      |    output z : UInt<1>
  //      |    node a = not(x)
  //      |    z <= x""".stripMargin
  //  val check =
  //    """circuit Top :
  //      |  module Top :
  //      |    input x : UInt<1>
  //      |    output z : UInt<1>
  //      |    skip
  //      |    z <= x""".stripMargin
  //  (parse(exec(input))) should be (parse(check))
  //}
  //"Unused ports" should "be deleted" in {
  //  val input =
  //    """circuit Top :
  //      |  module Top :
  //      |    input x : UInt<1>
  //      |    input y : UInt<1>
  //      |    output z : UInt<1>
  //      |    z <= x""".stripMargin
  //  val check =
  //    """circuit Top :
  //      |  module Top :
  //      |    input x : UInt<1>
  //      |    output z : UInt<1>
  //      |    z <= x""".stripMargin
  //  (parse(exec(input))) should be (parse(check))
  //}
  //"Chain of unread nodes" should "be deleted" in {
  //  val input =
  //    """circuit Top :
  //      |  module Top :
  //      |    input x : UInt<1>
  //      |    output z : UInt<1>
  //      |    node a = not(x)
  //      |    node b = or(a, a)
  //      |    node c = add(b, x)
  //      |    z <= x""".stripMargin
  //  val check =
  //    """circuit Top :
  //      |  module Top :
  //      |    input x : UInt<1>
  //      |    output z : UInt<1>
  //      |    skip
  //      |    skip
  //      |    skip
  //      |    z <= x""".stripMargin
  //  (parse(exec(input))) should be (parse(check))
  //}
  //"Chain of unread wires and their connections" should "be deleted" in {
  //  val input =
  //    """circuit Top :
  //      |  module Top :
  //      |    input x : UInt<1>
  //      |    output z : UInt<1>
  //      |    wire a : UInt<1>
  //      |    a <= x
  //      |    wire b : UInt<1>
  //      |    b <= a
  //      |    z <= x""".stripMargin
  //  val check =
  //    """circuit Top :
  //      |  module Top :
  //      |    input x : UInt<1>
  //      |    output z : UInt<1>
  //      |    skip
  //      |    skip
  //      |    skip
  //      |    skip
  //      |    z <= x""".stripMargin
  //  (parse(exec(input))) should be (parse(check))
  //}
  //"Read register" should "not be deleted" in {
  //  val input =
  //    """circuit Top :
  //      |  module Top :
  //      |    input clk : Clock
  //      |    input x : UInt<1>
  //      |    output z : UInt<1>
  //      |    reg r : UInt<1>, clk
  //      |    r <= x
  //      |    z <= r""".stripMargin
  //  val check =
  //    """circuit Top :
  //      |  module Top :
  //      |    input clk : Clock
  //      |    input x : UInt<1>
  //      |    output z : UInt<1>
  //      |    reg r : UInt<1>, clk with : (reset => (UInt<1>("h0"), r))
  //      |    r <= x
  //      |    z <= r""".stripMargin
  //  (parse(exec(input))) should be (parse(check))
  //}
  //// Printf is hard because the StringLit doesn't match
  //"Logic that feeds into simulation constructs" should "not be deleted" in {
  //  val input =
  //    """circuit Top :
  //      |  module Top :
  //      |    input clk : Clock
  //      |    input x : UInt<1>
  //      |    output z : UInt<1>
  //      |    node a = not(x)
  //      |    stop(clk, a, 0)
  //      |    z <= x""".stripMargin
  //  val check =
  //    """circuit Top :
  //      |  module Top :
  //      |    input clk : Clock
  //      |    input x : UInt<1>
  //      |    output z : UInt<1>
  //      |    node a = not(x)
  //      |    stop(clk, a, 0)
  //      |    z <= x""".stripMargin
  //  (parse(exec(input))) should be (parse(check))
  //}
  //"Globally dead module" should "should be deleted" in {
  //  val input =
  //    """circuit Top :
  //      |  module Dead :
  //      |    input x : UInt<1>
  //      |    output z : UInt<1>
  //      |    z <= x
  //      |  module Top :
  //      |    input x : UInt<1>
  //      |    output z : UInt<1>
  //      |    inst dead of Dead
  //      |    dead.x <= x
  //      |    z <= x""".stripMargin
  //  val check =
  //    """circuit Top :
  //      |  module Top :
  //      |    input x : UInt<1>
  //      |    output z : UInt<1>
  //      |    skip
  //      |    skip
  //      |    z <= x""".stripMargin
  //  (parse(exec(input))) should be (parse(check))
  //}
  //// bar.z is not used and thus is dead code, but foo.z is used so this code isn't eliminated
  //"Module deduplication" should "should be preserved despite unused output of ONE instance" in {
  //  val input =
  //    """circuit Top :
  //      |  module Child :
  //      |    input x : UInt<1>
  //      |    output y : UInt<1>
  //      |    output z : UInt<1>
  //      |    y <= not(x)
  //      |    z <= x
  //      |  module Top :
  //      |    input x : UInt<1>
  //      |    output z : UInt<1>
  //      |    inst foo of Child
  //      |    inst bar of Child
  //      |    foo.x <= x
  //      |    bar.x <= x
  //      |    z <= or(or(foo.y, foo.z), bar.y)""".stripMargin
  //  val check =
  //    """circuit Top :
  //      |  module Child :
  //      |    input x : UInt<1>
  //      |    output y : UInt<1>
  //      |    output z : UInt<1>
  //      |    y <= not(x)
  //      |    z <= x
  //      |  module Top :
  //      |    input x : UInt<1>
  //      |    output z : UInt<1>
  //      |    inst foo of Child
  //      |    inst bar of Child
  //      |    foo.x <= x
  //      |    bar.x <= x
  //      |    z <= or(or(foo.y, foo.z), bar.y)""".stripMargin
  //  (parse(exec(input))) should be (parse(check))
  //}
  //behavior of "Single dead instances"
  //ignore should "should be deleted" in {
  //  val input =
  //    """circuit Top :
  //      |  module Child :
  //      |    input x : UInt<1>
  //      |    output z : UInt<1>
  //      |    z <= x
  //      |  module Top :
  //      |    input x : UInt<1>
  //      |    output z : UInt<1>
  //      |    inst foo of Child
  //      |    inst bar of Child
  //      |    foo.x <= x
  //      |    bar.x <= x
  //      |    z <= foo.z""".stripMargin
  //  val check =
  //    """circuit Top :
  //      |  module Child :
  //      |    input x : UInt<1>
  //      |    output z : UInt<1>
  //      |    z <= x
  //      |  module Top :
  //      |    input x : UInt<1>
  //      |    output z : UInt<1>
  //      |    inst foo of Child
  //      |    skip
  //      |    foo.x <= x
  //      |    skip
  //      |    z <= foo.z""".stripMargin
  //  println(exec(input))
  //  (parse(exec(input))) should be (parse(check))
  //}
}
