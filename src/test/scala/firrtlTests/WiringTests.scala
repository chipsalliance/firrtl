package firrtlTests

import java.io._
import org.scalatest._
import org.scalatest.prop._
import firrtl._
import firrtl.ir.Circuit
import firrtl.passes._
import firrtl.Parser.IgnoreInfo
import Annotations._
import wiring.WiringUtils._
import wiring._

class WiringTests extends FirrtlFlatSpec {
  def parse (input:String) = Parser.parse(input.split("\n").toIterator, IgnoreInfo)
  private def executeTest(input: String, expected: Seq[String], passes: Seq[Pass]) = {
    val c = passes.foldLeft(Parser.parse(input.split("\n").toIterator)) {
      (c: Circuit, p: Pass) => p.run(c)
    }
    val lines = c.serialize.split("\n") map normalized

    expected foreach { e =>
      lines should contain(e)
    }
  }

  def passes = Seq(
    ToWorkingIR,
    ResolveKinds,
    InferTypes,
    ResolveGenders,
    InferWidths
  )

  "Wiring from r to X" should "work" in {
    val sinks = Map(("X"-> "pin"))
    val sas = WiringInfo("C", "r", sinks, "A")
    val input =
      """circuit Top :
        |  module Top :
        |    input clk: Clock
        |    inst a of A
        |    a.clk <= clk
        |  module A :
        |    input clk: Clock
        |    inst b of B
        |    b.clk <= clk
        |    inst x of X
        |    x.clk <= clk
        |    inst d of D
        |    d.clk <= clk
        |  module B :
        |    input clk: Clock
        |    inst c of C
        |    c.clk <= clk
        |    inst d of D
        |    d.clk <= clk
        |  module C :
        |    input clk: Clock
        |    reg r: UInt<5>, clk
        |  module D :
        |    input clk: Clock
        |    inst x1 of X
        |    x1.clk <= clk
        |    inst x2 of X
        |    x2.clk <= clk
        |  extmodule X :
        |    input clk: Clock
        |""".stripMargin
    val check =
      """circuit Top :
        |  module Top :
        |    input clk: Clock
        |    inst a of A
        |    a.clk <= clk
        |  module A :
        |    input clk: Clock
        |    inst b of B
        |    b.clk <= clk
        |    inst x of X
        |    x.clk <= clk
        |    inst d of D
        |    d.clk <= clk
        |    wire r: UInt<5>
        |    r <= b.r
        |    x.pin <= r
        |    d.r <= r
        |  module B :
        |    input clk: Clock
        |    output r: UInt<5>
        |    inst c of C
        |    c.clk <= clk
        |    inst d of D
        |    d.clk <= clk
        |    r <= c.r_0
        |    d.r <= r
        |  module C :
        |    input clk: Clock
        |    output r_0: UInt<5>
        |    reg r: UInt<5>, clk
        |    r_0 <= r
        |  module D :
        |    input clk: Clock
        |    input r: UInt<5>
        |    inst x1 of X
        |    x1.clk <= clk
        |    inst x2 of X
        |    x2.clk <= clk
        |    x1.pin <= r
        |    x2.pin <= r
        |  extmodule X :
        |    input clk: Clock
        |    input pin: UInt<5>
        |""".stripMargin
    val c = passes.foldLeft(parse(input)) {
      (c: Circuit, p: Pass) => p.run(c)
    }
    val wiringPass = new Wiring(sas)
    val retC = wiringPass.run(c)
    (parse(retC.serialize).serialize) should be (parse(check).serialize)
  }
}
