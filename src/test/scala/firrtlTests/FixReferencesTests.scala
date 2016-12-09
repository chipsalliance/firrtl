// See LICENSE for license details.

package firrtlTests

import java.io._
import org.scalatest._
import org.scalatest.prop._
import firrtl._
import firrtl.ir._
import firrtl.passes._
import firrtl.Mappers._
import firrtl.Parser.IgnoreInfo

class FixReferencesTests extends FirrtlFlatSpec {
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

  "Unresolved r" should "work" in {
    val input =
      """circuit Top :
        |  module Top :
        |    output p: UInt<2>
        |    wire x: UInt<2>
        |    x <= UInt(1)
        |""".stripMargin
    val check =
      """circuit Top :
        |  module Top :
        |    output p: UInt<2>
        |    wire x_0: UInt<2>
        |    p <= x_0
        |    wire x: UInt<2>
        |    x_0 <= x
        |    x <= UInt(1)
        |""".stripMargin
    val c = passes.foldLeft(parse(input)) {
      (c: Circuit, p: Pass) => p.run(c)
    }
    val u2 = UIntType(IntWidth(BigInt(2)))
    def addConnect(s: Statement): Statement = Block(Seq(Connect(NoInfo, WRef("p", u2, PortKind, FEMALE), WRef("x", u2, WireKind, MALE)), s))
    val cx = c.copy(modules = c.modules map {m => m map addConnect})
    val retC = FixReferences.run(cx)
    println(retC.serialize)
    (parse(retC.serialize).serialize) should be (parse(check).serialize)
  }
}

// vim: set ts=4 sw=4 et:
