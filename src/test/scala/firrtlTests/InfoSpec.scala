// See LICENSE for license details.

package firrtlTests

import firrtl._
import firrtl.ir._
import FirrtlCheckers._

class InfoSpec extends FirrtlFlatSpec {
  val compiler = new VerilogCompiler

  val Info1 = FileInfo(StringLit("Source.scala 1:4"))
  val Info2 = FileInfo(StringLit("Source.scala 2:4"))
  val Info3 = FileInfo(StringLit("Source.scala 3:4"))
  val Info4 = FileInfo(StringLit("Source.scala 4:4"))
  val Info5 = FileInfo(StringLit("Source.scala 5:4"))

  val testfirrtl = s"""
    |circuit Test :
    |  module Test :
    |    input clock : Clock $Info1
    |    input io : { in : UInt<8>, cond : UInt<1>, flip out : UInt<8> } $Info2
    |    reg r : UInt<8>, clock $Info3
    |    when io.cond :
    |      r <= io.in $Info4
    |    io.out <= r $Info5
    |""".stripMargin

  val result = compiler.compile(CircuitState(parse(testfirrtl), ChirrtlForm), List.empty)

  "FileInfo" should "be preserved for declarations" in {
    assert(result search { case Port(Info1, "clock", Input, _) => true })
    assert(result search { case DefRegister(Info3, "r", _,_,_,_) => true })
  }
  it should "be propagated for aggregate lowering" in {
    assert(result search { case Port(Info2, "io_in", Input, _) => true })
    assert(result search { case Port(Info2, "io_cond", Input, _) => true })
    assert(result search { case Port(Info2, "io_out", Output, _) => true })
  }
}
