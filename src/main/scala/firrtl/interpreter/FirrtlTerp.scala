// See LICENSE for license details.

package firrtl.interpreter

import firrtl.{ToLoFirrtl, Parser}
import firrtl.antlr.FIRRTLBaseVisitor

class ExecutionVisitor extends FIRRTLBaseVisitor {

}

class FirrtlTerp(input: String) {
  val ast = Parser.parse("", input.split("\n").toIterator)

  val lowered_ast = ToLoFirrtl.lower(ast)

  println(s"ast $lowered_ast")

//  DependencyMapper.apply(lowered_ast)

  val unitialized_state = CircuitState(lowered_ast)
}

object FirrtlTerp {
  def main(args: Array[String]) {
    println("hello world")

    val input =
      """circuit Test :
        |  module Test :
        |    input clk : Clock
        |    input a : UInt<1>
        |    input b : UInt<1>
        |    input select : UInt<1>
        |    output c : UInt<2>
        |    reg w : UInt<1>, clk
        |
        |    w <= a
        |    c <= w
        |    when select :
        |       c <= add(b, a)
      """.stripMargin
//    val input =
//    """circuit Test :
//      |  module Test :
//      |    input clk : Clock
//      |    input a : UInt<1>
//      |    input b : UInt<1>
//      |    input select : UInt<1>
//      |    output c : UInt<1>
//      |    c <= mux(select, a, b)
//    """.stripMargin
//    val input =
//      """circuit Test :
//        |  module Test :
//        |    input clk : Clock
//        |    input a : UInt<1>
//        |    input b : UInt<1>
//        |    input select : UInt<1>
//        |    output c : UInt<1>
//        |    mem m :
//        |      data-type => { a : UInt<8>, b : UInt<8>}[2]
//        |      depth => 32
//        |      read-latency => 0
//        |      write-latency => 1
//        |      reader => read
//        |      writer => write
//        |    m.read.clk <= clk
//        |    m.read.en <= UInt<1>(1)
//        |    m.read.addr is invalid
//        |    node x = m.read.data
//        |    node y = m.read.data[0].b
//        |
//        |    m.write.clk <= clk
//        |    m.write.en <= UInt<1>(0)
//        |    m.write.mask is invalid
//        |    m.write.addr is invalid
//        |    wire w : { a : UInt<8>, b : UInt<8>}[2]
//        |    w[0].a <= UInt<4>(2)
//        |    w[0].b <= UInt<4>(3)
//        |    w[1].a <= UInt<4>(4)
//        |    w[1].b <= UInt<4>(5)
//        |    m.write.data <= w
//        |    c <= a
//      """.stripMargin

    val interpreter = new FirrtlTerp(input)


  }
}
