// See LICENSE for license details.

package firrtlTests

import firrtl.{AnnotationSeq, ChirrtlForm, LowFirrtlCompiler, Parser}
import firrtl.passes.memlib.VerilogMemDelays
import org.scalatest.{FreeSpec, Matchers}

class VerilogMemDelaySpec extends FreeSpec with Matchers {
  "The following low FIRRTL should be parsed by VerilogMemDelays" in {
    val input =
      """
        |circuit Test :
        |  module Test :
        |    input clock : Clock
        |    input a : UInt<1>
        |    input b : UInt<1>
        |    input select : UInt<1>
        |    output c : UInt<1>
        |    mem m :
        |      data-type => { a : UInt<8>, b : UInt<8>}[2]
        |      depth => 32
        |      read-latency => 0
        |      write-latency => 1
        |      reader => read
        |      writer => write
        |    m.read.clk <= clock
        |    m.read.en <= UInt<1>(1)
        |    m.read.addr is invalid
        |    node x = m.read.data
        |    node y = m.read.data[0].b
        |
        |    m.write.clk <= clock
        |    m.write.en <= UInt<1>(0)
        |    m.write.mask is invalid
        |    m.write.addr is invalid
        |    wire w : { a : UInt<8>, b : UInt<8>}[2]
        |    w[0].a <= UInt<4>(2)
        |    w[0].b <= UInt<4>(3)
        |    w[1].a <= UInt<4>(4)
        |    w[1].b <= UInt<4>(5)
        |    m.write.data <= w
        |    c <= a
      """.stripMargin

    val circuit = Parser.parse(input)
    val compiler = new LowFirrtlCompiler

    val compileResult = compiler.compileAndEmit(firrtl.CircuitState(circuit, ChirrtlForm, AnnotationSeq(Seq.empty)))
    val circuit2 = VerilogMemDelays.run(compileResult.circuit)
    circuit2.serialize.length > 0 should be (true)
  }
}
