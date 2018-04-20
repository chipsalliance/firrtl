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

  "The following low FIRRTL should successfully transform by VerilogMemDelays" in {
    val input =
      """
        |circuit Test :
        |  module Test :
        |    input clock : Clock
        |    input a : UInt<1>
        |    input b : UInt<1>
        |    input select : UInt<1>
        |    output c : UInt<1>
        |
        |    mem m_0_a :
        |      data-type => UInt<8>
        |      depth => 32
        |      read-latency => 0
        |      write-latency => 1
        |      reader => read
        |      writer => write
        |      read-under-write => undefined
        |    mem m_0_b :
        |      data-type => UInt<8>
        |      depth => 32
        |      read-latency => 0
        |      write-latency => 1
        |      reader => read
        |      writer => write
        |      read-under-write => undefined
        |    mem m_1_a :
        |      data-type => UInt<8>
        |      depth => 32
        |      read-latency => 0
        |      write-latency => 1
        |      reader => read
        |      writer => write
        |      read-under-write => undefined
        |    mem m_1_b :
        |      data-type => UInt<8>
        |      depth => 32
        |      read-latency => 0
        |      write-latency => 1
        |      reader => read
        |      writer => write
        |      read-under-write => undefined
        |    node x_0_a = m_0_a.read.data
        |    node x_0_b = m_0_b.read.data
        |    node x_1_a = m_1_a.read.data
        |    node x_1_b = m_1_b.read.data
        |    node y = m_0_b.read.data
        |    node w_0_a = pad(UInt<4>("h2"), 8)
        |    node w_0_b = pad(UInt<4>("h3"), 8)
        |    node w_1_a = pad(UInt<4>("h4"), 8)
        |    node w_1_b = pad(UInt<4>("h5"), 8)
        |    c <= a
        |    m_0_a.read.addr is invalid
        |    m_0_b.read.addr is invalid
        |    m_1_a.read.addr is invalid
        |    m_1_b.read.addr is invalid
        |    m_0_a.read.en <= UInt<1>("h1")
        |    m_0_b.read.en <= UInt<1>("h1")
        |    m_1_a.read.en <= UInt<1>("h1")
        |    m_1_b.read.en <= UInt<1>("h1")
        |    m_0_a.read.clk <= clock
        |    m_0_b.read.clk <= clock
        |    m_1_a.read.clk <= clock
        |    m_1_b.read.clk <= clock
        |    m_0_a.write.addr is invalid
        |    m_0_b.write.addr is invalid
        |    m_1_a.write.addr is invalid
        |    m_1_b.write.addr is invalid
        |    m_0_a.write.en <= UInt<1>("h0")
        |    m_0_b.write.en <= UInt<1>("h0")
        |    m_1_a.write.en <= UInt<1>("h0")
        |    m_1_b.write.en <= UInt<1>("h0")
        |    m_0_a.write.clk <= clock
        |    m_0_b.write.clk <= clock
        |    m_1_a.write.clk <= clock
        |    m_1_b.write.clk <= clock
        |    m_0_a.write.data <= w_0_a
        |    m_0_b.write.data <= w_0_b
        |    m_1_a.write.data <= w_1_a
        |    m_1_b.write.data <= w_1_b
        |    m_0_a.write.mask is invalid
        |    m_0_b.write.mask is invalid
        |    m_1_a.write.mask is invalid
        |    m_1_b.write.mask is invalid
      """.stripMargin

    val circuit = Parser.parse(input)

    val circuit2 = VerilogMemDelays.run(circuit)
    circuit2.serialize.length > 0 should be (true)
  }
}
