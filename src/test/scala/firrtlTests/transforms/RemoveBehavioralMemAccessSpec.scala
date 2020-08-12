// See LICENSE for license details

package firrtlTests.transforms

import firrtl._
import firrtl.testutils._

import firrtl.passes.RemoveEmpty
import firrtl.transforms.RemoveBehavioralMemAccess

import firrtl.options.Dependency

class RemoveBehavioralMemAccessSpec extends FirrtlFlatSpec {
  private def lowerMems(input: String): ir.Circuit = {
    RemoveEmpty.execute(RemoveBehavioralMemAccess.execute(CircuitState(parse(input), Nil))).circuit
  }

  behavior of "RemoveBehavioralMemAccess in isolation"

  it should "lower a sequential read memory with a simple read and write" in {
    val input =
      """circuit test :
        |  module test :
        |    input clk : Clock
        |    input addr : UInt<1>
        |    input wen : UInt<1>
        |    input wdata : UInt<8>[2]
        |    mem m :
        |      data-type => UInt<8>[2]
        |      depth => 256
        |      read-latency => 1
        |      write-latency => 1
        |      read-under-write => undefined
        |    memaccess rp = addr, clk
        |    node rd = m(rp)
        |    memaccess wp = addr, clk
        |    when wen:
        |      m(wp) <= wdata
        |""".stripMargin
    val check =
      """circuit test :
        |  module test :
        |    input clk : Clock
        |    input addr : UInt<1>
        |    input wen : UInt<1>
        |    input wdata : UInt<8>[2]
        |    mem m :
        |      data-type => UInt<8>[2]
        |      depth => 256
        |      read-latency => 1
        |      write-latency => 1
        |      reader => rp
        |      writer => wp
        |      read-under-write => undefined
        |    m.rp.clk <= clk
        |    m.rp.en <= UInt<1>(1)
        |    m.rp.addr <= addr
        |    m.wp.clk <= clk
        |    m.wp.en <= UInt<1>("h0")
        |    m.wp.addr <= addr
        |    m.wp.mask is invalid
        |    m.wp.data is invalid
        |    node rd = m.rp.data
        |    when wen:
        |      m.wp.en <= UInt<1>(1)
        |      m.wp.data <= wdata
        |      m.wp.mask[0] <= UInt<1>(1)
        |      m.wp.mask[1] <= UInt<1>(1)
        |""".stripMargin

    val result = lowerMems(input)
    (result.serialize) should be (parse(check).serialize)
  }

  it should "lower a sequential read memory with a simple readwrite" in {
    val input =
      """circuit test :
        |  module test :
        |    input clk : Clock
        |    input addr : UInt<1>
        |    input wen : UInt<1>
        |    input wdata : UInt<8>[2]
        |    mem m :
        |      data-type => UInt<8>[2]
        |      depth => 256
        |      read-latency => 1
        |      write-latency => 1
        |      read-under-write => undefined
        |    memaccess rwp = addr, clk
        |    node rd = m(rwp)
        |    when wen:
        |      m(rwp) <= wdata
        |""".stripMargin
    val check =
      """circuit test :
        |  module test :
        |    input clk : Clock
        |    input addr : UInt<1>
        |    input wen : UInt<1>
        |    input wdata : UInt<8>[2]
        |    mem m :
        |      data-type => UInt<8>[2]
        |      depth => 256
        |      read-latency => 1
        |      write-latency => 1
        |      readwriter => rwp
        |      read-under-write => undefined
        |    m.rwp.clk <= clk
        |    m.rwp.en <= UInt<1>(1)
        |    m.rwp.addr <= addr
        |    m.rwp.wmask is invalid
        |    m.rwp.wdata is invalid
        |    m.rwp.wmode <= UInt<1>(0)
        |    node rd = m.rwp.rdata
        |    when wen:
        |      m.rwp.wmode <= UInt<1>(1)
        |      m.rwp.wdata <= wdata
        |      m.rwp.wmask[0] <= UInt<1>(1)
        |      m.rwp.wmask[1] <= UInt<1>(1)
        |""".stripMargin

    val result = lowerMems(input)
    (result.serialize) should be (parse(check).serialize)
  }
}

class RemoveBehavioralMemAccessIntegrationSpec extends FirrtlFlatSpec {

  private val compiler = (new stage.transforms.Compiler(Seq(Dependency(RemoveBehavioralMemAccess))))

  private def lowerMems(input: String): ir.Circuit = {
    compiler.execute(CircuitState(parse(input), Nil)).circuit
  }

  behavior of "RemoveBehavioralMemAccess as part of a compiler"

  it should "lower a sequential read memory with a simple read and write" in {
    val input =
      """circuit test :
        |  module test :
        |    input clk : Clock
        |    input addr : UInt<1>
        |    input wen : UInt<1>
        |    input wdata : UInt<8>[2]
        |    mem m :
        |      data-type => UInt<8>[2]
        |      depth => 256
        |      read-latency => 1
        |      write-latency => 1
        |      read-under-write => undefined
        |    memaccess rp = addr, clk
        |    node rd = m(rp)
        |    memaccess wp = addr, clk
        |    when wen:
        |      m(wp) <= wdata
        |""".stripMargin
    val check =
      """circuit test :
        |  module test :
        |    input clk : Clock
        |    input addr : UInt<1>
        |    input wen : UInt<1>
        |    input wdata : UInt<8>[2]
        |    mem m :
        |      data-type => UInt<8>[2]
        |      depth => 256
        |      read-latency => 1
        |      write-latency => 1
        |      reader => rp
        |      writer => wp
        |      read-under-write => undefined
        |    m.rp.clk <= clk
        |    m.rp.en <= UInt<1>(1)
        |    m.rp.addr <= addr
        |    m.wp.clk <= clk
        |    m.wp.en <= UInt<1>("h0")
        |    m.wp.addr <= addr
        |    m.wp.mask is invalid
        |    m.wp.data is invalid
        |    node rd = m.rp.data
        |    when wen:
        |      m.wp.en <= UInt<1>(1)
        |      m.wp.data <= wdata
        |      m.wp.mask[0] <= UInt<1>(1)
        |      m.wp.mask[1] <= UInt<1>(1)
        |""".stripMargin

    val result = lowerMems(input)
    println(result.serialize)
    (result.serialize) should be (parse(check).serialize)
  }
}
