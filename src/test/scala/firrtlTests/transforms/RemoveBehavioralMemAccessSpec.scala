// See LICENSE for license details

package firrtlTests.transforms

import firrtl._
import firrtl.testutils._
import firrtl.passes.RemoveEmpty
import firrtl.transforms.RemoveBehavioralMemAccess

class RemoveBehavioralMemAccessSpec extends FirrtlFlatSpec {
  private def lowerMems(input: String): ir.Circuit = {
    RemoveEmpty.execute(RemoveBehavioralMemAccess.execute(CircuitState(parse(input), Nil))).circuit
  }

  behavior of "RemoveBehavioralMemAccess"

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
}
