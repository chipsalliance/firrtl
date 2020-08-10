// See LICENSE for license details

package firrtlTests.transforms

import firrtl._
import firrtl.testutils._
import firrtl.transforms.RemoveBehavioralMemAccess

class RemoveBehavioralMemAccessSpec extends FirrtlFlatSpec {
  private def lowerMems(input: String): ir.Circuit = {
    RemoveBehavioralMemAccess.execute(CircuitState(parse(input), Nil)).circuit
  }

  behavior of "RemoveBehavioralMemAccess"

  it should "lower a sequential read memory with a simple read" in {
    val input =
      """circuit test :
        |  module test :
        |    input clk : Clock
        |    input addr : UInt<1>
        |    mem m :
        |      data-type => UInt<8>
        |      depth => 256
        |      read-latency => 1
        |      write-latency => 1
        |      read-under-write => undefined
        |    memaccess rp = addr, clk
        |    node rd = m(rp)
        |""".stripMargin
    val check =
      """circuit test :
        |  module test :
        |    input clk : Clock
        |    input addr : UInt<1>
        |    mem m :
        |      data-type => UInt<8>
        |      depth => 256
        |      read-latency => 1
        |      write-latency => 1
        |      reader => rp
        |      read-under-write => undefined
        |    m.rp.clk <= clk
        |    m.rp.en <= UInt<1>(1)
        |    m.rp.addr <= addr
        |    skip
        |    node rd = m.rp.data
        |""".stripMargin

    val result = lowerMems(input)
    (result.serialize) should be (parse(check).serialize)
  }
}
