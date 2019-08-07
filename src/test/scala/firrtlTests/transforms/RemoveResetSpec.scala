// See LICENSE for license details.

package firrtlTests.transforms

import firrtlTests.FirrtlFlatSpec
import firrtlTests.FirrtlCheckers._

import firrtl.{CircuitState, WRef}
import firrtl.ir.{Connect, Mux}
import firrtl.stage.{FirrtlCircuitAnnotation, FirrtlSourceAnnotation, FirrtlStage}

class RemoveResetSpec extends FirrtlFlatSpec {

  private def toLowFirrtl(string: String): CircuitState =
    (new FirrtlStage)
      .execute(Array("-X", "low"), Seq(FirrtlSourceAnnotation(string)))
      .collectFirst{
        case FirrtlCircuitAnnotation(a) => a
      }.map(a => firrtl.CircuitState(a, firrtl.UnknownForm))
      .get

  behavior of "RemoveReset"

  it should "not generate a mux for an invalid 1-bit, invalid init" in {
    val input =
      """|circuit Example :
         |  module Example :
         |    input clock : Clock
         |    input rst : UInt<1>
         |    input in : UInt<1>
         |    output out : UInt<1>
         |
         |    wire whatever : UInt<1>
         |    whatever is invalid
         |
         |    reg foo : UInt<1>, clock with : (reset => (rst, whatever))
         |    foo <= in
         |    out <= foo""".stripMargin

    val outputState = toLowFirrtl(input)

    info("foo is NOT connected to a mux")
    outputState shouldNot containTree { case Connect(_, WRef("foo",_,_,_), Mux(_,_,_,_)) => true }
  }

  it should "generate a mux for only the bit of an invalid aggregate that is reset" in {
    val input =
      """|circuit Example :
         |  module Example :
         |    input clock : Clock
         |    input rst : UInt<1>
         |    input in :  {a : UInt<1>[2], b : UInt<1>}
         |    output out :  {a : UInt<1>[2], b : UInt<1>}
         |
         |    wire whatever : {a : UInt<1>[2], b : UInt<1>}
         |    whatever is invalid
         |    whatever.a[1] <= UInt<1>(0)
         |
         |    reg foo :  {a : UInt<1>[2], b : UInt<1>}, clock with : (reset => (rst, whatever))
         |    foo <= in
         |    out <= foo""".stripMargin

    val outputState = toLowFirrtl(input)

    info("foo.a[0] is NOT connected to a mux")
    outputState shouldNot containTree { case Connect(_, WRef("foo_a_0",_,_,_), Mux(_,_,_,_)) => true }
    info("foo.a[1] is connected to a mux")
    outputState should    containTree { case Connect(_, WRef("foo_a_1",_,_,_), Mux(_,_,_,_)) => true }
    info("foo.b is NOT connected to a mux")
    outputState shouldNot containTree { case Connect(_, WRef("foo_b",_,_,_),   Mux(_,_,_,_)) => true }
  }

  it should "" in {
    val input =
      """|circuit Example :
         |  module Example :
         |    input clock : Clock
         |    input rst : UInt<1>
         |    input in : { a : UInt<1>, b : UInt<1> }
         |    output out : { a : UInt<1>, b : UInt<1> }
         |
         |    wire whatever : { a : UInt<1>, b : UInt<1> }
         |    whatever is invalid
         |    whatever.a <= UInt<1>(0)
         |
         |    wire tmp : { a : UInt<1>, b : UInt<1> }
         |    tmp is invalid
         |    tmp <= whatever
         |
         |    reg foo : { a : UInt<1>, b : UInt<1> }, clock with : (reset => (rst, tmp))
         |    foo <= in
         |    out <= foo""".stripMargin

    val outputState = toLowFirrtl(input)

    info("foo.a is connected to a mux")
    outputState should    containTree { case Connect(_, WRef("foo_a",_,_,_), Mux(_,_,_,_)) => true }
    info("foo.b is NOT connected to a mux")
    outputState shouldNot containTree { case Connect(_, WRef("foo_b",_,_,_), Mux(_,_,_,_)) => true }
  }

}
