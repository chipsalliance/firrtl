// See LICENSE for license details.

package firrtlTests
package transforms

import firrtl._
import firrtl.ir._
import firrtl.transforms._
import FirrtlCheckers._

class RestructureMuxesSpec extends FirrtlFlatSpec {
  def compile(input: String): CircuitState =
    (new LowFirrtlCompiler).compileAndEmit(CircuitState(parse(input), ChirrtlForm),
      List(new RestructureMuxes))
  def compileBody(body: String) = {
    val str = """
      |circuit Test :
      |  module Test :
      |    input a : UInt<1>
      |    input b : UInt<1>
      |    input x : UInt<8>
      |    input y : UInt<8>
      |    output z : UInt<8>
      |""".stripMargin + body.split("\n").mkString("    ", "\n    ", "")
    compile(str)
  }

  val cases = Seq(
    ("mux(a, _, y)", "mux(b, x, y)") -> "mux(and(a, b), x, y)",
    ("mux(a, _, y)", "mux(b, y, x)") -> "mux(and(a, not(b)), x, y)",
    ("mux(a, y, _)", "mux(b, x, y)") -> "mux(and(not(a), b), x, y)",
    ("mux(a, y, _)", "mux(b, y, x)") -> "mux(and(not(a), not(b)), x, y)"
  )
  for (((outer, inner), to) <- cases) {
    val from = outer.replace("_", inner)
    from should s"restructure to $to" in {
      val result = compileBody(s"z <= $from")
      result should containLine (s"z <= $to")
    }
    it should s"restructure to $to even with an intermediate node" in {
      val con = outer.replace("_", "n")
      val result = compileBody(s"node n = $inner\nz <= $con")
      result should containLine (s"z <= $to")
    }
  }
}
