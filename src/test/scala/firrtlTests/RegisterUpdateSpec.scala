// See LICENSE for license details.

package firrtlTests

import firrtl._
import firrtl.testutils._
import FirrtlCheckers._
import scala.util.matching.Regex

class RegisterUpdateSpec extends FirrtlFlatSpec {
  def compile(input: String): CircuitState =
    (new VerilogCompiler).compileAndEmit(CircuitState(parse(input), ChirrtlForm), List.empty)
  def compileBody(body: String) = {
    val str = """
      |circuit Test :
      |  module Test :
      |""".stripMargin + body.split("\n").mkString("    ", "\n    ", "")
    compile(str)
  }

  "Register update logic" should "not duplicate common subtrees" in {
    val result = compileBody(s"""
      |input clock : Clock
      |output io : { flip in : UInt<8>, flip a : UInt<1>, flip b : UInt<1>, flip c : UInt<1>, out : UInt<8>}
      |reg r : UInt<8>, clock
      |when io.a :
      |  r <= io.in
      |when io.b :
      |  when io.c :
      |    r <= UInt(2)
      |io.out <= r""".stripMargin
    )
    val verilog = result.getEmittedCircuit.value
    result  shouldNot containLine ("r <= io_in;")
    verilog shouldNot include     ("if (io_a) begin")
    result  should    containLine ("r <= _GEN_0;")
  }

  it should "not let duplicate subtrees on one register affect another" in {

    val result = compileBody(s"""
      |input clock : Clock
      |output io : { flip in : UInt<8>, flip a : UInt<1>, flip b : UInt<1>, flip c : UInt<1>, out : UInt<8>}

      |reg r : UInt<8>, clock
      |reg r2 : UInt<8>, clock
      |when io.a :
      |  r <= io.in
      |  r2 <= io.in
      |when io.b :
      |  r2 <= UInt(3)
      |  when io.c :
      |    r <= UInt(2)
      |io.out <= and(r, r2)""".stripMargin
    )
    val verilog = result.getEmittedCircuit.value
    result  shouldNot containLine ("r <= io_in;")
    result  should    containLine ("r <= _GEN_0;")
    result  should    containLine ("r2 <= io_in;")
    verilog should    include     ("if (io_a) begin") // For r2
    // 1 time for r2, old versions would have 3 occurences
    Regex.quote("if (io_a) begin").r.findAllMatchIn(verilog).size should be (1)
  }

}

