// SPDX-License-Identifier: Apache-2.0

package firrtlTests.passes

import firrtl.{
  CircuitState,
  Parser
}
import firrtl.passes.{
  CheckTypes,
  InferTypes,
  VerilogModulusCleanup
}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class VerilogModulusCleanupSpec extends AnyFlatSpec with Matchers {

  behavior of "VerilogModulusCleanup"

  it should "add an 'asSInt' prim op if the 'rem' prim op operates on 'SInt' types" in {

    val input =
      """|circuit Foo:
         |  module Bar:
         |    input a: SInt<1>
         |    output b: SInt<1>
         |    b <= rem(a, a)
         |""".stripMargin

    /* Running InferTypes/CheckTypes after VerilogModulusCleanup will fail if the asSInt prim op isn't generated */
    val output = (Parser.parse(_: String))
      .andThen(CircuitState(_, Seq.empty))
      .andThen(InferTypes.transform)
      .andThen(VerilogModulusCleanup.transform)
      .andThen(InferTypes.transform)
      .andThen(CheckTypes.transform)
      .apply(input)

    output.circuit.serialize should include ("asSInt")

  }

}
