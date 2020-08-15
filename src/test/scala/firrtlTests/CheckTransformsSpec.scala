// See LICENSE for license details.

package firrtlTests

import firrtl.{Compiler => _, _}
import firrtl.stage.Forms
import firrtl.stage.transforms.Compiler

import org.scalatest.{FlatSpec, Matchers}

class CheckTransformsSpec extends FlatSpec with Matchers {

  behavior of "All transforms making up the Verilog compiler"

  they should "properly declare their invalidates" in {

    val input =
"""
circuit Foo:
  module Foo:
    input in: UInt<1>
    output out: UInt<1>
    wire tmp: UInt<1>
    tmp <= not(not(in))
    out <= tmp
"""

    val c = new Compiler(targets = Forms.VerilogOptimized) {
      override val wrappers = Seq(
        (a: Transform) => firrtl.stage.transforms.ExpandPrepares(a),
        (a: Transform) => firrtl.stage.transforms.CatchCustomTransformExceptions(a),
        (a: Transform) => firrtl.stage.transforms.TrackTransforms(a),
        (a: Transform) => firrtl.stage.transforms.CheckTransforms(a),
        (a: Transform) => firrtl.stage.transforms.UpdateAnnotations(a)
      )
    }

    val state = CircuitState(Parser.parse(input), Seq.empty)

    c.transform(state)

  }

}
