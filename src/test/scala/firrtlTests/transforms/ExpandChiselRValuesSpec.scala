// SPDX-License-Identifier: Apache-2.0

package firrtlTests.transforms

import firrtl.{
  ir,
  CircuitState,
  Parser
}
import firrtl.options.Dependency
import firrtl.stage.Forms
import firrtl.testutils.FirrtlFlatSpec
import firrtl.testutils.FirrtlCheckers._

import org.scalatest.matchers.should._

object ExpandChiselRValuesSpec {

  def syntheticName = "_synthetic_output"

  def moduleCircuit = {
    Parser.parse(
      """|circuit Foo :
         |  module Foo :
         |    output element : UInt<1>
         |    output aggregate : { a : UInt<1>, b: UInt<1>}
         |    output vector : { b : { a : UInt<1> }[2] }
         |    output a : UInt<1>
         |    output b : { a : UInt<1>}
         |    output c : { a : UInt<1>}
         |    output d : { a : UInt<1>}
         |    output e : { a : UInt<1>}
         |    element is invalid
         |    aggregate.a <= UInt<1>(0)
         |    aggregate.b <= UInt<1>(0)
         |    vector is invalid
         |    a <= element
         |    b.a <= aggregate.a
         |    c.a <- aggregate.b
         |    d <= vector.b[element]
         |    e <= vector.b[1]
         |""".stripMargin)
  }

  val instanceCircuit =
    """|circuit Foo :
       |  module Bar :
       |    output a : { a : UInt<1>}
       |    a.a is invalid
       |  module Foo :
       |    output a : { a : UInt<1>}
       |    output b : { a : UInt<1>}
       |    output c : { a : UInt<1>}
       |    inst bar of Bar
       |    a.a <= bar.a.a
       |    b <- bar.a
       |    c <= bar.a
       |""".stripMargin

  trait Fixture {
    val stage = new firrtl.stage.transforms.Compiler(
      targets = Seq(Dependency(firrtl.transforms.ExpandChiselRValues))
    )

    implicit class CircuitHelpers(circuit: ir.Circuit) {

      def compile: CircuitState = stage.execute(CircuitState(circuit, Seq.empty))

    }

    implicit class StringHelpers(string: String) {

      def parse: ir.Circuit = Parser.parse(string)

    }

  }

}

class ExpandChiselRValuesSpec extends FirrtlFlatSpec {

  import ExpandChiselRValuesSpec._

  behavior of "firrtl.transforms.ExpandChiselRValues with module outputs used as sources"

  it should "replace a RHS usage of an element that was invalidated" in new Fixture {
     val output = """|circuit Foo:
                     |  module Foo:
                     |    output a: UInt<1>
                     |    output b: UInt<1>
                     |    a is invalid
                     |    b <= a
                     |"""
       .stripMargin
       .parse
       .compile
       .circuit
       .serialize

    output should include (s"wire $syntheticName : { a : UInt<1>}")
    output should include (s"$syntheticName.a is invalid")
    output should include (s"b <= $syntheticName.a")

  }

  it should "replace a RHS usage of an element that was connected" in new Fixture {
    val output =
      """|circuit Foo:
         |  module Foo:
         |    output a: UInt<1>
         |    output b: UInt<1>
         |    a <= UInt<1>(0)
         |    b <= a
         |"""
        .stripMargin
        .parse
        .compile
        .circuit
        .serialize

    output should include (s"wire $syntheticName : { a : UInt<1>}")
    output should include (s"""$syntheticName.a <= UInt<1>("h0")""")
    output should include (s"b <= $syntheticName.a")

  }

  it should "replace a RHS usage of an element that was partial connected" in new Fixture {
    val output =
      """|circuit Foo:
         |  module Foo:
         |    output a: UInt<1>
         |    output b: UInt<1>
         |    input c: UInt<1>
         |    a <- c
         |    b <- a
         |"""
        .stripMargin
        .parse
        .compile
        .circuit
        .serialize

    output should include (s"wire $syntheticName : { a : UInt<1>}")
    output should include (s"$syntheticName.a <= c")
    output should include (s"b <- $syntheticName.a")
  }

  it should "replace a RHS usage of an aggregate that was invalidated" in new Fixture {
    val output =
      """|circuit Foo:
         |  module Foo:
         |    output a: { b: UInt<1>, c: UInt<1> }
         |    output b: UInt<1>
         |    a is invalid
         |    b <= a.b
         |"""
        .stripMargin
        .parse
        .compile
        .circuit
        .serialize

    output should include (s"wire $syntheticName : { a : { b : UInt<1>}}")
    output should include (s"$syntheticName.a.b is invalid")
    output should include (s"b <= $syntheticName.a.b")

  }

  it should "replace a RHS usage of an element assigned in an aggregate bi-directional connect" in new Fixture {

    val output =
      """|circuit Foo:
         |  module Foo:
         |    input a: { b: UInt<1>, flip c: UInt<1> }
         |    output b: { b: UInt<1>, flip c: UInt<1> }
         |    output c: UInt<1>
         |    b <= a
         |    c <= b.b
         |"""
      .stripMargin
      .parse
      .compile
      .circuit
      .serialize

    output should include (s"wire $syntheticName : { b : { b : UInt<1>}}")
    output should include (s"$syntheticName.b.b <= a.b")
    output should include (s"c <= $syntheticName.b.b")

  }

  it should "replace a RHS usage of an aggregate that was partial connected" in new Fixture {

    val output =
      """|circuit Foo:
         |  module Foo:
         |    output a: { a: UInt<1>, flip b: UInt<1> }
         |    output b: { a: UInt<1> }
         |    b is invalid
         |    a <- b
         |"""
        .stripMargin
        .parse
        .compile
        .circuit
        .serialize

    info(output)

    output should include (s"wire $syntheticName : { b : { a : UInt<1>}}")
    output should include (s"$syntheticName.b.a is invalid")
    output should include (s"a <- $syntheticName.b")

  }

  it should "assign to a synthetic wire in when statements" in new Fixture {

    val output =
      """|circuit Foo:
         |  module Foo:
         |    input cond: UInt<1>
         |    output a: UInt<1>
         |    output b: UInt<1>
         |    when eq(cond, UInt<1>(0)):
         |      a <= UInt<1>(0)
         |    else:
         |      when eq(cond, UInt<1>(1)):
         |        a <= UInt<1>(1)
         |      else:
         |        a <= UInt<1>(0)
         |    b <= a
         |"""
        .stripMargin
        .parse
        .compile
        .circuit
        .serialize

    output should include (s"wire $syntheticName : { a : UInt<1>}")
    output should include (s"""$syntheticName.a <= UInt<1>("h0")""")
    output should include (s"""$syntheticName.a <= UInt<1>("h1")""")
    output should include (s"b <= $syntheticName.a")

  }

  it should "replace a RHS subaccess" in new Fixture {

    val output =
      """|circuit Foo:
         |  module Foo:
         |    input a: UInt<2>
         |    output b: UInt<1>
         |    output c: UInt<1>
         |    b is invalid
         |    c <= a[b]
         |"""
        .stripMargin
        .parse
        .compile
        .circuit
        .serialize

    output should include (s"wire $syntheticName : { b : UInt<1>}")
    output should include (s"c <= a[$syntheticName.b]")

  }

  it should "only include outputs in the synthetic wire" in new Fixture {

    val output =
      """|circuit Foo:
         |  module Foo:
         |    input decoupled: { flip ready: UInt<1>, valid: UInt<1>}
         |    decoupled.ready <= UInt<1>(0)
         |    node c = and(decoupled.ready, decoupled.valid)
         |"""
        .stripMargin
        .parse
        .compile
        .circuit
        .serialize

    output should include (s"wire $syntheticName : { decoupled : { ready : UInt<1>}}")
    output should include (s"""$syntheticName.decoupled.ready <= UInt<1>("h0")""")
    output should include (s"node c = and($syntheticName.decoupled.ready, decoupled.valid)")
  }

  it should "replace a RHS sink assignment to a wire" in new Fixture {

    val output =
      """|circuit Foo:
         |  module Foo:
         |    output a: { a: UInt<1> }
         |    output b: { a: UInt<1> }
         |    wire w : { a: UInt<1> }
         |    b is invalid
         |    w <= b
         |    a <= w
         |"""
        .stripMargin
        .parse
        .compile
        .circuit
        .serialize

    output should include (s"wire $syntheticName : { b : { a : UInt<1>}}")
    output should include (s"$syntheticName.b.a is invalid")
    output should include (s"w <= $syntheticName.b")

  }

  it should "replace a RHS sink assignment to a register" in new Fixture {

    val output =
      """|circuit Foo:
         |  module Foo:
         |    output a: { a: UInt<1> }
         |    output b: { a: UInt<1> }
         |    input clk: Clock
         |    reg r : { a: UInt<1> }, clk
         |    b is invalid
         |    r <= b
         |    a <= r
         |"""
        .stripMargin
        .parse
        .compile
        .circuit
        .serialize

    output should include (s"wire $syntheticName : { b : { a : UInt<1>}}")
    output should include (s"$syntheticName.b.a is invalid")
    output should include (s"r <= $syntheticName.b")

  }

  it should "replace a RHS sink assignment to a register clock" in new Fixture {

    val output =
      """|circuit Foo:
         |  module Foo:
         |    output a: UInt<1>
         |    input clkIn: Clock
         |    output clkOut: Clock
         |    reg r : UInt<1>, clkOut
         |    clkOut <= clkIn
         |"""
        .stripMargin
        .parse
        .compile
        .circuit
        .serialize

    output should include (s"reg r : UInt<1>, $syntheticName.clkOut")

  }

  it should "replace a RHS sink assignment to a register reset" in new Fixture {

    val output =
      """|circuit Foo:
         |  module Foo:
         |    output a: UInt<1>
         |    input clk: Clock
         |    reg r : UInt<1>, clk with : (reset => (a, UInt<1>(0)))
         |    a is invalid
         |    r is invalid
         |"""
        .stripMargin
        .parse
        .compile
        .circuit
        .serialize

    output should include (s"""reset => ($syntheticName.a, UInt<1>("h0"))""")

  }

  it should "replace a RHS sink assignment to a register init" in new Fixture {

    val output =
      """|circuit Foo:
         |  module Foo:
         |    output a: UInt<1>
         |    input clk: Clock
         |    input rst: UInt<1>
         |    reg r : UInt<1>, clk with : (reset => (rst, a))
         |    a is invalid
         |    r is invalid
         |"""
        .stripMargin
        .parse
        .compile
        .circuit
        .serialize

    output should include (s"""reset => (rst, $syntheticName.a)""")

  }

  it should "ignore a bi-directional connect" in new Fixture {

    val output =
      """|circuit Foo:
         |  module Foo:
         |    input in: { flip ready: UInt<1>, valid: UInt<1>}
         |    output out : { flip ready: UInt<1>, valid: UInt<1>}
         |    out <= in
         |"""
        .stripMargin
        .parse
        .compile
        .circuit
        .serialize

    info(output)

  }

  it should "ignorea false RHS" in new Fixture {
    val output =
      """|circuit Foo :
         |  module Foo :
         |    output a : { flip a : UInt<1> }
         |    input  b : { flip a : UInt<1> }
         |    a <= b
         |"""
        .stripMargin
        .parse
        .compile
        .circuit
        .serialize

    info(output)
    output should include ("a <= b")

  }

  it should "replace a RHS usage of a module output" in {
    // new firrtl.stage.transforms.Compiler(
    //   targets = Seq(Dependency(firrtl.transforms.ExpandChiselRValues))
    // ).execute(
    //   CircuitState(moduleCircuit, Seq.empty)
    // )

    (new firrtl.stage.FirrtlStage)
      .execute(
        Array("-cll", "firrtl.transforms.ExpandChiselRValues$:trace"),
        Seq(firrtl.stage.FirrtlCircuitAnnotation(moduleCircuit))
      ).foreach {
        case a: firrtl.EmittedVerilogCircuitAnnotation => println(a.value.value)
        case _ =>
      }
  }

  behavior of "firrtl.transforms.ExpandChiselRValues with instance inputs used as sources"

  it should "replace a RHS usage of an instance input" in new Fixture {

    val output =
      """|circuit Foo:
         |  module Bar:
         |    input a: UInt<1>
         |    output b: UInt<1>
         |    b is invalid
         |  module Foo:
         |    output a: UInt<1>
         |    output b: UInt<1>
         |    inst bar of Bar
         |    bar.a is invalid
         |    a <= bar.a
         |    b <= bar.b
         |"""
        .stripMargin
        .parse
        .compile
        .circuit
        .serialize

    output should include (s"wire $syntheticName : { bar : { a : UInt<1>}}")
    output should include (s"$syntheticName.bar.a is invalid")
    output should include (s"a <= $syntheticName.bar.a")
    output should include (s"b <= bar.b")

  }

  it should "ignore a bi-directional connect" in new Fixture {

    val output =
      """|circuit Foo:
         |  module Bar:
         |    input a: { flip b: UInt<1>, c: UInt<1>}
         |  module Foo:
         |    input a: { flip b: UInt<1>, c: UInt<1>}
         |    inst bar of Bar
         |    a <= bar.a
         |"""
        .stripMargin
        .parse
        .compile
        .circuit
        .serialize

    output should include (s"a <= bar.a")

  }

  /* This is low priority as Chisel cannot emit this. */
  it should "replace a RHS usage of a memory input (addr, etc.)" in (pending)

  "module type canonicalization" should "work" in new Fixture {

    val output =
      """|circuit Foo:
         |  module Foo:
         |    output decoupled: { flip ready: UInt<1>, valid: UInt<1>, bits: UInt<1> }
         |"""
        .stripMargin
        .parse
        .compile
        .circuit
        .serialize

    info(output)

  }

}
