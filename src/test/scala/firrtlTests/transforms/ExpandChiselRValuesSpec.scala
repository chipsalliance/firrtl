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
         |  module Foo:
         |    output a: UInt<1>
         |    inst bar of Bar
         |    bar.a is invalid
         |    a <= bar.a
         |"""
        .stripMargin
        .parse
        .compile
        .circuit
        .serialize

    output should include (s"wire $syntheticName : { bar : { a : UInt<1>}}")
    output should include (s"$syntheticName.bar.a is invalid")
    output should include (s"a <= $syntheticName.bar.a")

  }

  it should "replace a RHS usage of a memory input (addr, etc.)" in (pending)

}
