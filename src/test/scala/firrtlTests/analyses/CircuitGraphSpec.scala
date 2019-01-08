// See LICENSE for license details.

package firrtlTests.analyses

import firrtl.analyses.CircuitGraph
import firrtl.annotations.CircuitTarget
import firrtl.transforms.clockfinder.ClockFinder._
import firrtlTests.{FirrtlRunners, MiddleAnnotationSpec}
import firrtlTests.transforms.{DeepHierarchyStuff, MemStuff}

class CircuitGraphSpec extends MiddleAnnotationSpec with MemStuff with FirrtlRunners with DeepHierarchyStuff {

  "CircuitGraph" should "find register-register clock domain crossings" in {

    val C = CircuitTarget("Test")
    val Test = C.module("Test")
    val input =
      """circuit Test:
        |  module Test:
        |    input in: UInt<8>
        |    input reset: UInt<1>
        |    input clk1: Clock
        |    input clk2: Clock
        |    output out: UInt<8>
        |    reg r1: UInt<8>, clk1 with:
        |      (reset => (UInt(0), UInt(0)))
        |    reg r2: UInt<8>, clk2 with:
        |      (reset => (UInt(0), r2))
        |    node x = in
        |    r1 <= x
        |    inst c1 of Child
        |    inst c2 of Child
        |    c1.in <= r1
        |    c2.in <= c1.out
        |    r2 <= c2.out
        |    out <= r2
        |  module Child:
        |    input in: UInt<8>
        |    output out: UInt<8>
        |    node y = in
        |    out <= y
      """.stripMargin

    val circuit = toMiddleFIRRTL(parse(input))
    val circuitGraph = CircuitGraph(circuit)

    val regClockPath = Seq(Test.ref("clk2"), Test.ref("r2").clock, Test.ref("r2"))
    val c1 = Test.instOf("c1", "Child")
    val c2 = Test.instOf("c2", "Child")
    val regInputPath = Seq(
      Test.ref("clk1"),
      Test.ref("r1").clock,
      Test.ref("r1"),
      Test.ref("c1").field("in"),
      c1.ref("in"),
      c1.ref("y"),
      c1.ref("out"),
      Test.ref("c1").field("out"),
      Test.ref("c2").field("in"),
      c2.ref("in"),
      c2.ref("..."),
      c2.ref("out"),
      Test.ref("c2").field("out"),
      Test.ref("r2")
    )

    val result = circuitGraph.findClockCrossings(Set.empty, Map(Test.ref("in") -> Set(Test.ref("clk1"))))

    println(result.map(_.prettyPrint()).mkString("\n"))
    result shouldBe Seq(IllegalClockCrossing(Test.ref("r2"), Seq(regClockPath, regInputPath)))
    result.head.clockPaths.head.zip(regClockPath).foreach { x =>
      x._1 shouldBe x._2
    }
    result.head.clockPaths.last.zip(regInputPath).foreach { x =>
      x._1 shouldBe x._2
    }

    val result2 = circuitGraph.findClockCrossings(Set(Test.ref("r1")))
    println(result2.map(_.prettyPrint()).mkString("\n"))
    result2.head.clockPaths.head.zip(regClockPath).foreach { x =>
      x._1 shouldBe x._2
    }
    result2.head.clockPaths.last.zip(regInputPath).foreach { x =>
      x._1 shouldBe x._2
    }
    result2 shouldBe Seq(IllegalClockCrossing(Test.ref("r2"), Seq(regClockPath, regInputPath)))
  }

  "CircuitGraph" should "find paths with deep hierarchy quickly" in {
    (2 until 23 by 2).foreach { n =>
      val input = new StringBuilder()
      input ++=
        """circuit Child0:
          |""".stripMargin
      (0 until n).foreach { i => input ++= mkChild(i); input ++= "\n" }
      input ++= mkLeaf(n)
      val circuit = toMiddleFIRRTL(parse(input.toString()))
      val circuitGraph = CircuitGraph(circuit)
      val C = CircuitTarget("Child0")
      val Child0 = C.module("Child0")
      circuitGraph.path(Child0.ref("in"), Child0.ref("out"))
    }
  }

  "CircuitGraph" should "find paths with deep hierarchy quickly" in {
    (2 until 23 by 2).foreach { n =>
      val input = new StringBuilder()
      input ++=
        """circuit Child0:
          |""".stripMargin
      (0 until n).foreach { i => input ++= mkChild(i); input ++= "\n" }
      input ++= mkLeaf(n)
      val circuit = toMiddleFIRRTL(parse(input.toString()))
      val circuitGraph = CircuitGraph(circuit)
      val C = CircuitTarget("Child0")
      val Child0 = C.module("Child0")
      circuitGraph.path(Child0.ref("in"), Child0.ref("out"))
    }
  }

}
