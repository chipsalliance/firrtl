// See LICENSE for license details.

package firrtlTests.analyses

import de.danielbechler.diff.ObjectDifferBuilder
import firrtl.{ChirrtlForm, CircuitState, HighFirrtlCompiler, LowFirrtlCompiler, LowFirrtlOptimization, LowForm, MiddleFirrtlCompiler}
import firrtl.annotations.Annotation
import firrtl.passes.RemoveEmpty
import firrtl.passes.memlib.SimpleTransform
import firrtlTests.FirrtlFlatSpec

class DifferenceSpec extends FirrtlFlatSpec {

  private val customTransforms = Seq(
    new LowFirrtlOptimization
    , new SimpleTransform(RemoveEmpty, LowForm)
  )

  private def exec(input: String, check: String, annos: Seq[Annotation] = List.empty): Unit = {
    val state = CircuitState(parse(input), ChirrtlForm, annos)
    val highState = (new MiddleFirrtlCompiler).compileAndEmit(state)
    val finalState = (new LowFirrtlCompiler).compileAndEmit(state, customTransforms)
    //val res = finalState.getEmittedCircuit.value
    val differ = new firrtl.analyses.Difference(highState, finalState)
    //println(highState.circuit.serialize)
    //println(finalState.circuit.serialize)

    //differ.makeIterator.foreach { case (f1, f2) => println(f1.serialize); println("------"); println(f2.serialize)}
    val cost = differ.diffSashaAndZhang()
    //println(cost)
    val c = differ.buildDiffAST(highState.circuit, cost)
    val cx = differ.buildSerializableDiffAST(c)
    //println(firrtl.analyses.Difference.stringifyAST(cx))
    println(cx.serialize)


    //println(cost)
  }
  "test of myers diff on strings" should "work" in {
    val differ = new firrtl.analyses.ImperativeDiffer("ABC", "XBC")
    val x = differ.diff()
    println(x)
  }

  "Unread wire" should "be deleted" in {
    val input =
      """circuit Top :
        |  module Top :
        |    input x : UInt<2>
        |    output z : UInt<2>
        |    wire a : UInt<2>
        |    z <= and(a, x)
        |    a <= UInt(1)""".stripMargin
    val check =
      """circuit Top :
        |  module Top :
        |    input x : UInt<2>
        |    output z : UInt<2>
        |    z <= and(UInt(1), x)""".stripMargin
    exec(input, check)
  }
  "RocketCore" should "be diffable" in {
    val input = readFromFile("ICache", "regress").mkString("\n")
    exec(input, input)
  }

}
