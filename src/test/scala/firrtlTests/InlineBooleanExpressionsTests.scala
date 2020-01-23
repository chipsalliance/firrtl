// See LICENSE for license details.

package firrtlTests

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import firrtl.ir.Circuit
import firrtl.Parser
import firrtl.passes.PassExceptions
import firrtl.annotations._
import firrtl.passes.{InlineAnnotation, InlineInstances}
import firrtl.transforms.{InlineBooleanExpressions, NoCircuitDedupAnnotation}
import logger.{LogLevel, Logger}
import logger.LogLevel.Debug

/**
 * Tests inline instances transformation
 */
class InlineBooleanExpressionsTests extends LowTransformSpec {
  def transform = new InlineBooleanExpressions
  def inline(mod: String): Annotation = {
    val parts = mod.split('.')
    val modName = ModuleName(parts.head, CircuitName("Top")) // If this fails, bad input
    val name = if (parts.size == 1) modName else ComponentName(parts.tail.mkString("."), modName)
    InlineAnnotation(name)
  }
  // Set this to debug, this will apply to all tests
  // Logger.setLevel(this.getClass, Debug)
  "The module Top" should "have the boolean expressions combined" in {
    val input =
      """circuit Top :
        |  module Top :
        |    input a : UInt<1>
        |    input b : UInt<1>
        |    input c : UInt<1>
        |    output y : UInt<1>
        |    node _x = and(a, b)
        |    y <= and(_x, c)""".stripMargin
    val check =
      """circuit Top :
        |  module Top :
        |    input a : UInt<1>
        |    input b : UInt<1>
        |    input c : UInt<1>
        |    output y : UInt<1>
        |    node _x = and(a, b)
        |    y <= and(and(a, b), c)""".stripMargin
    execute(input, check, Seq(inline("Inline")))
  }

  // Set this to debug, this will apply to all tests
  // Logger.setLevel(this.getClass, Debug)
  "The module Top" should "not have the boolean expressions combined" in {
    val input =
      """circuit Top :
        |  module Top :
        |    input a : UInt<1>
        |    input b : UInt<1>
        |    input c : UInt<1>
        |    input d : UInt<1>
        |    output y : UInt<1>
        |    output z : UInt<1>
        |    node _x = and(a, b)
        |    y <= and(_x, c)
        |    z <= and(_x, d)
        |""".stripMargin
    val check =
      """circuit Top :
        |  module Top :
        |    input a : UInt<1>
        |    input b : UInt<1>
        |    input c : UInt<1>
        |    input d : UInt<1>
        |    output y : UInt<1>
        |    output z : UInt<1>
        |    node _x = and(a, b)
        |    y <= and(_x, c)
        |    z <= and(_x, d)
        |""".stripMargin
    execute(input, check, Seq(inline("Inline")))
  }

}