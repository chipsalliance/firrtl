// See LICENSE for license details.

package firrtlTests

import java.io.{Writer, StringWriter}

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner

import firrtl.ir.Circuit
import firrtl.{Parser, AnnotationMap}
import firrtl.{
   CircuitState,
   ResolveAndCheck,
   RenameMap,
   Compiler,
   ChirrtlForm,
   LowForm,
   VerilogCompiler,
   Transform
}
import firrtl.annotations.{
   Named,
   CircuitName,
   ModuleName,
   ComponentName,
   AnnotationException,
   Annotation
}

/**
 * An example methodology for testing Firrtl annotations.
 */
trait AnnotationSpec extends LowTransformSpec {
  // Dummy transform
  def transform = new CustomResolveAndCheck(LowForm)

  // Check if Annotation Exception is thrown
  override def failingexecute(writer: Writer, annotations: AnnotationMap, input: String) = {
    intercept[AnnotationException] {
      compile(CircuitState(parse(input), ChirrtlForm, Some(annotations)), writer)
    }
  }
  def execute(writer: Writer, annotations: AnnotationMap, input: String, check: Annotation) = {
    val cr = compile(CircuitState(parse(input), ChirrtlForm, Some(annotations)), writer)
    (cr.annotations.get.annotations) should be (Seq(check))
  }
}


/**
 * Tests for Annotation Permissibility and Tenacity
 *
 * WARNING(izraelevitz): Not a complete suite of tests, requires the LowerTypes
 * pass and ConstProp pass to correctly populate its RenameMap before Strict, Rigid, Firm,
 * Unstable, Fickle, and Insistent can be tested.
 */
class AnnotationTests extends AnnotationSpec with Matchers {
  def getAMap (a: Annotation): AnnotationMap = new AnnotationMap(Seq(a))
  val input =
    """circuit Top :
       |  module Top :
       |    input a : UInt<1>[2]
       |    input b : UInt<1>
       |    node c = b""".stripMargin
  val mName = ModuleName("Top", CircuitName("Top"))
  val aName = ComponentName("a", mName)
  val bName = ComponentName("b", mName)
  val cName = ComponentName("c", mName)

  "Loose and Sticky annotation on a node" should "pass through" in {
    val w = new StringWriter()
    val ta = Annotation(cName, classOf[Transform], "")
    execute(w, getAMap(ta), input, ta)
  }
}
