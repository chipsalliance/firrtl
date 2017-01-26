// See LICENSE for license details.

package firrtlTests.transforms

import java.io.StringWriter

import firrtl.annotations.{Annotation, CircuitName, ModuleName}
import firrtl.transforms._
import firrtl.{AnnotationMap, FIRRTLException, Transform}
import firrtlTests.HighTransformSpec
import org.scalacheck.Test.Failed
import org.scalatest.{FreeSpec, Matchers, Succeeded}


/**
 * Tests inline instances transformation
 */
class BlacklBoxSourceHelperSpec extends FreeSpec with Matchers {
  "BlackBoxSourceAnnotations" - {
    val modName = ModuleName("dog", CircuitName("fox"))
    val resource = "file://somefile.v"

    "should parse and unparse" in {

      val serialized = BlackBoxResource(resource).serialize
      BlackBoxSource.parse(serialized) match {
        case Some(BlackBoxResource(id)) =>
          id should be (resource)
          Succeeded
        case _ => Failed
      }
    }
    "should fail on unsupported kinds" in {
      intercept[FIRRTLException] {
        BlackBoxSourceAnnotation(modName, "bad value")
      }
      BlackBoxSourceAnnotation(modName, BlackBoxResource(resource).serialize).isInstanceOf[Annotation] should be(true)
    }
  }
}

class BlacklBoxSourceHelperTransformSpec extends HighTransformSpec {
   def transform: Transform = new BlackBoxSourceHelper

  private val moduleName = ModuleName("Top", CircuitName("Top"))
  private val input = """
    |circuit Top :
    |
    |  extmodule AdderExtModule :
    |    input foo : UInt<16>
    |    output bar : UInt<16>
    |
    |    defname = BBFAdd
    |
    |  module Top :
    |   input x : UInt<16>
    |   output y : UInt<16>
    |
    |   inst a1 of AdderExtModule
    |   a1.foo <= x
    |   y <= a1.bar
  """.stripMargin

  "annotated external modules" should "appear in output directory" in {

    val writer = new StringWriter()
    val aMap = AnnotationMap(Seq(
      Annotation(moduleName, classOf[BlackBoxSourceHelper], BlackBoxTargetDir("test_run_dir").serialize),
      Annotation(moduleName, classOf[BlackBoxSourceHelper], BlackBoxResource("/blackboxes/AdderExtModule.v").serialize)
    ))

    execute(writer, aMap, input, input)

    new java.io.File("test_run_dir/AdderExtModule.v").exists should be (true)
    new java.io.File(s"test_run_dir/${BlackBoxSourceHelper.FileListName}").exists should be (true)
  }
}

