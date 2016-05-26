//TODO(izraelevitz): Restructure package hierarchy to firrtl.tests
package firrtlTests

import org.scalatest.Matchers
import java.io.StringWriter
import firrtl.passes.InlineSASR
import firrtl.{Transform, TransformResult, Circuit}
import firrtl.Annotations.AnnotationMap

/**
 * InlineSASR is a Pass, we wrap it in a transform to test it.
 * [S|M]A[S|M]R means [Single|Multiple] Assignment [Single|Multiple] Read
 */
class InlineSASRTransform extends Transform {
   def execute (circuit: Circuit, annotations: AnnotationMap): TransformResult = {
      TransformResult(InlineSASR.run(circuit))
   }
}

/**
 * Tests the following legal candidates for inlining
 * 1) Reference that is ground type, referenced once, a wire, assigned to
 * once, and has a generated name
 * 2) Reference that is ground type, referenced once, a node, and assigned to
 * once, and has a generated name
 *
 * Tests the following illegal candidates for inlining
 * 1) Bundle-type reference
 * 2) Register
 * 3) Wire read from twice
 * 4) Wire assigned to twice
 */
class InlineSASRSpec extends HighTransformSpec with Matchers  {
   val transform = new InlineSASRTransform()
   // =============================
   "The SASR ground-typed wire T_0" should "be inlined" in {
      val input =
"""circuit Top :
  module Top :
    output y : UInt<5>
    wire T_0:UInt<5>
    T_0 <= UInt(0)
    y <= T_0
"""
      val check =
"""circuit Top :
  module Top :
    output y : UInt<5>
    skip
    skip
    y <= UInt(0)
"""
      execute(new StringWriter(), new AnnotationMap(Seq.empty), input, check)
   }
   // =============================
   "The SASR ground-typed node T_0" should "be inlined" in {
      val input =
"""circuit Top :
  module Top :
    output y : UInt<5>
    node T_0 = UInt(0)
    y <= T_0
"""
      val check =
"""circuit Top :
  module Top :
    output y : UInt<5>
    skip
    y <= UInt(0)
"""
      execute(new StringWriter(), new AnnotationMap(Seq.empty), input, check)
   }
   // =============================
   "The SASR bundle-typed node T_0" should "not be inlined" in {
      val input =
"""circuit Top :
  module Top :
    input x : {a:UInt<5>, b:UInt<5>}
    output y : {a:UInt<5>, b:UInt<5>}
    node T_0 = x
    y <= T_0
"""
      val check =
"""circuit Top :
  module Top :
    input x : {a:UInt<5>, b:UInt<5>}
    output y : {a:UInt<5>, b:UInt<5>}
    node T_0 = x
    y <= T_0
"""
      execute(new StringWriter(), new AnnotationMap(Seq.empty), input, check)
   }
   // =============================
   "The SASR ground-typed register T_0" should "not be inlined" in {
      val input =
"""circuit Top :
  module Top :
    input clk:Clock
    output y : UInt<5>
    reg T_0 : UInt<5>, clk with :
      reset => (UInt(0), T_0)
    T_0 <= UInt(0)
    y <= T_0
"""
      val check =
"""circuit Top :
  module Top :
    input clk:Clock
    output y : UInt<5>
    reg T_0 : UInt<5>, clk with :
      reset => (UInt(0), T_0)
    T_0 <= UInt(0)
    y <= T_0
"""
      execute(new StringWriter(), new AnnotationMap(Seq.empty), input, check)
   }
   // =============================
   "The SAMR ground-typed wire T_0" should "not be inlined" in {
      val input =
"""circuit Top :
  module Top :
    output y : UInt<5>
    output z : UInt<5>
    wire T_0 : UInt<5>
    T_0 <= UInt(0)
    y <= T_0
    z <= T_0
"""
      val check =
"""circuit Top :
  module Top :
    output y : UInt<5>
    output z : UInt<5>
    wire T_0 : UInt<5>
    T_0 <= UInt(0)
    y <= T_0
    z <= T_0
"""
      execute(new StringWriter(), new AnnotationMap(Seq.empty), input, check)
   }
   // =============================
   "The MASR ground-typed wire T_0" should "not be inlined" in {
      val input =
"""circuit Top :
  module Top :
    output y : UInt<5>
    wire T_0 : UInt<5>
    T_0 <= UInt(0)
    T_0 <= UInt(1)
    y <= T_0
"""
      val check =
"""circuit Top :
  module Top :
    output y : UInt<5>
    wire T_0 : UInt<5>
    T_0 <= UInt(0)
    T_0 <= UInt(1)
    y <= T_0
"""
      execute(new StringWriter(), new AnnotationMap(Seq.empty), input, check)
   }
}
