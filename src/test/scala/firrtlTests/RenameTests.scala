package firrtlTests

import org.scalatest.Matchers
import java.io.{StringWriter, Writer}
import firrtl.passes.RenameChiselTemps
import firrtl.{Circuit, Transform, TransformResult}
import firrtl.Annotations.AnnotationMap

/**
 * RenameChiselTemps is a Pass, and we wrap it in a transform to test it.
 */
class RenameChiselTempsTransform extends Transform {
  def execute (circuit: Circuit, annotations: AnnotationMap): TransformResult = {
    TransformResult(RenameChiselTemps.run(circuit))
  }
}

/**
 * Tests the following cases for rewriting:
 *   1) Rewriting a generated node
 *   2) Rewriting a generated node based on another temporary node
 *   3) Rewriting a generated node with a duplicate name
 *   3) Rewriting a generated register based off of itself
 */
class RenameChiselTempsSpec extends LowTransformSpec with Matchers  {
  val transform = new RenameChiselTempsTransform()
  // =============================
  "T_0" should "renamed to __a_813d5_MUX" in {
    val input =
"""circuit Top :
  module Top :
    input a : UInt<1>
    input b : UInt<5>
    input c : UInt<5>
    output y : UInt<5>
    node T_0 = mux(a, b, c)
    y <= T_0
"""
    val check =
"""circuit Top :
  module Top :
    input a : UInt<1>
    input b : UInt<5>
    input c : UInt<5>
    output y : UInt<5>
    node __a_813d5_MUX = mux(a, b, c)
    skip
    y <= __a_813d5_MUX
"""
    execute(new StringWriter(), new AnnotationMap(Seq.empty), input, check)
  }

  // =============================
  "T_0" should "renamed to __a_25a7a_ADD" in {
    val input =
"""circuit Top :
  module Top :
    input a : UInt<1>
    input b : UInt<5>
    input c : UInt<5>
    output y : UInt<6>
    node T_1 = mux(a, b, c)
    node T_0 = add(T_1, T_1)
    y <= T_0
"""
    val check =
"""circuit Top :
  module Top :
    input a : UInt<1>
    input b : UInt<5>
    input c : UInt<5>
    output y : UInt<6>
    node __a_813d5_MUX = mux(a, b, c)
    node __a_25a7a_ADD = add(__a_813d5_MUX, __a_813d5_MUX)
    skip
    y <= __a_25a7a_ADD 
"""
    execute(new StringWriter(), new AnnotationMap(Seq.empty), input, check)
  }

  // =============================
  "T_1" should "renamed to __a_813d5_MUX_0" in {
    val input =
"""circuit Top :
  module Top :
    input a : UInt<1>
    input b : UInt<5>
    input c : UInt<5>
    output y : UInt<5>
    output z : UInt<5>
    node T_0 = mux(a, b, c)
    node T_1 = mux(a, b, c)
    y <= T_0
    z <= T_1
"""
    val check =
"""circuit Top :
  module Top :
    input a : UInt<1>
    input b : UInt<5>
    input c : UInt<5>
    output y : UInt<5>
    output z : UInt<5>
    node __a_813d5_MUX = mux(a, b, c)
    node __a_813d5_MUX_0 = mux(a, b, c)
    skip
    skip
    y <= __a_813d5_MUX
    z <= __a_813d5_MUX_0
"""
    execute(new StringWriter(), new AnnotationMap(Seq.empty), input, check)
  }

  // =============================
  "T_0" should "renamed to __GEN_ab4ff_REG" in {
    val input =
"""circuit Top :
  module Top :
    input clk : Clock
    input reset : UInt<1>
    output z : UInt<5>
    reg T_0 : UInt<5>, clk with: (reset => (reset, UInt(0)))
    T_0 <= T_0
    z <= T_0
"""
    val check =
"""circuit Top :
  module Top :
    input clk : Clock
    input reset : UInt<1>
    output z : UInt<5>
    reg __GEN_ab4ff_REG : UInt<5>, clk with: (reset => (reset, UInt(0)))
    skip
    skip
    z <= __GEN_ab4ff_REG
    __GEN_ab4ff_REG <= __GEN_ab4ff_REG
"""
    execute(new StringWriter(), new AnnotationMap(Seq.empty), input, check)
  }
}
