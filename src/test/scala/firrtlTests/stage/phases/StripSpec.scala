// See LICENSE for license details.

package firrtlTests.stage.phases

import org.scalatest.{FlatSpec, Matchers}

import firrtl.NoneCompiler
import firrtl.stage._
import firrtl.stage.phases.Strip
import firrtl.ir
import firrtl.annotations.{CircuitName, DeletedAnnotation}
import firrtl.passes.{InlineInstances, InlineAnnotation}

class StripSpec extends FlatSpec with Matchers {

  trait WithAnnotations {
    val inlineTransform = new InlineInstances
    val annotations = Seq( FirrtlFileAnnotation("foo"),
                           OutputFileAnnotation("bar"),
                           InfoModeAnnotation("append"),
                           FirrtlSourceAnnotation("qux"),
                           CompilerAnnotation(new NoneCompiler()),
                           RunFirrtlTransformAnnotation(inlineTransform),
                           FirrtlCircuitAnnotation(ir.Circuit(ir.NoInfo, Seq.empty, "quuz")),
                           DeletedAnnotation("quzz", InlineAnnotation(CircuitName("corge"))) )
  }

  behavior of Strip.getClass.getName

  it should "strip all unnecessary/unneeded/deleted annotations" in new WithAnnotations {
    Strip.transform(annotations).toSeq should be (
      Seq(RunFirrtlTransformAnnotation(inlineTransform)))
  }

  it should "do nothing if a DontStripAnnotation is present" in new WithAnnotations {
    Strip.transform(DontStripAnnotation +: annotations).toSeq should be (DontStripAnnotation +: annotations)
  }
}
