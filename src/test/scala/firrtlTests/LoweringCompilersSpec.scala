// See LICENSE for license details.

package firrtlTests

import org.scalatest.{FlatSpec, Matchers}

import firrtl.{ChirrtlToHighFirrtl, CircuitForm, CircuitState, HighFirrtlToMiddleFirrtl, IRToWorkingIR,
  LowFirrtlOptimization, MiddleFirrtlToLowFirrtl, MinimumLowFirrtlOptimization, ResolveAndCheck, Transform}
import firrtl.passes
import firrtl.stage.{Forms, TransformManager}
import firrtl.transforms.IdentityTransform

object Orderings {
  val ChirrtlToHighFirrtl = Seq(
    classOf[passes.CheckChirrtl],
    classOf[passes.CInferTypes],
    classOf[passes.CInferMDir],
    classOf[passes.RemoveCHIRRTL] )
  val ResolveAndCheck = Seq(
    classOf[passes.CheckHighForm],
    classOf[passes.ResolveKinds],
    classOf[passes.InferTypes],
    classOf[passes.CheckTypes],
    classOf[passes.Uniquify],
    classOf[passes.ResolveKinds],
    classOf[passes.InferTypes],
    classOf[passes.ResolveGenders],
    classOf[passes.CheckGenders],
    classOf[passes.InferWidths],
    classOf[passes.CheckWidths] )
  val HighFirrtlToMiddleFirrtl = Seq(
    classOf[passes.PullMuxes],
    classOf[passes.ReplaceAccesses],
    classOf[passes.ExpandConnects],
    classOf[passes.RemoveAccesses],
    classOf[passes.Uniquify],
    classOf[passes.ResolveKinds],
    classOf[passes.InferTypes],
    classOf[passes.ExpandWhensAndCheck],
    classOf[passes.ResolveKinds],
    classOf[passes.InferTypes],
    classOf[passes.ResolveGenders],
    classOf[passes.InferWidths],
    classOf[passes.ConvertFixedToSInt],
    classOf[passes.ZeroWidth],
    classOf[passes.InferTypes] )
  val MiddleFirrtlToLowFirrtl = Seq(
    classOf[passes.LowerTypes],
    classOf[passes.ResolveKinds],
    classOf[passes.InferTypes],
    classOf[passes.ResolveGenders],
    classOf[passes.InferWidths],
    classOf[passes.Legalize],
    classOf[firrtl.transforms.RemoveReset],
    classOf[firrtl.transforms.CheckCombLoops],
    classOf[firrtl.transforms.RemoveWires] )
  val MinimumLowFirrtlOptimization = Seq(
    classOf[passes.RemoveValidIf],
    classOf[passes.Legalize],
    classOf[passes.memlib.VerilogMemDelays],
    classOf[passes.SplitExpressions] )
  val LowFirrtlOptimization = Seq(
    classOf[passes.RemoveValidIf],
    classOf[passes.Legalize],
    classOf[firrtl.transforms.ConstantPropagation],
    classOf[passes.PadWidths],
    classOf[passes.Legalize],
    classOf[firrtl.transforms.ConstantPropagation],
    classOf[passes.memlib.VerilogMemDelays],
    classOf[firrtl.transforms.ConstantPropagation],
    classOf[passes.SplitExpressions],
    classOf[firrtl.transforms.CombineCats],
    classOf[passes.CommonSubexpressionElimination],
    classOf[firrtl.transforms.DeadCodeElimination] )
  val VerilogMinimumOptimized = Seq(
    classOf[firrtl.transforms.BlackBoxSourceHelper],
    classOf[firrtl.transforms.ReplaceTruncatingArithmetic],
    classOf[firrtl.transforms.FlattenRegUpdate],
    classOf[passes.VerilogModulusCleanup],
    classOf[firrtl.transforms.VerilogRename],
    classOf[passes.VerilogPrep],
    classOf[firrtl.AddDescriptionNodes] )
  val VerilogOptimized = Seq(
    classOf[firrtl.transforms.BlackBoxSourceHelper],
    classOf[firrtl.transforms.ReplaceTruncatingArithmetic],
    classOf[firrtl.transforms.FlattenRegUpdate],
    classOf[firrtl.transforms.DeadCodeElimination],
    classOf[passes.VerilogModulusCleanup],
    classOf[firrtl.transforms.VerilogRename],
    classOf[passes.VerilogPrep],
    classOf[firrtl.AddDescriptionNodes] )
}

object Transforms {
  class IdentityTransformDiff(val inputForm: CircuitForm, val outputForm: CircuitForm) extends Transform {
    override def execute(state: CircuitState): CircuitState = state
  }
  class HighToHigh extends IdentityTransform(firrtl.HighForm)
  class MidToMid extends IdentityTransform(firrtl.MidForm)
  class MidToHigh extends IdentityTransformDiff(firrtl.MidForm, firrtl.HighForm)
  class LowToLow extends IdentityTransform(firrtl.LowForm)
  class LowToMid extends IdentityTransformDiff(firrtl.LowForm, firrtl.MidForm)
  class LowToHigh extends IdentityTransformDiff(firrtl.LowForm, firrtl.HighForm)
}

class LoweringCompilersSpec extends FlatSpec with Matchers {

  behavior of "ChirrtlToHighFirrtl"

  it should "replicate the old order" in {
    (new ChirrtlToHighFirrtl)
      .transforms
      .map(_.getClass) should be (Orderings.ChirrtlToHighFirrtl)
  }

  behavior of "IRToWorkingIR"

  it should "replicate the old order" in {
    (new IRToWorkingIR)
      .transforms
      .map(_.getClass) should be (Seq(classOf[passes.ToWorkingIR]))
  }

  behavior of "ResolveAndCheck"

  it should "replicate the old order" in {
    (new ResolveAndCheck)
      .transforms
      .map(_.getClass) should be (Orderings.ResolveAndCheck)
  }

  behavior of "HighFirrtlToMiddleFirrtl"

  it should "replicate the old order" in {
    (new HighFirrtlToMiddleFirrtl)
      .transforms
      .map(_.getClass) should be (Orderings.HighFirrtlToMiddleFirrtl)
  }

  behavior of "MiddleFirrtlToLowFirrtl"

  it should "replicate the old order" in {
    (new MiddleFirrtlToLowFirrtl)
      .transforms
      .map(_.getClass) should be (Orderings.MiddleFirrtlToLowFirrtl)
  }

  behavior of "MinimumLowFirrtlOptimization"

  it should "replicate the old order" in {
    (new MinimumLowFirrtlOptimization)
      .transforms
      .map(_.getClass) should be (Orderings.MinimumLowFirrtlOptimization)
  }

  behavior of "LowFirrtlOptimization"

  it should "replicate the old order" in {
    (new LowFirrtlOptimization)
      .transforms
      .map(_.getClass) should be (Orderings.LowFirrtlOptimization)
  }

  behavior of "VerilogMinimumOptimized"

  it should "replicate the old order" in {
    (new TransformManager(Forms.VerilogMinimumOptimized, Forms.LowFormMinimumOptimized))
      .flattenedTransformOrder
      .map(_.getClass) should be (Orderings.VerilogMinimumOptimized)
  }

  behavior of "VerilogOptimized"

  it should "replicate the old order" in {
    (new TransformManager(Forms.VerilogOptimized, Forms.LowFormOptimized))
      .flattenedTransformOrder
      .map(_.getClass) should be (Orderings.VerilogOptimized)
  }

  behavior of "Chirrtl to Verilog"

  it should "replicate the old order" in {
    val oldOrder =
      Orderings.ChirrtlToHighFirrtl ++
        Some(classOf[passes.ToWorkingIR]) ++
        Orderings.ResolveAndCheck ++
        Some(classOf[firrtl.transforms.DedupModules]) ++
        Orderings.HighFirrtlToMiddleFirrtl ++
        Orderings.MiddleFirrtlToLowFirrtl ++
        Orderings.LowFirrtlOptimization ++
        Some(classOf[firrtl.VerilogEmitter])
    (new TransformManager(Seq(classOf[firrtl.VerilogEmitter]), Forms.ChirrtlForm))
      .flattenedTransformOrder.map(_.getClass) should be (oldOrder)
  }

  behavior of "Legacy Custom Transforms"

  it should "work for High -> High" in {
    val expected = Orderings.ChirrtlToHighFirrtl ++
      Some(classOf[Transforms.HighToHigh]) ++
      Some(classOf[passes.ToWorkingIR]) ++
      Orderings.ResolveAndCheck ++
      Some(classOf[firrtl.transforms.DedupModules]) ++
      Orderings.HighFirrtlToMiddleFirrtl
    (new TransformManager(Forms.MidForm :+ classOf[Transforms.HighToHigh]))
      .flattenedTransformOrder
      .map(_.getClass) should be (expected)
  }

  it should "work for Mid -> Mid" in {
    val expected = Orderings.ChirrtlToHighFirrtl ++
      Some(classOf[passes.ToWorkingIR]) ++
      Orderings.ResolveAndCheck ++
      Some(classOf[firrtl.transforms.DedupModules]) ++
      Orderings.HighFirrtlToMiddleFirrtl ++
      Some(classOf[Transforms.MidToMid]) ++
      Orderings.MiddleFirrtlToLowFirrtl
    (new TransformManager(Forms.LowForm :+ classOf[Transforms.MidToMid]))
      .flattenedTransformOrder
      .map(_.getClass) should be (expected)
  }
  it should "work for Mid -> High" in {
    val expected = Orderings.ChirrtlToHighFirrtl ++
      Some(classOf[passes.ToWorkingIR]) ++
      Orderings.ResolveAndCheck ++
      Some(classOf[firrtl.transforms.DedupModules]) ++
      Orderings.HighFirrtlToMiddleFirrtl ++
      Some(classOf[Transforms.MidToHigh]) ++
      Some(classOf[passes.ToWorkingIR]) ++
      Orderings.ResolveAndCheck ++
      Some(classOf[firrtl.transforms.DedupModules]) ++
      Orderings.HighFirrtlToMiddleFirrtl ++
      Orderings.MiddleFirrtlToLowFirrtl
    (new TransformManager(Forms.LowForm :+ classOf[Transforms.MidToHigh]))
      .flattenedTransformOrder
      .map(_.getClass) should be (expected)
  }

  it should "work for Low -> Low" in {
    val expected = Orderings.ChirrtlToHighFirrtl ++
      Some(classOf[passes.ToWorkingIR]) ++
      Orderings.ResolveAndCheck ++
      Some(classOf[firrtl.transforms.DedupModules]) ++
      Orderings.HighFirrtlToMiddleFirrtl ++
      Orderings.MiddleFirrtlToLowFirrtl ++
      Some(classOf[Transforms.LowToLow]) ++
      Orderings.LowFirrtlOptimization
    (new TransformManager(Forms.LowFormOptimized :+ classOf[Transforms.LowToLow]))
      .flattenedTransformOrder
      .map(_.getClass) should be (expected)
  }

  it should "work for Low -> Mid" in {
    val expected = Orderings.ChirrtlToHighFirrtl ++
      Some(classOf[passes.ToWorkingIR]) ++
      Orderings.ResolveAndCheck ++
      Some(classOf[firrtl.transforms.DedupModules]) ++
      Orderings.HighFirrtlToMiddleFirrtl ++
      Orderings.MiddleFirrtlToLowFirrtl ++
      Some(classOf[Transforms.LowToMid]) ++
      Orderings.MiddleFirrtlToLowFirrtl ++
      Orderings.LowFirrtlOptimization
    (new TransformManager(Forms.LowFormOptimized :+ classOf[Transforms.LowToMid]))
      .flattenedTransformOrder
      .map(_.getClass) should be (expected)
  }

  it should "work for Low -> High" in {
    val expected = Orderings.ChirrtlToHighFirrtl ++
      Some(classOf[passes.ToWorkingIR]) ++
      Orderings.ResolveAndCheck ++
      Some(classOf[firrtl.transforms.DedupModules]) ++
      Orderings.HighFirrtlToMiddleFirrtl ++
      Orderings.MiddleFirrtlToLowFirrtl ++
      Some(classOf[Transforms.LowToHigh]) ++
      Some(classOf[passes.ToWorkingIR]) ++
      Orderings.ResolveAndCheck ++
      Some(classOf[firrtl.transforms.DedupModules]) ++
      Orderings.HighFirrtlToMiddleFirrtl ++
      Orderings.MiddleFirrtlToLowFirrtl ++
      Orderings.LowFirrtlOptimization
    (new TransformManager(Forms.LowFormOptimized :+ classOf[Transforms.LowToHigh]))
      .flattenedTransformOrder
      .map(_.getClass) should be (expected)
  }
}
