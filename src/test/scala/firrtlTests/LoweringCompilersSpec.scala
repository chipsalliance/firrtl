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
    classOf[passes.ResolveFlows],
    classOf[passes.CheckFlows],
    classOf[passes.InferWidths],
    classOf[passes.CheckWidths],
    classOf[firrtl.transforms.InferResets],
    classOf[passes.CheckTypes] )
  val HighFirrtlToMiddleFirrtl = Seq(
    classOf[passes.PullMuxes],
    classOf[passes.ReplaceAccesses],
    classOf[passes.ExpandConnects],
    classOf[passes.RemoveAccesses],
    classOf[passes.Uniquify],
    classOf[passes.ResolveKinds],
    classOf[passes.InferTypes],
    classOf[passes.ExpandWhensAndCheck],
    classOf[firrtl.checks.CheckResets],
    classOf[passes.ResolveKinds],
    classOf[passes.InferTypes],
    classOf[passes.ResolveFlows],
    classOf[passes.InferWidths],
    classOf[passes.ConvertFixedToSInt],
    classOf[passes.ZeroWidth],
    classOf[passes.InferTypes] )
  val MiddleFirrtlToLowFirrtl = Seq(
    classOf[passes.LowerTypes],
    classOf[passes.ResolveKinds],
    classOf[passes.InferTypes],
    classOf[passes.ResolveFlows],
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
  import firrtl.{ChirrtlForm => C, HighForm => H, MidForm => M, LowForm => L, UnknownForm => U}
  class ChirrtlToChirrtl extends IdentityTransformDiff(C, C)
  class HighToChirrtl    extends IdentityTransformDiff(H, C)
  class HighToHigh       extends IdentityTransformDiff(H, H)
  class MidToMid         extends IdentityTransformDiff(M, M)
  class MidToChirrtl     extends IdentityTransformDiff(M, C)
  class MidToHigh        extends IdentityTransformDiff(M, H)
  class LowToChirrtl     extends IdentityTransformDiff(L, C)
  class LowToHigh        extends IdentityTransformDiff(L, H)
  class LowToMid         extends IdentityTransformDiff(L, M)
  class LowToLow         extends IdentityTransformDiff(L, L)
}

class LoweringCompilersSpec extends FlatSpec with Matchers {

  def compare(a: Seq[Transform], b: Seq[Class[_ <: Transform]]): Unit = {
    a.map(_.getClass).zip(b).foreach{ case (aa, bb) =>
      info(s"${aa.getName} ok!")
      aa should be (bb)
    }

    info(s"found ${b.size} transforms")
    a.size should be (b.size)
  }

  behavior of "ChirrtlToHighFirrtl"

  it should "replicate the old order" in {
    compare((new ChirrtlToHighFirrtl).transforms, Orderings.ChirrtlToHighFirrtl)
  }

  behavior of "IRToWorkingIR"

  it should "replicate the old order" in {
    compare((new IRToWorkingIR).transforms, Seq(classOf[passes.ToWorkingIR]))
  }

  behavior of "ResolveAndCheck"

  it should "replicate the old order" in {
    compare((new ResolveAndCheck).transforms, Orderings.ResolveAndCheck)
  }

  behavior of "HighFirrtlToMiddleFirrtl"

  it should "replicate the old order" in {
    compare((new HighFirrtlToMiddleFirrtl).transforms, Orderings.HighFirrtlToMiddleFirrtl)
  }

  behavior of "MiddleFirrtlToLowFirrtl"

  it should "replicate the old order" in {
    compare((new MiddleFirrtlToLowFirrtl).transforms, Orderings.MiddleFirrtlToLowFirrtl)
  }

  behavior of "MinimumLowFirrtlOptimization"

  it should "replicate the old order" in {
    compare((new MinimumLowFirrtlOptimization).transforms, Orderings.MinimumLowFirrtlOptimization)
  }

  behavior of "LowFirrtlOptimization"

  it should "replicate the old order" in {
    compare((new LowFirrtlOptimization).transforms, Orderings.LowFirrtlOptimization)
  }

  behavior of "VerilogMinimumOptimized"

  it should "replicate the old order" in {
    compare((new TransformManager(Forms.VerilogMinimumOptimized, Forms.LowFormMinimumOptimized))
              .flattenedTransformOrder,
            Orderings.VerilogMinimumOptimized)
  }

  behavior of "VerilogOptimized"

  it should "replicate the old order" in {
    compare((new TransformManager(Forms.VerilogOptimized, Forms.LowFormOptimized)).flattenedTransformOrder,
            Orderings.VerilogOptimized)
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
    compare((new TransformManager(Seq(classOf[firrtl.VerilogEmitter]), Forms.ChirrtlForm)).flattenedTransformOrder,
            oldOrder)
  }

  behavior of "Legacy Custom Transforms"

  it should "work for CHIRRTL -> CHIRRTL" in {
  }

  it should "work for High -> High" in {
    val expected = Orderings.ChirrtlToHighFirrtl ++
      Some(classOf[passes.ToWorkingIR]) ++
      Orderings.ResolveAndCheck ++
      Some(classOf[firrtl.transforms.DedupModules]) ++
      Some(classOf[Transforms.HighToHigh]) ++
      Orderings.HighFirrtlToMiddleFirrtl
    compare((new TransformManager(Forms.MidForm :+ classOf[Transforms.HighToHigh])).flattenedTransformOrder, expected)
  }

  it should "work for Mid -> Mid" in {
    val expected = Orderings.ChirrtlToHighFirrtl ++
      Some(classOf[passes.ToWorkingIR]) ++
      Orderings.ResolveAndCheck ++
      Some(classOf[firrtl.transforms.DedupModules]) ++
      Orderings.HighFirrtlToMiddleFirrtl ++
      Some(classOf[Transforms.MidToMid]) ++
      Orderings.MiddleFirrtlToLowFirrtl
    compare((new TransformManager(Forms.LowForm :+ classOf[Transforms.MidToMid])).flattenedTransformOrder, expected)
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
    compare((new TransformManager(Forms.LowForm :+ classOf[Transforms.MidToHigh])).flattenedTransformOrder, expected)
  }

  it should "work for Low -> Low" in {
    val expected = Orderings.ChirrtlToHighFirrtl ++
      Some(classOf[passes.ToWorkingIR]) ++
      Orderings.ResolveAndCheck ++
      Some(classOf[firrtl.transforms.DedupModules]) ++
      Orderings.HighFirrtlToMiddleFirrtl ++
      Orderings.MiddleFirrtlToLowFirrtl ++
      Orderings.LowFirrtlOptimization ++
      Seq(classOf[Forms.LowFormOptimizedHook], classOf[Transforms.LowToLow])
    compare((new TransformManager(Forms.LowFormOptimized :+ classOf[Transforms.LowToLow])).flattenedTransformOrder,
            expected)
  }

  it should "work for Low -> Mid" in {
    val expected = Orderings.ChirrtlToHighFirrtl ++
      Some(classOf[passes.ToWorkingIR]) ++
      Orderings.ResolveAndCheck ++
      Some(classOf[firrtl.transforms.DedupModules]) ++
      Orderings.HighFirrtlToMiddleFirrtl ++
      Orderings.MiddleFirrtlToLowFirrtl ++
      Orderings.LowFirrtlOptimization ++
      Seq(classOf[Forms.LowFormOptimizedHook], classOf[Transforms.LowToMid]) ++
      Orderings.MiddleFirrtlToLowFirrtl ++
      Orderings.LowFirrtlOptimization
    compare((new TransformManager(Forms.LowFormOptimized :+ classOf[Transforms.LowToMid])).flattenedTransformOrder,
            expected)
  }

  it should "work for Low -> High" in {
    val expected = Orderings.ChirrtlToHighFirrtl ++
      Some(classOf[passes.ToWorkingIR]) ++
      Orderings.ResolveAndCheck ++
      Some(classOf[firrtl.transforms.DedupModules]) ++
      Orderings.HighFirrtlToMiddleFirrtl ++
      Orderings.MiddleFirrtlToLowFirrtl ++
      Orderings.LowFirrtlOptimization ++
      Seq(classOf[Forms.LowFormOptimizedHook], classOf[Transforms.LowToHigh]) ++
      Some(classOf[passes.ToWorkingIR]) ++
      Orderings.ResolveAndCheck ++
      Some(classOf[firrtl.transforms.DedupModules]) ++
      Orderings.HighFirrtlToMiddleFirrtl ++
      Orderings.MiddleFirrtlToLowFirrtl ++
      Orderings.LowFirrtlOptimization
    compare((new TransformManager(Forms.LowFormOptimized :+ classOf[Transforms.LowToHigh])).flattenedTransformOrder,
            expected)
  }
}
