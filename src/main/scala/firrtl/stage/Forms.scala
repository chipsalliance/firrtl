// See LICENSE for license details.

package firrtl.stage

import firrtl._
import firrtl.Transform

/*
 * - InferWidths should have InferTypes split out
 * - ConvertFixedToSInt should have InferTypes split out
 * - Move InferTypes out of ZeroWidth
 */

object Forms {

  val ChirrtlForm: Seq[Class[_ <: Transform]] = Seq.empty

  val HighForm: Seq[Class[_ <: Transform]] = ChirrtlForm ++
    Seq( classOf[passes.CheckChirrtl],
         classOf[passes.CInferTypes],
         classOf[passes.CInferMDir],
         classOf[passes.RemoveCHIRRTL] )

  val WorkingIR: Seq[Class[_ <: Transform]] = HighForm :+ classOf[passes.ToWorkingIR]

  val Resolved: Seq[Class[_ <: Transform]] = WorkingIR ++
    Seq( classOf[passes.CheckHighForm],
         classOf[passes.ResolveKinds],
         classOf[passes.InferTypes],
         classOf[passes.CheckTypes],
         classOf[passes.Uniquify],
         classOf[passes.ResolveGenders],
         classOf[passes.CheckGenders],
         classOf[passes.InferWidths],
         classOf[passes.CheckWidths] )

  val Deduped: Seq[Class[_ <: Transform]] = Resolved :+ classOf[firrtl.transforms.DedupModules]

  val MidForm: Seq[Class[_ <: Transform]] = Deduped ++
    Seq( classOf[passes.PullMuxes],
         classOf[passes.ReplaceAccesses],
         classOf[passes.ExpandConnects],
         classOf[passes.RemoveAccesses],
         classOf[passes.ExpandWhensAndCheck],
         classOf[passes.ConvertFixedToSInt],
         classOf[passes.ZeroWidth] )

  val LowForm: Seq[Class[_ <: Transform]] = MidForm ++
    Seq( classOf[passes.LowerTypes],
         classOf[passes.Legalize],
         classOf[firrtl.transforms.RemoveReset],
         classOf[firrtl.transforms.CheckCombLoops],
         classOf[firrtl.transforms.RemoveWires] )

  val LowFormMinimumOptimized: Seq[Class[_ <: Transform]] = LowForm ++
    Seq( classOf[passes.RemoveValidIf],
         classOf[passes.memlib.VerilogMemDelays],
         classOf[passes.SplitExpressions] )

  val LowFormOptimized: Seq[Class[_ <: Transform]] = LowFormMinimumOptimized ++
    Seq( classOf[firrtl.transforms.ConstantPropagation],
         classOf[passes.PadWidths],
         classOf[firrtl.transforms.CombineCats],
         classOf[passes.CommonSubexpressionElimination],
         classOf[firrtl.transforms.DeadCodeElimination] )

  val VerilogMinimumOptimized: Seq[Class[_ <: Transform]] = LowFormMinimumOptimized ++
    Seq( classOf[firrtl.transforms.BlackBoxSourceHelper],
         classOf[firrtl.transforms.ReplaceTruncatingArithmetic],
         classOf[firrtl.transforms.FlattenRegUpdate],
         classOf[passes.VerilogModulusCleanup],
         classOf[firrtl.transforms.VerilogRename],
         classOf[passes.VerilogPrep],
         classOf[firrtl.AddDescriptionNodes] )

  val VerilogOptimized: Seq[Class[_ <: Transform]] = LowFormOptimized ++
    Seq( classOf[firrtl.transforms.BlackBoxSourceHelper],
         classOf[firrtl.transforms.ReplaceTruncatingArithmetic],
         classOf[firrtl.transforms.FlattenRegUpdate],
         classOf[passes.VerilogModulusCleanup],
         classOf[firrtl.transforms.VerilogRename],
         classOf[passes.VerilogPrep],
         classOf[firrtl.AddDescriptionNodes] )

}
