// See LICENSE for license details.

package firrtl.stage

import firrtl._
import firrtl.options.DependencyID
import firrtl.stage.TransformManager.TransformDependency

/*
 * - InferWidths should have InferTypes split out
 * - ConvertFixedToSInt should have InferTypes split out
 * - Move InferTypes out of ZeroWidth
 */

object Forms {

  def ChirrtlForm: Seq[TransformDependency] = Seq.empty

  def MinimalHighForm: Seq[TransformDependency] = ChirrtlForm ++
    Seq( DependencyID[passes.CheckChirrtl],
         DependencyID[passes.CInferTypes],
         DependencyID[passes.CInferMDir],
         DependencyID[passes.RemoveCHIRRTL] )

  def WorkingIR: Seq[TransformDependency] = MinimalHighForm :+ DependencyID[passes.ToWorkingIR]

  def Resolved: Seq[TransformDependency] = WorkingIR ++
    Seq( DependencyID[passes.CheckHighForm],
         DependencyID[passes.ResolveKinds],
         DependencyID[passes.InferTypes],
         DependencyID[passes.CheckTypes],
         DependencyID[passes.Uniquify],
         DependencyID[passes.ResolveFlows],
         DependencyID[passes.CheckFlows],
         DependencyID[passes.InferBinaryPoints],
         DependencyID[passes.TrimIntervals],
         DependencyID[passes.InferWidths],
         DependencyID[passes.CheckWidths],
         DependencyID[firrtl.transforms.InferResets] )

  def Deduped: Seq[TransformDependency] = Resolved :+ DependencyID[firrtl.transforms.DedupModules]

  def HighForm: Seq[TransformDependency] = ChirrtlForm ++
    MinimalHighForm ++
    WorkingIR ++
    Resolved ++
    Deduped

  def MidForm: Seq[TransformDependency] = HighForm ++
    Seq( DependencyID[passes.PullMuxes],
         DependencyID[passes.ReplaceAccesses],
         DependencyID[passes.ExpandConnects],
         DependencyID[passes.RemoveAccesses],
         DependencyID[passes.ExpandWhensAndCheck],
         DependencyID[passes.RemoveIntervals],
         DependencyID[passes.ConvertFixedToSInt],
         DependencyID[passes.ZeroWidth] )

  def LowForm: Seq[TransformDependency] = MidForm ++
    Seq( DependencyID(passes.LowerTypes),
         DependencyID[passes.Legalize],
         DependencyID[firrtl.transforms.RemoveReset],
         DependencyID[firrtl.transforms.CheckCombLoops],
         DependencyID[checks.CheckResets],
         DependencyID[firrtl.transforms.RemoveWires] )

  def LowFormMinimumOptimized: Seq[TransformDependency] = LowForm ++
    Seq( DependencyID[passes.RemoveValidIf],
         DependencyID[passes.memlib.VerilogMemDelays],
         DependencyID[passes.SplitExpressions] )

  def LowFormOptimized: Seq[TransformDependency] = LowFormMinimumOptimized ++
    Seq( DependencyID[firrtl.transforms.ConstantPropagation],
         DependencyID[passes.PadWidths],
         DependencyID[firrtl.transforms.CombineCats],
         DependencyID[passes.CommonSubexpressionElimination],
         DependencyID[firrtl.transforms.DeadCodeElimination] )

  def VerilogMinimumOptimized: Seq[TransformDependency] = LowFormMinimumOptimized ++
    Seq( DependencyID[firrtl.transforms.BlackBoxSourceHelper],
         DependencyID[firrtl.transforms.ReplaceTruncatingArithmetic],
         DependencyID[firrtl.transforms.FlattenRegUpdate],
         DependencyID[passes.VerilogModulusCleanup],
         DependencyID[firrtl.transforms.VerilogRename],
         DependencyID[passes.VerilogPrep],
         DependencyID[firrtl.AddDescriptionNodes] )

  def VerilogOptimized: Seq[TransformDependency] = LowFormOptimized ++
    Seq( DependencyID[firrtl.transforms.BlackBoxSourceHelper],
         DependencyID[firrtl.transforms.ReplaceTruncatingArithmetic],
         DependencyID[firrtl.transforms.FlattenRegUpdate],
         DependencyID[passes.VerilogModulusCleanup],
         DependencyID[firrtl.transforms.VerilogRename],
         DependencyID[passes.VerilogPrep],
         DependencyID[firrtl.AddDescriptionNodes] )

}
