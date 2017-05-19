// See LICENSE for license details.

package firrtl

import transforms.core.passes._

sealed abstract class CoreTransform extends SeqTransform

/** This transforms "CHIRRTL", the chisel3 IR, to "Firrtl". Note the resulting
  * circuit has only IR nodes, not WIR.
  */
class ChirrtlToHighFirrtl extends CoreTransform {
  def inputForm = ChirrtlForm
  def outputForm = HighForm
  def transforms = Seq(
    CheckChirrtl,
    CInferTypes,
    CInferMDir,
    RemoveCHIRRTL)
}

/** Converts from the bare intermediate representation (ir.scala)
  * to a working representation (WIR.scala)
  */
class IRToWorkingIR extends CoreTransform {
  def inputForm = HighForm
  def outputForm = HighForm
  def transforms = Seq(ToWorkingIR)
}

/** Resolves types, kinds, and genders, and checks the circuit legality.
  * Operates on working IR nodes and high Firrtl.
  */
class ResolveAndCheck extends CoreTransform {
  def inputForm = HighForm
  def outputForm = HighForm
  def transforms = Seq(
    CheckHighForm,
    ResolveKinds,
    InferTypes,
    CheckTypes,
    Uniquify,
    ResolveKinds,
    InferTypes,
    ResolveGenders,
    CheckGenders,
    InferWidths,
    CheckWidths)
}

/** Expands aggregate connects, removes dynamic accesses, and when
  * statements. Checks for uninitialized values. Must accept a
  * well-formed graph.
  * Operates on working IR nodes.
  */
class HighFirrtlToMiddleFirrtl extends CoreTransform {
  def inputForm = HighForm
  def outputForm = MidForm
  def transforms = Seq(
    PullMuxes,
    ReplaceAccesses,
    ExpandConnects,
    RemoveAccesses,
    ExpandWhens,
    CheckInitialization,
    ResolveKinds,
    InferTypes,
    CheckTypes,
    ResolveGenders,
    InferWidths,
    CheckWidths,
    ConvertFixedToSInt,
    ZeroWidth)
}

/** Expands all aggregate types into many ground-typed components. Must
  * accept a well-formed graph of only middle Firrtl features.
  * Operates on working IR nodes.
  */
class MiddleFirrtlToLowFirrtl extends CoreTransform {
  def inputForm = MidForm
  def outputForm = LowForm
  def transforms = Seq(
    LowerTypes,
    ResolveKinds,
    InferTypes,
    ResolveGenders,
    InferWidths,
    Legalize,
    CheckCombLoops)
}

/** Runs a series of optimization passes on LowFirrtl
  * @note This is currently required for correct Verilog emission
  * TODO Fix the above note
  */
class LowFirrtlOptimization extends CoreTransform {
  def inputForm = LowForm
  def outputForm = LowForm
  def transforms = Seq(
    RemoveValidIf,
    ConstProp,
    PadWidths,
    ConstProp,
    Legalize,
    passes.memlib.VerilogMemDelays, // TODO move to Verilog emitter
    ConstProp,
    SplitExpressions,
    CommonSubexpressionElimination,
    new firrtl.transforms.DeadCodeElimination)
}


import firrtl.CompilerUtils.getLoweringTransforms
import firrtl.transforms.BlackBoxSourceHelper

/** Emits input circuit
  * Will replace Chirrtl constructs with Firrtl
  */
class HighFirrtlCompiler extends Compiler {
  def emitter = new HighFirrtlEmitter
  def transforms: Seq[Transform] = getLoweringTransforms(ChirrtlForm, HighForm)
}

/** Emits middle Firrtl input circuit */
class MiddleFirrtlCompiler extends Compiler {
  def emitter = new MiddleFirrtlEmitter
  def transforms: Seq[Transform] = getLoweringTransforms(ChirrtlForm, MidForm)
}

/** Emits lowered input circuit */
class LowFirrtlCompiler extends Compiler {
  def emitter = new LowFirrtlEmitter
  def transforms: Seq[Transform] = getLoweringTransforms(ChirrtlForm, LowForm)
}

/** Emits Verilog */
class VerilogCompiler extends Compiler {
  def emitter = new VerilogEmitter
  def transforms: Seq[Transform] = getLoweringTransforms(ChirrtlForm, LowForm) ++
    Seq(new LowFirrtlOptimization, new BlackBoxSourceHelper)
}
