// See LICENSE for license details.

package firrtlTests

import org.scalatest.{FlatSpec, Matchers}

import firrtl.{ChirrtlToHighFirrtl, CircuitForm, CircuitState, HighFirrtlToMiddleFirrtl, IRToWorkingIR,
  LowFirrtlOptimization, MiddleFirrtlToLowFirrtl, MinimumLowFirrtlOptimization, ResolveAndCheck, SeqTransform,
  Transform, UnknownForm}
import firrtl.passes
import firrtl.stage.{Forms, TransformManager}
import firrtl.transforms.IdentityTransform

sealed trait PatchAction { val line: Int }

case class Add(line: Int, transforms: Seq[Class[_ <: Transform]]) extends PatchAction
case class Del(line: Int) extends PatchAction

object Transforms {
  class IdentityTransformDiff(val inputForm: CircuitForm, val outputForm: CircuitForm) extends Transform {
    override def execute(state: CircuitState): CircuitState = state
    override def name: String = s">>>>> $inputForm -> $outputForm <<<<<"
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

  def compare(a: Seq[Transform], b: TransformManager): Unit = {
    info(s"""Transform Order:\n${b.prettyPrint("    ")}""")
    a.map(_.getClass).zip(b.flattenedTransformOrder.map(_.getClass)).foreach{ case (aa, bb) => bb should be (aa) }
    info(s"found ${b.flattenedTransformOrder.size} transforms")
    a.size should be (b.flattenedTransformOrder.size)
  }

  def compareLegacy(a: SeqTransform, b: TransformManager, patches: Seq[PatchAction] = Seq.empty): Unit = {
    info(s"""Transform Order:\n${b.prettyPrint("    ")}""")

    val m = new scala.collection.mutable.HashMap[Int, Seq[Class[_ <: Transform]]].withDefault(_ => Seq.empty)
    a.transforms.map(_.getClass).zipWithIndex.foreach{ case (t, idx) => m(idx) = Seq(t) }

    patches.foreach {
      case Add(line, txs) => m(line - 1) = m(line - 1) ++ txs
      case Del(line)      => m.remove(line - 1)
    }

    val patched = scala.collection.immutable.TreeMap(m.toArray:_*).values.flatten

    patched
      .zip(b.flattenedTransformOrder.map(_.getClass))
      .foreach{ case (aa, bb) => bb should be (aa) }

    info(s"found ${b.flattenedTransformOrder.size} transforms")
    patched.size should be (b.flattenedTransformOrder.size)
  }

  behavior of "ChirrtlToHighFirrtl"

  it should "replicate the old order" in {
    val tm = new TransformManager(Forms.MinimalHighForm, Forms.ChirrtlForm)
    compareLegacy(new firrtl.ChirrtlToHighFirrtl, tm)
  }

  behavior of "IRToWorkingIR"

  it should "replicate the old order" in {
    val tm = new TransformManager(Forms.WorkingIR, Forms.MinimalHighForm)
    compareLegacy(new firrtl.IRToWorkingIR, tm)
  }

  behavior of "ResolveAndCheck"

  it should "replicate the old order" in {
    val tm = new TransformManager(Forms.Resolved, Forms.WorkingIR)
    val patches = Seq(
      Add(13, Seq(classOf[firrtl.passes.CheckTypes]))
    )
    compareLegacy(new ResolveAndCheck, tm, patches)
  }

  behavior of "HighFirrtlToMiddleFirrtl"

  it should "replicate the old order" in {
    val tm = new TransformManager(Forms.MidForm, Forms.Deduped)
    val patches = Seq(
      Add(5, Seq(classOf[firrtl.passes.ResolveKinds],
                 classOf[firrtl.passes.InferTypes])),
      Del(6),
      Del(7),
      Add(6, Seq(classOf[firrtl.passes.ExpandWhensAndCheck])),
      Del(10),
      Del(11),
      Del(12),
      Add(11, Seq(classOf[firrtl.passes.ResolveFlows],
                  classOf[firrtl.passes.InferWidths])),
      Del(12),
      Add(12, Seq(classOf[firrtl.checks.CheckResets])),
      Del(13),
      Del(14)
    )
    compareLegacy(new HighFirrtlToMiddleFirrtl, tm, patches)
  }

  behavior of "MiddleFirrtlToLowFirrtl"

  it should "replicate the old order" in {
    val tm = new TransformManager(Forms.LowForm, Forms.MidForm)
    compareLegacy(new MiddleFirrtlToLowFirrtl, tm)
  }

  behavior of "MinimumLowFirrtlOptimization"

  it should "replicate the old order" in {
    val tm = new TransformManager(Forms.LowFormMinimumOptimized, Forms.LowForm)
    compareLegacy(new MinimumLowFirrtlOptimization, tm)
  }

  behavior of "LowFirrtlOptimization"

  it should "replicate the old order" in {
    val tm = new TransformManager(Forms.LowFormOptimized, Forms.LowForm)
    val patches = Seq(
      Add(7, Seq(classOf[firrtl.passes.Legalize]))
    )
    compareLegacy(new LowFirrtlOptimization, tm, patches)
  }

  behavior of "VerilogMinimumOptimized"

  it should "replicate the old order" in {
    val old = new SeqTransform {
      def inputForm = UnknownForm
      def outputForm = UnknownForm
      def transforms = Seq(
        new firrtl.transforms.BlackBoxSourceHelper,
        new firrtl.transforms.ReplaceTruncatingArithmetic,
        new firrtl.transforms.FlattenRegUpdate,
        new firrtl.passes.VerilogModulusCleanup,
        new firrtl.transforms.VerilogRename,
        new firrtl.passes.VerilogPrep,
        new firrtl.AddDescriptionNodes)
    }
    val tm = new TransformManager(Forms.VerilogMinimumOptimized, (new firrtl.VerilogEmitter).prerequisites)
    compareLegacy(old, tm)
  }

  behavior of "VerilogOptimized"

  it should "replicate the old order" in {
    val old = new SeqTransform {
      def inputForm = UnknownForm
      def outputForm = UnknownForm
      def transforms = Seq(
        new firrtl.transforms.BlackBoxSourceHelper,
        new firrtl.transforms.ReplaceTruncatingArithmetic,
        new firrtl.transforms.FlattenRegUpdate,
        new firrtl.transforms.DeadCodeElimination,
        new firrtl.passes.VerilogModulusCleanup,
        new firrtl.transforms.VerilogRename,
        new firrtl.passes.VerilogPrep,
        new firrtl.AddDescriptionNodes)
    }
    val tm = new TransformManager(Forms.VerilogOptimized, Forms.LowFormOptimized)
    compareLegacy(old, tm)
  }

  behavior of "Legacy Custom Transforms"

  it should "work for Chirrtl -> Chirrtl" in {
    val expected = new Transforms.ChirrtlToChirrtl :: new firrtl.ChirrtlEmitter :: Nil
    val tm = new TransformManager(classOf[firrtl.ChirrtlEmitter] :: classOf[Transforms.ChirrtlToChirrtl] :: Nil)
    compare(expected, tm)
  }

  it should "work for High -> High" in {
    val expected =
      new TransformManager(Forms.HighForm).flattenedTransformOrder ++
        Some(new Transforms.HighToHigh) ++
        (new TransformManager(Forms.MidForm, Forms.HighForm).flattenedTransformOrder)
    val tm = new TransformManager(Forms.MidForm :+ classOf[Transforms.HighToHigh])
    compare(expected, tm)
  }

  it should "work for High -> Chirrtl" in {
    val expected =
      new TransformManager(Forms.HighForm).flattenedTransformOrder ++
        Some(new Transforms.HighToChirrtl) ++
        (new TransformManager(Forms.HighForm, Forms.ChirrtlForm).flattenedTransformOrder)
    val tm = new TransformManager(Forms.HighForm :+ classOf[Transforms.HighToChirrtl])
    compare(expected, tm)
  }

  it should "work for Mid -> Mid" in {
    val expected =
      new TransformManager(Forms.MidForm).flattenedTransformOrder ++
        Some(new Transforms.MidToMid) ++
        (new TransformManager(Forms.LowForm, Forms.MidForm).flattenedTransformOrder)
    val tm = new TransformManager(Forms.LowForm :+ classOf[Transforms.MidToMid])
    compare(expected, tm)
  }

  it should "work for Mid -> High" in {
    val expected =
      new TransformManager(Forms.MidForm).flattenedTransformOrder ++
        Some(new Transforms.MidToHigh) ++
        (new TransformManager(Forms.LowForm, Forms.MinimalHighForm).flattenedTransformOrder)
    val tm = new TransformManager(Forms.LowForm :+ classOf[Transforms.MidToHigh])
    compare(expected, tm)
  }

  it should "work for Mid -> Chirrtl" in {
    val expected =
      new TransformManager(Forms.MidForm).flattenedTransformOrder ++
        Some(new Transforms.MidToChirrtl) ++
        (new TransformManager(Forms.LowForm, Forms.ChirrtlForm).flattenedTransformOrder)
    val tm = new TransformManager(Forms.LowForm :+ classOf[Transforms.MidToChirrtl])
    compare(expected, tm)
  }

  it should "work for Low -> Low" in {
    val expected =
      new TransformManager(Forms.LowFormOptimized).flattenedTransformOrder ++
        Seq(new Forms.LowFormOptimizedHook, new Transforms.LowToLow)
    val tm = new TransformManager(Forms.LowFormOptimized :+ classOf[Transforms.LowToLow])
    compare(expected, tm)
  }

  it should "work for Low -> Mid" in {
    val expected =
      new TransformManager(Forms.LowFormOptimized).flattenedTransformOrder ++
        Seq(new Forms.LowFormOptimizedHook, new Transforms.LowToMid) ++
        (new TransformManager(Forms.LowFormOptimized, Forms.MidForm).flattenedTransformOrder)
    val tm = new TransformManager(Forms.LowFormOptimized :+ classOf[Transforms.LowToMid])
    compare(expected, tm)
  }

  it should "work for Low -> High" in {
    val expected =
      new TransformManager(Forms.LowFormOptimized).flattenedTransformOrder ++
        Seq(new Forms.LowFormOptimizedHook, new Transforms.LowToHigh) ++
        (new TransformManager(Forms.LowFormOptimized, Forms.MinimalHighForm).flattenedTransformOrder)
    val tm = new TransformManager(Forms.LowFormOptimized :+ classOf[Transforms.LowToHigh])
    compare(expected, tm)
  }

  it should "work for Low -> Chirrtl" in {
    val expected =
      new TransformManager(Forms.LowFormOptimized).flattenedTransformOrder ++
        Seq(new Forms.LowFormOptimizedHook, new Transforms.LowToChirrtl) ++
        (new TransformManager(Forms.LowFormOptimized, Forms.ChirrtlForm).flattenedTransformOrder)
    val tm = new TransformManager(Forms.LowFormOptimized :+ classOf[Transforms.LowToChirrtl])
    compare(expected, tm)
  }

  it should "schedule inputForm=LowForm after MiddleFirrtlToLowFirrtl for the LowFirrtlEmitter" in {
    val expected =
      new TransformManager(Forms.LowForm).flattenedTransformOrder ++
        Seq(new Forms.LowFormOptimizedHook, new Transforms.LowToLow, new firrtl.LowFirrtlEmitter)
    val tm = (new TransformManager(Seq(classOf[firrtl.LowFirrtlEmitter], classOf[Transforms.LowToLow])))
    compare(expected, tm)
  }

  it should "schedule inputForm=LowForm after MinimumLowFirrtlOptimizations for the MinimalVerilogEmitter" in {
    val expected =
      new TransformManager(Forms.LowFormMinimumOptimized).flattenedTransformOrder ++
        Seq(new Forms.LowFormOptimizedHook, new Transforms.LowToLow, new firrtl.MinimumVerilogEmitter)
    val tm = (new TransformManager(Seq(classOf[firrtl.MinimumVerilogEmitter], classOf[Transforms.LowToLow])))
    compare(expected, tm)
  }

  it should "schedule inputForm=LowForm after LowFirrtlOptimizations for the VerilogEmitter" in {
    val expected =
      new TransformManager(Forms.LowFormOptimized).flattenedTransformOrder ++
        Seq(new Forms.LowFormOptimizedHook, new Transforms.LowToLow, new firrtl.VerilogEmitter)
    val tm = (new TransformManager(Seq(classOf[firrtl.VerilogEmitter], classOf[Transforms.LowToLow])))
    compare(expected, tm)
  }

}
