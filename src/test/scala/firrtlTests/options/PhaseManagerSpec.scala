// See LICENSE for license details.

package firrtlTests.options

import org.scalatest.{FlatSpec, Matchers}

import firrtl.AnnotationSeq
import firrtl.options.{DependencyManagerException, Phase, PhaseManager, PreservesAll, DependencyID}

import java.io.{File, PrintWriter}

import sys.process._

trait IdentityPhase extends Phase {
  def transform(annotations: AnnotationSeq): AnnotationSeq = annotations
}

/** Default [[Phase]] that has no prerequisites and invalidates nothing */
class A extends IdentityPhase {

  override def invalidates(phase: Phase): Boolean = false
}

/** [[Phase]] that requires [[A]] and invalidates nothing */
class B extends IdentityPhase {
  override def prerequisites = Seq(DependencyID[A])
  override def invalidates(phase: Phase): Boolean = false
}

/** [[Phase]] that requires [[B]] and invalidates nothing */
class C extends IdentityPhase {
  override def prerequisites = Seq(DependencyID[A])
  override def invalidates(phase: Phase): Boolean = false
}

/** [[Phase]] that requires [[A]] and invalidates [[A]] */
class D extends IdentityPhase {
  override def prerequisites = Seq(DependencyID[A])
  override def invalidates(phase: Phase): Boolean = phase match {
    case _: A => true
    case _ => false
  }
}

/** [[Phase]] that requires [[B]] and invalidates nothing */
class E extends IdentityPhase {
  override def prerequisites = Seq(DependencyID[B])
  override def invalidates(phase: Phase): Boolean = false
}

/** [[Phase]] that requires [[B]] and [[C]] and invalidates [[E]] */
class F extends IdentityPhase {
  override def prerequisites = Seq(DependencyID[B], DependencyID[C])
  override def invalidates(phase: Phase): Boolean = phase match {
    case _: E => true
    case _ => false
  }
}


/** [[Phase]] that requires [[C]] and invalidates [[F]] */
class G extends IdentityPhase {
  override def prerequisites = Seq(DependencyID[C])
  override def invalidates(phase: Phase): Boolean = phase match {
    case _: F => true
    case _ => false
  }
}

class CyclicA extends IdentityPhase with PreservesAll[Phase] {
  override def prerequisites = Seq(DependencyID[CyclicB])
}

class CyclicB extends IdentityPhase with PreservesAll[Phase] {
  override def prerequisites = Seq(DependencyID[CyclicA])
}

class CyclicC extends IdentityPhase {
  override def invalidates(a: Phase): Boolean = a match {
    case _: CyclicD => true
    case _ => false
  }
}

class CyclicD extends IdentityPhase {
  override def invalidates(a: Phase): Boolean = a match {
    case _: CyclicC => true
    case _ => false
  }
}

object ComplicatedFixture {

  class A extends IdentityPhase {
    override def invalidates(phase: Phase): Boolean = false
  }
  class B extends IdentityPhase {
    override def prerequisites = Seq(DependencyID[A])
    override def invalidates(phase: Phase): Boolean = false
  }
  class C extends IdentityPhase {
    override def prerequisites = Seq(DependencyID[A])
    override def invalidates(phase: Phase): Boolean = phase match {
      case _: B => true
      case _ => false
    }
  }
  class D extends IdentityPhase {
    override def prerequisites = Seq(DependencyID[B])
    override def invalidates(phase: Phase): Boolean = phase match {
      case _: C | _: E => true
      case _ => false
    }
  }
  class E extends IdentityPhase {
    override def prerequisites = Seq(DependencyID[B])
    override def invalidates(phase: Phase): Boolean = false
  }

}

object RepeatedAnalysisFixture {

  trait InvalidatesAnalysis extends IdentityPhase {
    override def invalidates(phase: Phase): Boolean = phase match {
      case _: Analysis => true
      case _ => false
    }
  }

  class Analysis extends IdentityPhase {
    override def invalidates(phase: Phase): Boolean = false
  }
  class A extends InvalidatesAnalysis {
    override def prerequisites = Seq(DependencyID[Analysis])
  }
  class B extends InvalidatesAnalysis {
    override def prerequisites = Seq(DependencyID[A], DependencyID[Analysis])
  }
  class C extends InvalidatesAnalysis {
    override def prerequisites = Seq(DependencyID[B], DependencyID[Analysis])
  }

}

object InvertedAnalysisFixture {

  class Analysis extends IdentityPhase {
    override def invalidates(phase: Phase): Boolean = false
  }
  class A extends IdentityPhase {
    override def prerequisites = Seq(DependencyID[Analysis])
    override def invalidates(phase: Phase): Boolean = phase match {
      case _: Analysis => true
      case _ => false
    }
  }
  class B extends IdentityPhase {
    override def prerequisites = Seq(DependencyID[Analysis])
    override def invalidates(phase: Phase): Boolean = phase match {
      case _: Analysis | _: A => true
      case _ => false
    }
  }
  class C extends IdentityPhase {
    override def prerequisites = Seq(DependencyID[Analysis])
    override def invalidates(phase: Phase): Boolean = phase match {
      case _: Analysis | _: B => true
      case _ => false
    }
  }

}

object DependentsFixture {

  class First extends IdentityPhase {
    override def invalidates(phase: Phase): Boolean = false
  }

  class Second extends IdentityPhase {
    override val prerequisites = Seq(DependencyID[First])
    override def invalidates(phase: Phase): Boolean = false
  }

  /* This models a situation where a user has a custom Phase that they need to run before some other Phase. This is an
   * abstract example of writing a Transform that cleans up combinational loops. This needs to run before combinational
   * loop detection.
   */
  class Custom extends IdentityPhase {
    override val prerequisites = Seq(DependencyID[First])
    override val dependents = Seq(DependencyID[Second])
    override def invalidates(phase: Phase): Boolean = false
  }

}

object ChainedInvalidationFixture {

  class A extends IdentityPhase {
    override def invalidates(phase: Phase): Boolean = phase match {
      case _: B => true
      case _ => false
    }
  }
  class B extends IdentityPhase {
    override def invalidates(phase: Phase): Boolean = phase match {
      case _: C => true
      case _ => false
    }
  }
  class C extends IdentityPhase {
    override def invalidates(phase: Phase): Boolean = phase match {
      case _: D => true
      case _ => false
    }
  }
  class D extends IdentityPhase {
    override def invalidates(phase: Phase): Boolean = false
  }
  class E extends IdentityPhase {
    override val prerequisites = Seq(DependencyID[A], DependencyID[B], DependencyID[C], DependencyID[D])
    override def invalidates(phase: Phase): Boolean = false
  }

}

object UnrelatedFixture {

  trait InvalidatesB8Dep { this: Phase =>
    override def invalidates(a: Phase) = a match {
      case _: B8Dep => true
      case _        => false
    }
  }

  class B0 extends IdentityPhase with InvalidatesB8Dep
  class B1 extends IdentityPhase with PreservesAll[Phase]
  class B2 extends IdentityPhase with PreservesAll[Phase]
  class B3 extends IdentityPhase with PreservesAll[Phase]
  class B4 extends IdentityPhase with PreservesAll[Phase]
  class B5 extends IdentityPhase with PreservesAll[Phase]
  class B6 extends IdentityPhase with PreservesAll[Phase]
  class B7 extends IdentityPhase with PreservesAll[Phase]

  class B8 extends IdentityPhase with PreservesAll[Phase]
  class B9 extends IdentityPhase with PreservesAll[Phase]
  class B10 extends IdentityPhase with PreservesAll[Phase]
  class B11 extends IdentityPhase with PreservesAll[Phase]
  class B12 extends IdentityPhase with PreservesAll[Phase]
  class B13 extends IdentityPhase with PreservesAll[Phase]
  class B14 extends IdentityPhase with PreservesAll[Phase]
  class B15 extends IdentityPhase with PreservesAll[Phase]

  class B6Sub extends B6 {
    override val prerequisites = Seq(DependencyID[B6])
    override val dependents = Seq(DependencyID[B7])
  }

  class B6_0 extends B6Sub
  class B6_1 extends B6Sub
  class B6_2 extends B6Sub
  class B6_3 extends B6Sub
  class B6_4 extends B6Sub
  class B6_5 extends B6Sub
  class B6_6 extends B6Sub
  class B6_7 extends B6Sub
  class B6_8 extends B6Sub
  class B6_9 extends B6Sub
  class B6_10 extends B6Sub
  class B6_11 extends B6Sub
  class B6_12 extends B6Sub
  class B6_13 extends B6Sub
  class B6_14 extends B6Sub
  class B6_15 extends B6Sub

  class B8Dep extends B8 {
    override val dependents = Seq(DependencyID[B8])
  }

  class B8_0 extends B8Dep
  class B8_1 extends B8Dep
  class B8_2 extends B8Dep
  class B8_3 extends B8Dep
  class B8_4 extends B8Dep
  class B8_5 extends B8Dep
  class B8_6 extends B8Dep
  class B8_7 extends B8Dep
  class B8_8 extends B8Dep
  class B8_9 extends B8Dep
  class B8_10 extends B8Dep
  class B8_11 extends B8Dep
  class B8_12 extends B8Dep
  class B8_13 extends B8Dep
  class B8_14 extends B8Dep
  class B8_15 extends B8Dep

}

object CustomAfterOptimizationFixture {

  class Root extends IdentityPhase with PreservesAll[Phase]

  class OptMinimum extends IdentityPhase with PreservesAll[Phase] {
    override val prerequisites = Seq(DependencyID[Root])
    override val dependents = Seq(DependencyID[AfterOpt])
  }

  class OptFull extends IdentityPhase with PreservesAll[Phase] {
    override val prerequisites = Seq(DependencyID[Root], DependencyID[OptMinimum])
    override val dependents = Seq(DependencyID[AfterOpt])
  }

  class AfterOpt extends IdentityPhase with PreservesAll[Phase]

  class DoneMinimum extends IdentityPhase with PreservesAll[Phase] {
    override val prerequisites = Seq(DependencyID[OptMinimum])
  }

  class DoneFull extends IdentityPhase with PreservesAll[Phase] {
    override val prerequisites = Seq(DependencyID[OptFull])
  }

  class Custom extends IdentityPhase with PreservesAll[Phase] {
    override val prerequisites = Seq(DependencyID[Root], DependencyID[AfterOpt])
    override val dependents = Seq(DependencyID[DoneMinimum], DependencyID[DoneFull])
  }

}

object OptionalPrerequisitesFixture {

  class Root extends IdentityPhase

  class OptMinimum extends IdentityPhase with PreservesAll[Phase] {
    override val prerequisites = Seq(DependencyID[Root])
  }

  class OptFull extends IdentityPhase with PreservesAll[Phase] {
    override val prerequisites = Seq(DependencyID[Root], DependencyID[OptMinimum])
  }

  class DoneMinimum extends IdentityPhase with PreservesAll[Phase] {
    override val prerequisites = Seq(DependencyID[OptMinimum])
  }

  class DoneFull extends IdentityPhase with PreservesAll[Phase] {
    override val prerequisites = Seq(DependencyID[OptFull])
  }

  class Custom extends IdentityPhase with PreservesAll[Phase] {
    override val prerequisites = Seq(DependencyID[Root])
    override val optionalPrerequisites = Seq(DependencyID[OptMinimum], DependencyID[OptFull])
    override val dependents = Seq(DependencyID[DoneMinimum], DependencyID[DoneFull])
  }

}

object OrderingFixture {

  class A extends IdentityPhase with PreservesAll[Phase]

  class B extends IdentityPhase {
    override def invalidates(phase: Phase): Boolean = phase match {
      case _: A => true
      case _    => false
    }
  }

  class C extends IdentityPhase {
    override val prerequisites = Seq(DependencyID[A], DependencyID[B])
    override def invalidates(phase: Phase): Boolean = phase match {
      case _: B => true
      case _    => false
    }
  }

  class Cx extends C {
    override val prerequisites = Seq(DependencyID[B], DependencyID[A])
  }

}

class PhaseManagerSpec extends FlatSpec with Matchers {

  def writeGraphviz(pm: PhaseManager, dir: String): Unit = {

    /** Convert a Graphviz file to PNG using */
    def maybeToPng(f: File): Unit = try {
      s"dot -Tpng -O ${f}" !
    } catch {
      case _: java.io.IOException =>
    }

    val d = new File(dir)
    d.mkdirs()

    {
      val f = new File(d + "/dependencyGraph.dot")
      val w = new PrintWriter(f)
      w.write(pm.dependenciesToGraphviz)
      w.close
      maybeToPng(f)
    }

    {
      val f = new File(d + "/transformOrder.dot")
      val w = new PrintWriter(new File(d + "/transformOrder.dot"))
      try {
        info("transform order:\n" + pm.prettyPrint("    "))
        w.write(pm.transformOrderToGraphviz())
        w.close
        maybeToPng(f)
      } catch {
        case _: DependencyManagerException =>
      }
    }

  }

  behavior of this.getClass.getName

  it should "do nothing if all targets are reached" in {
    val targets = Seq(DependencyID[A], DependencyID[B], DependencyID[C], DependencyID[D])
    val pm = new PhaseManager(targets, targets)

    writeGraphviz(pm, "test_run_dir/PhaseManagerSpec/DoNothing")

    pm.flattenedTransformOrder should be (empty)
  }

  it should "handle a simple dependency" in {
    val targets = Seq(DependencyID[B])
    val order = Seq(classOf[A], classOf[B])
    val pm = new PhaseManager(targets)

    writeGraphviz(pm, "test_run_dir/PhaseManagerSpec/SimpleDependency")

    pm.flattenedTransformOrder.map(_.getClass) should be (order)
  }

  it should "handle a simple dependency with an invalidation" in {
    val targets = Seq(DependencyID[A], DependencyID[B], DependencyID[C], DependencyID[D])
    val order = Seq(classOf[A], classOf[D], classOf[A], classOf[B], classOf[C])
    val pm = new PhaseManager(targets)

    writeGraphviz(pm, "test_run_dir/PhaseManagerSpec/OneInvalidate")

    pm.flattenedTransformOrder.map(_.getClass) should be (order)
  }

  it should "handle a dependency with two invalidates optimally" in {
    val targets = Seq(DependencyID[A], DependencyID[B], DependencyID[C], DependencyID[E], DependencyID[F], DependencyID[G])
    val pm = new PhaseManager(targets)

    writeGraphviz(pm, "test_run_dir/PhaseManagerSpec/TwoInvalidates")

    pm.flattenedTransformOrder.size should be (targets.size)
  }

  it should "throw an exception for cyclic prerequisites" in {
    val targets = Seq(DependencyID[CyclicA], DependencyID[CyclicB])
    val pm = new PhaseManager(targets)

    writeGraphviz(pm, "test_run_dir/PhaseManagerSpec/CyclicPrerequisites")

    intercept[DependencyManagerException]{ pm.flattenedTransformOrder }
      .getMessage should startWith ("No transform ordering possible")
  }

  it should "throw an exception for cyclic invalidates" in {
    val targets = Seq(DependencyID[CyclicC], DependencyID[CyclicD])
    val pm = new PhaseManager(targets)

    writeGraphviz(pm, "test_run_dir/PhaseManagerSpec/CyclicInvalidates")

    intercept[DependencyManagerException]{ pm.flattenedTransformOrder }
      .getMessage should startWith ("No transform ordering possible")
  }

  it should "handle a complicated graph" in {
    val f = ComplicatedFixture
    val targets = Seq(DependencyID[f.A], DependencyID[f.B], DependencyID[f.C], DependencyID[f.D], DependencyID[f.E])
    val pm = new PhaseManager(targets)

    writeGraphviz(pm, "test_run_dir/PhaseManagerSpec/Complicated")

    info("only one phase was recomputed")
    pm.flattenedTransformOrder.size should be (targets.size + 1)
  }

  it should "handle repeated recomputed analyses" in {
    val f = RepeatedAnalysisFixture
    val targets = Seq(DependencyID[f.A], DependencyID[f.B], DependencyID[f.C])
    val order =
      Seq( classOf[f.Analysis],
           classOf[f.A],
           classOf[f.Analysis],
           classOf[f.B],
           classOf[f.Analysis],
           classOf[f.C])
    val pm = new PhaseManager(targets)

    writeGraphviz(pm, "test_run_dir/PhaseManagerSpec/RepeatedAnalysis")

    pm.flattenedTransformOrder.map(_.getClass) should be (order)
  }

  it should "handle inverted repeated recomputed analyses" in {
    val f = InvertedAnalysisFixture
    val targets = Seq(DependencyID[f.A], DependencyID[f.B], DependencyID[f.C])
    val order =
      Seq( classOf[f.Analysis],
           classOf[f.C],
           classOf[f.Analysis],
           classOf[f.B],
           classOf[f.Analysis],
           classOf[f.A])
    val pm = new PhaseManager(targets)

    writeGraphviz(pm, "test_run_dir/PhaseManagerSpec/InvertedRepeatedAnalysis")

    pm.flattenedTransformOrder.map(_.getClass) should be (order)
  }

  /** This test shows how the dependents member can be used to run one transform before another. */
  it should "handle a custom Phase with a dependent" in {
    val f = DependentsFixture

    info("without the custom transform it runs: First -> Second")
    val pm = new PhaseManager(Seq(DependencyID[f.Second]))
    val orderNoCustom = Seq(classOf[f.First], classOf[f.Second])
    pm.flattenedTransformOrder.map(_.getClass) should be (orderNoCustom)

    info("with the custom transform it runs:    First -> Custom -> Second")
    val pmCustom = new PhaseManager(Seq(DependencyID[f.Custom], DependencyID[f.Second]))
    val orderCustom = Seq(classOf[f.First], classOf[f.Custom], classOf[f.Second])

    writeGraphviz(pmCustom, "test_run_dir/PhaseManagerSpec/SingleDependent")

    pmCustom.flattenedTransformOrder.map(_.getClass) should be (orderCustom)
  }

  it should "handle chained invalidation" in {
    val f = ChainedInvalidationFixture

    val targets = Seq(DependencyID[f.A], DependencyID[f.E])
    val current = Seq(DependencyID[f.B], DependencyID[f.C], DependencyID[f.D])

    val pm = new PhaseManager(targets, current)
    val order = Seq( classOf[f.A], classOf[f.B], classOf[f.C], classOf[f.D], classOf[f.E] )

    writeGraphviz(pm, "test_run_dir/PhaseManagerSpec/ChainedInvalidate")

    pm.flattenedTransformOrder.map(_.getClass) should be (order)
  }

  it should "maintain the order of input targets" in {
    val f = UnrelatedFixture

    /** A bunch of unrelated Phases. This ensures that these run in the order in which they are specified. */
    val targets =
      Seq( DependencyID[f.B0],
           DependencyID[f.B1],
           DependencyID[f.B2],
           DependencyID[f.B3],
           DependencyID[f.B4],
           DependencyID[f.B5],
           DependencyID[f.B6],
           DependencyID[f.B7],
           DependencyID[f.B8],
           DependencyID[f.B9],
           DependencyID[f.B10],
           DependencyID[f.B11],
           DependencyID[f.B12],
           DependencyID[f.B13],
           DependencyID[f.B14],
           DependencyID[f.B15] )
    /** A sequence of custom transforms that should all run after B6 and before B7. This exercises correct ordering of the
      * prerequisiteGraph and dependentsGraph.
      */
    val prerequisiteTargets =
      Seq( DependencyID[f.B6_0],
           DependencyID[f.B6_1],
           DependencyID[f.B6_2],
           DependencyID[f.B6_3],
           DependencyID[f.B6_4],
           DependencyID[f.B6_5],
           DependencyID[f.B6_6],
           DependencyID[f.B6_7],
           DependencyID[f.B6_8],
           DependencyID[f.B6_9],
           DependencyID[f.B6_10],
           DependencyID[f.B6_11],
           DependencyID[f.B6_12],
           DependencyID[f.B6_13],
           DependencyID[f.B6_14],
           DependencyID[f.B6_15] )
    /** A sequence of transforms that are invalidated by B0 and only define dependents on B8. This exercises the ordering
      * defined by "otherDependents".
      */
    val current =
      Seq( DependencyID[f.B8_0],
           DependencyID[f.B8_1],
           DependencyID[f.B8_2],
           DependencyID[f.B8_3],
           DependencyID[f.B8_4],
           DependencyID[f.B8_5],
           DependencyID[f.B8_6],
           DependencyID[f.B8_7],
           DependencyID[f.B8_8],
           DependencyID[f.B8_9],
           DependencyID[f.B8_10],
           DependencyID[f.B8_11],
           DependencyID[f.B8_12],
           DependencyID[f.B8_13],
           DependencyID[f.B8_14],
           DependencyID[f.B8_15] )

    /** The resulting order: B0--B6, B6_0--B6_B15, B7, B8_0--B8_15, B8--B15 */
    val expectedDeps = targets.slice(0, 7) ++ prerequisiteTargets ++ Some(targets(7)) ++ current ++ targets.drop(8)
    val expectedClasses = expectedDeps.map { case DependencyID(Left(c)) => c }

    val pm = new PhaseManager(targets ++ prerequisiteTargets ++ current, current.reverse)

    writeGraphviz(pm, "test_run_dir/PhaseManagerSpec/DeterministicOrder")

    pm.flattenedTransformOrder.map(_.getClass) should be (expectedClasses)
  }

  it should "allow conditional placement of custom transforms" in {
    val f = CustomAfterOptimizationFixture

    val targetsMinimum = Seq(DependencyID[f.Custom], DependencyID[f.DoneMinimum])
    val pmMinimum = new PhaseManager(targetsMinimum)

    val targetsFull = Seq(DependencyID[f.Custom], DependencyID[f.DoneFull])
    val pmFull = new PhaseManager(targetsFull)

    val expectedMinimum = Seq(classOf[f.Root], classOf[f.OptMinimum], classOf[f.AfterOpt], classOf[f.Custom], classOf[f.DoneMinimum])
    writeGraphviz(pmMinimum, "test_run_dir/PhaseManagerSpec/CustomAfterOptimization/minimum")
    pmMinimum.flattenedTransformOrder.map(_.getClass) should be (expectedMinimum)

    val expectedFull = Seq(classOf[f.Root], classOf[f.OptMinimum], classOf[f.OptFull], classOf[f.AfterOpt], classOf[f.Custom], classOf[f.DoneFull])
    writeGraphviz(pmFull, "test_run_dir/PhaseManagerSpec/CustomAfterOptimization/full")
    pmFull.flattenedTransformOrder.map(_.getClass) should be (expectedFull)
  }

  it should "support optional prerequisites" in {
    val f = OptionalPrerequisitesFixture

    val targetsMinimum = Seq(DependencyID[f.Custom], DependencyID[f.DoneMinimum])
    val pmMinimum = new PhaseManager(targetsMinimum)

    val targetsFull = Seq(DependencyID[f.Custom], DependencyID[f.DoneFull])
    val pmFull = new PhaseManager(targetsFull)

    val expectedMinimum = Seq(classOf[f.Root], classOf[f.OptMinimum], classOf[f.Custom], classOf[f.DoneMinimum])
    writeGraphviz(pmMinimum, "test_run_dir/PhaseManagerSpec/CustomAfterOptimization/minimum")
    pmMinimum.flattenedTransformOrder.map(_.getClass) should be (expectedMinimum)

    val expectedFull = Seq(classOf[f.Root], classOf[f.OptMinimum], classOf[f.OptFull], classOf[f.Custom], classOf[f.DoneFull])
    writeGraphviz(pmFull, "test_run_dir/PhaseManagerSpec/CustomAfterOptimization/full")
    pmFull.flattenedTransformOrder.map(_.getClass) should be (expectedFull)
  }

  /** This tests a situation the ordering of edges matters. Namely, this test is dependent on the ordering in which
    * DiGraph.linearize walks the edges of each node.
    */
  it should "choose the optimal solution irregardless of prerequisite ordering" in {
    val f = OrderingFixture

    {
      val targets = Seq(DependencyID[f.A], DependencyID[f.B], DependencyID[f.C])
      val order = Seq(classOf[f.B], classOf[f.A], classOf[f.C], classOf[f.B], classOf[f.A])
      (new PhaseManager(targets)).flattenedTransformOrder.map(_.getClass) should be (order)
    }

    {
      val targets = Seq(DependencyID[f.A], DependencyID[f.B], DependencyID[f.Cx])
      val order = Seq(classOf[f.B], classOf[f.A], classOf[f.Cx], classOf[f.B], classOf[f.A])
      (new PhaseManager(targets)).flattenedTransformOrder.map(_.getClass) should be (order)
    }
  }

}
