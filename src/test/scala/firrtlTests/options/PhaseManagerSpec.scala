// See LICENSE for license details.

package firrtlTests.options

import org.scalatest.{FlatSpec, Matchers}

import firrtl.AnnotationSeq
import firrtl.options.{DependencyManagerException, Phase, PhaseManager}
import firrtl.annotations.{Annotation, NoTargetAnnotation}

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
  override def prerequisites: Set[Class[Phase]] = Set(classOf[A])
  override def invalidates(phase: Phase): Boolean = false
}

/** [[Phase]] that requires [[B]] and invalidates nothing */
class C extends IdentityPhase {
  override def prerequisites: Set[Class[Phase]] = Set(classOf[A])
  override def invalidates(phase: Phase): Boolean = false
}

/** [[Phase]] that requires [[A]] and invalidates [[A]] */
class D extends IdentityPhase {
  override def prerequisites: Set[Class[Phase]] = Set(classOf[A])
  override def invalidates(phase: Phase): Boolean = phase match {
    case _: A => true
    case _ => false
  }
}

/** [[Phase]] that requires [[B]] and invalidates nothing */
class E extends IdentityPhase {
  override def prerequisites: Set[Class[Phase]] = Set(classOf[B])
  override def invalidates(phase: Phase): Boolean = false
}

/** [[Phase]] that requires [[B]] and [[C]] and invalidates [[E]] */
class F extends IdentityPhase {
  override def prerequisites: Set[Class[Phase]] = Set(classOf[B], classOf[C])
  override def invalidates(phase: Phase): Boolean = phase match {
    case _: E => true
    case _ => false
  }
}


/** [[Phase]] that requires [[C]] and invalidates [[F]] */
class G extends IdentityPhase {
  override def prerequisites: Set[Class[Phase]] = Set(classOf[C])
  override def invalidates(phase: Phase): Boolean = phase match {
    case _: F => true
    case _ => false
  }
}

class CyclicA extends IdentityPhase {
  override def prerequisites: Set[Class[Phase]] = Set(classOf[CyclicB])
}

class CyclicB extends IdentityPhase {
  override def prerequisites: Set[Class[Phase]] = Set(classOf[CyclicA])
}

object CyclicInvalidateFixture {

  class A extends IdentityPhase {
    override def invalidates(phase: Phase): Boolean = false
  }
  class B extends IdentityPhase {
    override def prerequisites: Set[Class[Phase]] = Set(classOf[A])
    override def invalidates(phase: Phase): Boolean = false
  }
  class C extends IdentityPhase {
    override def prerequisites: Set[Class[Phase]] = Set(classOf[A])
    override def invalidates(phase: Phase): Boolean = phase match {
      case _: B => true
      case _ => false
    }
  }
  class D extends IdentityPhase {
    override def prerequisites: Set[Class[Phase]] = Set(classOf[B])
    override def invalidates(phase: Phase): Boolean = phase match {
      case _: C | _: E => true
      case _ => false
    }
  }
  class E extends IdentityPhase {
    override def prerequisites: Set[Class[Phase]] = Set(classOf[B])
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
    override def prerequisites: Set[Class[Phase]] = Set(classOf[Analysis])
  }
  class B extends InvalidatesAnalysis {
    override def prerequisites: Set[Class[Phase]] = Set(classOf[A], classOf[Analysis])
  }
  class C extends InvalidatesAnalysis {
    override def prerequisites: Set[Class[Phase]] = Set(classOf[B], classOf[Analysis])
  }

}

object InvertedAnalysisFixture {

  class Analysis extends IdentityPhase {
    override def invalidates(phase: Phase): Boolean = false
  }
  class A extends IdentityPhase {
    override def prerequisites: Set[Class[Phase]] = Set(classOf[Analysis])
    override def invalidates(phase: Phase): Boolean = phase match {
      case _: Analysis => true
      case _ => false
    }
  }
  class B extends IdentityPhase {
    override def prerequisites: Set[Class[Phase]] = Set(classOf[Analysis])
    override def invalidates(phase: Phase): Boolean = phase match {
      case _: Analysis | _: A => true
      case _ => false
    }
  }
  class C extends IdentityPhase {
    override def prerequisites: Set[Class[Phase]] = Set(classOf[Analysis])
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
    override val prerequisites: Set[Class[Phase]] = Set(classOf[First])
    override def invalidates(phase: Phase): Boolean = false
  }

  /* This models a situation where a user has a custom Phase that they need to run before some other Phase. This is an
   * abstract example of writing a Transform that cleans up combinational loops. This needs to run before combinational
   * loop detection.
   */
  class Custom extends IdentityPhase {
    override val prerequisites: Set[Class[Phase]] = Set(classOf[First])
    override val dependents: Set[Class[Phase]] = Set(classOf[Second])
    override def invalidates(phase: Phase): Boolean = false
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
        w.write(pm.transformOrderToGraphviz())
        w.close
        maybeToPng(f)
      } catch {
        case _: DependencyManagerException =>
      }
    }

  }

  implicit def f(a: Class[_ <: Phase]): Class[Phase] = a.asInstanceOf[Class[Phase]]

  behavior of this.getClass.getName

  it should "do nothing if all targets are reached" in {
    val targets: Set[Class[Phase]] = Set(classOf[A], classOf[B], classOf[C], classOf[D])
    val pm = new PhaseManager(targets, targets)

    pm.flattenedTransformOrder should be (empty)
  }

  it should "handle a simple dependency" in {
    val targets: Set[Class[Phase]] = Set(classOf[B])
    val order: Seq[Class[Phase]] = Seq(classOf[A], classOf[B])
    val pm = new PhaseManager(targets)

    writeGraphviz(pm, "test_run_dir/SimpleDependency")

    pm.flattenedTransformOrder.map(_.asClass) should be (order)
  }

  it should "handle a simple dependency with an invalidation" in {
    val targets: Set[Class[Phase]] = Set(classOf[A], classOf[B], classOf[C], classOf[D])
    val order: Seq[Class[Phase]] = Seq(classOf[A], classOf[D], classOf[A], classOf[B], classOf[C])
    val pm = new PhaseManager(targets)

    writeGraphviz(pm, "test_run_dir/OneInvalidate")

    pm.flattenedTransformOrder.map(_.asClass) should be (order)
  }

  it should "handle a dependency with two invalidates optimally" in {
    val targets: Set[Class[Phase]] = Set(classOf[A], classOf[B], classOf[C], classOf[E], classOf[F], classOf[G])
    val pm = new PhaseManager(targets)

    writeGraphviz(pm, "test_run_dir/TwoInvalidates")

    pm.flattenedTransformOrder.size should be (targets.size)
  }

  it should "throw an exception for cyclic prerequisites" in {
    val targets: Set[Class[Phase]] = Set(classOf[CyclicA], classOf[CyclicB])
    val pm = new PhaseManager(targets)

    intercept[DependencyManagerException]{ pm.flattenedTransformOrder }
      .getMessage should startWith ("No transform ordering possible")
  }

  it should "handle invalidates that form a cycle" in {
    val f = CyclicInvalidateFixture
    val targets: Set[Class[Phase]] = Set(classOf[f.A], classOf[f.B], classOf[f.C], classOf[f.D], classOf[f.E])
      .map(_.asInstanceOf[Class[Phase]])
    val pm = new PhaseManager(targets)

    writeGraphviz(pm, "test_run_dir/CyclicInvalidate")

    info("only one phase was recomputed")
    pm.flattenedTransformOrder.size should be (targets.size + 1)
  }

  it should "handle repeated recomputed analyses" in {
    val f = RepeatedAnalysisFixture
    val targets: Set[Class[Phase]] = Set(classOf[f.A], classOf[f.B], classOf[f.C])
      .map(_.asInstanceOf[Class[Phase]])
    val order: Seq[Class[Phase]] =
      Seq( classOf[f.Analysis],
           classOf[f.A],
           classOf[f.Analysis],
           classOf[f.B],
           classOf[f.Analysis],
           classOf[f.C])
        .map(_.asInstanceOf[Class[Phase]])
    val pm = new PhaseManager(targets)

    writeGraphviz(pm, "test_run_dir/RepeatedAnalysis")

    pm.flattenedTransformOrder.map(_.asClass) should be (order)
  }

  it should "handle inverted repeated recomputed analyses" in {
    val f = InvertedAnalysisFixture
    val targets: Set[Class[Phase]] = Set(classOf[f.A], classOf[f.B], classOf[f.C])
      .map(_.asInstanceOf[Class[Phase]])
    val order: Seq[Class[Phase]] =
      Seq( classOf[f.Analysis],
           classOf[f.C],
           classOf[f.Analysis],
           classOf[f.B],
           classOf[f.Analysis],
           classOf[f.A])
        .map(_.asInstanceOf[Class[Phase]])
    val pm = new PhaseManager(targets)

    writeGraphviz(pm, "test_run_dir/InvertedRepeatedAnalysis")

    pm.flattenedTransformOrder.map(_.asClass) should be (order)
  }

  /** This test shows how the dependents member can be used to run one transform before another. */
  it should "handle a custom Phase with a dependent" in {
    val f = DependentsFixture

    info("without the custom transform it runs: First -> Second")
    val pm = new PhaseManager(Set(classOf[f.Second]).map(_.asInstanceOf[Class[Phase]]))
    val orderNoCustom: Seq[Class[Phase]] = Seq(classOf[f.First], classOf[f.Second])
      .map(_.asInstanceOf[Class[Phase]])
    pm.flattenedTransformOrder.map(_.asClass) should be (orderNoCustom)

    info("with the custom transform it runs:    First -> Custom -> Second")
    val pmCustom = new PhaseManager(Set(classOf[f.Custom], classOf[f.Second]).map(_.asInstanceOf[Class[Phase]]))
    val orderCustom: Seq[Class[Phase]] = Seq(classOf[f.First], classOf[f.Custom], classOf[f.Second])
      .map(_.asInstanceOf[Class[Phase]])

    writeGraphviz(pmCustom, "test_run_dir/SingleDependent")

    pmCustom.flattenedTransformOrder.map(_.asClass) should be (orderCustom)
  }
}
