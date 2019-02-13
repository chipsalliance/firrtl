// See LICENSE for license details.

package firrtlTests.options

import org.scalatest.{FlatSpec, Matchers}

import firrtl.AnnotationSeq
import firrtl.options.{Phase, PhaseManager, PhaseManagerException}
import firrtl.annotations.{Annotation, NoTargetAnnotation}

import java.io.{File, PrintWriter}

import sys.process._

trait IdentityPhase extends Phase {
  def transform(annotations: AnnotationSeq): AnnotationSeq = annotations
}

/** Default [[Phase]] that has no prerequisites and invalidates nothing */
case object A extends IdentityPhase {
  override def invalidates(phase: Phase): Boolean = false
}

/** [[Phase]] that requires [[A]] and invalidates nothing */
case object B extends IdentityPhase {
  override def prerequisites: Set[Phase] = Set(A)
  override def invalidates(phase: Phase): Boolean = false
}

/** [[Phase]] that requires [[B]] and invalidates nothing */
case object C extends IdentityPhase {
  override def prerequisites: Set[Phase] = Set(A)
  override def invalidates(phase: Phase): Boolean = false
}

/** [[Phase]] that requires [[A]] and invalidates [[A]] */
case object D extends IdentityPhase {
  override def prerequisites: Set[Phase] = Set(A)
  override def invalidates(phase: Phase): Boolean = phase match {
    case _: A.type => true
    case _ => false
  }
}

/** [[Phase]] that requires [[B]] and invalidates nothing */
case object E extends IdentityPhase {
  override def prerequisites: Set[Phase] = Set(B)
  override def invalidates(phase: Phase): Boolean = false
}

/** [[Phase]] that requires [[B]] and [[C]] and invalidates [[E]] */
case object F extends IdentityPhase {
  override def prerequisites: Set[Phase] = Set(B, C)
  override def invalidates(phase: Phase): Boolean = phase match {
    case _: E.type => true
    case _ => false
  }
}


/** [[Phase]] that requires [[C]] and invalidates [[F]] */
case object G extends IdentityPhase {
  override def prerequisites: Set[Phase] = Set(C)
  override def invalidates(phase: Phase): Boolean = phase match {
    case _: F.type => true
    case _ => false
  }
}

case object CyclicA extends IdentityPhase {
  override def prerequisites: Set[Phase] = Set(CyclicB)
}

case object CyclicB extends IdentityPhase {
  override def prerequisites: Set[Phase] = Set(CyclicA)
}

class CyclicInvalidateFixture {

  case object A extends IdentityPhase {
    override def invalidates(phase: Phase): Boolean = false
  }
  case object B extends IdentityPhase {
    override def prerequisites: Set[Phase] = Set(A)
    override def invalidates(phase: Phase): Boolean = false
  }
  case object C extends IdentityPhase {
    override def prerequisites: Set[Phase] = Set(A)
    override def invalidates(phase: Phase): Boolean = phase match {
      case _: B.type => true
      case _ => false
    }
  }
  case object D extends IdentityPhase {
    override def prerequisites: Set[Phase] = Set(B)
    override def invalidates(phase: Phase): Boolean = phase match {
      case _: C.type | _: E.type => true
      case _ => false
    }
  }
  case object E extends IdentityPhase {
    override def prerequisites: Set[Phase] = Set(B)
    override def invalidates(phase: Phase): Boolean = false
  }

}

trait AnalysisFixture {

  case object Analysis extends IdentityPhase {
    override def invalidates(phase: Phase): Boolean = false
  }

}

class RepeatedAnalysisFixture extends AnalysisFixture {

  trait InvalidatesAnalysis extends IdentityPhase {
    override def invalidates(phase: Phase): Boolean = phase match {
      case _: Analysis.type => true
      case _ => false
    }
  }

  case object A extends InvalidatesAnalysis {
    override def prerequisites: Set[Phase] = Set(Analysis)
  }
  case object B extends InvalidatesAnalysis {
    override def prerequisites: Set[Phase] = Set(A, Analysis)
  }
  case object C extends InvalidatesAnalysis {
    override def prerequisites: Set[Phase] = Set(B, Analysis)
  }

}

class InvertedAnalysisFixture extends AnalysisFixture {

  case object A extends IdentityPhase {
    override def prerequisites: Set[Phase] = Set(Analysis)
    override def invalidates(phase: Phase): Boolean = phase match {
      case _: Analysis.type => true
      case _ => false
    }
  }
  case object B extends IdentityPhase {
    override def prerequisites: Set[Phase] = Set(Analysis)
    override def invalidates(phase: Phase): Boolean = phase match {
      case _: Analysis.type | _: A.type => true
      case _ => false
    }
  }
  case object C extends IdentityPhase {
    override def prerequisites: Set[Phase] = Set(Analysis)
    override def invalidates(phase: Phase): Boolean = phase match {
      case _: Analysis.type | _: B.type => true
      case _ => false
    }
  }

}

class DependentsFixture {

  case object First extends IdentityPhase {
    override def invalidates(phase: Phase): Boolean = false
  }

  case object Second extends IdentityPhase {
    override val prerequisites: Set[Phase] = Set(First)
    override def invalidates(phase: Phase): Boolean = false
  }

  /* This models a situation where a user has a custom Phase that they need to run before some other Phase. This is an
   * abstract example of writing a Transform that cleans up combinational loops. This needs to run before combinational
   * loop detection.
   */
  case object Custom extends IdentityPhase {
    override val prerequisites: Set[Phase] = Set(First)
    override val dependents: Set[Phase] = Set(Second)
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
      val f = new File(d + "/phaseOrder.dot")
      val w = new PrintWriter(new File(d + "/phaseOrder.dot"))
      try {
        w.write(pm.phaseOrderToGraphviz())
        w.close
        maybeToPng(f)
      } catch {
        case _: PhaseManagerException =>
      }
    }

  }

  behavior of this.getClass.getName

  it should "do nothing if all targets are reached" in {
    val targets: Set[Phase] = Set(A, B, C, D)
    val pm = new PhaseManager(targets, targets)

    pm.flattenedPhaseOrder should be (empty)
  }

  it should "handle a simple dependency" in {
    val targets: Set[Phase] = Set(B)
    val pm = new PhaseManager(targets)

    writeGraphviz(pm, "test_run_dir/SimpleDependency")

    pm.flattenedPhaseOrder should be (Seq(A, B))
  }

  it should "handle a simple dependency with an invalidation" in {
    val targets: Set[Phase] = Set(A, B, C, D)
    val pm = new PhaseManager(targets)

    writeGraphviz(pm, "test_run_dir/OneInvalidate")

    pm.flattenedPhaseOrder should be (Seq(A, D, A, C, B))
  }

  it should "handle a dependency with two invalidates optimally" in {
    val targets: Set[Phase] = Set(A, B, C, E, F, G)
    val pm = new PhaseManager(targets)

    writeGraphviz(pm, "test_run_dir/TwoInvalidates")

    pm.flattenedPhaseOrder.size should be (targets.size)
  }

  it should "throw an exception for cyclic prerequisites" in {
    val targets: Set[Phase] = Set(CyclicA, CyclicB)
    val pm = new PhaseManager(targets)

    intercept[PhaseManagerException]{ pm.flattenedPhaseOrder }
      .getMessage should startWith ("No Phase ordering possible")
  }

  it should "handle invalidates that form a cycle" in {
    val f = new CyclicInvalidateFixture
    val targets: Set[Phase] = Set(f.A, f.B, f.C, f.D, f.E)
    val pm = new PhaseManager(targets)

    writeGraphviz(pm, "test_run_dir/CyclicInvalidate")

    info("only one phase was recomputed")
    pm.flattenedPhaseOrder.size should be (targets.size + 1)
  }

  it should "handle repeated recomputed analyses" in {
    val f = new RepeatedAnalysisFixture
    val targets: Set[Phase] = Set(f.A, f.B, f.C)
    val pm = new PhaseManager(targets)

    writeGraphviz(pm, "test_run_dir/RepeatedAnalysis")

    pm.flattenedPhaseOrder should be (Seq(f.Analysis, f.A, f.Analysis, f.B, f.Analysis, f.C))
  }

  it should "handle inverted repeated recomputed analyses" in {
    val f = new InvertedAnalysisFixture
    val targets: Set[Phase] = Set(f.A, f.B, f.C)
    val pm = new PhaseManager(targets)

    writeGraphviz(pm, "test_run_dir/InvertedRepeatedAnalysis")

    pm.flattenedPhaseOrder should be (Seq(f.Analysis, f.C, f.Analysis, f.B, f.Analysis, f.A))
  }

  /** This test shows how the dependents member can be used to run one Phase before another. */
  it should "handle a custom Phase with a dependent" in {
    val f = new DependentsFixture

    info("without the custom transform it runs: First -> Second")
    val pm = new PhaseManager(Set(f.Second))
    pm.flattenedPhaseOrder should be (Seq(f.First, f.Second))

    info("with the custom transform it runs:    First -> Custom -> Second")
    val pmCustom = new PhaseManager(Set(f.Custom, f.Second))

    writeGraphviz(pmCustom, "test_run_dir/SingleDependent")

    pmCustom.flattenedPhaseOrder should be (Seq(f.First, f.Custom, f.Second))
  }
}
