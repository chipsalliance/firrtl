package firrtl.fuzzer

import com.pholser.junit.quickcheck.From
import com.pholser.junit.quickcheck.generator.{Generator, GenerationStatus}
import com.pholser.junit.quickcheck.random.SourceOfRandomness

import firrtl.{
  ChirrtlForm,
  CircuitState,
  EmitCircuitAnnotation,
  LowFirrtlCompiler,
  MinimumVerilogEmitter,
  Namespace,
  VerilogEmitter
}
import firrtl.ir._
import firrtl.stage.TransformManager

import org.junit.Assert
import org.junit.Assume
import org.junit.runner.RunWith

import java.io.{File, FileWriter, PrintWriter, StringWriter}

import edu.berkeley.cs.jqf.fuzz.Fuzz;
import edu.berkeley.cs.jqf.fuzz.JQF;

case class ExprContext private (
  private val unboundRefs: Set[Reference],
  private val decls: Set[IsDeclaration],
  maxDepth: Int,
  maxWidth: Int,
  private val namespace: Namespace,
  generators: Seq[(Int, ExprGen[_ <: Expression])]) {

  require(maxWidth > 0, "maxWidth must be greater than zero")

  def decrementDepth: ExprContext = this.copy(
    maxDepth = maxDepth - 1,
  )
  def incrementDepth: ExprContext = this.copy(
    maxDepth = maxDepth + 1,
  )
}

// TODO: factor out to helper method. with frequency params for generators
object ExprContext {
  def apply(
    maxDepth: Int,
    maxWidth: Int,
    generators: Seq[(Int, ExprGen[_ <: Expression])]
  ): ExprContext = {
    ExprContext(
      Set.empty,
      Set.empty,
      maxDepth,
      maxWidth,
      Namespace(),
      generators
    )
  }

  implicit val exprStateExprContextInstance: ExprState[ExprContext] = new ExprState[ExprContext] {
    def withRef[G[_]: GenMonad](ref: Reference): StateGen[ExprContext, G, Reference] = {
      StateGen { (s: ExprContext) =>
        val refx = ref.copy(name = s.namespace.newName(ref.name))
        GenMonad[G].const(s.copy(unboundRefs = s.unboundRefs + refx) -> refx)
      }
    }
    def unboundRefs(s: ExprContext): Set[Reference] = s.unboundRefs
    def maxWidth(s: ExprContext): Int = s.maxWidth

    def exprGen[G[_]: GenMonad](tpe: Type): StateGen[ExprContext, G, Expression] = {
      StateGen { (s: ExprContext) =>
        import GenMonad.syntax._
        val leafGen: Type => StateGen[ExprContext, G, Expression] = ExprGen.leafExprGen(_)
        val branchGen: Type => StateGen[ExprContext, G, Expression] = (tpe: Type) => {
          ExprGen.recursiveExprGen(false, s.generators)(tpe).flatMap {
            case None => leafGen(tpe)
            case Some(e) => StateGen.pure(e)
          }
        }
        if (s.maxDepth > 0) {
          GenMonad.frequency(
            5 -> (branchGen(_)),
            1 -> (leafGen(_))
          ).flatMap(_(tpe).run(s.decrementDepth).map {
            case (ss, e) => ss.incrementDepth -> e
          })
        } else {
          leafGen(tpe).run(s)
        }
      }
    }
  }
}

trait SourceOfRandomnessGen[A] {
  def apply(): A

  def flatMap[B](f: A => SourceOfRandomnessGen[B]): SourceOfRandomnessGen[B] =
    SourceOfRandomnessGen { f(apply())() }

  def map[B](f: A => B): SourceOfRandomnessGen[B] =
    SourceOfRandomnessGen { f(apply()) }

  def widen[B >: A]: SourceOfRandomnessGen[B] =
    SourceOfRandomnessGen { apply() }
}

object SourceOfRandomnessGen {
  implicit def astGenGenMonadInstance(implicit r: SourceOfRandomness): GenMonad[SourceOfRandomnessGen] = new GenMonad[SourceOfRandomnessGen] {
    import scala.collection.JavaConverters.seqAsJavaList
    type G[T] = SourceOfRandomnessGen[T]
    def flatMap[A, B](a: G[A])(f: A => G[B]): G[B] = a.flatMap(f)
    def map[A, B](a: G[A])(f: A => B): G[B] = a.map(f)
    def choose(min: Int, max: Int): G[Int] = SourceOfRandomnessGen {
      r.nextLong(min, max).toInt // use r.nextLong instead of r.nextInt because r.nextInt is exclusive of max
    }
    def oneOf[T](items: T*): G[T] = {
      val arr = seqAsJavaList(items)
      const(arr).map(r.choose(_))
    }
    def const[T](c: T): G[T] = SourceOfRandomnessGen(c)
    def widen[A, B >: A](ga: G[A]): G[B] = ga.widen[B]
    def applyGen[A](ga: G[A]): A = ga.apply()
  }

  def apply[T](f: => T): SourceOfRandomnessGen[T] = new SourceOfRandomnessGen[T] {
    def apply(): T = f
  }
}


class FirrtlSingleModuleGenerator extends Generator[Circuit](classOf[Circuit]) {
  override def generate(random: SourceOfRandomness, status: GenerationStatus): Circuit = {
    implicit val r = random
    import ExprGen._

    val context = ExprContext(
      maxDepth = 50,
      maxWidth = 31,
      generators = Seq(
        1 -> AddDoPrimGen,
        1 -> SubDoPrimGen,
        1 -> MulDoPrimGen,
        1 -> DivDoPrimGen,
        1 -> LtDoPrimGen,
        1 -> LeqDoPrimGen,
        1 -> GtDoPrimGen,
        1 -> GeqDoPrimGen,
        1 -> EqDoPrimGen,
        1 -> NeqDoPrimGen,
        1 -> PadDoPrimGen,
        1 -> ShlDoPrimGen,
        1 -> ShrDoPrimGen,
        1 -> DshlDoPrimGen,
        1 -> CvtDoPrimGen,
        1 -> NegDoPrimGen,
        30 -> NotDoPrimGen,
        1 -> AndDoPrimGen,
        1 -> OrDoPrimGen,
        1 -> XorDoPrimGen,
        1 -> AndrDoPrimGen,
        1 -> OrrDoPrimGen,
        1 -> XorrDoPrimGen,
        1 -> CatDoPrimGen,
        1 -> BitsDoPrimGen,
        1 -> HeadDoPrimGen,
        1 -> TailDoPrimGen,
        1 -> AsUIntDoPrimGen,
        1 -> AsSIntDoPrimGen,
      )
    )

    val (_, circuit) = ExprGen.exprCircuit[ExprContext, SourceOfRandomnessGen].run(context)()
    println(circuit.serialize)
    circuit
  }
}

@RunWith(classOf[JQF])
class FirrtlCompileTests {
  private val lowFirrtlCompiler = new LowFirrtlCompiler()
  private val header = "=" * 50 + "\n"
  private val footer = header
  private def message(c: Circuit, t: Throwable): String = {
    val sw = new StringWriter()
    val pw = new PrintWriter(sw)
    t.printStackTrace(pw)
    pw.flush()
    header + c.serialize + "\n" + sw.toString + footer
  }

  @Fuzz
  def compileSingleModule(@From(value = classOf[FirrtlSingleModuleGenerator]) c: Circuit) = {
    compile(CircuitState(c, ChirrtlForm, Seq()))
  }

  // adapted from chisel3.Driver.execute and firrtl.Driver.execute
  def compile(c: CircuitState) = {
    val compiler = lowFirrtlCompiler
    try {
      val res = compiler.compile(c, Seq())
    } catch {
      case e: firrtl.CustomTransformException =>
        Assert.assertTrue(message(c.circuit, e.cause), false)
      case any : Throwable =>
        Assert.assertTrue(message(c.circuit, any), false)
    }
  }
}
