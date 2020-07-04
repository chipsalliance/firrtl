package firrtl.fuzzer

import com.pholser.junit.quickcheck.From
import com.pholser.junit.quickcheck.generator.{Generator, GenerationStatus}
import com.pholser.junit.quickcheck.random.SourceOfRandomness
import firrtl.{ChirrtlForm, CircuitState, HighFirrtlCompiler, MiddleFirrtlCompiler, LowFirrtlCompiler}
import org.junit.Assert
import org.junit.Assume
import org.junit.runner.RunWith
import firrtl.ir._
import firrtl.Namespace

import java.io.{PrintWriter, StringWriter}

import edu.berkeley.cs.jqf.fuzz.Fuzz;
import edu.berkeley.cs.jqf.fuzz.JQF;

case class ExprContext(
  unboundRefs: Set[Reference],
  decls: Set[IsDeclaration],
  minDepth: Int,
  maxDepth: Int,
  namespace: Namespace) {
  def decrementDepth: ExprContext = this.copy(
    maxDepth = maxDepth - 1,
    minDepth = minDepth - 1
  )
  def incrementDepth: ExprContext = this.copy(
    maxDepth = maxDepth + 1,
    minDepth = minDepth + 1
  )
}

object ExprContext {
  implicit val ESG: ExprState[ExprContext] = new ExprState[ExprContext] {
    def withRef[G[_]: GenMonad](ref: Reference): StateGen[ExprContext, G, Reference] = StateGen { (s: ExprContext) =>
      val refx = ref.copy(name = s.namespace.newName(ref.name))
      GenMonad[G].const(s.copy(unboundRefs = s.unboundRefs + refx) -> refx)
    }
    def unboundRefs(s: ExprContext): Set[Reference] = s.unboundRefs

    def exprGen[G[_]: GenMonad](tpe: Type): StateGen[ExprContext, G, Expression] = StateGen { (s: ExprContext) =>
      import GenMonad.syntax._
      val leafGen: Type => StateGen[ExprContext, G, Expression] = Fuzzers.genLeaf(_)(ESG, GenMonad[G])
      if (s.minDepth > 0) {
        val state = Fuzzers.recursiveExprGen(tpe)(ESG, GenMonad[G])
        state.fn(s.decrementDepth).map {
          case (ctxx, expr) => ctxx.incrementDepth -> expr
        }
      } else if (s.maxDepth > 0) {
        GenMonad[G].frequency(
          5 -> Fuzzers.recursiveExprGen(tpe)(ESG, GenMonad[G]),
          2 -> leafGen(tpe)
        ).flatMap(_.fn(s.decrementDepth).map {
          case (ctx, expr) => ctx.incrementDepth -> expr
        })
      } else {
        leafGen(tpe).fn(s)
      }
    }
  }
}


class FirrtlSingleModuleGenerator extends Generator[Circuit](classOf[Circuit]) {
  override def generate(random: SourceOfRandomness, status: GenerationStatus) : Circuit = {
    import GenMonad.instances._
    implicit val r = Random(random)

    val context = ExprContext(
      unboundRefs = Set.empty,
      decls = Set.empty,
      maxDepth = 1000,
      minDepth = 5,
      namespace = Namespace()
    )

    val (_, asdf) = Fuzzers.exprCircuit[ExprContext, ASTGen].fn(context)()
    println(asdf.serialize)
    asdf
  }
}

@RunWith(classOf[JQF])
class FirrtlCompileTests {
  private val highFirrtlCompiler = new HighFirrtlCompiler()
  private val middleFirrtlCompiler = new LowFirrtlCompiler()
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
    // val high = try {
    //   highFirrtlCompiler.compile(CircuitState(c, ChirrtlForm, Seq()), Seq())
    // } catch {
    //   case e@ (_: firrtl.passes.PassException | _: firrtl.CustomTransformException) =>
    //     Assume.assumeTrue(message(c, e), false)
    //     throw e
    // }
    // compile(high)

    compile(CircuitState(c, ChirrtlForm, Seq()))
  }

  // adapted from chisel3.Driver.execute and firrtl.Driver.execute
  def compile(c: CircuitState) = {
    //val compiler = new LowFirrtlCompiler()
    val compiler = middleFirrtlCompiler
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
