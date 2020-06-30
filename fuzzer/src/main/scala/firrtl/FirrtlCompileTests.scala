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

class FirrtlSingleModuleGenerator extends Generator[Circuit](classOf[Circuit]) {
  override def generate(random: SourceOfRandomness, status: GenerationStatus) : Circuit = {
    import firrtl.fuzzer._
    import GenMonad.implicits._
    implicit val r = Random(random)

    val context = ExprContext(
      unboundRefs = Set.empty,
      decls = Set.empty,
      maxDepth = 100,
      namespace = Namespace(),
      exprGenFn = (tpe: Type) => (ctx: Context[ASTGen]) => {
        val branchGen = (tpe: Type) => (ctx: Context[ASTGen]) => {
          val state: ASTGen[Fuzzers.State[ASTGen, Context[ASTGen], Expression]] = GenMonad[ASTGen].oneOf(
            //(OldFuzzers.genExpr: Fuzzers.State[ASTGen, Context[ASTGen], Expression]),
            Fuzzers.genAddPrimOp(tpe),
            Fuzzers.genSubPrimOp(tpe)
          )
          state.flatMap(_(ctx))
        }
        val leafGen: Type => Fuzzers.State[ASTGen, Context[ASTGen], Expression] = Fuzzers.genLeaf
        if (ctx.maxDepth > 0) {
          GenMonad[ASTGen].frequency(
            3 -> (branchGen(tpe)(_: Context[ASTGen]).map {
              case (ctxx, expr) => ctxx.incrementDepth -> expr
            }),
            4 -> (leafGen(tpe)(_))
          ).flatMap(_(ctx))
        } else {
          leafGen(tpe)(ctx)
        }
      })
    val gen = Fuzzers.exprCircuit[ASTGen](context)
    gen()
  }
}

@RunWith(classOf[JQF])
class FirrtlCompileTests {
  private val highFirrtlCompiler = new HighFirrtlCompiler()
  private val middleFirrtlCompiler = new LowFirrtlCompiler()
  private val header = "=" * 50
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
    val high = try {
      highFirrtlCompiler.compile(CircuitState(c, ChirrtlForm, Seq()), Seq())
    } catch {
      case e@ (_: firrtl.passes.PassException | _: firrtl.CustomTransformException) =>
        Assume.assumeTrue(message(c, e), false)
        throw e
    }
    compile(high)
    // compile(CircuitState(c, ChirrtlForm, Seq()))
  }

  // adapted from chisel3.Driver.execute and firrtl.Driver.execute
  def compile(c: CircuitState) = {
    //val compiler = new LowFirrtlCompiler()
    val compiler = middleFirrtlCompiler
    try {
      val res = compiler.compile(c, Seq())
    } catch {
      case e: firrtl.CustomTransformException =>
        Assert.assertTrue(message(c.circuit, e), false)
      case any : Throwable =>
        Assert.assertTrue(message(c.circuit, any), false)
    }

  }
}
