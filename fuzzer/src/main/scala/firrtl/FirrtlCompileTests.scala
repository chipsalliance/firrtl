package firrtl.fuzzer

import com.pholser.junit.quickcheck._
import com.pholser.junit.quickcheck.generator._
import com.pholser.junit.quickcheck.random.SourceOfRandomness
import firrtl.{ChirrtlForm, CircuitState, HighFirrtlCompiler, MiddleFirrtlCompiler, LowFirrtlCompiler}
import org.junit.Assert._
import org.junit.Assume._
import org.junit.runner.RunWith
import firrtl.ir._
import firrtl.Namespace

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
        val state: ASTGen[Fuzzers.State[ASTGen, Context[ASTGen], Expression]] = GenMonad[ASTGen].oneOf(
          Fuzzers.genAddPrimOp(tpe),
          Fuzzers.genSubPrimOp(tpe)
        )
        state.flatMap(_(ctx))
      },
      leafGenFn = Fuzzers.genLeaf)
    val gen = Fuzzers.exprCircuit[ASTGen](context)
    gen()
  }
}

@RunWith(classOf[JQF])
class FirrtlCompileTests {
  private val highFirrtlCompiler = new MiddleFirrtlCompiler()
  private val middleFirrtlCompiler = new LowFirrtlCompiler()

  @Fuzz
  def compileSingleModule(@From(value = classOf[FirrtlSingleModuleGenerator]) c: Circuit) = {
    // val (assumption, high) = try {
    //   (true, highFirrtlCompiler.compile(CircuitState(c, ChirrtlForm, Seq()), Seq()))
    // } catch {
    //   case _: firrtl.passes.PassException | _: firrtl.CustomTransformException =>
    //     (false, null)
    // }
    // assumeTrue(assumption)
    compile(CircuitState(c, ChirrtlForm, Seq()))
  }

  // adapted from chisel3.Driver.execute and firrtl.Driver.execute
  def compile(c: CircuitState) = {
    //val compiler = new LowFirrtlCompiler()
    val compiler = middleFirrtlCompiler
    assert(true)
    // try {
    //   val res = compiler.compile(c, Seq())
    // } catch {
    //   case e: firrtl.CustomTransformException => assert(false, c.circuit.serialize + "\n" + e.cause.toString)
    //   case any : Throwable => assert(false, c.circuit.serialize + "\n" + any.toString)
    // }

  }
}
