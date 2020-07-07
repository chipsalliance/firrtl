package firrtl.fuzzer

import org.scalacheck.{Gen, Prop, Properties}
import firrtl.{ChirrtlForm, CircuitState, CustomTransformException, LowFirrtlCompiler, Namespace}
import firrtl.passes.CheckWidths

object FirrtlCompileProperties extends Properties("FirrtlCompile") {
  property("compile") = {
    import ScalaCheckGenMonad._

    val gen = Gen.sized { size =>
      val context = ExprContext(
        unboundRefs = Set.empty,
        decls = Set.empty,
        maxDepth = size / 3,
        minDepth = 0,
        maxWidth = math.min(size + 1, CheckWidths.MaxWidth),
        namespace = Namespace()
      )

      ExprGen.exprCircuit[ExprContext, Gen].fn(context)
    }
    val lowFirrtlCompiler = new firrtl.LowFirrtlCompiler()
    Prop.forAll(gen) { case (_, circuit) =>
      val state = CircuitState(circuit, ChirrtlForm, Seq())
      //val compiler = new LowFirrtlCompiler()
      val compiler = lowFirrtlCompiler
      try {
        val res = compiler.compile(state, Seq())
        true
      } catch {
        case e: CustomTransformException => false
        case any : Throwable => false
      }
    }
  }
}
