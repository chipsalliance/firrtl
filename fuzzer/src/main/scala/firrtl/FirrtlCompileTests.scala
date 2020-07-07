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
  maxWidth: Int,
  namespace: Namespace) {

  require(maxWidth > 0, "maxWidth must be greater than zero")

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
    def withRef[G[_]: GenMonad](ref: Reference): StateGen[ExprContext, G, Reference] = {
      StateGen { (s: ExprContext) =>
        val refx = ref.copy(name = s.namespace.newName(ref.name))
        GenMonad[G].const(s.copy(unboundRefs = s.unboundRefs + refx) -> refx)
      }
    }
    def unboundRefs(s: ExprContext): Set[Reference] = s.unboundRefs
    def maxWidth(s: ExprContext): Int = s.maxWidth

    def exprGen[G[_]: GenMonad](tpe: Type): StateGen[ExprContext, G, Expression] = {
      val leafGen: Type => StateGen[ExprContext, G, Expression] = Generators.leafExprGen(_)
      val branchGen: Type => StateGen[ExprContext, G, Expression] = (tpe: Type) => {
        Generators.recursiveExprGen(tpe).flatMap {
          case None => leafGen(tpe)
          case Some(e) => StateGen.pure(e)
        }
      }
      StateGen { (s: ExprContext) =>
        import GenMonad.syntax._
        (if (s.minDepth > 0) {
          val state = branchGen(tpe)
          state.fn(s.decrementDepth).map {
            case (ss, expr) => ss.incrementDepth -> expr
          }
        } else if (s.maxDepth > 0) {
          GenMonad.frequency(
            5 -> (branchGen(_)),
            1 -> (leafGen(_))
          ).flatMap(_(tpe).fn(s.decrementDepth).map {
            case (ss, e) => ss.incrementDepth -> e
          })
        } else {
          leafGen(tpe).fn(s)
        })
        // .map { case (s, e) =>
        //   println(s"TYPE: ${tpe.serialize}, EXPR: ${e.serialize}")
        //   (s, e)
        // }
      }
    }
  }
}

trait ASTGen[A] {
  def apply(): A
  def flatMap[B](f: A => ASTGen[B]): ASTGen[B] = ASTGen { f(apply())() }
  def map[B](f: A => B): ASTGen[B] = ASTGen { f(apply()) }
  def widen[B >: A]: ASTGen[B] = ASTGen { apply() }
}

object ASTGen {
  implicit def astGenGenMonadInstance(implicit r: SourceOfRandomness): GenMonad[ASTGen] = new GenMonad[ASTGen] {
    import scala.collection.JavaConverters.seqAsJavaList
    type G[T] = ASTGen[T]
    def flatMap[A, B](a: G[A])(f: A => G[B]): G[B] = a.flatMap(f)
    def map[A, B](a: G[A])(f: A => B): G[B] = a.map(f)
    def choose(min: Int, max: Int): G[Int] = ASTGen {
      r.nextLong(min, max).toInt // r.nextInt is inclusive of min but exclusive of max but r.nextLong isn't?
    }
    def oneOf[T](items: T*): G[T] = {
      val arr = seqAsJavaList(items)
      const(arr).map(r.choose(_))
    }
    def const[T](c: T): G[T] = ASTGen(c)
    def widen[A, B >: A](ga: G[A]): G[B] = ga.widen[B]
    private val AlphaSeq: Seq[String] = (('a' to 'z') ++ ('A' to 'Z') ++ Seq('_')).map(_.toString)
    private val Alpha = seqAsJavaList(AlphaSeq)
    private val AlphaNumSeq: Seq[String] = AlphaSeq ++ ('0' to '9').map(_.toString)
    private val AlphaNum = seqAsJavaList(AlphaNumSeq)
    def identifier(maxLength: Int): G[String] = {
      // (12 Details about Syntax):
      // > The following characters are allowed in identifiers: upper and lower case letters, digits, and _.
      // > Identifiers cannot begin with a digit.
      assert(maxLength >= 1)
      ASTGen {
        val len = r.nextInt(1, maxLength)
        val start = r.choose(Alpha)
        if (len == 1) { start } else {
          start + (1 until len).map(_ => r.choose(AlphaNum)).reduce(_ + _)
        }
      }
    }
    def applyGen[A](ga: G[A]): A = ga.apply()
  }

  def apply[T](f: => T): ASTGen[T] = new ASTGen[T] {
    def apply(): T = f
  }
}


class FirrtlSingleModuleGenerator extends Generator[Circuit](classOf[Circuit]) {
  override def generate(random: SourceOfRandomness, status: GenerationStatus): Circuit = {
    implicit val r = random

    val context = ExprContext(
      unboundRefs = Set.empty,
      decls = Set.empty,
      maxDepth = 1000,
      minDepth = 0,
      maxWidth = 500,
      namespace = Namespace()
    )

    println("START")
    val (_, asdf) = Generators.exprCircuit[ExprContext, ASTGen].fn(context)()
    println(asdf.serialize)
    println("END")
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
