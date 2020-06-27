package firrtl.fuzzer

import firrtl.ir._
import firrtl.passes.CheckWidths
import firrtl.{Namespace, PrimOps, Utils}

import scala.annotation.tailrec

trait ASTGen[A] {
  def apply(): A
  def flatMap[B](f: A => ASTGen[B]): ASTGen[B] = ASTGen { f(apply())() }
  def map[B](f: A => B): ASTGen[B] = ASTGen { f(apply()) }
  def widen[B >: A]: ASTGen[B] = ASTGen { apply() }
}

object ASTGen {
  def apply[T](f: => T): ASTGen[T] = new ASTGen[T] {
    def apply(): T = f
  }
}


trait Random {
  def nextInt(min: Int, max: Int) : Int
  def oneOf[T](items: Seq[T]) : T
}

object Random {
  import com.pholser.junit.quickcheck.random.SourceOfRandomness

  def apply(sor: SourceOfRandomness): Random = new Random {
    def nextInt(min: Int, max: Int) : Int = sor.nextInt(min, max)
    def oneOf[T](items: Seq[T]) : T = {
      val a = scala.collection.JavaConverters.seqAsJavaList(items)
      sor.choose(a)
    }
  }
}


trait GenMonad[G[_]] {
  def flatMap[A, B](a: G[A])(f: A => G[B]): G[B]
  def flatten[A](gga: G[G[A]]): G[A]
  def map[A, B](a: G[A])(f: A => B): G[B]
  def choose(min: Int, max: Int): G[Int]
  def oneOf[A](items: A*): G[A]
  def const[A](c: A): G[A]
  def widen[A, B >: A](ga: G[A]): G[B]

  def identifier(maxLength: Int): G[String]
}

object GenMonad {
  object implicits {
    implicit def astGenGenMonadInstance(implicit r: Random): GenMonad[ASTGen] = new GenMonad[ASTGen] {
      type G[T] = ASTGen[T]
      def flatMap[A, B](a: G[A])(f: A => G[B]): G[B] = a.flatMap(f)
      def flatten[A](gga: G[G[A]]): G[A] = gga.flatMap(ga => ga)
      def map[A, B](a: G[A])(f: A => B): G[B] = a.map(f)
      def choose(min: Int, max: Int): G[Int] = ASTGen {
        r.nextInt(min, max)
      }
      def oneOf[T](items: T*): G[T] = {
        const(items).map(r.oneOf(_))
      }
      def const[T](c: T): G[T] = ASTGen(c)
      def widen[A, B >: A](ga: G[A]): G[B] = ga.widen[B]
      private val Alpha : Seq[String] = (('a' to 'z') ++ ('A' to 'Z') ++ Seq('_')).map(_.toString)
      private val AlphaNum : Seq[String] = Alpha ++ ('0' to '9').map(_.toString)
      def identifier(maxLength: Int): G[String] = {
        // (12 Details about Syntax):
        // > The following characters are allowed in identifiers: upper and lower case letters, digits, and _.
        // > Identifiers cannot begin with a digit.
        assert(maxLength >= 1)
        ASTGen {
          val len = r.nextInt(1, maxLength)
          val start = r.oneOf(Alpha)
          if (len == 1) { start } else {
            start + (1 until len).map(_ => r.oneOf(AlphaNum)).reduce(_ + _)
          }
        }
      }
    }
  }

  def apply[G[_]: GenMonad] = implicitly[GenMonad[G]]

  object syntax {
    final class GenMonadOps[G[_], A](ga: G[A]) {
      def flatMap[B](f: A => G[B])(implicit GM: GenMonad[G]): G[B] = {
        GM.flatMap(ga)(f)
      }
      def map[B](f: A => B)(implicit GM: GenMonad[G]): G[B] = {
        GM.map(ga)(f)
      }
      def widen[B >: A](implicit GM: GenMonad[G]): G[B] = {
        GM.widen[A, B](ga)
      }
    }
    final class GenMonadFlattenOps[G[_], A](gga: G[G[A]]) {
      def flatten(implicit GM: GenMonad[G]): G[A] = GM.flatten(gga)
    }

    implicit final def genMonadOps[G[_], A](ga: G[A]): GenMonadOps[G, A] =
      new GenMonadOps(ga)
    implicit final def genMonadFlattenOps[G[_], A](gga: G[G[A]]): GenMonadFlattenOps[G, A] =
      new GenMonadFlattenOps(gga)
  }
}

object Fuzzers {
  import GenMonad.implicits._
  import GenMonad.syntax._

  def intWidth[G[_]: GenMonad]: G[IntWidth] =
    GenMonad[G].choose(0, CheckWidths.MaxWidth).map(w => IntWidth(BigInt(w)))

  def intOrUnknownWidth[G[_]: GenMonad]: G[Width] =
    GenMonad[G].oneOf(
      intWidth.widen[Width],
      GenMonad[G].const(UnknownWidth).widen[Width]
    ).flatten

  def uintLit[G[_]: GenMonad]: G[UIntLiteral] = for {
    value <- GenMonad[G].choose(0, Int.MaxValue)
    width <- intOrUnknownWidth
  } yield {
    UIntLiteral(value, width)
  }

  def sintLit[G[_]: GenMonad]: G[SIntLiteral] = for {
    value <- GenMonad[G].choose(Int.MinValue, Int.MaxValue)
    width <- intOrUnknownWidth
  } yield {
    SIntLiteral(value, width)
  }

  def binaryPrimOp[G[_]: GenMonad]: G[PrimOp] = {
    import PrimOps._
    GenMonad[G].oneOf(
      Add, Sub, Mul, Div, Rem, Lt, Leq, Gt, Geq,
      Eq, Neq, /*Dshl, Dshr,*/ And, Or, Xor, Cat,
      //Clip, Wrap, Squeeze
    )
  }

  trait Context {
    def refs: Set[Reference]
    def maxDepth: Int
    def withRef(ref: Reference): Context
    def decrementDepth: Context
    def namespace: Namespace
  }

  def binDoPrim[G[_]: GenMonad](ctx0: Context): G[(Context, Expression)] = {
    for {
      op <- binaryPrimOp
      (ctx1, expr1) <- genExpr(ctx0)
      (ctx2, expr2) <- genExpr(ctx1)
    } yield {
      ctx2 -> PrimOps.set_primop_type(
        DoPrim(op, Seq(expr1, expr2), Seq.empty, UnknownType))
    }
  }

  def ref[G[_]: GenMonad](ctx0: Context, nameOpt: Option[String] = None): G[(Context, Reference)] = for {
    width <- GenMonad[G].choose(0, CheckWidths.MaxWidth)
    tpe <- GenMonad[G].oneOf(
      SIntType(IntWidth(BigInt(width))),
      UIntType(IntWidth(BigInt(width)))
    )
    name <- nameOpt.map(GenMonad[G].const).getOrElse(GenMonad[G].identifier(20))
  } yield {
    val ref = Reference(ctx0.namespace.newName(name), tpe)
    ctx0.withRef(ref) -> ref
  }

  def leaf[G[_]: GenMonad](ctx0: Context): G[(Context, Expression)] = {
    GenMonad[G].oneOf(
      uintLit.map((e: Expression) => ctx0 -> e),
      sintLit.map((e: Expression) => ctx0 -> e),
      ref(ctx0).widen[(Context, Expression)]
    ).flatten
  }

  def genExpr[G[_]: GenMonad](ctx0: Context): G[(Context, Expression)] = {
    if (ctx0.maxDepth <= 0) {
      leaf(ctx0)
    } else {
      GenMonad[G].oneOf(
        binDoPrim(ctx0.decrementDepth),
        leaf(ctx0),
      ).flatten
    }
  }


  case class Ctx(refs: Set[Reference], maxDepth: Int, namespace: Namespace) extends Context {
    def withRef(ref: Reference): Context = this.copy(refs = refs + ref)
    def decrementDepth: Context = this.copy(maxDepth = maxDepth - 1)
  }

  def exprMod[G[_]: GenMonad](maxDepth: Int): G[(Context, Module)] = {
    for {
      (ctx0, expr) <- genExpr(Ctx(Set.empty, maxDepth, Namespace()))
      (ctx1, outputPortRef) <- ref(ctx0, Some("outputPort"))
    } yield {
      val outputPort = Port(
        NoInfo,
        outputPortRef.name,
        Output,
        outputPortRef.tpe
      )
      ctx1 -> Module(
        NoInfo,
        "foo",
        ctx0.refs.map { ref =>
          Port(NoInfo, ref.name, Input, ref.tpe)
        }.toSeq.sortBy(_.name) :+ outputPort,
        Connect(NoInfo, outputPortRef, expr)
      )
    }
  }

  def exprCircuit[G[_]: GenMonad](maxDepth: Int): G[Circuit] = {
    exprMod(maxDepth).map { case (_, m) =>
      Circuit(NoInfo, Seq(m), m.name)
    }
  }
}

/*
  def subField(
    expr: Gen[Expression],
    name: Gen[String]
  ): Gen[SubField] = for {
    e <- expr
    n <- name
  } yield {
    SubField(e, n)
  }

  def subIndex(
    expr: Gen[Expression],
    value: Gen[Int],
  ): Gen[SubIndex] = for {
    e <- expr
    v <- value
  } yield {
    SubIndex(e, v, UnknownType)
  }

  def mux(
    cond: Gen[Expression],
    tval: Gen[Expression],
    fval: Gen[Expression]
  ): Gen[Mux] = for {
    c <- cond
    t <- tval
    f <- fval
  } yield {
    Mux(c, t, f, UnknownType)
  }

  def validIf(
    cond: Gen[Expression],
    value: Gen[Expression]
  ): Gen[ValidIf] = for {
    c <- cond
    v <- value
  } yield {
    ValidIf(c, v, UnknownType)
  }

  def groundType(gen: Gen[Expression]): Gen[Expression] = {
    gen.flatMap { expr =>
      expr.tpe match {
        case _: GroundType => Gen.const(expr)
        case BundleType(fields) =>
          groundType(Gen.oneOf(fields).map { f =>
            SubField(expr, f.name, f.tpe)
          })
        case VectorType(tpe, size) =>
          groundType(Gen.choose(0, size - 1).map { i =>
            SubIndex(expr, i, tpe)
          })
      }
    }
  }

  // def boolType(exprs: Gen[Expression]*): Gen[PrimOp] = {
  //   for {
  //     exprs
  //   }
  // }

  def binaryDoPrim(exprs: Gen[Expression]*): Gen[DoPrim] = {
    for {
      op <- binaryPrimOp
      expr1 <- Gen.oneOf(exprs).flatMap(groundType)
      expr2 <- Gen.oneOf(exprs).flatMap(groundType)
    } yield {
      PrimOps.set_primop_type(
        DoPrim(PrimOps.Add, Seq(expr1, expr2), Seq.empty, UnknownType))
    }
  }

  sealed trait TypeConstraint
  case object AnyType extends TypeConstraint
  case class Equals(tpe: Type) extends TypeConstraint
  case class ContainsField(field: String) extends TypeConstraint
  case class ContainsIndex(tpe: Type) extends TypeConstraint
  */

/*
  def anyExprGen(ctx: Context): ASTGen[Expression] = ???

  def mux(ctx0: Context): ASTGen[Mux] = {
    for {
      (ctx1, cond) <- exprGenWithType(ctx0, UIntType(IntWidth(1)))
      (ctx2, tval) <- anyExprGen(ctx1)
      (ctx3, fval) <- exprGenWithType(ctx2, tval.tpe)
    } yield {
      ctx3 -> Mux(cond, tval, fval)
    }
  }

  def getType(op: PrimOp, t1: Type, t2: Type): Type = {
    op match {
      case (UIntType, UIntType) =>
    }
  }

  def exprGenWithType(ctx: Context, tpe: Type): ASTGen[Expression] = {
    tpe match {
      case UIntType(UnknownWidth) =>
        doPrim()
      case UIntType(IntWidth(width)) =>
      case SIntType(UnknownWidth) =>
      case SIntType(IntWidth(width)) =>
      case FixedType =>
      case IntervalType =>
      case ClockType =>
      case ResetType =>
      case AsyncResetType =>
      case AnalogType =>
    }
  }

  def exprGenWithType(ctx: Context, tpe: Type): ASTGen[Expression] = ???

  def groundTypeBinaryDoPrim(ctx0: Context, op: PrimOp, isSigned: Boolean): ASTGen[DoPrim] = {
    val tpe = if (isSigned) SIntType(UnknownWidth) else UIntType(UnknownWidth)
    for {
      (ctx1, expr1) <- exprGenWithType(ctx0, tpe)
      (ctx2, expr2) <- exprGenWithType(ctx1, tpe)
    } yield {
      ctx2 -> PrimOps.set_primop_type(
        DoPrim(op, Seq(expr1, expr2), Seq.empty, UnknownType))
    }
  }
*/


