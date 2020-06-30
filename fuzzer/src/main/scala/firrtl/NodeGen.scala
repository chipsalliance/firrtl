package firrtl.fuzzer

import firrtl.ir._
import firrtl.passes.CheckWidths
import firrtl.{Namespace, PrimOps, Utils}

trait Context[Gen[_]] {
  def unboundRefs: Set[Reference] // should have type set
  def decls: Set[IsDeclaration]
  def maxDepth: Int
  def withRef(ref: Reference): Context[Gen]
  def decrementDepth: Context[Gen]
  def incrementDepth: Context[Gen]
  def namespace: Namespace
  def exprGen(tpe: Type): Gen[(Context[Gen], Expression)]
}

case class ExprContext(
  unboundRefs: Set[Reference],
  decls: Set[IsDeclaration],
  maxDepth: Int,
  namespace: Namespace,
  exprGenFn: Type => Fuzzers.State[ASTGen, Context[ASTGen], Expression]) extends Context[ASTGen] {
  def withRef(ref: Reference): ExprContext = this.copy(unboundRefs = unboundRefs + ref)
  def decrementDepth: ExprContext = this.copy(maxDepth = maxDepth - 1)
  def incrementDepth: ExprContext = this.copy(maxDepth = maxDepth + 1)
  def exprGen(tpe: Type): ASTGen[(Context[ASTGen], Expression)] = exprGenFn(tpe)(this)
}

object Fuzzers {
  import GenMonad.implicits._
  import GenMonad.syntax._

  type State[Gen[_], Ctx, A] = Ctx => Gen[(Ctx, A)]

  def widthOp(width: Width)(op: BigInt => BigInt): Width = width match {
    case IntWidth(i) => IntWidth(op(i))
    case UnknownWidth => UnknownWidth
  }

  def makeBinPrimOpGen[G[_]: GenMonad](
    primOp: PrimOp,
    typeFn: Type => G[(Type, Type)],
    tpe: Type): State[G, Context[G], Expression] = (ctx0: Context[G]) => {
    for {
      (tpe1, tpe2) <- typeFn(tpe)
      (ctx1, expr1) <- ctx0.exprGen(tpe1)
      (ctx2, expr2) <- ctx1.exprGen(tpe2)
    } yield {
      ctx2 -> DoPrim(primOp, Seq(expr1, expr2), Seq.empty, tpe)
    }
  }

  def genAddPrimOp[G[_]: GenMonad](tpe: Type): State[G, Context[G], Expression] = {
    val typeFn: Type => G[(Type, Type)] = {
      case UIntType(width) =>
        val tpe = UIntType(widthOp(width)(_ - 1))
        GenMonad[G].const(tpe -> tpe)
      case SIntType(width) =>
        val tpe = UIntType(widthOp(width)(_ - 1))
        GenMonad[G].const(tpe -> tpe)
      case UnknownType => for {
        width1 <- GenMonad[G].choose(0, CheckWidths.MaxWidth)
        width2 <- GenMonad[G].choose(0, CheckWidths.MaxWidth)
        tpe <- GenMonad[G].oneOf(UIntType(_), SIntType(_))
      } yield {
        tpe(IntWidth(width1)) -> tpe(IntWidth(width2))
      }
    }
    makeBinPrimOpGen(PrimOps.Add, typeFn, tpe)
  }

  def genSubPrimOp[G[_]: GenMonad](tpe: Type): State[G, Context[G], Expression] = {
    val typeFn: Type => G[(Type, Type)] = {
      case UIntType(width) =>
        val tpe = UIntType(widthOp(width)(_ - 1))
        GenMonad[G].const(tpe -> tpe)
      case SIntType(width) =>
        val tpe = UIntType(widthOp(width)(_ - 1))
        GenMonad[G].const(tpe -> tpe)
      case UnknownType => for {
        width1 <- GenMonad[G].choose(0, CheckWidths.MaxWidth)
        width2 <- GenMonad[G].choose(0, CheckWidths.MaxWidth)
        tpe <- GenMonad[G].oneOf(UIntType(_), SIntType(_))
      } yield {
        tpe(IntWidth(width1)) -> tpe(IntWidth(width2))
      }
    }
    makeBinPrimOpGen(PrimOps.Sub, typeFn, tpe)
  }

  def genUIntLiteralLeaf[G[_]: GenMonad](tpe: UIntType): State[G, Context[G], Expression] = (ctx: Context[G]) => {
    val genWidth: G[Int] = tpe.width match {
      case UnknownWidth => GenMonad[G].choose(0, CheckWidths.MaxWidth)
      case IntWidth(width) => GenMonad[G].choose(0, math.max(CheckWidths.MaxWidth, width.toInt))
    }
    genWidth.flatMap { width =>
      GenMonad[G].choose(0, math.max(Int.MaxValue, 1 << width)).map { value =>
        ctx -> UIntLiteral(value, IntWidth(width))
      }
    }
  }
  def genSIntLiteralLeaf[G[_]: GenMonad](tpe: SIntType): State[G, Context[G], Expression] = (ctx: Context[G]) => {
    val genWidth: G[Int] = tpe.width match {
      case UnknownWidth => GenMonad[G].choose(0, CheckWidths.MaxWidth)
      case IntWidth(width) => GenMonad[G].choose(0, math.max(CheckWidths.MaxWidth, width.toInt))
    }
    genWidth.flatMap { width =>
      GenMonad[G].choose(
        math.min(Int.MinValue, 1 - (1 << width)),
        math.max(Int.MaxValue, 1 << width)).map { value =>
          ctx -> SIntLiteral(value, IntWidth(width))
        }
    }
  }

  def genLiteralLeaf[G[_]: GenMonad](tpe: Type): State[G, Context[G], Expression] = {
    tpe match {
      case u: UIntType => genUIntLiteralLeaf(u)
      case s: SIntType => genSIntLiteralLeaf(s)
      case UnknownType => (ctx: Context[G]) =>
        GenMonad[G].choose(0, CheckWidths.MaxWidth).flatMap { width =>
          GenMonad[G].oneOf(
            genUIntLiteralLeaf(UIntType(IntWidth(width))),
            genSIntLiteralLeaf(SIntType(IntWidth(width)))
          ).flatMap(_(ctx))
        }
    }
  }

  def genRefLeaf[G[_]: GenMonad](
    nameOpt: Option[String]
  )(tpe: Type): State[G, Context[G], Reference] = (ctx0: Context[G]) => {
    val genType: G[Type] = tpe match {
      case GroundType(UnknownWidth) =>
        GenMonad[G].choose(0, CheckWidths.MaxWidth)
          .map(w => tpe.mapWidth(_ => IntWidth(w)))
      case GroundType(IntWidth(width)) =>
        GenMonad[G].choose(0, math.max(CheckWidths.MaxWidth, width.toInt))
          .map(w => tpe.mapWidth(_ => IntWidth(w)))
      case UnknownType => GenMonad[G].const(UnknownType)
    }

    for {
      tpe <- genType
      name <- nameOpt.map(GenMonad[G].const).getOrElse(GenMonad[G].identifier(20))
    } yield {
      val ref = Reference(ctx0.namespace.newName(name), tpe)
      ctx0.withRef(ref) -> ref
    }
  }

  def genLeaf[G[_]: GenMonad](tpe: Type): State[G, Context[G], Expression] = (ctx0: Context[G]) => {
    val stateGen: G[State[G, Context[G], Expression]] = GenMonad[G].oneOf(
      (ctx: Context[G]) => genRefLeaf(None)(tpe)(GenMonad[G])(ctx).map {
         case (s, a) => (s, a.asInstanceOf[Expression])
       },
      genLiteralLeaf(tpe),
    )
    stateGen.flatMap(_(ctx0))
  }
/*
  def genSubPrimOp[G[_]: GenMonad](tpe: Type): Context[G] => G[(Context[G], Expression)] = {
    val typeFn = (_: Type) match {
      case UIntType(width) => UIntType(widthOp(width)(_ - 1))
      case SIntType(width) => UIntType(widthOp(width)(_ - 1))
    }
    makeBinPrimOpGen(
      PrimOps.Add, typeFn, typeFn, tpe)
  }*/


  def exprMod[G[_]: GenMonad](ctx0: Context[G]): G[(Context[G], Module)] = {
    for {
      tpe <- GenMonad[G].oneOf(
        GenMonad[G].choose(0, CheckWidths.MaxWidth)
          .map(w => UIntType(IntWidth(w))).widen[Type],
        GenMonad[G].choose(0, CheckWidths.MaxWidth)
          .map(w => SIntType(IntWidth(w))).widen[Type]
      ).flatten
      (ctx1, expr) <- ctx0.exprGen(tpe)
      (ctx2, outputPortRef) <- genRefLeaf(Some("outputPort"))(tpe)(GenMonad[G])(ctx1)
    } yield {
      val outputPort = Port(
        NoInfo,
        outputPortRef.name,
        Output,
        outputPortRef.tpe
      )
      ctx2 -> Module(
        NoInfo,
        "foo",
        ctx2.unboundRefs.flatMap {
          case ref if ref.name == outputPortRef.name => None
          case ref => Some(Port(NoInfo, ref.name, Input, ref.tpe))
        }.toSeq.sortBy(_.name) :+ outputPort,
        Connect(NoInfo, outputPortRef, expr)
      )
    }
  }

  def exprCircuit[G[_]: GenMonad](ctx: Context[G]): G[Circuit] = {
    exprMod(ctx).map { case (_, m) =>
      Circuit(NoInfo, Seq(m), m.name)
    }
  }
}

