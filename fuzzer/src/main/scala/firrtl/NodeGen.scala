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

  def genAddSubPrimOp[G[_]: GenMonad](isAdd: Boolean)(tpe: Type): State[G, Context[G], Expression] = {
    val typeFn: Type => G[(Type, Type)] = {
      case UIntType(width) =>
        val tpe = UIntType(widthOp(width)(i => math.max(i.toInt - 1, 0)))
        GenMonad[G].const(tpe -> tpe)
      case SIntType(width) =>
        val tpe = UIntType(widthOp(width)(i => math.max(i.toInt - 1, 0)))
        GenMonad[G].const(tpe -> tpe)
      case UnknownType => for {
        width1 <- GenMonad[G].choose(0, CheckWidths.MaxWidth)
        width2 <- GenMonad[G].choose(0, CheckWidths.MaxWidth)
        tpe <- GenMonad[G].oneOf(UIntType(_), SIntType(_))
      } yield {
        tpe(IntWidth(width1)) -> tpe(IntWidth(width2))
      }
    }
    makeBinPrimOpGen(if (isAdd) PrimOps.Add else PrimOps.Sub, typeFn, tpe)
  }

  def genAddPrimOp[G[_]: GenMonad](tpe: Type): State[G, Context[G], Expression] =
    genAddSubPrimOp(true)(tpe)

  def genSubPrimOp[G[_]: GenMonad](tpe: Type): State[G, Context[G], Expression] =
    genAddSubPrimOp(false)(tpe)


  def genMulPrimOp[G[_]: GenMonad](tpe: Type): State[G, Context[G], Expression] = {
    val anyWidth: G[(Width, Width)] = for {
      totalWidth <- GenMonad[G].choose(0, CheckWidths.MaxWidth)
      width1 <- GenMonad[G].choose(0, totalWidth)
    } yield {
      val width2 = math.max(totalWidth - width1, 0)
      IntWidth(width1) -> IntWidth(width2)
    }
    val typeFn: Type => G[(Type, Type)] = (_: Type) match {
      case UIntType(UnknownWidth) =>
        anyWidth.map { case (w1, w2) => UIntType(w1) -> UIntType(w2) }
      case SIntType(UnknownWidth) =>
        anyWidth.map { case (w1, w2) => SIntType(w1) -> SIntType(w2) }
      case UIntType(IntWidth(totalWidth)) => for {
        width1 <- GenMonad[G].choose(0, totalWidth.toInt)
      } yield {
        UIntType(IntWidth(width1)) -> UIntType(IntWidth(math.max(totalWidth.toInt - width1, 0)))
      }
      case SIntType(IntWidth(totalWidth)) => for {
        width1 <- GenMonad[G].choose(0, totalWidth.toInt)
      } yield {
        SIntType(IntWidth(width1)) -> SIntType(IntWidth(math.max(totalWidth.toInt - width1, 0)))
      }
      case UnknownType => for {
        totalWidth <- GenMonad[G].choose(0, CheckWidths.MaxWidth)
        width1 <- GenMonad[G].choose(0, totalWidth)
        tpe <- GenMonad[G].oneOf(UIntType(_), SIntType(_))
      } yield {
        val width2 = math.max(totalWidth - width1, 0)
        tpe(IntWidth(width1)) -> tpe(IntWidth(width2))
      }
    }
    makeBinPrimOpGen(PrimOps.Mul, typeFn, tpe)
  }

  def anyWidth[G[_]: GenMonad]: G[IntWidth] = GenMonad[G].choose(1, CheckWidths.MaxWidth).map(IntWidth(_))

  def genDivPrimOp[G[_]: GenMonad](tpe: Type): State[G, Context[G], Expression] = {
    val typeFn: Type => G[(Type, Type)] = (_: Type) match {
      case UIntType(UnknownWidth) => for {
        w1 <- anyWidth
        w2 <- anyWidth
      } yield {
        UIntType(w1) -> UIntType(w2)
      }
      case SIntType(UnknownWidth) => for {
        w1 <- anyWidth
        w2 <- anyWidth
      } yield {
        SIntType(w1) -> SIntType(w2)
      }
      case UIntType(IntWidth(w)) => for {
        w1 <- GenMonad[G].choose(0, w.toInt)
        w2 <- anyWidth
      } yield {
        UIntType(IntWidth(w1)) -> UIntType(w2)
      }
      case SIntType(IntWidth(w)) => for {
        w1 <- GenMonad[G].choose(0, math.max(w.toInt - 1, 0))
        w2 <- anyWidth
      } yield {
        SIntType(IntWidth(w1)) -> SIntType(w2)
      }
    }
    makeBinPrimOpGen(PrimOps.Div, typeFn, tpe)
  }

  def genRemPrimOp[G[_]: GenMonad](tpe: Type): State[G, Context[G], Expression] = {
    val typeFn: Type => G[(Type, Type)] = (_: Type) match {
      case UIntType(UnknownWidth) => for {
        w1 <- anyWidth
        w2 <- anyWidth
      } yield {
        UIntType(w1) -> UIntType(w2)
      }
      case SIntType(UnknownWidth) => for {
        w1 <- anyWidth
        w2 <- anyWidth
      } yield {
        SIntType(w1) -> SIntType(w2)
      }
      case UIntType(IntWidth(w)) => for {
        w1 <- GenMonad[G].choose(w.toInt, CheckWidths.MaxWidth)
        w2 <- GenMonad[G].choose(w.toInt, CheckWidths.MaxWidth)
      } yield {
        UIntType(IntWidth(w1)) -> UIntType(IntWidth(w2))
      }
      case SIntType(IntWidth(w)) => for {
        w1 <- GenMonad[G].choose(w.toInt, CheckWidths.MaxWidth)
        w2 <- GenMonad[G].choose(w.toInt, CheckWidths.MaxWidth)
      } yield {
        SIntType(IntWidth(w1)) -> SIntType(IntWidth(w2))
      }
    }
    makeBinPrimOpGen(PrimOps.Rem, typeFn, tpe)
  }

  def makeCmpPrimOpGen[G[_]: GenMonad](primOp: PrimOp)(tpe: Type): State[G, Context[G], Expression] = {
    val typeFn: Type => G[(Type, Type)] = (_: Type) match {
      case _: UIntType => for {
        w1 <- anyWidth
        w2 <- anyWidth
      } yield {
        UIntType(w1) -> UIntType(w2)
      }
      case _: SIntType => for {
        w1 <- anyWidth
        w2 <- anyWidth
      } yield {
        SIntType(w1) -> SIntType(w2)
      }
    }
    makeBinPrimOpGen(primOp, typeFn, tpe)
  }

  def genCmpPrimOp[G[_]: GenMonad](tpe: Type): State[G, Context[G], Expression] = (ctx: Context[G]) => {
    GenMonad[G].oneOf(PrimOps.Lt, PrimOps.Leq, PrimOps.Gt, PrimOps.Geq, PrimOps.Eq, PrimOps.Neq).flatMap { primOp =>
      makeCmpPrimOpGen(primOp)(tpe)(GenMonad[G])(ctx)
    }
  }

  def genPadPrimOp[G[_]: GenMonad](tpe: Type): State[G, Context[G], Expression] = (ctx0: Context[G]) => {
    tpe match {
      case UIntType(UnknownWidth) => for {
        w1 <- GenMonad[G].choose(0, CheckWidths.MaxWidth)
        w2 <- GenMonad[G].choose(0, CheckWidths.MaxWidth)
        (ctx1, expr) <- ctx0.exprGen(UIntType(IntWidth(w1)))
      } yield {
        ctx1 -> DoPrim(PrimOps.Pad, Seq(expr), Seq(w2), tpe)
      }
      case SIntType(UnknownWidth) => for {
        w1 <- GenMonad[G].choose(0, CheckWidths.MaxWidth)
        w2 <- GenMonad[G].choose(0, CheckWidths.MaxWidth)
        (ctx1, expr) <- ctx0.exprGen(SIntType(IntWidth(w1)))
      } yield {
        ctx1 -> DoPrim(PrimOps.Pad, Seq(expr), Seq(w2), tpe)
      }
      case UIntType(IntWidth(w)) => for {
        w1 <- GenMonad[G].choose(0, w.toInt)
        w2 <- GenMonad[G].choose(0, w.toInt)
        (ctx1, expr) <- ctx0.exprGen(UIntType(IntWidth(w1)))
      } yield {
        ctx1 -> DoPrim(PrimOps.Pad, Seq(expr), Seq(w2), tpe)
      }
      case SIntType(IntWidth(w)) => for {
        w1 <- GenMonad[G].choose(0, w.toInt)
        w2 <- GenMonad[G].choose(0, w.toInt)
        (ctx1, expr) <- ctx0.exprGen(SIntType(IntWidth(w1)))
      } yield {
        ctx1 -> DoPrim(PrimOps.Pad, Seq(expr), Seq(w2), tpe)
      }
    }
  }

  def genShlPrimOp[G[_]: GenMonad](tpe: Type): State[G, Context[G], Expression] = (ctx0: Context[G]) => {
    tpe match {
      case UIntType(UnknownWidth) => for {
        w1 <- anyWidth
        w2 <- GenMonad[G].choose(0, CheckWidths.MaxWidth - w1.width.toInt)
        (ctx1, expr) <- ctx0.exprGen(UIntType(w1))
      } yield {
        ctx1 -> DoPrim(PrimOps.Shl, Seq(expr), Seq(w2), tpe)
      }
      case SIntType(UnknownWidth) => for {
        w1 <- anyWidth
        w2 <- GenMonad[G].choose(0, CheckWidths.MaxWidth - w1.width.toInt)
        (ctx1, expr) <- ctx0.exprGen(SIntType(w1))
      } yield {
        ctx1 -> DoPrim(PrimOps.Shl, Seq(expr), Seq(w2), tpe)
      }
      case UIntType(IntWidth(totalWidth)) => for {
        width1 <- GenMonad[G].choose(0, totalWidth.toInt)
        (ctx1, expr) <- ctx0.exprGen(UIntType(IntWidth(width1)))
      } yield {
        val width2 = math.max(totalWidth.toInt - width1, 0)
        ctx1 -> DoPrim(PrimOps.Shl, Seq(expr), Seq(width2), tpe)
      }
      case SIntType(IntWidth(totalWidth)) => for {
        width1 <- GenMonad[G].choose(0, totalWidth.toInt)
        (ctx1, expr) <- ctx0.exprGen(SIntType(IntWidth(width1)))
      } yield {
        val width2 = math.max(totalWidth.toInt - width1, 0)
        ctx1 -> DoPrim(PrimOps.Shl, Seq(expr), Seq(width2), tpe)
      }
      case UnknownType => for {
        totalWidth <- GenMonad[G].choose(0, CheckWidths.MaxWidth)
        width1 <- GenMonad[G].choose(0, totalWidth)
        tpe <- GenMonad[G].oneOf(UIntType(_), SIntType(_))
        (ctx1, expr) <- ctx0.exprGen(tpe(IntWidth(width1)))
      } yield {
        val width2 = math.max(totalWidth - width1, 0)
        ctx1 -> DoPrim(PrimOps.Shl, Seq(expr), Seq(width2), tpe(IntWidth(totalWidth)))
      }
    }
  }

  def genShrPrimOp[G[_]: GenMonad](tpe: Type): State[G, Context[G], Expression] = (ctx0: Context[G]) => {
    tpe match {
      case UIntType(UnknownWidth) => for {
        shamt <- GenMonad[G].choose(0, CheckWidths.MaxWidth)
        exprWidth <- GenMonad[G].choose(shamt.toInt, CheckWidths.MaxWidth)
        (ctx1, expr) <- ctx0.exprGen(UIntType(IntWidth(exprWidth)))
      } yield {
        ctx1 -> DoPrim(PrimOps.Shl, Seq(expr), Seq(shamt), UIntType(IntWidth(exprWidth - shamt)))
      }
      case SIntType(UnknownWidth) => for {
        shamt <- GenMonad[G].choose(0, CheckWidths.MaxWidth)
        exprWidth <- GenMonad[G].choose(shamt.toInt, CheckWidths.MaxWidth)
        (ctx1, expr) <- ctx0.exprGen(SIntType(IntWidth(exprWidth)))
      } yield {
        ctx1 -> DoPrim(PrimOps.Shl, Seq(expr), Seq(shamt), SIntType(IntWidth(exprWidth - shamt)))
      }
      case UIntType(IntWidth(minWidth)) => for {
        exprWidth <- GenMonad[G].choose(minWidth.toInt, CheckWidths.MaxWidth)
        (ctx1, expr) <- ctx0.exprGen(UIntType(IntWidth(exprWidth)))
      } yield {
        val  shamt = exprWidth - minWidth
        ctx1 -> DoPrim(PrimOps.Shl, Seq(expr), Seq(shamt), tpe)
      }
      case SIntType(IntWidth(minWidth)) => for {
        exprWidth <- GenMonad[G].choose(minWidth.toInt, CheckWidths.MaxWidth)
        (ctx1, expr) <- ctx0.exprGen(SIntType(IntWidth(exprWidth)))
      } yield {
        val  shamt = exprWidth - minWidth
        ctx1 -> DoPrim(PrimOps.Shl, Seq(expr), Seq(shamt), tpe)
      }
      case UnknownType => for {
        shamt <- GenMonad[G].choose(0, CheckWidths.MaxWidth)
        tpeFn <- GenMonad[G].oneOf(UIntType(_), SIntType(_))
        exprWidth <- GenMonad[G].choose(shamt.toInt, CheckWidths.MaxWidth)
        (ctx1, expr) <- ctx0.exprGen(tpeFn(IntWidth(exprWidth)))
      } yield {
        ctx1 -> DoPrim(PrimOps.Shl, Seq(expr), Seq(shamt), tpeFn(IntWidth(exprWidth - shamt)))
      }
    }
  }

  def recursiveExprGen[G[_]: GenMonad](tpe: Type, ctx: Context[G]): G[(Context[G], Expression)] = {
    val anyWidthBinOp: G[Type => Fuzzers.State[G, Context[G], Expression]] = GenMonad[G].oneOf(
      genAddPrimOp(_),
      genSubPrimOp(_),
      genDivPrimOp(_),
      genMulPrimOp(_),
      genRemPrimOp(_),
      genPadPrimOp(_),
      genShlPrimOp(_),
      genShrPrimOp(_),
    )
    val boolBinOp: G[Type => Fuzzers.State[G, Context[G], Expression]] = GenMonad[G].oneOf(
      PrimOps.Lt,
      PrimOps.Leq,
      PrimOps.Gt,
      PrimOps.Geq,
      PrimOps.Eq,
      PrimOps.Neq
    ).flatMap { primOp =>
      GenMonad[G].const(makeCmpPrimOpGen(primOp)(_))
    }
    tpe match {
      case UnknownType | Utils.BoolType => GenMonad[G].oneOf(
        boolBinOp,
        anyWidthBinOp
      ).flatten.map(_(tpe)(ctx)).flatten
      case _ => anyWidthBinOp.map(_(tpe)(ctx)).flatten
    }
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
