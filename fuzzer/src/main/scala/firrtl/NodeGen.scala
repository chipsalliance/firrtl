package firrtl.fuzzer

import firrtl.ir._
import firrtl.passes.CheckWidths
import firrtl.{Namespace, PrimOps, Utils}

trait Context {
  def unboundRefs: Set[Reference] // should have type set
  def decls: Set[IsDeclaration]
  def maxDepth: Int
  def minDepth: Int
  def withRef(ref: Reference): Context
  def decrementDepth: Context
  def incrementDepth: Context
  def namespace: Namespace
  def exprGen[G[_]: GenMonad](tpe: Type): G[(Context, Expression)]
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
    typeFn: Type => G[(Type, Type, Type)],
    tpe: Type): State[G, Context, Expression] = (ctx0: Context) => {
    for {
      (tpe1, tpe2, exprTpe) <- typeFn(tpe)
      (ctx1, expr1) <- ctx0.exprGen(tpe1)
      (ctx2, expr2) <- ctx1.exprGen(tpe2)
    } yield {
      ctx2 -> DoPrim(primOp, Seq(expr1, expr2), Seq.empty, exprTpe)
    }
  }


  final val MAX_WIDTH: Int = CheckWidths.MaxWidth
  final val MAX_WIDTH_LOG2: Int = log2Ceil(MAX_WIDTH)

  def handleUnknown[A, G[_]: GenMonad](fn: Type => G[A]): Type => G[A] = {
    case UnknownType =>
      GenMonad[G].oneOf(UIntType(_), SIntType(_)).flatMap { typeFn =>
        anyWidth.flatMap { width =>
          fn(typeFn(width))
        }
      }
    case known => fn(known)
  }

  def genAddSubPrimOp[G[_]: GenMonad](isAdd: Boolean)(tpe: Type): State[G, Context, Expression] = {
    val typeFn: Type => G[(Type, Type, Type)] = handleUnknown {
      case UIntType(width) =>
        val argTpe = UIntType(widthOp(width)(i => math.max(i.toInt - 1, 0)))
        GenMonad[G].const((argTpe, argTpe, tpe))
      case SIntType(width) =>
        val argTpe = SIntType(widthOp(width)(i => math.max(i.toInt - 1, 0)))
        GenMonad[G].const((argTpe, argTpe, tpe))
    }
    makeBinPrimOpGen(if (isAdd) PrimOps.Add else PrimOps.Sub, typeFn, tpe)
  }

  def genAddPrimOp[G[_]: GenMonad](tpe: Type): State[G, Context, Expression] =
    genAddSubPrimOp(true)(tpe)

  def genSubPrimOp[G[_]: GenMonad](tpe: Type): State[G, Context, Expression] =
    genAddSubPrimOp(false)(tpe)


  def genMulPrimOp[G[_]: GenMonad](tpe: Type): State[G, Context, Expression] = {
    val anyWidth: G[(IntWidth, IntWidth)] = for {
      totalWidth <- GenMonad[G].choose(0, MAX_WIDTH)
      width1 <- GenMonad[G].choose(0, totalWidth)
    } yield {
      val width2 = math.max(totalWidth - width1, 0)
      IntWidth(width1) -> IntWidth(width2)
    }
    val typeFn: Type => G[(Type, Type, Type)] = handleUnknown {
      case UIntType(UnknownWidth) =>
        anyWidth.map { case (w1, w2) =>
          (UIntType(w1), UIntType(w2), UIntType(IntWidth(w1.width + w2.width)))
        }
      case SIntType(UnknownWidth) =>
        anyWidth.map { case (w1, w2) =>
          (SIntType(w1), SIntType(w2), SIntType(IntWidth(w1.width + w2.width)))
        }
      case UIntType(IntWidth(totalWidth)) => for {
        width1 <- GenMonad[G].choose(0, totalWidth.toInt)
      } yield {
        (UIntType(IntWidth(width1)), UIntType(IntWidth(math.max(totalWidth.toInt - width1, 0))), tpe)
      }
      case SIntType(IntWidth(totalWidth)) => for {
        width1 <- GenMonad[G].choose(0, totalWidth.toInt)
      } yield {
        (SIntType(IntWidth(width1)), SIntType(IntWidth(math.max(totalWidth.toInt - width1, 0))), tpe)
      }
    }
    makeBinPrimOpGen(PrimOps.Mul, typeFn, tpe)
  }

  def anyWidth[G[_]: GenMonad]: G[IntWidth] = GenMonad[G].choose(1, MAX_WIDTH).map(IntWidth(_))

  def genDivPrimOp[G[_]: GenMonad](tpe: Type): State[G, Context, Expression] = {
    val typeFn: Type => G[(Type, Type, Type)] = handleUnknown {
      case UIntType(UnknownWidth) => for {
        w1 <- anyWidth
        w2 <- anyWidth
      } yield {
        (UIntType(w1), UIntType(w2), UIntType(w1))
      }
      case SIntType(UnknownWidth) => for {
        w1 <- anyWidth
        w2 <- anyWidth
      } yield {
        (SIntType(w1), SIntType(w2), SIntType(IntWidth(math.max(w1.width.toInt - 1, 0))))
      }
      case UIntType(IntWidth(w)) => for {
        w1 <- GenMonad[G].choose(0, w.toInt)
        w2 <- anyWidth
      } yield {
        (UIntType(IntWidth(w1)), UIntType(w2), tpe)
      }
      case SIntType(IntWidth(w)) => for {
        w1 <- GenMonad[G].choose(0, math.max(w.toInt - 1, 0))
        w2 <- anyWidth
      } yield {
        (SIntType(IntWidth(w1)), SIntType(w2), tpe)
      }
    }
    makeBinPrimOpGen(PrimOps.Div, typeFn, tpe)
  }

  def genRemPrimOp[G[_]: GenMonad](tpe: Type): State[G, Context, Expression] = {
    val typeFn: Type => G[(Type, Type, Type)] = handleUnknown {
      case UIntType(UnknownWidth) => for {
        w1 <- anyWidth
        w2 <- anyWidth
      } yield {
        (UIntType(w1), UIntType(w2), UIntType(IntWidth(math.min(w1.width.toInt, w2.width.toInt))))
      }
      case SIntType(UnknownWidth) => for {
        w1 <- anyWidth
        w2 <- anyWidth
      } yield {
        (SIntType(w1), SIntType(w2), SIntType(IntWidth(math.min(w1.width.toInt, w2.width.toInt))))
      }
      case UIntType(IntWidth(w)) => for {
        w1 <- GenMonad[G].choose(w.toInt, MAX_WIDTH)
        w2 <- GenMonad[G].choose(w.toInt, MAX_WIDTH)
      } yield {
        (UIntType(IntWidth(w1)), UIntType(IntWidth(w2)), tpe)
      }
      case SIntType(IntWidth(w)) => for {
        w1 <- GenMonad[G].choose(w.toInt, MAX_WIDTH)
        w2 <- GenMonad[G].choose(w.toInt, MAX_WIDTH)
      } yield {
        (SIntType(IntWidth(w1)), SIntType(IntWidth(w2)), tpe)
      }
    }
    makeBinPrimOpGen(PrimOps.Rem, typeFn, tpe)
  }

  def makeCmpPrimOpGen[G[_]: GenMonad](primOp: PrimOp)(tpe: Type): State[G, Context, Expression] = {
    val typeFn: Type => G[(Type, Type, Type)] = handleUnknown {
      case _: UIntType => for {
        w1 <- anyWidth
        w2 <- anyWidth
      } yield {
        (UIntType(w1), UIntType(w2), Utils.BoolType)
      }
      case _: SIntType => for {
        w1 <- anyWidth
        w2 <- anyWidth
      } yield {
        (SIntType(w1), SIntType(w2), Utils.BoolType)
      }
    }
    makeBinPrimOpGen(primOp, typeFn, tpe)
  }

  def genCmpPrimOp[G[_]: GenMonad](tpe: Type): State[G, Context, Expression] = (ctx: Context) => {
    GenMonad[G].oneOf(PrimOps.Lt, PrimOps.Leq, PrimOps.Gt, PrimOps.Geq, PrimOps.Eq, PrimOps.Neq).flatMap { primOp =>
      makeCmpPrimOpGen(primOp)(tpe)(GenMonad[G])(ctx)
    }
  }

  def genPadPrimOp[G[_]: GenMonad](tpe: Type): State[G, Context, Expression] = (ctx0: Context) => {
    tpe match {
      case UIntType(UnknownWidth) => for {
        w1 <- anyWidth
        w2 <- GenMonad[G].choose(0, MAX_WIDTH)
        (ctx1, expr) <- ctx0.exprGen(UIntType(w1))
      } yield {
        ctx1 -> DoPrim(PrimOps.Pad, Seq(expr), Seq(w2), UIntType(IntWidth(math.max(w1.width.toInt, w2))))
      }
      case SIntType(UnknownWidth) => for {
        w1 <- anyWidth
        w2 <- GenMonad[G].choose(0, MAX_WIDTH)
        (ctx1, expr) <- ctx0.exprGen(SIntType(w1))
      } yield {
        ctx1 -> DoPrim(PrimOps.Pad, Seq(expr), Seq(w2), SIntType(IntWidth(math.max(w1.width.toInt, w2))))
      }
      case UIntType(IntWidth(w)) => for {
        w1 <- anyWidth
        w2 <- GenMonad[G].choose(0, w.toInt)
        (ctx1, expr) <- ctx0.exprGen(UIntType(w1))
      } yield {
        ctx1 -> DoPrim(PrimOps.Pad, Seq(expr), Seq(w2), tpe)
      }
      case SIntType(IntWidth(w)) => for {
        w1 <- anyWidth
        w2 <- GenMonad[G].choose(0, w.toInt)
        (ctx1, expr) <- ctx0.exprGen(SIntType(w1))
      } yield {
        ctx1 -> DoPrim(PrimOps.Pad, Seq(expr), Seq(w2), tpe)
      }
    }
  }

  def genShlPrimOp[G[_]: GenMonad](tpe: Type): State[G, Context, Expression] = (ctx0: Context) => {
    tpe match {
      case UIntType(UnknownWidth) => for {
        w1 <- anyWidth
        w2 <- GenMonad[G].choose(0, MAX_WIDTH - w1.width.toInt)
        (ctx1, expr) <- ctx0.exprGen(UIntType(w1))
      } yield {
        ctx1 -> DoPrim(PrimOps.Shl, Seq(expr), Seq(w2), UIntType(IntWidth(w1.width + w2)))
      }
      case SIntType(UnknownWidth) => for {
        w1 <- anyWidth
        w2 <- GenMonad[G].choose(0, MAX_WIDTH - w1.width.toInt)
        (ctx1, expr) <- ctx0.exprGen(SIntType(w1))
      } yield {
        ctx1 -> DoPrim(PrimOps.Shl, Seq(expr), Seq(w2), SIntType(IntWidth(w1.width + w2)))
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
        totalWidth <- GenMonad[G].choose(0, MAX_WIDTH)
        width1 <- GenMonad[G].choose(0, totalWidth)
        tpe <- GenMonad[G].oneOf(UIntType(_), SIntType(_))
        (ctx1, expr) <- ctx0.exprGen(tpe(IntWidth(width1)))
      } yield {
        val width2 = math.max(totalWidth - width1, 0)
        ctx1 -> DoPrim(PrimOps.Shl, Seq(expr), Seq(width2), tpe(IntWidth(totalWidth)))
      }
    }
  }

  def genShrPrimOp[G[_]: GenMonad](tpe: Type): State[G, Context, Expression] = (ctx0: Context) => {
    tpe match {
      case UIntType(UnknownWidth) => for {
        shamt <- GenMonad[G].choose(0, MAX_WIDTH)
        exprWidth <- GenMonad[G].choose(shamt.toInt, MAX_WIDTH)
        (ctx1, expr) <- ctx0.exprGen(UIntType(IntWidth(exprWidth)))
      } yield {
        ctx1 -> DoPrim(PrimOps.Shr, Seq(expr), Seq(shamt), UIntType(IntWidth(exprWidth - shamt)))
      }
      case SIntType(UnknownWidth) => for {
        shamt <- GenMonad[G].choose(0, MAX_WIDTH)
        exprWidth <- GenMonad[G].choose(shamt.toInt, MAX_WIDTH)
        (ctx1, expr) <- ctx0.exprGen(SIntType(IntWidth(exprWidth)))
      } yield {
        ctx1 -> DoPrim(PrimOps.Shr, Seq(expr), Seq(shamt), SIntType(IntWidth(exprWidth - shamt)))
      }
      case UIntType(IntWidth(minWidth)) => for {
        exprWidth <- GenMonad[G].choose(minWidth.toInt, MAX_WIDTH)
        (ctx1, expr) <- ctx0.exprGen(UIntType(IntWidth(exprWidth)))
      } yield {
        val  shamt = exprWidth - minWidth
        ctx1 -> DoPrim(PrimOps.Shr, Seq(expr), Seq(shamt), tpe)
      }
      case SIntType(IntWidth(minWidth)) => for {
        exprWidth <- GenMonad[G].choose(minWidth.toInt, MAX_WIDTH)
        (ctx1, expr) <- ctx0.exprGen(SIntType(IntWidth(exprWidth)))
      } yield {
        val  shamt = exprWidth - minWidth
        ctx1 -> DoPrim(PrimOps.Shr, Seq(expr), Seq(shamt), tpe)
      }
      case UnknownType => for {
        shamt <- GenMonad[G].choose(0, MAX_WIDTH)
        tpeFn <- GenMonad[G].oneOf(UIntType(_), SIntType(_))
        exprWidth <- GenMonad[G].choose(shamt.toInt, MAX_WIDTH)
        (ctx1, expr) <- ctx0.exprGen(tpeFn(IntWidth(exprWidth)))
      } yield {
        ctx1 -> DoPrim(PrimOps.Shr, Seq(expr), Seq(shamt), tpeFn(IntWidth(exprWidth - shamt)))
      }
    }
  }


  def log2Ceil(i: Int): Int = BigInt(i - 1).bitLength

  def log2Floor(i: Int): Int = {
    if (i > 0 && ((i & (i - 1)) == 0)) {
      log2Ceil(i)
    } else if (i == 0) {
      0
    } else {
      log2Ceil(i) - 1
    }
  }

  def genDshlPrimOp[G[_]: GenMonad](tpe: Type): State[G, Context, Expression] = {
    val typeFn: Type => G[(Type, Type, Type)] = handleUnknown {
      case UIntType(UnknownWidth) => for {
        shWidth <- GenMonad[G].choose(0, MAX_WIDTH_LOG2)
        w1 <- GenMonad[G].choose(0, 1 << (MAX_WIDTH_LOG2 - shWidth))
      } yield {
        (UIntType(IntWidth(w1)), UIntType(IntWidth(shWidth)), UIntType(IntWidth(w1 + (1 << shWidth) - 1)))
      }
      case SIntType(UnknownWidth) => for {
        shWidth <- GenMonad[G].choose(0, MAX_WIDTH_LOG2)
        w1 <- GenMonad[G].choose(0, 1 << (MAX_WIDTH_LOG2 - shWidth))
      } yield {
        (SIntType(IntWidth(w1)), UIntType(IntWidth(shWidth)), SIntType(IntWidth(w1 + (1 << shWidth) - 1)))
      }
      case UIntType(IntWidth(totalWidth)) => for {
        shWidth <- GenMonad[G].choose(0, log2Floor(totalWidth.toInt))
        w1 <- GenMonad[G].choose(0, math.max(totalWidth.toInt - (1 << shWidth), 0))
      } yield {
        (UIntType(IntWidth(w1)), UIntType(IntWidth(shWidth)), tpe)
      }
      case SIntType(IntWidth(totalWidth)) => for {
        shWidth <- GenMonad[G].choose(0, log2Floor(totalWidth.toInt))
        w1 <- GenMonad[G].choose(0, math.max(totalWidth.toInt - (1 << shWidth), 0))
      } yield {
        (SIntType(IntWidth(w1)), UIntType(IntWidth(shWidth)), tpe)
      }
    }
    makeBinPrimOpGen(PrimOps.Dshl, typeFn, tpe)
  }

  def genDshrPrimOp[G[_]: GenMonad](tpe: Type): State[G, Context, Expression] = {
    val typeFn: Type => G[(Type, Type, Type)] = handleUnknown {
      case UIntType(UnknownWidth) => for {
        w1 <- anyWidth
        w2 <- anyWidth
      } yield {
        (UIntType(w1), UIntType(w2), UIntType(w1))
      }
      case SIntType(UnknownWidth) => for {
        w1 <- anyWidth
        w2 <- anyWidth
      } yield {
        (SIntType(w1), UIntType(w2), SIntType(w1))
      }
      case UIntType(IntWidth(w1)) => for {
        w2 <- anyWidth
      } yield {
        (UIntType(IntWidth(w1)), UIntType(w2), tpe)
      }
      case SIntType(IntWidth(w1)) => for {
        w2 <- anyWidth
      } yield {
        (SIntType(IntWidth(w1)), UIntType(w2), tpe)
      }
    }
    makeBinPrimOpGen(PrimOps.Dshr, typeFn, tpe)
  }

  def makeUnaryPrimOpGen[G[_]: GenMonad](
    primOp: PrimOp,
    typeFn: Type => G[(Type, Type)],
    tpe: Type): State[G, Context, Expression] = (ctx0: Context) => {
    for {
      (tpe1, exprTpe) <- typeFn(tpe)
      (ctx1, expr1) <- ctx0.exprGen(tpe1)
    } yield {
      ctx1 -> DoPrim(primOp, Seq(expr1), Seq.empty, exprTpe)
    }
  }

  def genCvtPrimOp[G[_]: GenMonad](tpe: Type): State[G, Context, Expression] = {
    val typeFn: Type => G[(Type, Type)] = {
      case SIntType(UnknownWidth) => for {
        width <- anyWidth
        isUInt <- GenMonad[G].oneOf(true, false)
      } yield {
        if (isUInt) {
          UIntType(IntWidth(math.max(width.width.toInt - 1, 0))) -> SIntType(width)
        } else {
          SIntType(width) -> SIntType(width)
        }
      }
      case SIntType(IntWidth(width)) => for {
        isUInt <- GenMonad[G].oneOf(true, false)
      } yield {
        if (isUInt) {
          UIntType(IntWidth(math.max(width.toInt - 1, 0))) -> tpe
        } else {
          tpe -> tpe
        }
      }
    }
    makeUnaryPrimOpGen(PrimOps.Cvt, typeFn, tpe)
  }

  def genNegPrimOp[G[_]: GenMonad](tpe: Type): State[G, Context, Expression] = {
    val typeFn: Type => G[(Type, Type)] = {
      case SIntType(UnknownWidth) => for {
        width <- anyWidth
        isUInt <- GenMonad[G].oneOf(true, false)
      } yield {
        if (isUInt) {
          UIntType(IntWidth(math.max(width.width.toInt - 1, 0))) -> SIntType(width)
        } else {
          SIntType(IntWidth(math.max(width.width.toInt - 1, 0))) -> SIntType(width)
        }
      }
      case SIntType(IntWidth(width)) => for {
        isUInt <- GenMonad[G].oneOf(true, false)
      } yield {
        if (isUInt) {
          UIntType(IntWidth(math.max(width.toInt - 1, 0))) -> tpe
        } else {
          SIntType(IntWidth(math.max(width.toInt - 1, 0))) -> tpe
        }
      }
    }
    makeUnaryPrimOpGen(PrimOps.Neg, typeFn, tpe)
  }

  def genNotPrimOp[G[_]: GenMonad](tpe: Type): State[G, Context, Expression] = {
    val typeFn: Type => G[(Type, Type)] = {
      case UIntType(UnknownWidth) => for {
        width <- anyWidth
        isUInt <- GenMonad[G].oneOf(true, false)
      } yield {
        if (isUInt) {
          UIntType(width) -> UIntType(width)
        } else {
          SIntType(width) -> UIntType(width)
        }
      }
      case UIntType(width) => for {
        isUInt <- GenMonad[G].oneOf(true, false)
      } yield {
        if (isUInt) {
          tpe -> tpe
        } else {
          SIntType(width) -> tpe
        }
      }
    }
    makeUnaryPrimOpGen(PrimOps.Not, typeFn, tpe)
  }

  def makeBitwisePrimOpGen[G[_]: GenMonad](primOp: PrimOp)(tpe: Type): State[G, Context, Expression] = {
    val typeFn: Type => G[(Type, Type, Type)] = {
      case UIntType(UnknownWidth) | UnknownType => for {
        width1 <- anyWidth
        width2 <- anyWidth
      } yield {
        (UIntType(width1), UIntType(width2), UIntType(IntWidth(math.max(width1.width.toInt, width2.width.toInt))))
      }
      case UIntType(IntWidth(width)) => for {
        width1 <- anyWidth
        width2 <- anyWidth
      } yield {
        (UIntType(width1), UIntType(width2), UIntType(IntWidth(math.max(width1.width.toInt, width2.width.toInt))))
      }
    }
    makeBinPrimOpGen(primOp, typeFn, tpe)
  }

  def makeReducePrimOpGen[G[_]: GenMonad](primOp: PrimOp)(tpe: Type): State[G, Context, Expression] = {
    val typeFn: Type => G[(Type, Type)] = {
      case Utils.BoolType | UnknownType => for {
        width1 <- anyWidth
        tpeFn <- GenMonad[G].oneOf(UIntType(_), SIntType(_))
      } yield {
        (tpeFn(width1), Utils.BoolType)
      }
    }
    makeUnaryPrimOpGen(primOp, typeFn, tpe)
  }

  case class TraceException(trace: Seq[String], cause: Throwable) extends Exception(
      s"failed: $cause\ntrace:\n${trace.reverse.mkString("\n")}"
    )

  def wrap[G[_]: GenMonad](name: String, fn: Type => State[G, Context, Expression]): Type => State[G, Context, Expression] = {
    (tpe: Type) => (ctx: Context) => {
      GenMonad[G].choose(0, 1).map ( _ =>
        try {
          GenMonad[G].applyGen(fn(tpe)(ctx))//.map(s => wrap[G](name, (_: Type) => s)(tpe))
        } catch {
          case e: TraceException if e.trace.size < 10 =>
            throw e.copy(trace = name +: e.trace)
          case e: IllegalArgumentException =>
            throw TraceException(Seq(name), e)
        }
      )
    }
  }

  def recursiveExprGen[G[_]: GenMonad](tpe: Type): State[G, Context, Expression] = (ctx: Context) => {
    val anyBinOp: G[Type => State[G, Context, Expression]] = GenMonad[G].oneOf(
      genAddPrimOp(_),
      genSubPrimOp(_),
      genDivPrimOp(_),
      genMulPrimOp(_),
      genRemPrimOp(_),
      genPadPrimOp(_),
      genShlPrimOp(_),
      genShrPrimOp(_),
      genDshlPrimOp(_),
      genDshrPrimOp(_)
    )
    val boolBinOp: G[Type => State[G, Context, Expression]] = GenMonad[G].oneOf(
      makeCmpPrimOpGen(PrimOps.Lt)(_),
      makeCmpPrimOpGen(PrimOps.Leq)(_),
      makeCmpPrimOpGen(PrimOps.Gt)(_),
      makeCmpPrimOpGen(PrimOps.Geq)(_),
      makeCmpPrimOpGen(PrimOps.Eq)(_),
      makeCmpPrimOpGen(PrimOps.Neq)(_),

      makeReducePrimOpGen(PrimOps.Andr)(_),
      makeReducePrimOpGen(PrimOps.Orr)(_),
      makeReducePrimOpGen(PrimOps.Xorr)(_),
    )
    val uintBinOp: G[Type => State[G, Context, Expression]] = GenMonad[G].oneOf(
      makeBitwisePrimOpGen(PrimOps.And)(_),
      makeBitwisePrimOpGen(PrimOps.Or)(_),
      makeBitwisePrimOpGen(PrimOps.Xor)(_),
    )
    val sintBinOp: G[Type => State[G, Context, Expression]] = GenMonad[G].oneOf(
      genCvtPrimOp(_),
      genNegPrimOp(_),
    )
    tpe match {
      case Utils.BoolType => GenMonad[G].oneOf(
        boolBinOp,
      ).flatten.map(_(tpe)(ctx)).flatten
      case _: UIntType => GenMonad[G].oneOf(
        uintBinOp,
        anyBinOp,
      ).flatten.map(_(tpe)(ctx)).flatten
      case _: SIntType => GenMonad[G].oneOf(
        sintBinOp,
        anyBinOp,
      ).flatten.map(_(tpe)(ctx)).flatten
      case _ =>
        anyBinOp.flatMap(_(tpe)(ctx))
    }
  }


  def genUIntLiteralLeaf[G[_]: GenMonad](tpe: UIntType): State[G, Context, Expression] = (ctx: Context) => {
    val genWidth: G[Int] = tpe.width match {
      case UnknownWidth => GenMonad[G].choose(1, MAX_WIDTH)
      case IntWidth(width) => GenMonad[G].const(width.toInt)
    }
    genWidth.flatMap { width =>
      GenMonad[G].choose(0, math.min(Int.MaxValue, (1 << width) - 1)).map { value =>
        ctx -> UIntLiteral(value, IntWidth(width))
      }
    }
  }

  def genSIntLiteralLeaf[G[_]: GenMonad](tpe: SIntType): State[G, Context, Expression] = (ctx: Context) => {
    val genWidth: G[Int] = tpe.width match {
      case UnknownWidth => GenMonad[G].choose(0, MAX_WIDTH)
      case IntWidth(width) => GenMonad[G].const(width.toInt)
    }
    genWidth.flatMap { width =>
      GenMonad[G].choose(
        math.min(Int.MinValue, -(1 << (width - 1))),
        math.max(Int.MaxValue, (1 << (width - 1)) - 1)).map { value =>
          ctx -> SIntLiteral(value, IntWidth(width))
        }
    }
  }

  def genLiteralLeaf[G[_]: GenMonad](tpe: Type): State[G, Context, Expression] = {
    tpe match {
      case u: UIntType => genUIntLiteralLeaf(u)
      case s: SIntType => genSIntLiteralLeaf(s)
      case UnknownType => (ctx: Context) =>
        GenMonad[G].choose(0, MAX_WIDTH).flatMap { width =>
          GenMonad[G].oneOf(
            genUIntLiteralLeaf(UIntType(IntWidth(width))),
            genSIntLiteralLeaf(SIntType(IntWidth(width)))
          ).flatMap(_(ctx))
        }
    }
  }

  def genRefLeaf[G[_]: GenMonad](
    nameOpt: Option[String]
  )(tpe: Type): State[G, Context, Reference] = (ctx0: Context) => {
    val genType: G[Type] = tpe match {
      case GroundType(UnknownWidth) =>
        GenMonad[G].choose(0, MAX_WIDTH)
          .map(w => tpe.mapWidth(_ => IntWidth(w)))
      case GroundType(IntWidth(width)) => GenMonad[G].const(tpe)
      case UnknownType => for {
        typeFn <- GenMonad[G].oneOf(UIntType(_), SIntType(_))
        width <- anyWidth
      } yield {
        typeFn(width)
      }
    }

    for {
      tpe <- genType
      name <- nameOpt.map(GenMonad[G].const).getOrElse(GenMonad[G].identifier(20))
    } yield {
      val ref = Reference(ctx0.namespace.newName(name), tpe)
      ctx0.withRef(ref) -> ref
    }
  }

  def genLeaf[G[_]: GenMonad](tpe: Type): State[G, Context, Expression] = (ctx0: Context) => {
    val stateGen: G[State[G, Context, Expression]] = GenMonad[G].oneOf(
      (ctx: Context) => genRefLeaf(None)(tpe)(GenMonad[G])(ctx).map {
         case (s, a) => (s, a.asInstanceOf[Expression])
       },
      genLiteralLeaf(tpe),
    )
    stateGen.flatMap(_(ctx0))
  }
  //def genLeaf[G[_]: GenMonad]: Type => State[G, Context, Expression] = wrap("leaf", genLeafa(_))

  def exprMod[G[_]: GenMonad](ctx0: Context): G[(Context, Module)] = {
    for {
      width <- GenMonad[G].oneOf(anyWidth, GenMonad[G].const(IntWidth(1))).flatten
      tpe <- GenMonad[G].oneOf(UIntType(width), SIntType(width))
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

  def exprCircuit[G[_]: GenMonad](ctx: Context): G[Circuit] = {
    exprMod(ctx).map { case (_, m) =>
      Circuit(NoInfo, Seq(m), m.name)
    }
  }
}
