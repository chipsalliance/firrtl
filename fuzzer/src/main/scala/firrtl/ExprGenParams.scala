package firrtl.fuzzer

import firrtl.{Namespace, Utils}
import firrtl.ir._

sealed trait ExprGenParams {
  def maxDepth: Int
  def maxWidth: Int
  def generators: Seq[(Int, ExprGen[_ <: Expression])]

  protected def unboundRefs: Set[Reference]
  protected def decls: Set[IsDeclaration]
  protected def namespace: Namespace
  protected def decrementDepth: ExprGenParams
  protected def incrementDepth: ExprGenParams
  protected def withRef(ref: Reference): ExprGenParams


  private def exprMod[G[_]: GenMonad]: StateGen[ExprGenParams, G, Module] = {
    for {
      width <- StateGen.inspectG((s: ExprGenParams) => ExprGen.genWidth(1, ExprState[ExprGenParams].maxWidth(s)))
      tpe <- StateGen.liftG(GenMonad.frequency(
        2 -> UIntType(width),
        2 -> SIntType(width),
        1 -> Utils.BoolType
      ))
      expr <- ExprState[ExprGenParams].exprGen(tpe)
      outputPortRef <- tpe match {
        case UIntType(IntWidth(width)) if width == BigInt(1) => ExprGen.ReferenceGen.boolUIntGen[ExprGenParams, G].get
        case UIntType(IntWidth(width)) => ExprGen.ReferenceGen.uintGen[ExprGenParams, G].get(width)
        case SIntType(IntWidth(width)) if width == BigInt(1) => ExprGen.ReferenceGen.boolSIntGen[ExprGenParams, G].get
        case SIntType(IntWidth(width)) => ExprGen.ReferenceGen.sintGen[ExprGenParams, G].get(width)
      }
      unboundRefs <- StateGen.inspect { ExprState[ExprGenParams].unboundRefs(_) }
    } yield {
      val outputPort = Port(
        NoInfo,
        outputPortRef.name,
        Output,
        outputPortRef.tpe
      )
      Module(
        NoInfo,
        "foo",
        unboundRefs.flatMap {
          case ref if ref.name == outputPortRef.name => None
          case ref => Some(Port(NoInfo, ref.name, Input, ref.tpe))
        }.toSeq.sortBy(_.name) :+ outputPort,
        Connect(NoInfo, outputPortRef, expr)
      )
    }
  }

  def generateSingleExprCircuit[G[_]: GenMonad](): Circuit = {
    import GenMonad.syntax._
    exprMod.map { m =>
      Circuit(NoInfo, Seq(m), m.name)
    }.run(this).map(_._2).generate()
  }
}

object ExprGenParams {
  private case class ExprGenParamsImp(
    maxDepth: Int,
    maxWidth: Int,
    generators: Seq[(Int, ExprGen[_ <: Expression])],
    protected val unboundRefs: Set[Reference],
    protected val decls: Set[IsDeclaration],
    protected val namespace: Namespace) extends ExprGenParams {

    protected def decrementDepth: ExprGenParams = this.copy(maxDepth = maxDepth - 1)
    protected def incrementDepth: ExprGenParams = this.copy(maxDepth = maxDepth + 1)
    protected def withRef(ref: Reference): ExprGenParams = this.copy(unboundRefs = unboundRefs + ref)
  }

  def apply(
    maxDepth: Int,
    maxWidth: Int,
    generators: Seq[(Int, ExprGen[_ <: Expression])]
  ): ExprGenParams = {
    require(maxWidth > 0, "maxWidth must be greater than zero")
    ExprGenParamsImp(
      maxDepth,
      maxWidth,
      generators,
      Set.empty,
      Set.empty,
      Namespace()
    )
  }

  implicit val exprGenParamsExprStateInstance: ExprState[ExprGenParams] = new ExprState[ExprGenParams] {
    def withRef[G[_]: GenMonad](ref: Reference): StateGen[ExprGenParams, G, Reference] = {
      StateGen { (s: ExprGenParams) =>
        val refx = ref.copy(name = s.namespace.newName(ref.name))
        GenMonad[G].const(s.withRef(refx) -> refx)
      }
    }
    def unboundRefs(s: ExprGenParams): Set[Reference] = s.unboundRefs
    def maxWidth(s: ExprGenParams): Int = s.maxWidth

    def exprGen[G[_]: GenMonad](tpe: Type): StateGen[ExprGenParams, G, Expression] = {
      import GenMonad.syntax._
      StateGen { (s: ExprGenParams) =>

        val leafGen: Type => StateGen[ExprGenParams, G, Expression] = (tpe: Type) => ExprGen.combineExprGens(Seq(
          1 -> ExprGen.LiteralGen,
          1 -> ExprGen.ReferenceGen
        ))(tpe).map(e => e.get) // should be safe because leaf generators are defined for all types

        val branchGen: Type => StateGen[ExprGenParams, G, Expression] = (tpe: Type) => {
          ExprGen.combineExprGens(s.generators)(tpe).flatMap {
            case None => leafGen(tpe)
            case Some(e) => StateGen.pure(e)
          }
        }

        if (s.maxDepth > 0) {
          // for recrusive generators, decrement maxDepth before recursing then increment when finished
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
