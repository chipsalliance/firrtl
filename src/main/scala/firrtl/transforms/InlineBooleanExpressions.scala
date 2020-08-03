// See LICENSE for license details.

package firrtl
package transforms

import firrtl.annotations.Target
import firrtl.annotations.TargetToken.{fromStringToTargetToken, OfModule, Ref}
import firrtl.ir._
import firrtl.passes.{InferTypes, LowerTypes, SplitExpressions}
import firrtl.options.Dependency
import firrtl.PrimOps._
import firrtl.WrappedExpression._

import scala.collection.mutable

/** Inline Bool expressions
  *
  * The following conditions must be satisfied to inline
  * 1. has type [[Utils.BoolType]]
  * 2. is bound to a [[firrtl.ir.DefNode DefNode]] with name starting with '_'
  * 3. is bound to a [[firrtl.ir.DefNode DefNode]] with the same source locator
  * 4. the resulting expression can be emitted to verilog without needing any
  *    parentheses
  */
class InlineBooleanExpressions extends Transform with DependencyAPIMigration {

  override def prerequisites = Seq(
    Dependency(InferTypes),
    Dependency(LowerTypes)
  )

  override def optionalPrerequisites = Seq(
    Dependency(SplitExpressions)
  )

  override def optionalPrerequisiteOf = Seq.empty

  override def invalidates(a: Transform) = a match {
    case _: DeadCodeElimination => true
    case _ => false
  }

  type Netlist = mutable.HashMap[(WrappedExpression, Info), Expression]

  private def isArgN(outerExpr: DoPrim, subExpr: Expression, n: Int): Boolean = {
    outerExpr.args.lift(n) match {
      case Some(arg) => arg eq subExpr
      case _ => false
    }
  }

  /** Maps a [[PrimOp]] to a precedence number, lower number means higher precedence
    *
    * Only the [[PrimOp]]s contained in this map will be inlined. [[PrimOp]]s
    * like [[PrimOp.Neg]] are not in this map because inlining them may result
    * in illegal verilog like '--2sh1'
    */
  private val precedenceMap: Map[PrimOp, Int] = {
    val precedenceSeq = Seq(
      Set(Head, Tail, Bits),
      Set(Andr, Orr, Xorr),
      Set(Lt, Leq, Gt, Geq),
      Set(Eq, Neq),
      Set(And),
      Set(Xor),
      Set(Or)
    )
    precedenceSeq.zipWithIndex.foldLeft(Map.empty[PrimOp, Int]) {
      case (map, (ops, idx)) => map ++ ops.map(_ -> idx)
    }
  }

  /** true if op1 has greater or equal precendence than op2
    */
  private def precedenceGeq(op1: PrimOp, op2: PrimOp): Boolean = {
    precedenceMap(op1) <= precedenceMap(op2)
  }

  /** true if op1 has greater precendence than op2
    */
  private def precedenceGt(op1: PrimOp, op2: PrimOp): Boolean = {
    precedenceMap(op1) < precedenceMap(op2)
  }

  /** true if the reference can be inlined into the containing expressoin
    *
    * @param outerExpr the containing expression
    * @param ref the reference to be inlined, must be a subexpression of outerExpr
    * @param refExpr the expression that is bound to ref
    */
  private def canInline(outerExpr: Expression, ref: WRef, refExpr: Expression): Boolean = {
    if (refExpr.tpe == Utils.BoolType) {
      (outerExpr, refExpr) match {
        case (outerExpr: DoPrim, refExpr: DoPrim) if precedenceMap.contains(outerExpr.op) && precedenceMap.contains(refExpr.op) =>
          // only inline nested DoPrim if one of these conditions is satisfied
          // 1. sub DoPrim has greater precedence
          // 2. sub DoPrim has greater or equal precedence and is the left
          //    operand (all ops except conditional are left associative)
          precedenceGt(refExpr.op, outerExpr.op) ||
          (precedenceGeq(refExpr.op, outerExpr.op) && isArgN(outerExpr, ref, 0))

         // inline any op into Mux, verilog conditional has lowest precedence
        case (outerExpr: Mux, refExpr: DoPrim) => true

         // only inline nested mux in false value, verilog conditional is right associative
        case (outerExpr: Mux, refExpr: Mux) => outerExpr.fval eq ref

        case _ => false
      }
    } else {
      false
    }
  }

  private def onExpr(
    netlist: Netlist,
    dontTouches: Set[Ref],
    info: Info,
    outerExpr: Option[Expression])(expr: Expression): Expression = {
    expr match {
      case ref: WRef if !dontTouches.contains(ref.name.Ref) && ref.name.head == '_' =>
        netlist.get(we(ref) -> info) match {
          case Some(refExpr) if !outerExpr.isDefined => refExpr
          case Some(refExpr) if canInline(outerExpr.get, ref, refExpr) => refExpr
          case other => ref
        }
      case other => other.mapExpr(onExpr(netlist, dontTouches, info, Some(other)))
    }
  }

  private def onStmt(netlist: Netlist, dontTouches: Set[Ref])(stmt: Statement): Statement = {
    stmt.mapStmt(onStmt(netlist, dontTouches)) match {
      case hasInfo: HasInfo =>
        val stmtx = hasInfo.mapExpr(onExpr(netlist, dontTouches, hasInfo.info, None))
        stmtx match {
          case node @ DefNode(info, name, value) =>
            netlist(we(WRef(name)) -> info) = value
          case _ =>
        }
        stmtx
      case other => other
    }
  }

  def execute(state: CircuitState): CircuitState = {
    val dontTouchMap: Map[OfModule, Set[Ref]] = {
      val refTargets = state.annotations.flatMap {
        case anno: HasDontTouches => anno.dontTouches
        case o => Nil
      }
      val dontTouches: Seq[(OfModule, Ref)] = refTargets.map {
        case r => Target.referringModule(r).module.OfModule -> r.ref.Ref
      }
      dontTouches.groupBy(_._1).mapValues(_.map(_._2).toSet).toMap
    }

    val modulesx = state.circuit.modules.map { m =>
      m.mapStmt(onStmt(new Netlist, dontTouchMap.getOrElse(m.name.OfModule, Set.empty[Ref])))
    }

    state.copy(circuit = state.circuit.copy(modules = modulesx))
  }
}
