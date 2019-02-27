// See LICENSE for license details.

package firrtl.passes

import firrtl._
import firrtl.ir._
import firrtl.Utils._
import firrtl.traversals.Foreachers._

import annotation.tailrec
import scala.collection.mutable

/** Reports errors for any references that are not fully initialized
  *
  * @note This pass looks for [[firrtl.WVoid]]s left behind by [[ExpandWhens]]
  * @note Assumes single connection (ie. no last connect semantics)
  */
object CheckInitialization extends Pass {
  private case class VoidExpr(stmt: Statement, voidDeps: Seq[Expression])

  class RefNotInitializedException(info: Info, mname: String, name: String, trace: Seq[Statement]) extends PassException(
      s"$info : [module $mname]  Reference $name is not fully initialized.\n" + 
      trace.map(s => s"  ${get_info(s)} : ${s.serialize}").mkString("\n")
    )

  private def getTrace(expr: WrappedExpression, voidExprs: Map[WrappedExpression, VoidExpr]): Seq[Statement] = {
    @tailrec
    def rec(e: WrappedExpression, map: Map[WrappedExpression, VoidExpr], trace: Seq[Statement]): Seq[Statement] = {
      val voidExpr = map(e) 
      val newTrace = voidExpr.stmt +: trace
      if (voidExpr.voidDeps.nonEmpty) rec(voidExpr.voidDeps.head, map, newTrace) else newTrace
    }
    rec(expr, voidExprs, Seq())
  }

  def run(c: Circuit): Circuit = {
    val errors = new Errors()


    c.modules.foreach {
      case m: Module => checkInitM(m, errors)
      case m => // Do nothing
    }
    errors.trigger()
    c
  }

  def checkInitM(m: Module, errors: Errors): Seq[DefNode] = {
    val voidExprs = collection.mutable.HashMap[WrappedExpression, VoidExpr]()

    def hasVoidExpr(e: Expression): (Boolean, Seq[Expression]) = {
      var void = false
      val voidDeps = collection.mutable.ArrayBuffer[Expression]()
      def hasVoid(e: Expression): Unit = e match {
        case WVoid =>
          void = true
        case (_: WRef | _: WSubField) =>
          if (voidExprs.contains(e)) {
            void = true
            voidDeps += e
          }
        case _ => e.foreach(hasVoid)
      }
      hasVoid(e)
      (void, voidDeps)
    }
    def checkInitS(s: Statement): Unit = {
      s match {
        case con: Connect =>
          val (hasVoid, voidDeps) = hasVoidExpr(con.expr)
          if (hasVoid) voidExprs(con.loc) = VoidExpr(con, voidDeps)
        case node: DefNode =>
          val (hasVoid, voidDeps) = hasVoidExpr(node.value)
          if (hasVoid) {
            val nodeRef = WRef(node.name, node.value.tpe, NodeKind, MALE)
            voidExprs(nodeRef) = VoidExpr(node, voidDeps)
          }
        case sx => sx.foreach(checkInitS)
      }
    }
    checkInitS(m.body)

    val voidedNodes = mutable.ArrayBuffer[DefNode]()

    // Build Up Errors
    for ((expr, _) <- voidExprs) {
      getDeclaration(m, expr.e1) match {
        case node: DefNode => // Ignore nodes
          voidedNodes += node
        case decl: IsDeclaration =>
          val trace = getTrace(expr, voidExprs.toMap)
          errors append new RefNotInitializedException(decl.info, m.name, decl.name, trace)
      }
    }
    voidedNodes
  }
}
