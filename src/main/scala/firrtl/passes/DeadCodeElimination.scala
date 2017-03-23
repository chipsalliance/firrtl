// See LICENSE for license details.

package firrtl.passes

import firrtl._
import firrtl.ir._
import firrtl.Utils._
import firrtl.Mappers._

import annotation.tailrec

object DeadCodeElimination extends Pass {
  def name = "Dead Code Elimination"

  private def dceOnce(ports: Seq[Port], s: Statement): (Statement, Long) = {
    val referenced = collection.mutable.HashSet[String](ports.map(_.name):_*)
    var nEliminated = 0L

    def checkExpressionUse(e: Expression): Expression = {
      e match {
        case WRef(name, _, _, _) => referenced += name
        case _ => e map checkExpressionUse
      }
      e
    }

    def checkUse(s: Statement): Statement = {
      val out = s match {
        case x: Connect => x.copy(expr = checkExpressionUse(x.expr))
        case x: PartialConnect => x.copy(expr = checkExpressionUse(x.expr))
        case x: IsInvalid => x
        case _ => s map checkUse map checkExpressionUse
      }
      out
    }

    def maybeEliminate(x: Statement, name: String) =
      if (referenced(name)) x
      else {
        nEliminated += 1
        EmptyStmt
      }

    def maybeEliminateExp(s: Statement, expr: Expression) = expr match {
      case x: WRef => maybeEliminate(s, x.name)
      case _ => s
    }

    def removeUnused(s: Statement): Statement = s match {
      case x: DefRegister => maybeEliminate(x, x.name)
      case x: DefWire => maybeEliminate(x, x.name)
      case x: DefNode => maybeEliminate(x, x.name)
      case x: Connect => maybeEliminateExp(x, x.loc)
      case x: PartialConnect => maybeEliminateExp(x, x.loc)
      case x: IsInvalid => maybeEliminateExp(x, x.expr)
      case x => s map removeUnused
    }

    checkUse(s)
    (removeUnused(s), nEliminated)
  }

  @tailrec
  private def dce(ports: Seq[Port], s: Statement): Statement = {
    val (res, n) = dceOnce(ports, s)
    if (n > 0) dce(ports, res) else res
  }

  def run(c: Circuit): Circuit = {
    val modulesx = c.modules.map {
      case m: ExtModule => m
      case m: Module => Module(m.info, m.name, m.ports, dce(m.ports, m.body))
    }
    Circuit(c.info, modulesx, c.main)
  }
}
