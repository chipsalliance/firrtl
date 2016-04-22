// See LICENSE for license details.

package firrtl.interpreter

import com.typesafe.scalalogging.LazyLogging
import firrtl._


object DependencyMapper extends LazyLogging {
  def apply(m: Module): Map[Expression, Expression] = {
    val dependencies = collection.mutable.HashMap[Expression, Expression]()

    def enumExpr(e: Expression): Expression = e match {
      case (_: UIntValue | _: SIntValue) => e
      case (_: Ref | _: WRef | _: WSubField) => e
      case m: Mux => m
      //TODO: Validate that the following are not LoFIRRTL
      //        case v: ValidIf => v.cond, v.value flatMap enumExpr
      //        case d: DoPrim => d.args flatMap enumExpr
      case _ =>
        println(s"Got node I can't handle $e")
        UIntValue(0, IntWidth(1))
    }

    def getDepsStmt(s: Stmt): Stmt = s match {
      case begin: Begin =>
        // println(s"got a begin $begin")
        begin.stmts map getDepsStmt
        begin
      case con: Connect =>
        // println(s"Got a connect $con")
        dependencies(con.loc) = con.exp
        con
      case conditionally: Conditionally =>
        // println(s"got a conditionally $conditionally")
        conditionally
      case _ => s
    }

    m match {
      case i: InModule => getDepsStmt(i.body)
      case e: ExModule => // Do nothing
    }
    println(s"For ${m.name} dependencies =")
    dependencies foreach { case (k, v) =>
      println(s"  $k -> (" + v.toString + ")")
      //println(s"  $k -> $v")
    }
    dependencies.toMap
  }

  def apply(c: Circuit): Map[String, Map[Expression, Expression]] = {

    c.modules.map { module =>
      module.name -> apply(module)
    }.toMap
  }

}
