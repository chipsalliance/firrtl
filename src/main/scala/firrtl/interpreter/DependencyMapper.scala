// See LICENSE for license details.

package firrtl.interpreter

import com.typesafe.scalalogging.LazyLogging
import firrtl._


object DependencyMapper extends LazyLogging {
  def getCircuitDependencies(c: Circuit): Map[String, Map[Expression, Expression]] = {
    def getModuleDependencies(m: Module): Map[Expression, Expression] = {
      val deps = collection.mutable.HashMap[Expression, Expression]()

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
          println(s"got a begin $begin")
          begin.stmts map getDepsStmt
          begin
        case con: Connect =>
          println(s"Got a connect $con")
//          enumExpr(con.exp) foreach { e =>
//            val d = deps.getOrElseUpdate(e, new collection.mutable.ArrayBuffer[Expression]())
//            d += con.loc
//          }
          deps(con.loc) = con.exp
          con
        case conditionally: Conditionally =>
          println(s"got a conditionally $conditionally")
          conditionally
//        case n: DefNode =>
//          println(s"got a DefNode $n")
//          val expr = WRef(n.name, Utils.tpe(n.value), NodeKind(), MALE)
//          enumExpr(n.value) foreach { v =>
//            val d = deps.getOrElseUpdate(v, new collection.mutable.ArrayBuffer[Expression]())
//            d += expr
//          }
//          n
        case _ => s
      }

      m match {
        case i: InModule => getDepsStmt(i.body)
        case e: ExModule => // Do nothing
      }
      println(s"For ${m.name} deps =")
      deps foreach { case (k, v) =>
        println(s"  ${k.serialize} -> (" + v.toString + ")")
        //println(s"  $k -> $v")
      }
      deps.toMap
    }

    c.modules.map { module =>
      module.name -> getModuleDependencies(module)
    }.toMap
  }

}
