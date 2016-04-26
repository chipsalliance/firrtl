// See LICENSE for license details.

package firrtl.interpreter

import com.typesafe.scalalogging.LazyLogging
import firrtl._
import collection.mutable
import scala.collection.mutable.ArrayBuffer

object DependencyGraph extends LazyLogging {
  def apply(m: Module): DependencyGraph = {
    val dependencies = new DependencyGraph

    val node_map = new collection.mutable.HashMap[String, Expression]

    def enumExpr(e: Expression): Expression = e match {
      case w: WRef => e
      case (_: UIntValue | _: SIntValue) => e
      case (_: Ref |_: WRef |_: WSubField) => e
      case m: Mux => m
      //TODO: Validate that the following are not LoFIRRTL
      //        case v: ValidIf => v.cond, v.value flatMap enumExpr
      //        case d: DoPrim => d.args flatMap enumExpr
      case _ =>
        println(s"Got node I can't handle $e")
        UIntValue(0, IntWidth(1))
    }

//   1 /**
//      *
//      * @param expression
//      * @param nameSoFar
//      * @return
//      */
//    def lhsGetName(expression: Expression, nameSoFar: String = ""): String = {
//      expression match {
//        case WRef(name, _, _, _) => name
//        case WSubField(subExpression, name, _, _ ) => lhsGetName(subExpression) + "." + name
//        case WSubIndex(subExpression, value, _, _ ) => lhsGetName(subExpression) + "." + value.toString
//        case _ => throw new InterpreterException(s"error:unsupported lhs $expression")
//      }
//    }

    def getDepsStmt(s: Stmt): Stmt = s match {
      case begin: Begin =>
        // println(s"got a begin $begin")
        begin.stmts map getDepsStmt
        begin
      case con: Connect =>
        con.loc match {
          case WRef(name,_,_,_) => dependencies(name) = enumExpr(con.exp)
          case (_: WSubField | _: WSubIndex) =>
            val name = con.loc.serialize
            dependencies(name) = enumExpr(con.exp)
        }
        con
      case DefNode(_, name, expression) =>
        println(s"declaration:node: $s")
        dependencies.recordLhs(name)
        dependencies(name) = expression
        s
      case DefWire(_, name, _) =>
        println(s"declaration:node: $s")
        dependencies.recordLhs(name)
        s
      case DefRegister(_, name, tpe, _, _, _) =>
        println(s"declaration:reg: $s")
        dependencies.registerNames += name
        dependencies.recordLhs(name)
        dependencies.recordType(name, tpe)
        s
      case Stop(info, returnValue, _, expression) =>
        dependencies.addStop(expression, returnValue, info)
        s
      case conditionally: Conditionally =>
        // println(s"got a conditionally $conditionally")
        throw new InterpreterException(s"conditionally unsupported in interpreter $conditionally")
      case _ =>
        println(s"TODO: Unhandled statement $s")
        s
    }

    m match {
      case i: InModule => getDepsStmt(i.body)
      case e: ExModule => // Do nothing
    }
    println(s"For ${m.name} dependencies =")
    dependencies.nameToExpression foreach { case (k, v) =>
      println(s"  $k -> (" + v.toString + ")")
    }
    dependencies
  }

//  def apply(c: Circuit): Map[String, Map[Expression, Expression]] = {
//    c.modules.map { module =>
//      module.name -> apply(module)
//    }.toMap
//  }
}

case class StopCondition(expression: Expression, returnValue : Int, info: Info)

class DependencyGraph {
  val nameToExpression = new scala.collection.mutable.HashMap[String, Expression]
  val lhsEntities      = new mutable.HashSet[String]
  val nameToType       = new mutable.HashMap[String, Type]
  val registerNames    = new mutable.HashSet[String]
  val stops            = new ArrayBuffer[StopCondition]

  def update(key: String, e: Expression): Unit = nameToExpression(key) = e
  def apply(key: String): Option[Expression] = {
    recordLhs(key)
    nameToExpression.get(key)
  }
  def keys: Iterable[String] = nameToExpression.keys
  def recordLhs(key: String): Unit = lhsEntities += key
  def recordType(key: String, tpe: Type): Unit = {nameToType(key) = tpe}
  def getType(key: String): Type = nameToType(key)
  def getNameSet: mutable.HashSet[String] = mutable.HashSet(nameToExpression.keys.toSeq:_*)
  def addStop(expression: Expression, returnValue: Int, info: Info): Unit = {
    stops += StopCondition(expression, returnValue, info)
  }
}
