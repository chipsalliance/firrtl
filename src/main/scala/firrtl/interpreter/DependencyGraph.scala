// See LICENSE for license details.

package firrtl.interpreter

import com.typesafe.scalalogging.LazyLogging
import firrtl._
import collection.mutable

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

    def getDepsStmt(s: Stmt): Stmt = s match {
      case begin: Begin =>
        // println(s"got a begin $begin")
        begin.stmts map getDepsStmt
        begin
      case con: Connect =>
        con.loc match {
          case WRef(name,_,_,_) => dependencies(name) = enumExpr(con.exp)
          case _ => throw new InterpreterException(s"error:unsupported lhs $con")
        }
        con
      case DefNode(_, name, expression) =>
        println(s"declaration:node: $s")
        dependencies.register(name)
        dependencies(name) = expression
        s
      case DefWire(_, name, _) =>
        println(s"declaration:node: $s")
        dependencies.register(name)
        s
      case DefRegister(_, name, tpe, _, _, _) =>
        println(s"declaration:reg: $s")
        dependencies.registers += name
        dependencies.register(name)
        dependencies.recordType(name, tpe)
        s
      case conditionally: Conditionally =>
        // println(s"got a conditionally $conditionally")
        throw new InterpreterException(s"conditionally unsupported in interpreter $conditionally")
      case _ =>
        println(s"TODO: Unhandled statement ${s}")
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

abstract class DependencyKey extends Expression { val name: String }
case class RegisterDependencyKey(val name: String) extends DependencyKey
case class NodeDependencyKey(val name: String) extends DependencyKey

class DependencyGraph {
  val nameToExpression = new scala.collection.mutable.HashMap[String, Expression]
  val lhsEntities      = new mutable.HashSet[String]
  val nameToType       = new mutable.HashMap[String, Type]
  val registers        = new mutable.HashSet[String]

  def update(key: String, e: Expression): Unit = nameToExpression(key) = e
  def apply(key: String): Option[Expression] = {
    register(key)
    nameToExpression.get(key)
  }
  def keys: Iterable[String] = nameToExpression.keys
  def register(key: String): Unit = lhsEntities += key
  def recordType(key: String, tpe: Type): Unit = {nameToType(key) = tpe}
  def getNameSet: mutable.HashSet[String] = mutable.HashSet(nameToExpression.keys.toSeq:_*)
}
