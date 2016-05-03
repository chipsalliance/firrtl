// See LICENSE for license details.

package firrtl.interpreter

import com.typesafe.scalalogging.LazyLogging
import firrtl._
import collection.mutable
import scala.collection.mutable.ArrayBuffer

object DependencyGraph extends LazyLogging {
  def apply(m: Module): DependencyGraph = {
    val dependencies = new DependencyGraph

    def getDepsStmt(s: Stmt): Stmt = s match {
      case begin: Begin =>
        // println(s"got a begin $begin")
        begin.stmts map getDepsStmt
        begin
      case con: Connect =>
        con.loc match {
          case WRef(name,_,_,_) => dependencies(name) = con.exp
          case (_: WSubField | _: WSubIndex) =>
            val name = con.loc.serialize
            dependencies(name) = con.exp
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
      case DefRegister(_, name, tpe, _, resetExpression, initValueExpression) =>
        println(s"declaration:reg: $s")
        dependencies.registerNames += name
        dependencies.recordLhs(name)
        dependencies.recordType(name, tpe)
        dependencies.registers += s.asInstanceOf[DefRegister]
        s
      case defMemory: DefMemory =>
        println(s"declaration:mem $defMemory")
        dependencies.addMemory(defMemory)
        s
      case stopStatement: Stop =>
        dependencies.addStop(stopStatement)
        s
      case printStatement: Print =>
        dependencies.addPrint(printStatement)
        s
      case conditionally: Conditionally =>
        // println(s"got a conditionally $conditionally")
        throw new InterpreterException(s"conditionally unsupported in interpreter $conditionally")
      case _ =>
        println(s"TODO: Unhandled statement $s")
        s
    }

    m match {
      case i: InModule =>
        for(port <- i.ports) {
          dependencies.nameToType(port.name) = port.tpe
        }
        getDepsStmt(i.body)
      case e: ExModule => // Do nothing
    }
    println(s"For module ${m.name} dependencies =")
    dependencies.nameToExpression.keys.toSeq.sorted foreach { case k =>
      val v = dependencies.nameToExpression(k)
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

class DependencyGraph {
  val nameToExpression = new scala.collection.mutable.HashMap[String, Expression]
  val lhsEntities      = new mutable.HashSet[String]
  val nameToType       = new mutable.HashMap[String, Type]
  val registerNames    = new mutable.HashSet[String]
  val registers        = new ArrayBuffer[DefRegister]
  val memories         = new mutable.HashMap[String, Memory]
  val stops            = new ArrayBuffer[Stop]
  val prints           = new ArrayBuffer[Print]

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
  def addStop(stopStatement: Stop): Unit = { stops += stopStatement }
  def addPrint(printStatement: Print): Unit = { prints += printStatement }
  def addMemory(defMemory: DefMemory): Unit = {
    memories(defMemory.name) = Memory(defMemory)
  }
}
