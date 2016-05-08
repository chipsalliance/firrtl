// See LICENSE for license details.

package firrtl.interpreter

import com.typesafe.scalalogging.LazyLogging
import firrtl._
import collection.mutable
import scala.collection.mutable.ArrayBuffer

object DependencyGraph extends LazyLogging {
  def apply(circuit: Circuit): DependencyGraph = {
    val module = circuit.modules.head

    val dependencyGraph = new DependencyGraph(circuit, module)

    def getDepsStmt(s: Stmt): Stmt = s match {
      case begin: Begin =>
        // println(s"got a begin $begin")
        begin.stmts map getDepsStmt
        begin
      case con: Connect =>
        con.loc match {
          case WRef(name,_,_,_) => dependencyGraph(name) = con.exp
          case (_: WSubField | _: WSubIndex) =>
            val name = con.loc.serialize
            dependencyGraph(name) = con.exp
        }
        con
      case DefNode(_, name, expression) =>
        println(s"declaration:node: $s")
        dependencyGraph.recordName(name)
        dependencyGraph(name) = expression
        s
      case DefWire(_, name, _) =>
        println(s"declaration:node: $s")
        dependencyGraph.recordName(name)
        s
      case DefRegister(_, name, tpe, _, resetExpression, initValueExpression) =>
        println(s"declaration:reg: $s")
        dependencyGraph.registerNames += name
        dependencyGraph.recordName(name)
        dependencyGraph.recordType(name, tpe)
        dependencyGraph.registers += s.asInstanceOf[DefRegister]
        s
      case defMemory: DefMemory =>
        println(s"declaration:mem $defMemory")
        dependencyGraph.addMemory(defMemory)
        s
      case stopStatement: Stop =>
        dependencyGraph.addStop(stopStatement)
        s
      case printStatement: Print =>
        dependencyGraph.addPrint(printStatement)
        s
      case e: Empty =>
        s
      case conditionally: Conditionally =>
        // println(s"got a conditionally $conditionally")
        throw new InterpreterException(s"conditionally unsupported in interpreter $conditionally")
      case _ =>
        println(s"TODO: Unhandled statement $s")
        s
    }

    module match {
      case i: InModule =>
        for(port <- i.ports) {
          dependencyGraph.nameToType(port.name) = port.tpe
          if(port.direction == INPUT) {
            dependencyGraph.inputPorts += port.name
            dependencyGraph.recordName(port.name)
          }
          else if(port.direction == OUTPUT) {
            dependencyGraph.outputPorts += port.name
            dependencyGraph.recordName(port.name)
          }
        }
        getDepsStmt(i.body)
      case e: ExModule => // Do nothing
    }

    println(s"For module ${module.name} dependencyGraph =")
    dependencyGraph.nameToExpression.keys.toSeq.sorted foreach { case k =>
      val v = dependencyGraph.nameToExpression(k)
      println(s"  $k -> (" + v.toString + ")")
    }
    dependencyGraph
  }
}

class DependencyGraph(val circuit: Circuit, val module: Module) {
  val nameToExpression = new scala.collection.mutable.HashMap[String, Expression]
  val validNames       = new mutable.HashSet[String]
  val nameToType       = new mutable.HashMap[String, Type]
  val registerNames    = new mutable.HashSet[String]
  val registers        = new ArrayBuffer[DefRegister]
  val memories         = new mutable.HashMap[String, Memory]
  val stops            = new ArrayBuffer[Stop]
  val prints           = new ArrayBuffer[Print]

  val inputPorts       = new mutable.HashSet[String]
  val outputPorts      = new mutable.HashSet[String]

  def update(key: String, e: Expression): Unit = nameToExpression(key) = e
  def apply(key: String): Option[Expression] = {
    recordName(key)
    nameToExpression.get(key)
  }
  def keys: Iterable[String] = nameToExpression.keys
  def recordName(key: String): Unit = validNames += key
  def recordType(key: String, tpe: Type): Unit = {nameToType(key) = tpe}
  def getType(key: String): Type = nameToType(key)
  def getNameSet: mutable.HashSet[String] = mutable.HashSet(nameToExpression.keys.toSeq:_*)
  def addStop(stopStatement: Stop): Unit = { stops += stopStatement }
  def addPrint(printStatement: Print): Unit = { prints += printStatement }
  def addMemory(defMemory: DefMemory): Unit = {
    memories(defMemory.name) = Memory(defMemory)
  }

  def hasInput(name: String): Boolean = inputPorts.contains(name)
  def hasOutput(name: String): Boolean = outputPorts.contains(name)
}
