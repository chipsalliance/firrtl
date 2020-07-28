// See LICENSE for license details.

package firrtl

import firrtl.ir._
import firrtl.passes.MemPortUtils

import scala.collection.mutable

/** This trait represents a data structure that stores information
  * on all the symbols available in a single firrtl module.
  * The module can either be scanned all at once using the
  * scanModule helper function from the companion object or
  * the SymbolTable can be updated while traversing the module by
  * calling the declare method every time a declaration is encountered.
  * Different implementations of SymbolTable might want to store different
  * information (e.g., only the names without the types) or build
  * different indices depending on what information the transform needs.
  * */
trait SymbolTable {
  // methods that need to be implemented by any Symbol table
  def declare(name: String, tpe: Type, kind: Kind): Unit
  def declareInstance(name: String, module: String): Unit

  // convenience methods
  def declare(d: DefInstance): Unit = declareInstance(d.name, d.module)
  def declare(d: DefMemory): Unit = declare(d.name, MemPortUtils.memType(d), firrtl.MemKind)
  def declare(d: DefNode): Unit = declare(d.name, d.value.tpe, firrtl.NodeKind)
  def declare(d: DefWire): Unit = declare(d.name, d.tpe, firrtl.WireKind)
  def declare(d: DefRegister): Unit = declare(d.name, d.tpe, firrtl.RegKind)
  def declare(d: Port): Unit = declare(d.name, d.tpe, firrtl.PortKind)
}

/** Implements the symbol table interface with a single backing store.
  * Trusts the type annotation on DefInstance nodes instead of re-deriving the type from
  * the module ports which would require global (cross-module) information.
  */
private[firrtl] class LocalSymbolTable  extends SymbolTable {
  import LocalSymbolTable._
  def declare(name: String, tpe: Type, kind: Kind): Unit = {
    assert(!symbols.contains(name), s"Symbol $name already declared: ${symbols(name)}")
    symbols(name) = Symbol(tpe, kind)
  }
  def declareInstance(name: String, module: String): Unit = declare(name, UnknownType, InstanceKind)
  override def declare(d: WDefInstance): Unit = declare(d.name, d.tpe, InstanceKind)
  private val symbols = mutable.HashMap[String, Symbol]()
  def getSymbols: Map[String, Symbol] = symbols.toMap
}

private[firrtl] object LocalSymbolTable { case class Symbol(tpe: Type, kind: Kind) }

/** Provides convenience methods to populate SymbolTables. */
object SymbolTable {
  def scanModule[T <: SymbolTable](m: DefModule, t: T): T = {
    implicit val table: T = t
    m.foreachPort(table.declare)
    m.foreachStmt(scanStatement)
    table
  }
  private def scanStatement(s: Statement)(implicit table: SymbolTable): Unit = s match {
    case d: DefInstance => table.declare(d)
    case d: DefMemory => table.declare(d)
    case d: DefNode => table.declare(d)
    case d: DefWire => table.declare(d)
    case d: DefRegister => table.declare(d)
    case other => other.foreachStmt(scanStatement)
  }
}
