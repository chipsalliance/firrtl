// See LICENSE for license details.

package firrtl

import firrtl.ir._
import firrtl.passes.MemPortUtils

import scala.collection.mutable

/** works on a per module basis */
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
}

/** implements the symbol table interface with a single backing store */
private[firrtl] class StandardSymbolTable(global: GlobalSymbolTable)  extends SymbolTable {
  def declare(name: String, tpe: Type, kind: Kind): Unit = {
    assert(!symbols.contains(name), s"Symbol ${}")
  }
  def declareInstance(name: String, module: String): Unit = {
    // TODO: should we support declarations without a complete global symbol table?
    //       i.e. using the tpe of the DefInstance node
    val t = global.moduleType(module).get
    // TODO: InstanceKind vs PortKind?
    symbols(name) = Symbol(t, firrtl.InstanceKind)
  }

  private val symbols = mutable.HashMap[String, Symbol]()
  case class Symbol(tpe: Type, kind: Kind)
}

object SymbolTable {
  def scanModule(t: SymbolTable, m: DefModule): SymbolTable = {
    val table: SymbolTable = t
    m match {
      case Module(_, _, _, body) => scanStatement(body)(table)
      case _ : ExtModule =>
    }
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

/** contains information about the modules types which are globally visible */
trait GlobalSymbolTable {
  def declare(name: String, tpe: BundleType): Unit
  def moduleType(name: String): Option[BundleType]
  def declare(m: DefModule): Unit = declare(m.name, Utils.module_type(m))
}

private class StandardGlobalSymbolTable extends GlobalSymbolTable {
  private def moduleTypes = mutable.HashMap[String, BundleType]()
  def declare(name: String, tpe: BundleType): Unit = moduleTypes(name) = tpe
  def moduleType(name: String): Option[BundleType] = moduleTypes.get(name)
}

object GlobalSymbolTable {
  def scanModuleTypes[T <: GlobalSymbolTable](c: Circuit, t: => T): T = scanModuleTypes(c.modules, t)
  def scanModuleTypes(c: Circuit): GlobalSymbolTable = scanModuleTypes(c.modules, new StandardGlobalSymbolTable)
  def scanModuleTypes[T <: GlobalSymbolTable](m: Iterable[DefModule], t: => T): T = { m.foreach(t.declare) ; t }
}