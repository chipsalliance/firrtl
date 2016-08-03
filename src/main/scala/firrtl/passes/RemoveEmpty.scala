package firrtl
package passes

import scala.collection.mutable
import firrtl.Mappers.{ExpMap, StmtMap}
import firrtl.ir._

object RemoveEmpty extends Pass {
  def name = "Remove Empty Statements"
  private def onModule(m: DefModule): DefModule = {
    def onScope(s: Statement): Statement = {
      val newStmts = mutable.ArrayBuffer[Statement]()
      def onStmt(s: Statement): Unit = s match {
        case EmptyStmt => 
        case Block(stmts) => 
          stmts.foreach(onStmt(_))
        case c: Conditionally => c map onScope
        case s => newStmts += s
      }
      s match {
        case Block(stmts) =>
          stmts.foreach(onStmt(_))
          Block(newStmts.toSeq)
        case s => s map onScope
      }
    }
    m match {
      case m: Module => Module(m.info, m.name, m.ports, onScope(m.body))
      case m: ExtModule => m
    }
  }
  def run(c: Circuit): Circuit = Circuit(c.info, c.modules.map(onModule _), c.main)
}

// vim: set ts=4 sw=4 et:
