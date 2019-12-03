package firrtl
package analyses

import firrtl.ir.{Statement, DefNode}

import firrtl.Utils.NodeMap

object NodeMap {

  /** Processes a single [[Statement]] mutating the netlist argument */
  def processStmt(netlist: NodeMap)(stmt: Statement): Unit = stmt match {
    case DefNode(_, name, value) => netlist(name) = value
    case _                       =>
  }
}
