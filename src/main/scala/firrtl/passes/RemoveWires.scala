// See LICENSE for license details.

package firrtl.passes

import firrtl._
import firrtl.ir._
import firrtl.Utils._
import firrtl.Mappers._

import annotation.tailrec

/** Remove forward propagating wires
  *
  * Replace wires with nodes where possible.
  * This should replace all wires except those used in DefRegister definitions
  */
// TODO For now this only removes wires that are constants
object RemoveWires extends Pass {
  //// TODO combine with similar code in DCE
  //def extractRefs(expr: Expression): Seq[Expression] = {
  //  val refs = mutable.ArrayBuffer.empty[Expression]
  //  def rec(e: Expression): Expression = {
  //    e match {
  //      case ref @ (_: WRef | _: WSubField) => refs += ref
  //      case nested @ (_: Mux | _: DoPrim | _: ValidIf) => nested map rec
  //      case ignore @ (_: Literal) => // Do nothing
  //      case unexpected => throwInternalError
  //    }
  //    e
  //  }
  //  rec(expr)
  //  refs
  //}

  private def onModule(mod: Module): Module = {
    val constWires = collection.mutable.HashMap.empty[String, Expression]

    def findConstWires(stmt: Statement): Statement = stmt match {
      case Connect(_, WRef(name, _, WireKind, _), lit: Literal) =>
        constWires(name) = lit
        stmt
      case other => other map findConstWires
    }
    def replaceConstWires(stmt: Statement): Statement = stmt match {
      case DefWire(info, name, _) if constWires.contains(name) =>
        DefNode(info, name, constWires(name))
      case Connect(_, WRef(name, _, WireKind, _), _) if constWires.contains(name) =>
        EmptyStmt
      case block: Block => block map replaceConstWires
      case other => other
    }

    findConstWires(mod.body)
    val bodyx = replaceConstWires(mod.body)
    mod.copy(body = bodyx)
  }

  def run(c: Circuit): Circuit = {
    val modulesx = c.modules.map {
      case m: ExtModule => m
      case m: Module => onModule(m)
    }
    Circuit(c.info, modulesx, c.main)
  }
}
