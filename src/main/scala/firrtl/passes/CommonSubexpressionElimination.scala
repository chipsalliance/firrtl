// See LICENSE for license details.

package firrtl.passes

import firrtl._
import firrtl.ir._
import firrtl.Mappers._
import firrtl.options.PreservesAll


class CommonSubexpressionElimination extends Pass with PreservesAll[Transform] {

  override val prerequisites = firrtl.stage.Forms.LowForm ++
    Seq( classOf[firrtl.passes.RemoveValidIf],
         classOf[firrtl.transforms.ConstantPropagation],
         classOf[firrtl.passes.memlib.VerilogMemDelays],
         classOf[firrtl.passes.SplitExpressions],
         classOf[firrtl.transforms.CombineCats] )

  override val dependents = Seq(classOf[SystemVerilogEmitter], classOf[VerilogEmitter])

  private def cse(s: Statement): Statement = {
    val expressions = collection.mutable.HashMap[MemoizedHash[Expression], String]()
    val nodes = collection.mutable.HashMap[String, Expression]()

    def eliminateNodeRef(e: Expression): Expression = e match {
      case WRef(name, tpe, kind, gender) => nodes get name match {
        case Some(expression) => expressions get expression match {
          case Some(cseName) if cseName != name =>
            WRef(cseName, tpe, kind, gender)
          case _ => e
        }
        case _ => e
      }
      case _ => e map eliminateNodeRef
    }

    def eliminateNodeRefs(s: Statement): Statement = {
      s map eliminateNodeRef match {
        case x: DefNode =>
          nodes(x.name) = x.value
          expressions.getOrElseUpdate(x.value, x.name)
          x
        case other => other map eliminateNodeRefs
      }
    }

    eliminateNodeRefs(s)
  }

  def run(c: Circuit): Circuit = {
    val modulesx = c.modules.map {
      case m: ExtModule => m
      case m: Module => Module(m.info, m.name, m.ports, cse(m.body))
    }
    Circuit(c.info, modulesx, c.main)
  }
}

object CommonSubexpressionElimination extends Pass with DeprecatedPassObject {

  override lazy val underlying = new CommonSubexpressionElimination

}
