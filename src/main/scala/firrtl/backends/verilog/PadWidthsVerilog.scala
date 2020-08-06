// See LICENSE for license details.

package firrtl.backends.verilog

import firrtl.ir._
import firrtl.PrimOps._
import firrtl.Mappers._
import firrtl.{Dshlw, SystemVerilogEmitter, Transform, VerilogEmitter, bitWidth}
import firrtl.options.Dependency
import firrtl.passes.SplitExpressions
import firrtl.stage.Forms
import firrtl.stage.TransformManager.TransformDependency


/** Adds padding for mul, div, rem, dshl which breaks firrtl width invariance, but is nmeeded to match Verilog semantics */
private[firrtl] object PadWidthsVerilog extends firrtl.passes.Pass {

  override def prerequisites: Seq[TransformDependency] = Forms.MidForm ++
    Seq( Dependency(firrtl.passes.LowerTypes),
         Dependency(firrtl.transforms.RemoveReset),
         Dependency[firrtl.transforms.RemoveWires] )

  override def optionalPrerequisiteOf =
    Seq( Dependency(firrtl.passes.memlib.VerilogMemDelays),
      Dependency[SystemVerilogEmitter],
      Dependency[VerilogEmitter] )

  override def invalidates(a: Transform): Boolean = a match {
    case SplitExpressions => true // we generate pad and bits operations inline which need to be split up
    case _ => false
  }

  import firrtl.passes.PadWidths.forceWidth
  private def getWidth(e: Expression): Int = bitWidth(e.tpe).toInt

  // Recursive, updates expression so children exp's have correct widths
  private def onExp(e: Expression): Expression = e.map(onExp) match {
    case ex: DoPrim => ex.op match {
      case Mul | Div | Rem => ex.map(forceWidth(ex.args.map(getWidth).max))
      case Dshl =>
        // special case as args aren't all same width
        ex copy (op = Dshlw, args = Seq(forceWidth(getWidth(ex))(ex.args.head), ex.args(1)))
      case _ => ex
    }
    case ex => ex
  }

  private def onStmt(s: Statement): Statement = s.map(onExp).map(onStmt)
  def run(c: Circuit): Circuit = c.copy(modules = c.modules.map(_.map(onStmt)))
}
