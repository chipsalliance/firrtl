package firrtl
package transforms

import firrtl.ir._
import firrtl.Mappers._
import firrtl.PrimOps._

import firrtl.Utils.{isCast, NodeMap}
import firrtl.analyses.NodeMap

object VerilogOptimizations {

  // NOTE This is an experimental implementation and as such is private
  private type Analysis = Statement => Unit
  private type StmtMapper = Statement => Statement
  private type ExprMapper = Expression => Expression

  private abstract class ModuleTransformer {
    def analyses: Seq[Analysis]
    def stmtMappers: Seq[StmtMapper]
    def exprMappers: Seq[ExprMapper]

    def onExpr(expr: Expression): Expression = {
      val exprx = expr.map(onExpr)
      exprMappers.foldLeft(exprx)((e, f) => f(e))
    }
    def onStmt(stmt: Statement): Statement = {
      val stmtx = stmt.map(onStmt)
      analyses.foreach(f => f(stmtx))
      stmtMappers.foldLeft(stmtx)((s, f) => f(s))
    }
    def transform(module: DefModule): DefModule = module match {
      case ext: ExtModule => ext
      case mod: Module => mod.map(onStmt)
    }
  }

  private class VerilogOptModTransformer extends ModuleTransformer {
    private val netlist = new NodeMap
    def analyses = Seq(NodeMap.processStmt(netlist))
    def stmtMappers = Seq()
    def exprMappers = ???
  }

}

///** Inline nodes that are simple casts */
//class InlineCastsTransform extends Transform {
//  def inputForm = LowForm
//  def outputForm = LowForm
//
//  def execute(state: CircuitState): CircuitState = {
//    val modulesx = state.circuit.modules.map(InlineCastsTransform.onMod(_))
//    state.copy(circuit = state.circuit.copy(modules = modulesx))
//  }
//}
