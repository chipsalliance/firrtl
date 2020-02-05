// See LICENSE for license details.

package firrtl.transforms

import firrtl.Mappers._
import firrtl.PrimOps._
import firrtl.traversals.Foreachers._
import firrtl.Utils.isAddorSubExpr
import firrtl._
import firrtl.WrappedExpression.we
import firrtl.analyses.InstanceGraph
import firrtl.ir._

import scala.collection.mutable

object SimpleArithmetic {
  // Checks if an Expression is made up of only expressions terminated by a Literal or Reference.
  // private because it's not clear if this definition of "Simple Expression" would be useful elsewhere.
  // Note that this can have false negatives but MUST NOT have false positives.
  private def isSimpleExpr(expr: Expression): Boolean = expr match {
      case _: WRef | _: Literal | _: WSubField => true
      case e@DoPrim(op, args, _, _) if isAddorSubExpr(e) => args.forall(isSimpleExpr)
      case _ => false
    }

  def notMax(n: BigInt, w: BigInt): Boolean = {
    val n1 = -((BigInt(1) << (w.toInt)) - 1)
    n != n1
  }

  /**
    *
    * @param e
    * @return
    */
  def replaceSimple(e: Expression): Expression = {
    e match {
      case DoPrim(Add, Seq(e, lit@SIntLiteral(n, _)), consts, tpe) if (n < 0) =>  DoPrim(Sub, Seq(e, lit.copy(n * -1)), consts, tpe)
      case DoPrim(Addw, Seq(e, lit@SIntLiteral(n, _)), consts, tpe) if (n < 0) =>  DoPrim(Subw, Seq(e, lit.copy(n * -1)), consts, tpe)
      case DoPrim(Sub, Seq(e, lit@SIntLiteral(n, IntWidth(w))), consts, tpe) if (n < 0) && notMax(n, w) =>
        DoPrim(Add, Seq(e, lit.copy(n * -1)), consts, tpe)
      case DoPrim(Subw, Seq(e, lit@SIntLiteral(n, IntWidth(w))), consts, tpe) if (n < 0) && notMax(n, w) =>
        DoPrim(Addw, Seq(e, lit.copy(n * -1)), consts, tpe)
      case _ => e
    }
  }

  /** Mapping from references to the [[firrtl.ir.Expression Expression]]s that drive them */
  type Netlist = mutable.HashMap[WrappedExpression, Expression]

  def onExpr(netlist: Netlist, symbolTable: Map[WrappedExpression, Int])(expr: Expression): Expression = {
    expr.map(onExpr(netlist, symbolTable)) match {
      case e@DoPrim(op, args, _, _) if isAddorSubExpr(e) => replaceSimple(expr)
      case other => other
    }
  }

  def onStmt(netlist: Netlist, symbolTable: Map[WrappedExpression, Int])(stmt: Statement): Statement = {
    stmt.map(onStmt(netlist, symbolTable)).map(onExpr(netlist, symbolTable))
  }

  /** Replaces simple artihmetic epressions with one constant in a Module */
  def onMod(mod: DefModule,
    netlist: Netlist,
    symbolTable: Map[WrappedExpression, Int]): DefModule =
    mod.map(onStmt(netlist, symbolTable))
}

/**
  * {{{
  *   circuit Top :
  *   module Top :
  *     input a : SInt<2>
  *     output y : SInt<2>
  *     y <= add(a, SInt(-1))
  * }}}
  *
  * converts to
  *
  * {{{
  *   circuit Top :
  *   module Top :
  *     input a : SInt<2>
  *     output y : SInt<2>
  *     y <= sub(a, SInt(1))
  * }}}
  *
  * ________________________
  *
  * * {{{
  *   *   circuit Top :
  *   *   module Top :
  *   *     input a : SInt<2>
  *   *     output y : SInt<2>
  *   *     y <= sub(a, SInt(-1))
  *   * }}}
  * *
  * * converts to
  * *
  * * {{{
  *   *   circuit Top :
  *   *   module Top :
  *   *     input a : SInt<2>
  *   *     output y : SInt<2>
  *   *     y <= add(a, SInt(1))
  *   * }}}
   */
class SimpleArithmetic extends Transform {
  def inputForm = LowForm
  def outputForm = LowForm

  import SimpleArithmetic._

  def makeModuleInfo(c: Circuit): (Map[String, Map[String, String]], Seq[String], InstanceGraph, Map[String, DefModule]) = {
    val iGraph = (new InstanceGraph(c))
    val childInstances = iGraph.getChildrenInstances.mapValues(_.map(inst => inst.name -> inst.module).toMap).toMap

    // Order from leaf modules to root so that any module driving an output
    // with a constant will be visible to modules that instantiate it
    // TODO Generating order as we execute constant propagation on each module would be faster
    val x = iGraph.moduleOrder.reverse

    val moduleNameTraversalOrder = x.map{ m => m.name }

    val modNameModules = x.map{ m => (m.name -> m)}.toMap
    (childInstances, moduleNameTraversalOrder, iGraph, modNameModules)
  }

  /** Build a [[Netlist]] from a Module's connections and Nodes
    * This assumes [[firrtl.LowForm LowForm]]
    * @param mod [[firrtl.ir.Module Module]] from which to build a [[Netlist]]
    * @return [[Netlist]] of the module's connections and nodes
    */
  def buildNetlist(mod: DefModule): Netlist = {
    val netlist = new Netlist()
    def onStmt(stmt: Statement): Statement = {
      stmt.map(onStmt) match {
        case Connect(_, lhs  , rhs) => netlist(WrappedExpression(lhs)) = rhs
        case DefNode(_, nname, rhs) => netlist(WrappedExpression(WRef(nname))) = rhs
        case _ => // Do nothing
      }
      stmt
    }
    mod.map(onStmt)
    netlist
  }

  def buildSymTab(mod: DefModule): Map[WrappedExpression, Int] = {
    val symTab = mutable.HashMap[WrappedExpression, Int]()

    def update(signalName: WrappedExpression): Unit = symTab(signalName) = symTab.getOrElse(signalName, 0) + 1

    def onExpr(expr: Expression): Unit = expr match {
      case lhs @ DoPrim(_, args, _, _) if isSimpleExpr(lhs) => args.foreach(e => update(we(e)))
      case _ => expr.foreach(onExpr)
    }

    def onStmt(stmt: Statement): Unit = stmt match {
      case Connect(_, _, rhs) => onExpr(rhs)
      case DefNode(_, _, rhs) => onExpr(rhs)
      case Conditionally(_, pred, conseq, alt) =>
        onExpr(pred)
        onStmt(conseq)
        onStmt(alt)
      case _ => stmt.foreach(onStmt)
    }

    mod.foreach(onStmt)
    symTab.toMap
  }

  def execute(state: CircuitState): CircuitState = {

    val symTabs = state.circuit.modules.map{ m => (m.name -> buildSymTab(m)) }.toMap

    val netlists = state.circuit.modules.map{ m => (m.name -> buildNetlist(m)) }.toMap


    val modulesx = state.circuit.modules.map {
      m => SimpleArithmetic.onMod(m, netlists(m.name), symTabs(m.name))
    }

    state.copy(circuit = state.circuit.copy(modules = modulesx))
  }
}
