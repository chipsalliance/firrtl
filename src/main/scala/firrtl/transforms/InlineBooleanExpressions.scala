// See LICENSE for license details.

package firrtl.transforms

import firrtl.Mappers._
import firrtl.traversals.Foreachers._
import firrtl.Utils.isBooleanExpr
import firrtl._
import firrtl.WrappedExpression.we
import firrtl.analyses.InstanceGraph
import firrtl.ir._
import firrtl.transforms.InlineBooleanExpressionsTransforms.{Netlist, isSimpleExpr}

import scala.collection.mutable

/**
  * 1. Find all references to an LHS of a boolean statement
  * 2. If the LHS reference to the boolean statement is not referenced more than FANOUT_LIMIT or DEPTH
  *    then replace the references to the LHS in other statements with the RHS
  *
  *    wire x = a & b
  *    wire y = x & c
  *    convert to
  *    y = (a & b) & c
  *
  *
  */

object InlineBooleanExpressionsTransforms {

  // Checks if an Expression is made up of only boolean expressions terminated by a Literal or Reference.
  // private because it's not clear if this definition of "Simple Expression" would be useful elsewhere.
  // Note that this can have false negatives but MUST NOT have false positives.
  def isSimpleExpr(expr: Expression): Boolean = expr match {
    case _: WRef | _: Literal | _: WSubField => true
    case DoPrim(op, args, _,_) if isBooleanExpr(op) => args.forall(isSimpleExpr)
    case _ => false
  }

  /** Mapping from references to the [[firrtl.ir.Expression Expression]]s that drive them */
  type Netlist = mutable.HashMap[WrappedExpression, Expression]

  private def onArg(netlist: Netlist, symbolTable: Map[WrappedExpression, Int], depth: Int)
                   (expr: Expression): Expression =
    netlist.get(we(expr)) match {
      case Some(e) if isBooleanExpr(e) && symbolTable(we(expr)) <= 1 => e
      case _ => expr
    }

  /** Recursively replace [[WRef]]s with new [[Expression]]s
    *
    * @param netlist a '''mutable''' HashMap mapping references to [[firrtl.ir.DefNode DefNode]]s to their connected
    * [[firrtl.ir.Expression Expression]]s. It is '''not''' mutated in this function
    * @param expr the Expression being transformed
    * @return Returns expr with Bits inlined
    */
  def onExpr(netlist: Netlist, symbolTable: Map[WrappedExpression, Int], depth: Int)(expr: Expression): Expression = {
    expr.map(onExpr(netlist, symbolTable, depth + 1)) match {
      case lhs: DoPrim if isSimpleExpr(lhs) => lhs.map(onArg(netlist, symbolTable, depth))
      case other => other
    }
  }

  /**
    * During the recursive traversal of the rhs expression the depth that the boolean expressions are combined is controlled
    * StartingDepth is set to 0
    */
  val StartingDepth = 0

  /** Inline bits in a Statement
    *
    * @param netlist a '''mutable''' HashMap mapping references to [[firrtl.ir.DefNode DefNode]]s to their connected
    * [[firrtl.ir.Expression Expression]]s. This function '''will''' mutate it if stmt is a [[firrtl.ir.DefNode
    * DefNode]] with a Temporary name and a value that is a [[PrimOp]] Bits
    * @param stmt the Statement being searched for nodes and transformed
    * @return Returns stmt with Bits inlined
    */
  def onStmt(netlist: Netlist, symbolTable: Map[WrappedExpression, Int])(stmt: Statement): Statement =
  stmt.map(onStmt(netlist, symbolTable)).map(onExpr(netlist, symbolTable, StartingDepth))

  /** Replaces bits in a Module */
  def onMod(mod: DefModule,
    netlist: Netlist,
    symbolTable: Map[WrappedExpression, Int]): DefModule =
    mod.map(onStmt(netlist, symbolTable))
}

/** Inline nodes that are simple bits */
class InlineBooleanExpressions extends Transform {
  def inputForm = LowForm
  def outputForm = LowForm

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
      case _ => stmt.foreach(onStmt)
    }

    mod.foreach(onStmt)
    symTab.toMap
  }

  def execute(state: CircuitState): CircuitState = {

    val symTabs = state.circuit.modules.map{ m => (m.name -> buildSymTab(m)) }.toMap

    val netlists = state.circuit.modules.map{ m => (m.name -> buildNetlist(m)) }.toMap


    val modulesx = state.circuit.modules.map {
      m => InlineBooleanExpressionsTransforms.onMod(m, netlists(m.name), symTabs(m.name))
  }

    state.copy(circuit = state.circuit.copy(modules = modulesx))
  }
}
