// See LICENSE for license details.

package firrtl.transforms

import firrtl.Mappers._
import firrtl.traversals.Foreachers._
import firrtl.Utils.isBooleanExpr
import firrtl._
import firrtl.analyses.InstanceGraph
import firrtl.ir._
import firrtl.WrappedExpression._

import scala.collection.mutable

object InlineBooleanExpressions {
  // Checks if an Expression is made up of only boolean expressions terminated by a Literal or Reference.
  // private because it's not clear if this definition of "Simple Expression" would be useful elsewhere.
  // Note that this can have false negatives but MUST NOT have false positives.
  private def isSimpleExpr(expr: Expression): Boolean = expr match {
    case _: WRef | _: Literal | _: WSubField => true
    case DoPrim(op, args, _,_) if isBooleanExpr(op) => args.forall(isSimpleExpr)
    case _ => false
  }

  private def lookup(s: String, netlist: Netlist): Option[Expression] = {
    if (isTmpVar(s)) {
      netlist.get(s) match {
        case Some(e) => Some(e)
        case _ => throw new IllegalArgumentException(s"Error: netlist does not contain ${s}")
      }
    }
    else {
      None
    }
  }

  /** Mapping from references to the [[firrtl.ir.Expression Expression]]s that drive them */
  type Netlist = mutable.HashMap[String, Expression]

  private def onArg(netlist: Netlist, symbolTable: Map[WrappedExpression, Int])
                   (expr: Expression): Expression = {
    if (isTmpVar(expr.serialize) && !netlist.contains(expr.serialize)) {
      throw new IllegalArgumentException(s"Error: netlist does not contain ${expr.serialize}")
    }
    else {
      lookup(expr.serialize, netlist) match {
        case Some(e) if isBooleanExpr(e) && symbolTable(we(expr)) <= 1 => e
        case _ => expr
      }
    }
  }

  def isTmpVar(s: String): Boolean = s.charAt(0) == '_'

  /** Recursively replace [[WRef]]s with new [[Expression]]s
    *
    * @param netlist a '''mutable''' HashMap mapping references to [[firrtl.ir.DefNode DefNode]]s to their connected
    * [[firrtl.ir.Expression Expression]]s. It is '''not''' mutated in this function
    * @param expr the Expression being transformed
    * @return Returns expr with Boolean Expression inlined
    */
  def onExpr(netlist: Netlist, symbolTable: Map[WrappedExpression, Int])(expr: Expression): Expression = {
    expr.map(onExpr(netlist, symbolTable)) match {
      case dp: DoPrim if (isSimpleExpr(dp)) =>
        dp.map(onArg(netlist, symbolTable))
      case other => other
    }
  }

  /** Inline bits in a Statement
    *
    * @param netlist a '''mutable''' HashMap mapping references to [[firrtl.ir.DefNode DefNode]]s to their connected
    * [[firrtl.ir.Expression Expression]]s. This function '''will''' mutate it if stmt is a [[firrtl.ir.DefNode
    * DefNode]] with a Temporary name and a value that is a [[PrimOp]] Bits
    * @param stmt the Statement being searched for nodes and transformed
    * @return Returns stmt with Bits inlined
    */
  def onStmt(netlist: Netlist, symbolTable: Map[WrappedExpression, Int])(stmt: Statement): Statement = {
//    stmt.map(onStmt(netlist, symbolTable)).map(onExpr(netlist, symbolTable))
    stmt.map(onStmt(netlist, symbolTable)).map{
      println(s"stmt = ${stmt.serialize}")

      onExpr(netlist, symbolTable)
    }


  }
  /** Replaces bits in a Module */
  def onMod(mod: DefModule,
    netlist: Netlist,
    symbolTable: Map[WrappedExpression, Int]): DefModule =
    mod.map(onStmt(netlist, symbolTable))
}

/**
  * 1. Find all references to an LHS of a boolean statement
  * 2. If the LHS reference to the boolean statement is not referenced more than FANOUT_LIMIT or DEPTH
  *    then replace the references to the LHS in other statements with the RHS
  *
  *    {{{wire x = a & b
  *    wire y = x & c
  *    convert to
  *    y = (a & b) & c}}}
  *
  *
  * for each node count the number of usages (the fanout) and if the node is assigned to a boolean expression then if the world node has a fanout count of 1 then replace the node with the RHS expression else do nothing.
  *
  * Original:
  *
  * {{{circuit Top :
  * module Top :
  * input a : UInt<1>
  * input b : UInt<1>
  * input c : UInt<1>
  * output y : UInt<1>
  * node _x = and(a, b)
  * y <= and(_x, c)""".stripMargin
  * // replaces _x with and(a, b)}}}
  *
  * Transformed to:
  *
  * {{{circuit Top :
  * module Top :
  * input a : UInt<1>
  * input b : UInt<1>
  * input c : UInt<1>
  * output y : UInt<1>
  * y <= and(and(a, b), c)""".stripMargin}}}
  *
  * Original:
  *
  * {{{circuit Top :
  * module Top :
  * input a : UInt<1>
  * input b : UInt<1>
  * input c : UInt<1>
  * input d : UInt<1>
  * output y : UInt<1>
  * output z : UInt<1>
  * node _y = and(a, b)
  * node _z = and(a, b)
  * y <= and(_y, c)
  * z <= and(_z, d)
  * // Replacement of _y and _z is performed because the expressions are assigned to two different nodes}}}
  *
  * Transformed to:
  *
  * {{{circuit Top :
  * module Top :
  * input a : UInt<1>
  * input b : UInt<1>
  * input c : UInt<1>
  * input d : UInt<1>
  * output y : UInt<1>
  * output z : UInt<1>
  * node _y = and(a, b)
  * node _z = and(a, b)
  * y <= and(_y, c)
  * z <= and(_z, d)}}}
  *
  * No replacement of _x is performed because _x has a fanout of 2
  *
  * Original:
  *
  * {{{circuit Top :
  * module Top :
  * input a : UInt<1>
  * input b : UInt<1>
  * input c : UInt<1>
  * input d : UInt<1>
  * output y : UInt<1>
  * output z : UInt<1>
  * node _x = and(a, b)
  * y <= and(_x, c)
  * z <= and(_x, d)}}}
  *
  * Transformed to:
  *
  * {{{circuit Top :
  * module Top :
  * input a : UInt<1>
  * input b : UInt<1>
  * input c : UInt<1>
  * input d : UInt<1>
  * output y : UInt<1>
  * output z : UInt<1>
  * node _y = and(a, b)
  * y <= and(_y, c)
  * z <= and(_z, d)}}}
  */
class InlineBooleanExpressions extends Transform {
  def inputForm = LowForm
  def outputForm = LowForm

  import InlineBooleanExpressions._

  def makeModuleInfo(c: Circuit): (Map[String, Map[String, String]], Seq[String], InstanceGraph, Map[String, DefModule]) = {
    val iGraph = (new InstanceGraph(c))
    val childInstances = iGraph.getChildrenInstances.mapValues(_.map(inst => inst.name -> inst.module).toMap).toMap

    // Order from leaf modules to root so that any module driving an output
    // with a constant will be visible to modules that instantiate it
    // TODO Generating order as we execute constant propagation on each module would be faster
    val x = iGraph.moduleOrder.reverse

    val moduleNameTraversalOrder = x.map { m => m.name }

    val modNameModules = x.map { m => (m.name -> m) }.toMap
    (childInstances, moduleNameTraversalOrder, iGraph, modNameModules)
  }

  /** Build a [[Netlist]] from a Module's connections and Nodes
    * This assumes [[firrtl.LowForm LowForm]]
    *
    * @param mod [[firrtl.ir.Module Module]] from which to build a [[Netlist]]
    * @return [[Netlist]] of the module's connections and nodes
    */
  def buildNetlist(mod: DefModule): Netlist = {
    val netlist = new Netlist()
    def onStmt(stmt: Statement): Statement = {
      println(s"stmt = ${stmt.serialize}")

      stmt.map(onStmt) match {
        case Connect(_, lhs, rhs) if (isTmpVar(lhs.serialize)) => netlist(lhs.serialize) = rhs
        case DefNode(_, nname, rhs) if (isTmpVar(nname)) => netlist(nname) = rhs
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

    def onExpr(expr: Expression): Expression =
      expr map onExpr match {
        case w: WRef if (isTmpVar(expr.serialize) && w.flow == SourceFlow) =>
          update(we(expr))
          expr
        case other => expr
      }

    def onStmt(stmt: Statement): Unit = {
      stmt.foreach(onStmt)
      stmt.foreach(onExpr)
    }

    mod.foreach(onStmt)
    symTab.toMap
  }

  def execute(state: CircuitState): CircuitState = {

      val symTabs = state.circuit.modules.map { m => (m.name -> buildSymTab(m)) }.toMap

      val netlists = state.circuit.modules.map { m => (m.name -> buildNetlist(m)) }.toMap

      val modulesx = state.circuit.modules.map {
        m => InlineBooleanExpressions.onMod(m, netlists(m.name), symTabs(m.name))
      }

      state.copy(circuit = state.circuit.copy(modules = modulesx))
    }
  }