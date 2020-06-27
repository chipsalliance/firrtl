// See LICENSE for license details.

package firrtl
package transforms

import firrtl.ir._
import firrtl.Mappers._
import firrtl.Utils._
import firrtl.options.Dependency

import scala.collection.mutable

object FlattenRegUpdate {

  /** Mapping from references to the [[firrtl.ir.Expression Expression]]s that drive them */
  type Netlist = mutable.HashMap[WrappedExpression, Expression]

  /** Build a [[Netlist]] from a Module's connections and Nodes
    *
    * This assumes [[firrtl.LowForm LowForm]]
    *
    * @param mod [[firrtl.ir.Module Module]] from which to build a [[Netlist]]
    * @return [[Netlist]] of the module's connections and nodes
    */
  def buildNetlist(mod: Module): Netlist = {
    val netlist = new Netlist()
    def onStmt(stmt: Statement): Statement = {
      stmt.map(onStmt) match {
        case Connect(_, lhs, rhs) =>
          netlist(lhs) = rhs
        case DefNode(_, nname, rhs) =>
          netlist(WRef(nname)) = rhs
        case _: IsInvalid => throwInternalError("Unexpected IsInvalid, should have been removed by now")
        case _ => // Do nothing
      }
      stmt
    }
    mod.map(onStmt)
    netlist
  }

  /** Flatten Register Updates
    *
    * Constructs nested mux trees (up to a certain arbitrary threshold) for register updates. This
    * can result in dead code that this function does NOT remove.
    *
    * @param mod [[firrtl.ir.Module Module]] to transform
    * @return [[firrtl.ir.Module Module]] with register updates flattened
    */
  def flattenReg(mod: Module): Module = {
    val regUpdates = mutable.ArrayBuffer.empty[Connect]
    val netlist = buildNetlist(mod)

    // We want to flatten Mux trees for reg updates into if-trees for improved
    // QoR for conditional updates. However, when-otherwise structure often
    // leads to defaults that are repeated all over and result in unreachable
    // branches. Before performing the inline, we determine which references
    // show up on multiple paths and mark those as endpoints where we stop
    // inlining
    val maxDepth = 4 // max depth of inlining
    def determineEndpoints(expr: Expression): collection.Set[WrappedExpression] = {
      val seen = mutable.HashSet.empty[WrappedExpression]
      val endpoint = mutable.HashSet.empty[WrappedExpression]
      def rec(depth: Int)(e: Expression): Unit = {
        val ex = kind(e) match {
          case NodeKind | WireKind if depth < maxDepth && !seen(e) =>
            seen += e
            netlist.getOrElse(e, e)
          case _ => e
        }
        ex match {
          case Mux(_, tval, fval, _) =>
            rec(depth + 1)(tval)
            rec(depth + 1)(fval)
          case _ =>
            // Mark e not ex because original reference is the endpoint, not op or whatever
            endpoint += ex
        }
      }
      rec(0)(expr)
      endpoint
    }

    def constructRegUpdate(expr: Expression): Expression = {
      val endpoints = determineEndpoints(expr)
      def rec(e: Expression): Expression = {
        val ex = kind(e) match {
          case NodeKind | WireKind if !endpoints(e)=> netlist.getOrElse(e, e)
          case _ => e
        }
        ex match {
          case Mux(cond, tval, fval, tpe) => Mux(cond, rec(tval), rec(fval), tpe)
          case _  => ex
        }
      }
      rec(expr)
    }

    def onStmt(stmt: Statement): Statement = stmt.map(onStmt) match {
      case reg @ DefRegister(_, rname, _,_, resetCond, _) =>
        assert(resetCond.tpe == AsyncResetType || resetCond == Utils.zero,
          "Synchronous reset should have already been made explicit!")
        val ref = WRef(reg)
        val update = Connect(NoInfo, ref, constructRegUpdate(netlist.getOrElse(ref, ref)))
        regUpdates += update
        reg
      // Remove connections to Registers so we preserve LowFirrtl single-connection semantics
      case Connect(_, lhs, _) if kind(lhs) == RegKind => EmptyStmt
      case other => other
    }

    val bodyx = onStmt(mod.body)
    mod.copy(body = Block(bodyx +: regUpdates))
  }

}

/** Flatten register update
  *
  * This transform flattens register updates into a single expression on the rhs of connection to
  * the register
  */
// TODO Preserve source locators
class FlattenRegUpdate extends Transform with DependencyAPIMigration {

  override def prerequisites = firrtl.stage.Forms.LowFormMinimumOptimized ++
    Seq( Dependency[BlackBoxSourceHelper],
         Dependency[FixAddingNegativeLiterals],
         Dependency[ReplaceTruncatingArithmetic],
         Dependency[InlineBitExtractionsTransform],
         Dependency[InlineCastsTransform],
         Dependency[LegalizeClocksTransform] )

  override def optionalPrerequisites = firrtl.stage.Forms.LowFormOptimized

  override def optionalPrerequisiteOf = Seq.empty

  override def invalidates(a: Transform): Boolean = a match {
    case _: DeadCodeElimination => true
    case _ => false
  }

  def execute(state: CircuitState): CircuitState = {
    val modulesx = state.circuit.modules.map {
      case mod: Module => FlattenRegUpdate.flattenReg(mod)
      case ext: ExtModule => ext
    }
    state.copy(circuit = state.circuit.copy(modules = modulesx))
  }
}
