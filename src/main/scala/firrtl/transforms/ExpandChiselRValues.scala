// SPDX-License-Identifier: Apache-2.0

package firrtl.transforms

import firrtl.{
  ir,
  CircuitState,
  DependencyAPIMigration,
  DuplexFlow,
  Flow,
  Namespace,
  RenameMap,
  SinkFlow,
  SourceFlow,
  Transform,
  UnknownFlow,
  Utils,
  WrappedExpression
}
import firrtl.annotations.{
  CircuitTarget,
  ModuleTarget,
  Target
}
import firrtl.options.Dependency
import firrtl.passes.ResolveFlows
import firrtl.stage.Forms
import firrtl.traversals.Foreachers._
import firrtl.Mappers._

import scala.annotation.tailrec
import scala.collection.mutable

/** Expands right-hand-side usages of module outputs, instance inputs, and memory inputs into a synthetic wire. This
  * avoids using sink flow as a source.
  */
object ExpandChiselRValues extends Transform with DependencyAPIMigration {

  override def prerequisites = Seq(
    Dependency(firrtl.passes.RemoveCHIRRTL)
  )
  override def optionalPrerequisites = Seq.empty
  override def optionalPrerequisiteOf = Forms.HighEmitters
  override def invalidates(a: Transform) = false

  implicit class ExpressionHelpers(expr: ir.RefLikeExpression) {

    /** Convert a reference expression to a pruned bundle necessary to represent just that type. */
    def toBundle: ir.BundleType = {

      println(s"'${expr.serialize}'.toBundle")

      def rec(tpe: ir.Type, e: ir.RefLikeExpression): ir.BundleType = e match {
        /* Base case. All ref-likes end in a base reference */
        case ir.Reference(name, _, _, _) =>
          ir.BundleType(Seq(ir.Field(name, ir.Default, tpe)))
        /* Prune the subfield to a bundle */
        case ir.SubField(ex: ir.RefLikeExpression, name, _, _) =>
          rec(ir.BundleType(Seq(ir.Field(name, ir.Default, tpe))), ex)
        /* This could be pruned to a field, but it's complicated to merge and may collide */
        case ir.SubIndex(ex: ir.RefLikeExpression, value, _, _) =>
          // rec(ir.BundleType(Seq(ir.Field(value.toString, ir.Default, tpe))), ex)
          rec(ex.tpe, ex)
        /* This cannot be pruned */
        case ir.SubAccess(ex: ir.RefLikeExpression, _, _, _) =>
          rec(ex.tpe, ex)
      }

      val bundle = rec(expr.tpe, expr)
      println(bundle.serialize)
      bundle
    }

  }

  implicit class BundleTypeHelpers(tpe: ir.BundleType) {

    /** Merge two sorted bundles together. Assume that fields are sorted. */
    def ++(that: ir.BundleType): ir.BundleType = {

      @tailrec
      def rec(a: Seq[ir.Field], b: Seq[ir.Field], acc: Seq[ir.Field]): Seq[ir.Field] = (a, b) match {
        case (a, b) if a.isEmpty                  => acc ++ b
        case (a, b) if b.isEmpty                  => acc ++ a
        case (a, b) if a.head.name < b.head.name  => rec(a.tail, b,      acc :+ a.head)
        case (a, b) if a.head.name > b.head.name  => rec(a,      b.tail, acc :+ b.head)
        case (a, b) if a.head.name == b.head.name => (a.head.tpe, b.head.tpe) match {
          case (at, bt) if at == bt                   => rec(a.tail, b.tail, acc :+ a.head)
          case (at: ir.BundleType, bt: ir.BundleType) => rec(a.tail, b.tail, acc :+ a.head.copy(tpe=(at ++ bt)))
          case _ => ???
        }
      }

      ir.BundleType(rec(tpe.fields, that.fields, Seq.empty))
    }

  }

  /** Determine if an expression is a sink port */
  private def isSink(
    rhs: ir.Expression,
    portMap: Map[String, ir.Orientation]
  ): Boolean = rhs match {
    case _: ir.Literal => false
    case _ => portMap.get(Utils.splitRef(rhs)._1.name) match {
      /* The RHS is not a port. No modification necessary. */
      case None => false
      /* The RHS is a port. This is a sink if it is resolved as a SinkFlow.
       *
       * This uses flow resolution from the ResolveFlows pass, but seeded with the orientation of port.
       */
      case Some(orientation) =>
        (Utils.to_dir(_: ir.Orientation))
          .andThen(Utils.to_flow)
          .andThen(ResolveFlows.resolve_e(_)(rhs))
          .andThen{
            _ match {
              case b: ir.RefLikeExpression => b.flow == SinkFlow
              case _ => ???
            }
          }.apply(orientation)
    }
  }

  private def analyzeStatement(
    statement: ir.Statement,
    portMap: Map[String, ir.Orientation],
    rhsSinks: scala.collection.mutable.Set[ir.RefLikeExpression]
  ): Unit = {
    statement match {
      case a: ir.DefInstance =>
        println(Utils.stmtToType(a))
          ???
      case a@ ir.Connect(_, _, rhs: ir.RefLikeExpression) if isSink(rhs, portMap)  =>
        rhsSinks += rhs
      case a@ ir.PartialConnect(_, _, rhs: ir.RefLikeExpression) if isSink(rhs, portMap) =>
        rhsSinks += rhs
      case a =>
        a.foreach(analyzeStatement(_, portMap, rhsSinks))
    }
  }

  private def onExpression(
    expression: ir.Expression,
    exps: Seq[WrappedExpression]
  ): ir.Expression = {
    expression match {
      case a if {
        println(s"onExpression: '${a.serialize}'")
        println(s"  - $a")
        false
      } => ???
      case a: ir.Reference =>
        val synthetic = Utils.mergeRef(ir.Reference("_synthetic_output"), a)
        exps.contains(WrappedExpression(synthetic)) match {
          case true => synthetic
          case false => a
        }
      case _ => expression.map(onExpression(_, exps))
    }
  }

  private def onStatement(
    statement: ir.Statement,
    syntheticWire: ir.DefWire,
    exps: Seq[WrappedExpression]
  ): ir.Statement = {
    val syntheticWireType = syntheticWire.tpe match {
      case a: ir.BundleType => a
      case _ => ???
    }
    statement.map(onStatement(_, syntheticWire, exps)) match {
      case a if {
        println(s"Examining: '${a.serialize}'")
        println(s"  - $a")
        false
      } => ???
      /* Connect the synthetic wire to the RHS */
      case a@ ir.Connect(info, lhs: ir.RefLikeExpression, rhs) =>
        lazy val rhsExps = Utils.create_exps(rhs)
        lazy val refs = Utils.expandRef(ir.Reference(syntheticWire)).map(WrappedExpression(_))
        ir.Block (
          a.copy(expr = onExpression(rhs, refs)) +:
            Utils.get_valid_points(syntheticWireType, lhs.toBundle, ir.Default, ir.Default)
            .map { case (i, j) => (exps(i), rhsExps(j)) }
            .map { case (x, y) => ir.Connect(info, x.e1, y) }
        )
      case a@ ir.PartialConnect(info, lhs: ir.RefLikeExpression, rhs) =>
        lazy val rhsExps = Utils.create_exps(rhs)
        lazy val refs = Utils.expandRef(ir.Reference(syntheticWire)).map(WrappedExpression(_))
        ir.Block (
          a.copy(expr = onExpression(rhs, refs)) +:
            Utils.get_valid_points(syntheticWireType, lhs.toBundle, ir.Default, ir.Default)
            .map { case (i, _) => (exps(i), rhsExps(i)) }
            .map { case (x, y) => ir.Connect(info, x.e1, y) }
        )
      case a@ ir.IsInvalid(info, lhs: ir.RefLikeExpression) =>
        ir.Block (
          a +: Utils.get_valid_points(syntheticWireType, lhs.toBundle, ir.Default, ir.Default)
            .map{ case (i, _) => exps(i) }
            .map( b =>  ir.IsInvalid(info, b.e1) )
        )
      /* Replace connections to original wires */
      case a@ ir.Connect(_, _, rhs) if ??? =>
        ???
      case a@ ir.PartialConnect(_, _, rhs) if ??? =>
        ???
      /* Skip other statements */
      case a =>
        println(s"  Skipping '${a.serialize}'")
        a
    }
  }

  private def onModule(module: ir.DefModule, circuitTarget: CircuitTarget): ir.DefModule = module match {
    case a: ir.ExtModule => a
    case a: ir.Module =>
      val portMap: Map[String, ir.Orientation] =
        Utils
          .module_type(module)
          .fields
          .map { case ir.Field(name, flip, _) => name -> flip }
          .toMap
      val rhsSinks = mutable.LinkedHashSet.empty[ir.RefLikeExpression]
      a.body.foreach(analyzeStatement(_, portMap, rhsSinks))
      rhsSinks.map(_.serialize).foreach(println)
      println("----------------------------------------")
      rhsSinks.foreach(println)
      println("----------------------------------------")
      /* This is the synthetic wire that we will assign everything to. */
      val synthetic: Option[ir.BundleType] = rhsSinks
        .map(_.toBundle) match {
          case b if b.isEmpty => None
          case b              => Some(b.reduce(_ ++ _)) // This is an insertion sort...
        }
      println("----------------------------------------")
      synthetic match {
        case None => module
        case Some(tpe) =>
          val syntheticWire = ir.DefWire(ir.NoInfo, Namespace(a).newName("_synthetic_output"), tpe)
          println("Synthetic wire: ")
          println(s"  - ${syntheticWire.serialize}")
          println(s"  - $syntheticWire")
          val renames = mutable.HashMap.empty[WrappedExpression, ir.Expression]
          val moduleTarget = circuitTarget.module(module.name)
          val exps = Utils.create_exps(ir.Reference(syntheticWire)).map(WrappedExpression(_))
          println(exps.mkString("Exprssions:\n  - ", "\n  - ", ""))
          val bodyx = a.body.map(onStatement(_, syntheticWire, exps)) match {
            case ir.Block(stmts) => ir.Block(syntheticWire +: stmts)
            case stmt => ir.Block(Seq(syntheticWire, stmt))
          }
          println(renames)
          a.copy(body = bodyx)
      }
  }

  override def execute(state: CircuitState) = {
    val circuitTarget = CircuitTarget(state.circuit.main)
    val circuitx = state.circuit.map(onModule(_, circuitTarget))
    state.copy(circuit = circuitx)
  }

}
