// SPDX-License-Identifier: Apache-2.0

package firrtl.transforms

import firrtl.{
  ir,
  CircuitState,
  DependencyAPIMigration,
  DuplexFlow,
  EmptyExpression,
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
import firrtl.analyses.InstanceKeyGraph
import firrtl.annotations.{
  CircuitTarget,
  ModuleTarget,
  ReferenceTarget,
  Target,
  TargetToken
}
import firrtl.ir.UnifiedTypes
import firrtl.ir.UnifiedTypes.TypeHelpers
import firrtl.options.Dependency
import firrtl.passes.ResolveFlows
import firrtl.stage.Forms
import firrtl.traversals.Foreachers._
import firrtl.Mappers._

import scala.annotation.tailrec
import scala.collection.{
  immutable,
  mutable
}

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

      // println(s"'${expr.serialize}'.toBundle")

      def rec(tpe: ir.Type, e: ir.RefLikeExpression): ir.BundleType = e match {
        /* Base case. All ref-likes end in a base reference */
        case ir.Reference(name, _, _, _) =>
          ir.BundleType(Seq(ir.Field(name, ir.Default, tpe)))
        /* Prune the subfield to a bundle */
        case ir.SubField(ex: ir.RefLikeExpression, name, _, _) =>
          rec(ir.BundleType(Seq(ir.Field(name, ir.Default, tpe))), ex)
        /* This could be pruned to a field, but it's complicated to merge and may collide */
        case ir.SubIndex(ex: ir.RefLikeExpression, value, _, _) =>
          rec(ex.tpe, ex)
        /* This cannot be pruned */
        case ir.SubAccess(ex: ir.RefLikeExpression, _, _, _) =>
          rec(ex.tpe, ex)
      }

      val bundle = rec(expr.tpe, expr)
      // println(bundle.serialize)
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

  /** Determine the flow of an expression given auxiliary type information. Return None if the type does not match the
    * expression, e.g., the expression includes a subfield not in the type.
    */
  private def toFlow(expression: ir.Expression, bundle: UnifiedTypes.Type): Option[Flow] = {

    def rec(e: ir.Expression, b: UnifiedTypes.Type): Option[(ir.Orientation, UnifiedTypes.Type)] = {

      e match {
        /* Base case: All expressions end with a reference. Return the root orientation and strip the outermost type
         * from the unified type. If the type doesn't match, return None.
         */
        case ir.Reference(name, _, _, _)  => b match {
          case UnifiedTypes.Flip(UnifiedTypes.Bundle(elements)) => elements.get(name) match {
            case Some(x) => Some((ir.Flip, elements(name)))
            case None    => None
          }
          case UnifiedTypes.Bundle(elements) => elements.get(name) match {
            case Some(x) => Some((ir.Default, elements(name)))
            case None    => None
          }
          case _ => None
        }
        /* Post order recursion across subfields. The current orientation and type is returned. Use this to update the
         * type and strip the outermost unified type. If not a bundle type, return None.
         */
        case ir.SubField(expr, name, _, _) => rec(expr, b) match {
          case None => None
          case Some((orientation, bx)) => bx match {
            case UnifiedTypes.Flip(UnifiedTypes.Bundle(elements)) => elements.get(name) match {
              case Some(x) => Some((Utils.swap(orientation), elements(name)))
              case None    => None
            }
            case UnifiedTypes.Bundle(elements) =>
              elements.get(name) match {
              case Some(x) => Some(((orientation), elements(name)))
              case None    => None
            }
            case _ => None
          }
        }
        /* Skip over index and access as they don't affect the type. */
        case a: ir.SubIndex  => rec(a.expr, b)
        case a: ir.SubAccess => rec(a.expr, b)
        /* Ignore EmptyExpression (if found) and literals. */
        case EmptyExpression => None
        case a: ir.Literal   => None
        /* Anything else shouldn't be possible */
        case _ =>
          println(e.serialize)
          ???
      }

    }

    rec(expression, bundle).map {
      /* Unwrap a swap on the returned type */
      case (orientation, UnifiedTypes.Flip(tpe)) => (Utils.swap(orientation), tpe)
      case other                                 => other
    }.map {
      case (_,          b) if !b.passive => DuplexFlow
      case (ir.Default, _)               => SinkFlow
      case (ir.Flip,    _)               => SourceFlow
    }

  }

  private def isPort(a: UnifiedTypes.Type, b: String): Boolean = a match {
    case UnifiedTypes.Flip(UnifiedTypes.Bundle(elements)) => elements.contains(b)
    case UnifiedTypes.Bundle(elements)                    => elements.contains(b)
    case _                                                => false
  }

  /** Determine if an expression is a sink. Returns None if this is a non-sink instance reference indicating that this
    * should not be recursively explored. Returns Some if this is a reference that is or is not a sink, but may contain
    * sub-expressions that should be explored.
    */
  private def isSink(
    rhs: ir.Expression,
    portMap: UnifiedTypes.Type,
    modulePortMap: scala.collection.Map[String, UnifiedTypes.Type],
    instanceMap: scala.collection.Map[String, String],
  ): Option[Boolean] = {

    rhs match {
      case _: ir.Literal => Some(false)
      case _ =>
        val (car, cdr) = Utils.splitRef(rhs)

        car match {
          /* This is a reference to a module port */
          case _ if isPort(portMap, car.name) =>
            toFlow(rhs, portMap).map(_ == SinkFlow)
          /* This is a reference to an instance port. The logic is the same, but the comparison is against SourceFlow.
           */
          case _ if instanceMap.contains(car.name) =>
            toFlow(cdr, modulePortMap(instanceMap(car.name))).map(_ == SourceFlow)
          case _ =>
            Some(false)
        }
    }
  }

  private def analyzeExpression(
    expression: ir.Expression,
    portMap: UnifiedTypes.Type,
    modulePortMap: scala.collection.Map[String, UnifiedTypes.Type],
    instanceMap: scala.collection.Map[String, String],
    rhsSinks: mutable.Set[ir.RefLikeExpression],
  ): Unit = {
    println(s"    analyzeExpression: ${expression.serialize}")
    expression match {
      case e: ir.SubAccess => isSink(e, portMap, modulePortMap, instanceMap) match {
        case None        =>
          println("      - None")
          e.foreach(analyzeExpression(_, portMap, modulePortMap, instanceMap, rhsSinks))
        case Some(true)  =>
          println("      - Some(true)")
          e.foreach(analyzeExpression(_, portMap, modulePortMap, instanceMap, rhsSinks))
          rhsSinks += e
        case Some(false) =>
          println("      - Some(false)")
          e.foreach(analyzeExpression(_, portMap, modulePortMap, instanceMap, rhsSinks))
      }
      case e: ir.RefLikeExpression => isSink(e, portMap, modulePortMap, instanceMap) match {
        case None        =>
          println("      - None")
          e
        case Some(true)  =>
          println("      - Some(true)")
          rhsSinks += e
        case Some(false) =>
          println("      - Some(false)")
          e.foreach(analyzeExpression(_, portMap, modulePortMap, instanceMap, rhsSinks))
      }
      case e => e.foreach(analyzeExpression(_, portMap, modulePortMap, instanceMap, rhsSinks))
    }

  }

  private def analyzeStatement(
    statement: ir.Statement,
    portMap: UnifiedTypes.Type,
    rhsSinks: mutable.Set[ir.RefLikeExpression],
    modulePortMap: scala.collection.Map[String, UnifiedTypes.Type],
    instanceMap: mutable.Map[String, String],
  ): Unit = {
    println(s"  analyzeStatement: ${statement.serialize}")
    statement match {
      case a@ ir.DefInstance(_, instance, module, tpe) =>
        println(s"    - instanceMap append '$instance -> $module'")
        instanceMap(instance) = module
      /* Find and analyze any RHS connections where the LHS and RHS are both sinks.
       *
       * For example, this would analyze the RHS "b" in "a <= b" below:
       *
       * circuit Baz:
       *   module Baz:
       *     output a: { a: UInt<1> }
       *     output b: { a: UInt<1> }
       *     b is invalid
       *     a <= b
       *
       * Note that the same circuit with types and connections inverted is skipped. In the circuit below "b" will not be
       * analyzed as this circuit is disallowed by CheckFlows.
       *
       * circuit Qux:
       *   module Qux:
       *     input a: { flip a: UInt<1> }
       *     input b: { flip a: UInt<1> }
       *     b is invalid
       *     b <= a
       *
       */
      case ir.Connect(_, lhs, rhs) =>
        /* @todo This needs to compute the flow differently for different kinds. This should look at the modulePortMap for
         * instances (and flip the output). This can look at the type of anything else (wire, register, or node) as that
         * type should be correct.
         */
        val lhsFlow = toFlow(lhs, portMap)
        val rhsFlow = toFlow(rhs, portMap)
        println(s"   lhs: ${lhs.serialize}")
        println(s"     raw: $lhs")
        println(s"     flow: $lhsFlow")
        println(s"   rhs: ${rhs.serialize}")
        println(s"     raw: $rhs")
        println(s"     flow: $rhsFlow")

        val xhs = (lhsFlow, rhsFlow) match {
          case (Some(SinkFlow),   Some(SinkFlow))   => Some(rhs)
          case (Some(DuplexFlow), Some(SinkFlow))   => None
          case (Some(SourceFlow), Some(SourceFlow)) => None
          case _                                    => None
        }
        xhs.foreach(analyzeExpression(_, portMap, modulePortMap, instanceMap, rhsSinks))
        Seq(lhs, rhs).foreach {
          case a: ir.SubAccess => analyzeExpression(a.index, portMap, modulePortMap, instanceMap, rhsSinks)
          case _ =>
        }
      /* Partial connections are the same as connect, except the RHS also should be analyzed when the LHS is duplex.
       *
       * For example, this would analyze "b" in "a <- b":
       *
       * circuit Foo:
       *   module Foo:
       *     output a: { a: UInt<1>, flip b: UInt<1> }
       *     output b: { a: UInt<1> }
       *     b is invalid
       *     a <- b
       *
       */
      case ir.PartialConnect(_, lhs, rhs) =>
        val lhsFlow = toFlow(lhs, portMap)
        val rhsFlow = toFlow(rhs, portMap)
        println(s"   lhs: ${lhs.serialize}")
        println(s"     flow: $lhsFlow")
        println(s"   rhs: ${rhs.serialize}")
        println(s"     flow: $rhsFlow")

        val xhs = (lhsFlow, rhsFlow) match {
          case (Some(SinkFlow),   Some(SinkFlow))   => Some(rhs)
          case (Some(DuplexFlow), Some(SinkFlow))   => Some(rhs)
          case (Some(SourceFlow), Some(SourceFlow)) => None
          case _                                    => None
        }
        xhs.foreach(analyzeExpression(_, portMap, modulePortMap, instanceMap, rhsSinks))
        Seq(lhs, rhs).foreach {
          case a: ir.SubAccess => analyzeExpression(a.index, portMap, modulePortMap, instanceMap, rhsSinks)
          case _ =>
        }
      /* Ignore invalid. This is a LHS usage of an expression. */
      case _: ir.IsInvalid =>
      /* Ignore attach. This is weird. */
      case _: ir.Attach =>
      /* Process other statements according to normal foreaching. */
      case a =>
        // println("    - LHS and RHS recursion")
        a.foreach(analyzeStatement(_, portMap, rhsSinks, modulePortMap, instanceMap))
        a.foreach(analyzeExpression(_, portMap, modulePortMap, instanceMap, rhsSinks))
    }
  }

  private def onExpression(
    expression: ir.Expression,
    exps: Seq[WrappedExpression]
  ): ir.Expression = {
    expression match {
      case a if {
        // println(s"onExpression: '${a.serialize}'")
        // println(s"  - $a")
        false
      } => ???
      /* SubAccess needs to be processed recursively */
      case a: ir.SubAccess =>
        expression.map(onExpression(_, exps))
      /* Other reference-like expressions must match exactly. Do not recurse (into simpler statements). */
      case a: ir.RefLikeExpression =>
        val synthetic = Utils.mergeRef(ir.Reference("_synthetic_output"), a)
        exps.contains(WrappedExpression(synthetic)) match {
          case true => synthetic
          case false => a
        }
      /* Everything else should be handled recursively */
      case e => expression.map(onExpression(_, exps))
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
        // println(s"Examining: '${a.serialize}'")
        // println(s"  - $a")
        false
      } => ???
      /* Handle connections. Replace RHS usages with the synthetic wire. Add statements that replicate assignments as
       * assignments to the synthetic wire.
       */
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
      case a => a.mapExpr{ rhs =>
        lazy val refs = Utils.expandRef(ir.Reference(syntheticWire)).map(WrappedExpression(_))
        onExpression(rhs, refs)
      }
    }
  }

  private def onModule(
    module: ir.DefModule,
    modulePortMap: mutable.Map[String, UnifiedTypes.Type]
  ): ir.DefModule = {

    println(s"onModule: ${module.name}")

    println(s"  - module_type: ${Utils.module_type(module).serialize}")
    println(s"  - unified: ${Utils.module_type(module).asUnified}")

    modulePortMap(module.name) = Utils.module_type(module).asUnified
    val portMap = modulePortMap(module.name)

    module match {
      case a: ir.ExtModule => a
      case a: ir.Module =>
        val instanceMap = mutable.HashMap.empty[String, String]
        val rhsSinks = mutable.LinkedHashSet.empty[ir.RefLikeExpression]
        a.body.foreach(analyzeStatement(_, portMap, rhsSinks, modulePortMap, instanceMap))
        // rhsSinks.map(_.serialize).foreach(println)
        println("----------------------------------------")
        // rhsSinks.foreach(println)
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
            // println(s"  - $syntheticWire")
            val renames = mutable.HashMap.empty[WrappedExpression, ir.Expression]
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
  }

  override def execute(state: CircuitState) = {
    val modulePortMap = mutable.HashMap.empty[String, UnifiedTypes.Type]

    /* Update all modules in reverse topological order. This ensures that modules are declared before instantiation. */
    val modulesx: immutable.Map[String, ir.DefModule] =
      InstanceKeyGraph(state.circuit)
        .moduleOrder
        .reverse
        .map(onModule(_, modulePortMap))
        .groupBy(_.name)
        .mapValues(_.head)

    val circuitx = state.circuit.map((module: ir.DefModule) => modulesx(module.name))

    state.copy(circuit = circuitx)
  }

}
