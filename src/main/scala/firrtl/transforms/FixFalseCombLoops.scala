package firrtl
package transforms

import firrtl.annotations.NoTargetAnnotation
import firrtl.ir.{IntWidth, UIntType}
import firrtl.options.{HasShellOptions, ShellOption}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** Enables this pass to run
  * Pass --fix-false-comb-loops when running firrtl binary to enable
  */
case object EnableFixFalseCombLoops extends NoTargetAnnotation with HasShellOptions {
  val options = Seq(
    new ShellOption[Unit](
      longOption = "fix-false-comb-loops",
      toAnnotationSeq = _ => Seq(EnableFixFalseCombLoops),
      helpText = "Resolves a subset of word-level combinational loops."
    )
  )
}

/** Transforms a circuit with a combinational loop to a logically equivalent circuit with that loop removed.
  * Resolves a subset of word-level combinational loops which are not combinational loops at the bit-level.
  *
  * Given a circuit and a loop it contains, provided by the output of the CheckCombLoops pass,
  * this pass will transform all variables and logic involved in the combinational loop to bit-level equivalents.
  *
  * This pass is repeatedly called within CheckCombLoops until either no combinational loop remains
  * or the circuit contains a combinational loop which cannot be fixed or is not fixed by this pass.
  *
  * @throws firrtl.transforms.CheckCombLoops.CombLoopException if loop could not be fixed
  * @note Input form: Low FIRRTL and a combinational loop detected in CheckCombLoops
  * @note Output form: Low FIRRTL with provided combinational loop resolved
  * @note This pass does not attempt to resolve loops involving module/instance input or output ports.
  *       As a result, this pass does not resolve intra-module loops or loops through instances.
  */
object FixFalseCombLoops {

  //TODO: Create case object to pass around common parameters to reduce clutter

  /** Transforms a circuit with a combinational loop to a logically equivalent circuit with that loop removed.
    * @param state          the circuit to be transformed
    * @param combLoopsError the error string from the CheckCombLoops pass
    * @return the modified circuit
    */
  def fixFalseCombLoops(state: CircuitState, combLoopsError: String): CircuitState = {
    //Stores the new transformation of the circuit to be returned
    val resultCircuit = ListBuffer[ir.Statement]()
    //Retrieve loops variables from error string
    val moduleToBadVars = parseLoopVariables(combLoopsError)
    if (moduleToBadVars.keys.size > 1) {
      //Multi-module loops are currently not handled
      state
    } else {
      //Recursive call on module
      state.copy(circuit = state.circuit.mapModule(onModule(_, resultCircuit, moduleToBadVars)))
    }
  }

  //Parse error string into list of variables
  private def parseLoopVariables(combLoopsError: String): Map[String, ListBuffer[String]] = {
    var moduleToLoopVars = Map[String, ListBuffer[String]]()

    val split = combLoopsError.split("(\\r\\n|\\r|\\n)").drop(1).dropRight(1)
    split.foreach { x =>
      val moduleName = x.split("\\.")(0)
      val varName = x.split("\\.")(1).replaceAll("\\t", "")
      if (moduleToLoopVars.contains(moduleName)) {
        val updatedVarList: ListBuffer[String] = moduleToLoopVars(moduleName) += varName
        moduleToLoopVars += (moduleName -> updatedVarList)
      } else {
        moduleToLoopVars += (moduleName -> ListBuffer(varName))
      }
    }

    moduleToLoopVars
  }

  /** Stores variables (context) to pass along with functions per module
    */
  private case class ModuleContext(
    resultCircuit: ListBuffer[ir.Statement],
    namespace:     Namespace,
    bitWireNames:  mutable.LinkedHashMap[(String, Int), String],
    combLoopVars:  ListBuffer[String])

  private def onModule(
    m:                ir.DefModule,
    resultCircuit:    ListBuffer[ir.Statement],
    moduleToLoopVars: Map[String, ListBuffer[String]]
  ): ir.DefModule = m match {
    case mod: ir.Module =>
      if (moduleToLoopVars.contains(mod.name)) {
        //Stores memoized namespace for repeated references
        val bitWireNames = mutable.LinkedHashMap[(String, Int), String]()
        val ctx = ModuleContext(resultCircuit, Namespace(mod), bitWireNames, moduleToLoopVars(mod.name))
        //Recursive call per statement
        onStmt(mod.body, ctx)
        //Update circuit to new modified form
        mod.copy(body = ir.Block(resultCircuit.toList))
      } else {
        mod
      }
    case other => other
  }

  private def onStmt(s: ir.Statement, ctx: ModuleContext): Unit = s match {
    case ir.Block(block) =>
      block.foreach(onStmt(_, ctx))

    case node: ir.DefNode =>
      val newNode =
        ir.DefNode(ir.NoInfo, node.name, onExpr(node.value, ctx))
      ctx.resultCircuit += newNode
    //TODO: Figure out why we added this
    //        if (ctx.combLoopVars.contains(node.name)) {
    //          if (getWidth(node.value.tpe) == 1) {
    //            //Removes 1 bit nodes from combLoopVars to avoid unnecessary computation
    //            ctx.combLoopVars -= node.name
    //          }
    //        }
    //        resultCircuit(node.serialize) = node

    case wire: ir.DefWire =>
      //Summary: Splits wire into individual bits (wire x -> wire x_0, ..., wire x_n)
      if (ctx.combLoopVars.contains(wire.name)) {
        val wireWidth = getWidth(wire.tpe)
        if (wireWidth == 1) {
          //Removes 1 bit wires from combLoopVars to avoid unnecessary computation
          ctx.combLoopVars -= wire.name
          ctx.resultCircuit += wire
        } else {
          //Create new wire for every bit in wire
          for (i <- 0 until wireWidth) {
            val bitWire = ir.DefWire(ir.NoInfo, genName(ctx, wire.name, i), Utils.BoolType)
            ctx.resultCircuit += bitWire
          }
          //Creates node wire = cat(a_n # ... # a_0)
          val newNode =
            ir.DefNode(ir.NoInfo, wire.name, convertToCats(ctx, wire.name, wireWidth - 1, 0))
          ctx.resultCircuit += newNode
        }
      } else {
        ctx.resultCircuit += wire
      }

    case connect: ir.Connect =>
      var newConnect = connect
      if (newConnect.expr.isInstanceOf[ir.Expression]) {
        //Summary: Process expr (rhs) of Connect
        val newExpr = onExpr(newConnect.expr, ctx)
        newConnect = ir.Connect(ir.NoInfo, newConnect.loc, newExpr)
      }

      //At this point, it is certain: a -> (an # ... # a0)

      newConnect.loc match {
        case ref: ir.Reference =>
          if (ctx.combLoopVars.contains(ref.name)) {
            bitwiseAssignment(newConnect.expr, ctx, ref.name, getWidth(ref.tpe) - 1, 0)
          } else {
            //If lhs is ir.Reference, but isn't in combLoopVars
            ctx.resultCircuit += newConnect
          }

        //If lhs is not a ir.Reference
        case _ =>
          ctx.resultCircuit += newConnect
      }
    case other =>
      ctx.resultCircuit += other
  }

  private def onExpr(s: ir.Expression, ctx: ModuleContext, high: Int = -1, low: Int = -1): ir.Expression = s match {
    case prim: ir.DoPrim =>
      prim.op match {
        //Summary: Replaces bits(a, i, j) with (aj # ... # ai)
        case PrimOps.Bits =>
          //TODO: what if high and low are already set?
          val leftIndex = prim.consts(0)
          val rightIndex = prim.consts(1)
          val arg = onExpr(prim.args(0), ctx, leftIndex.toInt, rightIndex.toInt)
          val returnWidth = leftIndex - rightIndex + 1
          if (getWidth(arg.tpe) == returnWidth) {
            arg
          } else {
            ir.DoPrim(PrimOps.Bits, Seq(arg), prim.consts, s.tpe)
          }

        case PrimOps.AsUInt =>
          val processedArg = onExpr(prim.args(0), ctx, high, low)
          val newWidth = getWidth(processedArg.tpe)
          ir.DoPrim(PrimOps.AsUInt, Seq(processedArg), prim.consts, UIntType(IntWidth(newWidth)))

        case _ =>
          //Summary: Recursively calls onExpr on each of the arguments to DoPrim
          var newPrimArgs = Seq[ir.Expression]()
          for (arg <- prim.args) {
            arg match {
              case ref: ir.Reference =>
                newPrimArgs = newPrimArgs :+ ref
              case other =>
                newPrimArgs =
                  newPrimArgs :+ onExpr(other, ctx, high, low)
            }
          }
          ir.DoPrim(prim.op, newPrimArgs, prim.consts, prim.tpe)
      }

    case ref: ir.Reference =>
      val name = ref.name
      if (ctx.combLoopVars.contains(name)) {
        //Summary: replaces loop var a with a_high # ... # a_low
        var hi = high
        var lo = low
        if (high == -1) {
          hi = getWidth(ref.tpe) - 1
          lo = 0
        }
        convertToCats(ctx, name, hi, lo)
      } else {
        ref
      }

    case other => other
  }

  //Creates a reference to the bit wire for a given variable
  private def genRef(ctx: ModuleContext, name: String, index: Int): ir.Reference = {
    ir.Reference(genName(ctx, name, index), Utils.BoolType, WireKind, UnknownFlow)
  }

  //Generates the name for a newly created bit wire
  private def genName(ctx: ModuleContext, name: String, index: Int): String = {
    if (ctx.namespace.contains(name + index.toString)) {
      ctx.bitWireNames.getOrElseUpdate((name, index), ctx.namespace.newName(name + index.toString))
    } else {
      name + index.toString
    }
  }

  //Returns width associated with inputted statement
  private def getWidth(stmt: ir.Type): Int = stmt match {
    case uint: ir.UIntType =>
      uint.width.asInstanceOf[ir.IntWidth].width.toInt
    case sint: ir.SIntType =>
      sint.width.asInstanceOf[ir.IntWidth].width.toInt

  }

  //Converts variable (x) into nested cats (x_high # ... # x_low)
  private def convertToCats(ctx: ModuleContext, name: String, high: Int, low: Int): ir.Expression = {
    if (high == low) {
      genRef(ctx, name, low)
    } else {
      ir.DoPrim(
        PrimOps.Cat,
        Seq(genRef(ctx, name, high), convertToCats(ctx, name, high - 1, low)),
        Seq.empty,
        UIntType(IntWidth(high - low + 1))
      )
    }
  }

  private def bitwiseAssignment(
    expr:       ir.Expression,
    ctx:        ModuleContext,
    lhsName:    String,
    leftIndex:  Int,
    rightIndex: Int
  ): Unit = {
    //LHS is wider than right hand side
    if (leftIndex - rightIndex + 1 > getWidth(expr.tpe)) {
      //Assign right bits
      val croppedLeftIndex = rightIndex + getWidth(expr.tpe) - 1
      bitwiseAssignmentRecursion(expr, ctx, croppedLeftIndex, rightIndex)
      //Extend left bits (UInt)
      for (i <- croppedLeftIndex + 1 to leftIndex) {
        val tempConnect = ir.Connect(ir.NoInfo, genRef(ctx, lhsName, i), ir.UIntLiteral(0))
        ctx.resultCircuit += tempConnect
      }
    }

    bitwiseAssignmentRecursion(expr, ctx, leftIndex, rightIndex)

    //Takes in potentially recursive cat, returns
    def bitwiseAssignmentRecursion(expr: ir.Expression, ctx: ModuleContext, leftIndex: Int, rightIndex: Int): Unit = {
      val rhsWidth = getWidth(expr.tpe)

      var r = rightIndex
      expr match {
        case prim: ir.DoPrim => {
          prim.op match {
            case PrimOps.Cat =>
              for (i <- 0 until 2) {
                val argWidth = getWidth(prim.args(i).tpe)
                bitwiseAssignment(prim.args.reverse(i), ctx, lhsName, math.min(leftIndex, r + argWidth - 1), r)
                r += argWidth
              }
            case other => //TODO
              assignBits(prim)
          }
        }
        case ref: ir.Reference =>
          //If arg is a bad var, replace with new vars
          if (ctx.combLoopVars.contains(ref.name)) {
            for (j <- 0 until math.min(rhsWidth, leftIndex - rightIndex + 1)) {
              val tempConnect = ir.Connect(ir.NoInfo, genRef(ctx, lhsName, r), genRef(ctx, ref.name, j))
              ctx.resultCircuit += tempConnect
              r += 1
            }
          } else {
            //If arg is not a bad var, replace with bit prims
            assignBits(ref)
          }
        //TODO: see if other cases exist
        case other =>
          assignBits(other)
      }

      def assignBits(expr: ir.Expression): Unit = {
        if (rhsWidth == 1) {
          val tempConnect = ir.Connect(ir.NoInfo, genRef(ctx, lhsName, rightIndex), expr)
          ctx.resultCircuit += tempConnect
        } else {
          for (j <- 0 until math.min(rhsWidth, leftIndex - rightIndex + 1)) {
            val tempConnect = ir.Connect(
              ir.NoInfo,
              genRef(ctx, lhsName, r),
              ir.DoPrim(PrimOps.Bits, Seq(expr), Seq(j, j), Utils.BoolType)
            )
            ctx.resultCircuit += tempConnect
            r += 1
          }
        }
      }

    }
  }

}
