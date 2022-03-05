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
        //TODO: add flattenCats?
        var (newExpr, modified) = loopVarsToCats(newConnect.expr, ctx)
        //Motivation: don't want to unnecessarily modify expr's without loop vars
        if (modified) {
          newExpr = simplifyBits(newExpr)
        }

        newConnect = ir.Connect(ir.NoInfo, newConnect.loc, newExpr)
      }

      //At this point, it is certain: a -> (an # ... # a0)

      newConnect.loc match {
        case ref: ir.Reference =>
          if (ctx.combLoopVars.contains(ref.name)) {
            val bitMappings =
              bitwiseAssignment(newConnect.expr, ref.name, getWidth(ref.tpe))
            for (key <- bitMappings.keys) {
              bitMappings(key) = simplifyBits(bitMappings(key))
              //TODO: fix param for genref
              ctx.resultCircuit += ir.Connect(ir.NoInfo, genRef(ctx, key, 0), bitMappings(key))
            }
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

  //Replaces loop vars in expr s with equivalent concats (a => an # an-1 # ... # a0). Returns transformed expr
  // & boolean flag of whether anything was modified
  def loopVarsToCats(s: ir.Expression, ctx: ModuleContext): (ir.Expression, Boolean) = s match {
    case ref: ir.Reference =>
      if (ctx.combLoopVars.contains(ref.name)) {
        return (convertToCats(ctx, ref.name, getWidth(ref.tpe) - 1, 0), true)
      }
      (ref, false)

    case prim: ir.DoPrim =>
      val newArgs = Seq[ir.Expression]()
      var modified = false
      for (arg <- prim.args) {
        val (newArg, newModified) = loopVarsToCats(arg, ctx)
        newArgs :+ newArg
        modified = modified || newModified
      }
      (ir.DoPrim(prim.op, newArgs, prim.consts, prim.tpe), modified)

    //TODO: See if other cases exist
    case other => (other, false)
  }

  //TODO: replace this function
  private def onExpr(s: ir.Expression, ctx: ModuleContext, high: Int = -1, low: Int = -1): ir.Expression = {

//    var expr = replaceLoopVars(s)

    return s match {

      // flattencats cat(cat(a, b), cat(c, d)) => (cat(a, cat(b, (cat(c, cat(d)))))
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
                  newPrimArgs = newPrimArgs :+ onExpr(other, ctx, high, low)
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
  }

  //Creates a reference to the bitWire for a given variable
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
  def getWidth(stmt: ir.Type): Int = stmt match {
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

  //TODO: fill in function, reference from onExpr
  def simplifyBits(expr: ir.Expression): ir.Expression = {

    expr

  }

  //Assigns bitwise by wrapping in bits()
  def bitwiseAssignment(
    expr:           ir.Expression,
    name:           String,
    width:          Int
  ): mutable.Map[String, ir.Expression] = {
    val bitwiseMapping = mutable.Map[String, ir.Expression]()

    val widthLimit = math.min(width, getWidth(expr.tpe))
    for (i <- 0 until widthLimit) {
      //TODO: add genName
      bitwiseMapping(name + i.toString) = ir.DoPrim(PrimOps.Bits, Seq(expr), Seq(i, i), Utils.BoolType)
    }

    if (width > widthLimit) {
      for (i <- widthLimit until width) {
        //TODO: add genName
        bitwiseMapping(name + i.toString) = ir.UIntLiteral(0)
      }
    }

    bitwiseMapping
  }

}
