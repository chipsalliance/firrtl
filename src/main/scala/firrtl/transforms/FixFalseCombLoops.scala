package firrtl
package transforms

import firrtl.annotations.NoTargetAnnotation
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
      var (newExpr, modified) = loopVarsToCats(node.value, ctx)
      if (modified) {
        newExpr = simplifyExpr(newExpr)
      }
      val newNode = ir.DefNode(ir.NoInfo, node.name, newExpr)
      if (ctx.combLoopVars.contains(node.name)) {
        if (getWidth(node.value.tpe) == 1) {
          //Removes 1 bit nodes from combLoopVars to avoid unnecessary computation
          ctx.combLoopVars -= node.name
        }
      }
      ctx.resultCircuit += newNode

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
        var (newExpr, modified) = loopVarsToCats(newConnect.expr, ctx)
        //Motivation: don't want to unnecessarily modify expr's without loop vars
        if (modified) {
          newExpr = simplifyExpr(newExpr)
        }

        newConnect = ir.Connect(ir.NoInfo, newConnect.loc, newExpr)
      }

      //At this point, it is certain: a -> (an # ... # a0)

      //Check if lhs is a loop var
      newConnect.loc match {
        case ref: ir.Reference =>
          if (ctx.combLoopVars.contains(ref.name)) {
            val bitMappings =
              bitwiseAssignment(ctx, newConnect.expr, ref.name, getWidth(ref.tpe))
            for (key <- bitMappings.keys) {
              bitMappings(key) = simplifyExpr(bitMappings(key))
              ctx.resultCircuit += ir.Connect(ir.NoInfo, key, bitMappings(key))
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

  //Replaces loop vars in expr s with equivalent concats (a => an # an-1 # ... # a0).
  // Returns transformed expr and boolean flag of whether anything was modified
  private def loopVarsToCats(s: ir.Expression, ctx: ModuleContext): (ir.Expression, Boolean) = s match {
    case ref: ir.Reference =>
      if (ctx.combLoopVars.contains(ref.name)) {
        (convertToCats(ctx, ref.name, getWidth(ref.tpe) - 1, 0), true)
      } else {
        (ref, false)
      }

    case prim: ir.DoPrim =>
      var newArgs = Seq[ir.Expression]()
      var modified = false
      for (arg <- prim.args) {
        val (newArg, newModified) = loopVarsToCats(arg, ctx)
        newArgs = newArgs :+ newArg
        modified = modified || newModified
      }
      (ir.DoPrim(prim.op, newArgs, prim.consts, prim.tpe), modified)

    //TODO: See if other cases exist
    case other => (other, false)
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
        ir.UIntType(ir.IntWidth(high - low + 1))
      )
    }
  }

  //Source: https://github.com/ucb-bar/maltese-smt/blob/main/src/maltese/smt/SMTSimplifier.scala
  private def simplifyExpr(expr: ir.Expression): ir.Expression = expr match {
    case prim: ir.DoPrim =>
      prim.op match {
        case PrimOps.Bits =>
          simplifyBits(prim)
        case PrimOps.Cat =>
          simplifyCat(prim)
        case _ =>
          var newArgs = Seq[ir.Expression]()
          for (arg <- prim.args) {
            newArgs = newArgs :+ simplifyExpr(arg)
          }
          ir.DoPrim(prim.op, newArgs, prim.consts, prim.tpe)
      }
    case _ => expr
  }

  //Desired input: some expression
  //Desired output: equivalent expression with as many unnecessary variables removed

  private def simplifyBits(expr: ir.DoPrim): ir.Expression = {
    val high = expr.consts.head
    val low = expr.consts(1)

    expr.args.head match {
      case noop: ir.Expression if low == 0 && high == getWidth(noop.tpe) - 1 => noop

      case prim: ir.DoPrim =>
        prim.op match {
          case PrimOps.Bits =>
            //Handles bits(bits()
            val innerLow = prim.consts(1)
            simplifyExpr(combineBits(prim.args.head, innerLow, high, low))

          case PrimOps.Cat =>
            simplifyExpr(pushDownBits(prim.args.head, prim.args(1), high, low))

          case PrimOps.AsUInt =>
            //TODO
            simplifyExpr(createBits(prim.args.head, high, low))

          case PrimOps.AsSInt =>
            //TODO Sign Extend if necessary
            simplifyExpr(createBits(prim.args.head, high, low))

          case _ =>
            createBits(simplifyExpr(expr.args.head), high, low)
        }
      case _ => expr
    }
  }

  private def simplifyCat(expr: ir.DoPrim): ir.Expression = {
    expr.args.head match {
      case prim1: ir.DoPrim if expr.args(1).isInstanceOf[ir.DoPrim] =>
        val prim2 = expr.args(1).asInstanceOf[ir.DoPrim]
        if (prim1.op == PrimOps.Bits && prim2.op == PrimOps.Bits) {
          val hi1 = prim1.consts.head
          val lo1 = prim1.consts(1)
          val hi2 = prim2.consts.head
          val lo2 = prim2.consts(1)
          val e1 = prim1.args.head
          val e2 = prim2.args.head
          if (lo1 == hi2 + 1 && e1.serialize == e2.serialize) {
            simplifyBits(createBits(e1, hi1, lo2))
          } else {
            createCat(simplifyExpr(prim1), simplifyExpr(prim2))
          }
        } else {
          createCat(simplifyExpr(expr.args.head), simplifyExpr(expr.args(1)))
        }
      case _ =>
        createCat(simplifyExpr(expr.args.head), simplifyExpr(expr.args(1)))
    }
  }

  private def createBits(expr: ir.Expression, high: BigInt, low: BigInt): ir.DoPrim = {
    ir.DoPrim(PrimOps.Bits, Seq(expr), Seq(high, low), ir.UIntType(ir.IntWidth(high - low + 1)))
  }

  private def createCat(msb: ir.Expression, lsb: ir.Expression): ir.DoPrim = {
    ir.DoPrim(PrimOps.Cat, Seq(msb, lsb), Seq(), ir.UIntType(ir.IntWidth(getWidth(msb.tpe) + getWidth(lsb.tpe))))
  }

  //Simplifies nested bits: (x[a:b])[c:d] -> x[c+b:d+b]
  private def combineBits(expr: ir.Expression, innerLow: BigInt, high: BigInt, low: BigInt): ir.DoPrim = {
    val combinedLow = low + innerLow
    val combinedHigh = high + innerLow
    createBits(expr, combinedHigh, combinedLow)
  }

  private def pushDownBits(msb: ir.Expression, lsb: ir.Expression, high: BigInt, low: BigInt): ir.Expression = {
    if (getWidth(lsb.tpe) > high) { createBits(lsb, high, low) }
    else if (low >= getWidth(lsb.tpe)) { createBits(msb, high - getWidth(lsb.tpe), low - getWidth(lsb.tpe)) }
    else {
      createCat(createBits(msb, high - getWidth(lsb.tpe), 0), createBits(lsb, getWidth(lsb.tpe) - 1, low))
    }
  }

  //Assigns bitwise by wrapping in bits()
  private def bitwiseAssignment(
    ctx:   ModuleContext,
    expr:  ir.Expression,
    name:  String,
    width: Int
  ): mutable.Map[ir.Reference, ir.Expression] = {
    val bitwiseMapping = mutable.Map[ir.Reference, ir.Expression]()

    val widthLimit = math.min(width, getWidth(expr.tpe))
    for (i <- 0 until widthLimit) {
      bitwiseMapping(genRef(ctx, name, i)) = ir.DoPrim(PrimOps.Bits, Seq(expr), Seq(i, i), Utils.BoolType)
    }

    if (width > widthLimit) {
      val extendBits = expr.tpe match {
        case _: ir.UIntType =>
          ir.UIntLiteral(0)
        case _: ir.SIntType =>
          bitwiseMapping(genRef(ctx, name, widthLimit - 1))
      }

      for (i <- widthLimit until width) {
        bitwiseMapping(genRef(ctx, name, i)) = extendBits
      }
    }

    bitwiseMapping
  }

}
