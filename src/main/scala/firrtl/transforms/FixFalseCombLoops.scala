package firrtl
package transforms

import firrtl.annotations.NoTargetAnnotation
import firrtl.ir.{IntWidth, UIntType}
import firrtl.options.{HasShellOptions, ShellOption}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case object EnableFixFalseCombLoops extends NoTargetAnnotation with HasShellOptions {

  val options = Seq(
    new ShellOption[Unit](
      longOption = "fix-false-comb-loops",
      toAnnotationSeq = _ => Seq(EnableFixFalseCombLoops),
      helpText = "Resolves a subset of word-level combinational loops."
    )
  )

}

/**
  * Transforms a circuit with a combinational loop to a logically equivalent circuit with that loop removed.
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

  /**
    * Transforms a circuit with a combinational loop to a logically equivalent circuit with that loop removed.
    *
    * @param state          the circuit to be transformed
    * @param combLoopsError the error string from the CheckCombLoops pass
    * @return the modified circuit
    */
  def fixFalseCombLoops(state: CircuitState, combLoopsError: String): CircuitState = {
    //Stores the new transformation of the circuit to be returned
    //TODO: Make this into only a list of ir.Statement
    val result_circuit = mutable.LinkedHashMap[String, ir.Statement]()
    val moduleToBadVars = parseLoopVariables(combLoopsError)
    if (moduleToBadVars.keys.size > 1) {
      //Multi-module loops are currently not handled.
      return state
    }
    state.copy(circuit = state.circuit.mapModule(onModule(_, result_circuit, moduleToBadVars)))
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

  private def onModule(m: ir.DefModule, result_circuit: mutable.LinkedHashMap[String, ir.Statement], moduleToLoopVars: Map[String, ListBuffer[String]]): ir.DefModule = m match {
    case mod: ir.Module =>
      if (moduleToLoopVars.contains(mod.name)) {
        onStmt(mod.body, result_circuit, moduleToLoopVars(mod.name))
        mod.copy(body = ir.Block(result_circuit.values.toList))
      } else {
        mod
      }
    case other => other
  }


  def onStmt(s: ir.Statement, result_circuit: mutable.LinkedHashMap[String, ir.Statement], combLoopVars: ListBuffer[String]): Unit = s match {
    case ir.Block(block) => block.foreach(onStmt(_, result_circuit, combLoopVars))
    case node: ir.DefNode =>
      val newNode = ir.DefNode(ir.NoInfo, node.name, onExpr(node.value, result_circuit, combLoopVars))
      result_circuit(newNode.serialize) = newNode
    //TODO: Figure out why we added this
    //        if (combLoopVars.contains(node.name)) {
    //          if (getWidth(node.value.tpe) == 1) {
    //            //Removes 1 bit nodes from combLoopVars to avoid unnecessary computation
    //            combLoopVars -= node.name
    //          }
    //        }
    //        result_circuit(node.serialize) = node

    case wire: ir.DefWire =>
      //Summary: Splits wire into individual bits (wire x -> wire x_0, ..., wire x_n)
      if (combLoopVars.contains(wire.name)) {
        val wireWidth = getWidth(wire.tpe)
        if (wireWidth == 1) {
          //Removes 1 bit wires from combLoopVars to avoid unnecessary computation
          combLoopVars -= wire.name
          result_circuit(wire.serialize) = wire
        } else {
          //Create new wire for every bit in wire
          for (i <- 0 until wireWidth) {
            //TODO: Handle case where there is a repeated name. Can't use wire.name + i.toString
            val bitWire = ir.DefWire(ir.NoInfo, wire.name + i.toString, Utils.BoolType)
            result_circuit(bitWire.serialize) = bitWire
          }
          //Creates node wire = cat(a_n # ... # a_0)
          val newNode =
            ir.DefNode(ir.NoInfo, wire.name, convertToCats(wireWidth - 1, 0, wire.name))
          result_circuit(newNode.serialize) = newNode
        }
      } else {
        result_circuit(wire.serialize) = wire
      }

    case connect: ir.Connect =>
      var newConnect = connect
      if (newConnect.expr.isInstanceOf[ir.Expression]) {
        //Summary: Process expr (rhs) of Connect
        val newExpr = onExpr(newConnect.expr, result_circuit, combLoopVars)
        newConnect = ir.Connect(ir.NoInfo, newConnect.loc, newExpr)
      }

      //At this point, it is certain:
      //a -> (an # ... # a0)

      newConnect.loc match {
        case ref: ir.Reference =>
          if (combLoopVars.contains(ref.name)) {
            bitwiseAssignment(newConnect.expr, result_circuit, combLoopVars, ref.name, getWidth(ref.tpe) - 1, 0)
          } else {
            //If lhs is ir.Reference, but isn't in combLoopVars
            result_circuit(newConnect.serialize) = newConnect
          }

        //If lhs is not a ir.Reference
        case _ =>
          result_circuit(newConnect.serialize) = newConnect
      }
    case other =>
      result_circuit(other.serialize) = other
  }

  def onExpr(s: ir.Expression, result_circuit: mutable.LinkedHashMap[String, ir.Statement], combLoopVars: ListBuffer[String], high: Int = -1, low: Int = -1): ir.Expression = s match {
    case prim: ir.DoPrim =>
      prim.op match {
        //Summary: Replaces bits(a, i, j) with (aj # ... # ai)
        case PrimOps.Bits =>
          //TODO: what if high and low are already set?
          val leftIndex = prim.consts(0)
          val rightIndex = prim.consts(1)
          val arg = onExpr(prim.args(0), result_circuit, combLoopVars, leftIndex.toInt, rightIndex.toInt)
          val returnWidth = leftIndex - rightIndex + 1
          if (getWidth(arg.tpe) == returnWidth) {
            arg
          } else {
            ir.DoPrim(PrimOps.Bits, Seq(arg), prim.consts, s.tpe)
          }

        case PrimOps.AsUInt =>
          val processedArg = onExpr(prim.args(0), result_circuit, combLoopVars, high, low)
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
                newPrimArgs = newPrimArgs :+ onExpr(other, result_circuit, combLoopVars, high, low)
            }
          }
          ir.DoPrim(prim.op, newPrimArgs, prim.consts, prim.tpe)
      }

    case ref: ir.Reference =>
      val name = ref.name
      if (combLoopVars.contains(name)) {
        //Summary: replaces loop var a with a_high # ... # a_low
        var hi = high
        var lo = low
        if (high == -1) {
          hi = getWidth(ref.tpe) - 1
          lo = 0
        }
        convertToCats(hi, lo, name)
      } else {
        ref
      }

    case other => other
  }

  //Creates a reference to the bitWire for a given variable
  def genRef(name: String, index: Int): ir.Reference = {
    //TODO: Handle case where there is a repeated name. Can't use name + index.toString
    ir.Reference(name + index.toString, Utils.BoolType, WireKind, UnknownFlow)
  }

  //Returns width associated with inputted statement
  def getWidth(stmt: ir.Type): Int = stmt match {
    case uint: ir.UIntType =>
      uint.width.asInstanceOf[ir.IntWidth].width.toInt
    case sint: ir.SIntType =>
      sint.width.asInstanceOf[ir.IntWidth].width.toInt

  }

  //Converts variable (x) into nested cats (x_high # ... # x_low)
  def convertToCats(high: Int, low: Int, name: String): ir.Expression = {
    if (high == low) {
      genRef(name, low)
    } else {
      ir.DoPrim(
        PrimOps.Cat,
        Seq(genRef(name, high), convertToCats(high - 1, low, name)),
        Seq.empty,
        UIntType(IntWidth(high - low + 1))
      )
    }
  }

  def bitwiseAssignment(expr: ir.Expression, result_circuit: mutable.LinkedHashMap[String, ir.Statement], combLoopVars: ListBuffer[String], lhsName: String, leftIndex: Int, rightIndex: Int): Unit = {
    //LHS is wider than right hand side
    if (leftIndex - rightIndex + 1 > getWidth(expr.tpe)) {
      //Assign right bits
      val croppedLeftIndex = rightIndex + getWidth(expr.tpe) - 1
      bitwiseAssignmentRecursion(expr, combLoopVars, croppedLeftIndex, rightIndex)
      //Extend left bits (UInt)
      for (i <- croppedLeftIndex + 1 to leftIndex) {
        val tempConnect = ir.Connect(ir.NoInfo, genRef(lhsName, i), ir.UIntLiteral(0))
        result_circuit(tempConnect.serialize) = tempConnect
      }
    }

    bitwiseAssignmentRecursion(expr, combLoopVars, leftIndex, rightIndex)

    //Takes in potentially recursive cat, returns
    def bitwiseAssignmentRecursion(expr: ir.Expression, combLoopVars: ListBuffer[String], leftIndex: Int, rightIndex: Int): Unit = {
      val rhsWidth = getWidth(expr.tpe)

      var r = rightIndex
      expr match {
        case prim: ir.DoPrim => {
          prim.op match {
            case PrimOps.Cat =>
              for (i <- 0 until 2) {
                val argWidth = getWidth(prim.args(i).tpe)
                bitwiseAssignment(prim.args.reverse(i), result_circuit, combLoopVars, lhsName, math.min(leftIndex, r + argWidth - 1), r)
                r += argWidth
              }
            case other => //TODO
              assignBits(prim)
          }
        }
        case ref: ir.Reference =>
          //If arg is a bad var, replace with new vars
          if (combLoopVars.contains(ref.name)) {
            for (j <- 0 until math.min(rhsWidth, leftIndex - rightIndex + 1)) {
              val tempConnect = ir.Connect(ir.NoInfo, genRef(lhsName, r), genRef(ref.name, j))
              result_circuit(tempConnect.serialize) = tempConnect
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
          val tempConnect = ir.Connect(ir.NoInfo, genRef(lhsName, rightIndex), expr)
          result_circuit(tempConnect.serialize) = tempConnect
        } else {
          for (j <- 0 until math.min(rhsWidth, leftIndex - rightIndex + 1)) {
            val tempConnect = ir.Connect(
              ir.NoInfo,
              genRef(lhsName, r),
              ir.DoPrim(PrimOps.Bits, Seq(expr), Seq(j, j), Utils.BoolType)
            )
            result_circuit(tempConnect.serialize) = tempConnect
            r += 1
          }
        }
      }

    }
  }



}
