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
    val result_circuit = ListBuffer[ir.Statement]()
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

  private def onModule(
    m:                ir.DefModule,
    result_circuit:   ListBuffer[ir.Statement],
    moduleToLoopVars: Map[String, ListBuffer[String]]
  ): ir.DefModule = m match {
    case mod: ir.Module =>
      if (moduleToLoopVars.contains(mod.name)) {
        onStmt(mod.body, result_circuit, moduleToLoopVars(mod.name))
        mod.copy(body = ir.Block(result_circuit.toList))
      } else {
        mod
      }
    case other => other
  }

  def onStmt(s: ir.Statement, result_circuit: ListBuffer[ir.Statement], combLoopVars: ListBuffer[String]): Unit = {

    s match {
      case ir.Block(block) => block.foreach(onStmt(_, result_circuit, combLoopVars))
      case node: ir.DefNode =>
        val newNode = ir.DefNode(ir.NoInfo, node.name, onExpr(node.value, result_circuit, combLoopVars))
        result_circuit += newNode
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
            result_circuit += wire
          } else {
            //Create new wire for every bit in wire
            for (i <- 0 until wireWidth) {
              //TODO: Handle case where there is a repeated name. Can't use wire.name + i.toString
              val bitWire = ir.DefWire(ir.NoInfo, wire.name + i.toString, Utils.BoolType)
              result_circuit += bitWire
            }
            //Creates node wire = cat(a_n # ... # a_0)
            val newNode =
              ir.DefNode(ir.NoInfo, wire.name, convertToCats(wireWidth - 1, 0, wire.name))
            result_circuit += newNode
          }
        } else {
          result_circuit += wire
        }

      case connect: ir.Connect =>
        var newConnect = connect

        if (newConnect.expr.isInstanceOf[ir.Expression]) {
          //Summary: Process expr (rhs) of Connect
          //TODO: add flattenCats?
          var (newExpr, modified) = loopVarsToCats(newConnect.expr, combLoopVars)
          //Motivation: don't want to unnecessarily modify expr's without loop vars
          if (modified) {
            newExpr = simplifyBits(newExpr)
          }

          newConnect = ir.Connect(ir.NoInfo, newConnect.loc, newExpr)
        }

        //At this point, it is certain:
        //a -> (an # ... # a0)

        newConnect.loc match {
          case ref: ir.Reference =>
            if (combLoopVars.contains(ref.name)) {
              val bitMappings =
                bitwiseAssignment(newConnect.expr, ref.name, getWidth(ref.tpe))
              for (key <- bitMappings.keys) {
                bitMappings(key) = simplifyBits(bitMappings(key))
                //TODO: fix param for genref
                result_circuit += ir.Connect(ir.NoInfo, genRef(key, 0), bitMappings(key))
              }
            } else {
              //If lhs is ir.Reference, but isn't in combLoopVars
              result_circuit += newConnect
            }

          //If lhs is not a ir.Reference
          case _ =>
            result_circuit += newConnect
        }
      case other =>
        result_circuit += other
    }
  }

  //Replaces loop vars in expr s with equivalent concats (a => an # an-1 # ... # a0). Returns transformed expr
  // & boolean flag of whether anything was modified
  def loopVarsToCats(s: ir.Expression, combLoopVars: ListBuffer[String]): (ir.Expression, Boolean) = s match {
    case ref: ir.Reference =>
      if (combLoopVars.contains(ref.name)) {
        return (convertToCats(getWidth(ref.tpe) - 1, 0, ref.name), true)
      }
      (ref, false)

    case prim: ir.DoPrim =>
      val newArgs = Seq[ir.Expression]()
      var modified = false
      for (arg <- prim.args) {
        val (newArg, newModified) = loopVarsToCats(arg, combLoopVars)
        newArgs :+ newArg
        modified = modified || newModified
      }
      (ir.DoPrim(prim.op, newArgs, prim.consts, prim.tpe), modified)

    //TODO: See if other cases exist
    case other => (other, false)
  }

  //TODO: replace this function
  def onExpr(
    s:              ir.Expression,
    result_circuit: ListBuffer[ir.Statement],
    combLoopVars:   ListBuffer[String],
    high:           Int = -1,
    low:            Int = -1
  ): ir.Expression = {

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
