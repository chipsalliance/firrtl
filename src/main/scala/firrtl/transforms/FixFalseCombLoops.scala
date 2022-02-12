package firrtl
package transforms

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object FixFalseCombLoops {

  def fixFalseCombLoops(state: CircuitState, combLoopsError: String): CircuitState = {
    val moduleToBadVars = parseLoopVariables(combLoopsError)
    state.copy(circuit = state.circuit.mapModule(onModule(_, moduleToBadVars)))
  }

  //Parse error string into list of variables
  def parseLoopVariables(combLoopsError: String): Map[String, ListBuffer[String]] = {
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

  private def onModule(m: ir.DefModule, moduleToLoopVars: Map[String, ListBuffer[String]]): ir.DefModule = m match {
    case mod: ir.Module =>
      if (moduleToLoopVars.contains(mod.name)) {
        val values = helper(mod, moduleToLoopVars(mod.name))
        mod.copy(body = ir.Block(values))
      } else {
        mod
      }
    case other => other
  }

  private def helper(m: ir.Module, combLoopVars: ListBuffer[String]): List[ir.Statement] = {
    //TODO: Make this into only a list of ir.Statement
    val conds = mutable.LinkedHashMap[String, ir.Statement]()

    def onStmt(s: ir.Statement): Unit = s match {
      case ir.Block(block) => block.foreach(onStmt)
      case node: ir.DefNode =>
        if (combLoopVars.contains(node.name)) {
          if (getWidth(node.value.tpe) == 1) {
            //Removes 1 bit nodes from combLoopVars to avoid unnecessary computation
            combLoopVars -= node.name
          }
        }
        conds(node.serialize) = node

      case wire: ir.DefWire =>
        //Summary: Splits wire into individual bits (wire x -> wire x_0, ..., wire x_n)
        if (combLoopVars.contains(wire.name)) {
          val wireWidth = getWidth(wire.tpe)
          if (wireWidth == 1) {
            //Removes 1 bit wires from combLoopVars to avoid unnecessary computation
            combLoopVars -= wire.name
            conds(wire.serialize) = wire
          } else {
            //Create new wire for every bit in wire
            for (i <- 0 until wireWidth) {
              //TODO: Handle case where there is a repeated name. Can't use wire.name + i.toString
              val bitWire = ir.DefWire(ir.NoInfo, wire.name + i.toString, Utils.BoolType)
              conds(bitWire.serialize) = bitWire
            }
            //Creates node wire = cat(a_n # ... # a_0)
            val newNode =
              ir.DefNode(ir.NoInfo, wire.name, convertToCats(wireWidth - 1, 0, wire.name))
            conds(newNode.serialize) = newNode
          }
        } else {
          conds(wire.serialize) = wire
        }

      case connect: ir.Connect =>
        var newConnect = connect
        if (newConnect.expr.isInstanceOf[ir.Expression]) {
          //Summary: Process expr (rhs) of Connect
          val newExpr = onExpr(newConnect.expr)
          newConnect = ir.Connect(ir.NoInfo, newConnect.loc, newExpr)
        }

        //At this point, it is certain:
        //a -> (an # ... # a0)

        newConnect.loc match {
          case ref: ir.Reference =>
            if (combLoopVars.contains(ref.name)) {
              newConnect.expr match {
                case prim: ir.DoPrim =>
                  //TODO: Convert to match on prim.op
                  if (prim.op == PrimOps.Cat) {
                    //Summary: If lhs in combLoopVars, rhs is DoPrim(cat); split lhs into bits as: (x_0 = rhs(0), ..., x_n = rhs(n))

                    //TODO: OLD CODE
//                    for (i <- 0 until getWidth(ref.tpe)) {
//                      //TODO: Doesn't work if any arg.length > 1. Need to split such args up also.
//                      if (getWidth(prim.args.reverse(i).tpe) == 1) {
//                        val tempConnect = ir.Connect(ir.NoInfo, genRef(ref.name, i), prim.args.reverse(i))
//                        conds(tempConnect.serialize) = tempConnect
//                      }
//                    }

                    //TODO: NEW CODE
                    val refWidth = getWidth(ref.tpe)
                    var currBit = 0
                    for (i <- 0 until 2) {
                      val bitsLeft = refWidth - currBit
                      // If total arg bits are less than lhs bits, stop
                      if (bitsLeft > 0) {
                        val argWidth = getWidth(prim.args.reverse(i).tpe)
                        if (argWidth == 1) {
                          val tempConnect = ir.Connect(ir.NoInfo, genRef(ref.name, currBit), prim.args.reverse(i))
                          conds(tempConnect.serialize) = tempConnect
                          currBit += 1
                        } else {
                          //If arg is a bad var, replace with new vars
                          if (combLoopVars.contains(prim.args.reverse(i).asInstanceOf[ir.Reference].name)) {
                            val name = prim.args.reverse(i).asInstanceOf[ir.Reference].name

                            for (j <- 0 until math.min(argWidth, bitsLeft)) {
                              val tempConnect = ir.Connect(ir.NoInfo, genRef(ref.name, currBit), genRef(name, j))
                              conds(tempConnect.serialize) = tempConnect
                              currBit += 1
                            }
                          } else {
                            //If arg is not a bad var, replace with bit prims
                            for (j <- 0 until math.min(argWidth, bitsLeft)) {
                              val tempConnect = ir.Connect(
                                ir.NoInfo,
                                genRef(ref.name, currBit),
                                ir.DoPrim(PrimOps.Bits, Seq(prim.args.reverse(i)), Seq(j, j), Utils.BoolType)
                              )
                              conds(tempConnect.serialize) = tempConnect
                              currBit += 1
                            }
                          }
                        }
                      }
                    }

                  } else {
                    //If lhs is ir.Reference, in combLoopVars, is ir.DoPrim, but not Cat
                    //TODO: Handle when lhs is more than 1 bit
                    if (getWidth(ref.tpe) == 1) {
                      //TODO: This case may never run, as 1 bit lhs will not be modified
                      val tempConnect = ir.Connect(ir.NoInfo, genRef(ref.name, 0), newConnect.expr)
                      conds(tempConnect.serialize) = tempConnect
                    } else {
                      conds(newConnect.serialize) = newConnect
                    }
                  }

                case _ =>
                  //TODO: Do something in this case
                  //If lhs is ir.Reference, in combLoopVars, but isn't ir.DoPrim
                  conds(newConnect.serialize) = newConnect
              }
            } else {
              //If lhs is ir.Reference, but isn't in combLoopVars
              conds(newConnect.serialize) = newConnect
            }

          //If lhs is not a ir.Reference
          case _ =>
            conds(newConnect.serialize) = newConnect
        }
      case other =>
        conds(other.serialize) = other
    }

    def onExpr(s: ir.Expression): ir.Expression = s match {
      case prim: ir.DoPrim =>
        if (prim.op == PrimOps.Bits && combLoopVars.contains(prim.args(0).asInstanceOf[ir.Reference].name)) {
          //Summary: Replaces bits(a, i, j) with (aj # ... # ai)
          val high = prim.consts(0)
          val low = prim.consts(1)
          val name = prim.args(0).asInstanceOf[ir.Reference].name
          //TODO: Should these be BigInt?
          convertToCats(high.toInt, low.toInt, name)
        } else {
          //Summary: Recursively calls onExpr on each of the arguments to DoPrim
          var newPrimArgs = Seq[ir.Expression]()
          for (arg <- prim.args) {
            newPrimArgs = newPrimArgs :+ onExpr(arg)
          }
          ir.DoPrim(prim.op, newPrimArgs, prim.consts, prim.tpe)
        }
      case other => other
    }

    //Creates a reference to the bitWire for a given variable
    def genRef(name: String, index: Int): ir.Reference = {
      //TODO: Handle case where there is a repeated name. Can't use name + index.toString
      ir.Reference(name + index.toString, Utils.BoolType, WireKind, UnknownFlow)
    }

    //Returns width associated with inputted statement
    def getWidth(stmt: ir.Type): Int = {
      stmt.asInstanceOf[ir.UIntType].width.asInstanceOf[ir.IntWidth].width.toInt
    }

    //Converts variable (x) into nested cats (x_high # ... # x_low)
    def convertToCats(high: Int, low: Int, name: String): ir.Expression = {
      if (high == low) {
        genRef(name, low)
      } else {
        //TODO: Should this be tpe = BoolType?
        ir.DoPrim(PrimOps.Cat, Seq(genRef(name, high), convertToCats(high - 1, low, name)), Seq.empty, Utils.BoolType)
      }
    }

    onStmt(m.body)
    conds.values.toList
  }
}
