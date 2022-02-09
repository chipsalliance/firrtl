package firrtl
package transforms

import firrtl.ir.UIntType

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

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

    def genRef(name: String, index: Int): ir.Reference = {
      //TODO: Handle case where there is a repeated name. Can't use name + index.toString
      ir.Reference(name + index.toString, Utils.BoolType, WireKind, UnknownFlow)
    }

    def onStmt(s: ir.Statement): Unit = s match {
      case ir.Block(block) => block.foreach(onStmt)
      case wire: ir.DefWire =>
        //Summary: Splits wire into individual bits (wire x -> wire x_0, ..., wire x_n)
        if (combLoopVars.contains(wire.name)) {
          if (wire.tpe.asInstanceOf[UIntType].width.asInstanceOf[ir.IntWidth].width.toInt == 1) {
            //Removes 1 bit wires from combLoopVars to avoid unnecessary computation
            combLoopVars -= wire.name
            conds(wire.serialize) = wire
          } else {
            //TODO: Can append width data to combLoopVars here
            for (i <- 0 until wire.tpe.asInstanceOf[UIntType].width.asInstanceOf[ir.IntWidth].width.toInt) {
              val bitWire = ir.DefWire(ir.NoInfo, wire.name + i.toString, Utils.BoolType)
              conds(bitWire.serialize) = bitWire
            }
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
                    for (i <- 0 until ref.tpe.asInstanceOf[UIntType].width.asInstanceOf[ir.IntWidth].width.toInt) {
                      //TODO: Doesn't work if any arg.length > 1. Need to split such args up also.
                      if (
                        prim.args.reverse(i).tpe.asInstanceOf[UIntType].width.asInstanceOf[ir.IntWidth].width.toInt == 1
                      ) {
                        val tempConnect = ir.Connect(ir.NoInfo, genRef(ref.name, i), prim.args.reverse(i))
                        conds(tempConnect.serialize) = tempConnect
                      }
                    }
                  } else {
                    //If lhs is ir.Reference, in combLoopVars, is ir.DoPrim, but not Cat
                    //TODO: Handle when lhs is more than 1 bit
                    if (ref.tpe.asInstanceOf[UIntType].width.asInstanceOf[ir.IntWidth].width.toInt == 1) {
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
          var newPrimArgs = new ArrayBuffer[ir.Expression]()
          for (arg <- prim.args) {
            newPrimArgs = newPrimArgs :+ onExpr(arg)
          }
          ir.DoPrim(prim.op, newPrimArgs, prim.consts, prim.tpe)
        }
      case ref: ir.Reference =>
        if (combLoopVars.contains(ref.name)) {
          //Summary: Replaces a (in expr i.e. rhs) with (an # ... # a0)
          val size = ref.tpe.asInstanceOf[UIntType].width.asInstanceOf[ir.IntWidth].width.toInt
          convertToCats(size - 1, 0, ref.name)
        } else {
          ref
        }
      case other => other
    }

    //Converts variable (x) into cats (x_high # ... # x_low)
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
