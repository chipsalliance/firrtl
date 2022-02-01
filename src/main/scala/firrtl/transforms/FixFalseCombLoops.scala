package firrtl
package transforms

import firrtl.ir.UIntType
import firrtl.passes.Errors

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object FixFalseCombLoops {

  def fixFalseCombLoops(state: CircuitState): CircuitState = {
    //Modify circuit
    //Steps:
    //1) Create wires a0, a1
    //1.3) Delete a
    //1.5) For first line:
    //Split cat up across a0, a1

    //2) For second line:
    //Replace bits(a,0,0) with a0
    //3) For fourth line, replace a with a1 # a0

    state.copy(circuit = state.circuit.mapModule(onModule))
  }


  private def onModule(m: ir.DefModule): ir.DefModule = m match {
    case mod: ir.Module =>
      val values = helper(mod)
      mod.copy(body = ir.Block(values))
    case other => other
  }

  private def helper(m: ir.Module): List[ir.Statement] = {
    val conds = mutable.LinkedHashMap[String, ir.Statement]()

    def onStmt(s: ir.Statement): Unit = s match {
      case ir.Block(block) => block.foreach(onStmt)
      case wire: ir.DefWire =>
        if (wire.name == "a") {
          conds("a0") = ir.DefWire(ir.NoInfo, "a0", Utils.BoolType)
          conds("a1") = ir.DefWire(ir.NoInfo, "a1", Utils.BoolType)
        } else {
          conds(wire.serialize) = wire
        }
      case connect: ir.Connect =>
        var newConnect = connect
        val a0Ref = ir.Reference("a0", Utils.BoolType, WireKind, UnknownFlow)
        val a1Ref = ir.Reference("a1", Utils.BoolType, WireKind, UnknownFlow)
        val aList = Seq(a1Ref, a0Ref)

        if (newConnect.expr.isInstanceOf[ir.Expression]) {
          //Summary: Process expr (rhs) of Connect
          val newExpr = onExpr(newConnect.expr, aList)
          newConnect = ir.Connect(ir.NoInfo, newConnect.loc, newExpr)
        }

        //At this point, it is certain:
        //a -> (an # ... # a0)

        if (newConnect.loc.isInstanceOf[ir.Reference] && newConnect.loc.asInstanceOf[ir.Reference].name == "a") {
          //Summary: If a on lhs and DoPrim(cat) on rhs, split a as: (a0 = cat(0), a1 = cat(1), etc.)
          //Currently works if cat two references, but not if any arg is another DoPrim
          if (newConnect.expr.asInstanceOf[ir.DoPrim].op == PrimOps.Cat) {
            val args = newConnect.expr.asInstanceOf[ir.DoPrim].args
            for (i <- aList.indices) {
              //if arg.length > 1, split arg into individual bits. Create wires for each bit of them
              //else, if arg.length == 1, use their values directly.
              if (args(i).tpe.asInstanceOf[UIntType].width == ir.IntWidth(1)) {
                val tempConnect = ir.Connect(ir.NoInfo, aList(i), args(i))
                conds(tempConnect.serialize) = tempConnect
              }
            }
          }
        } else {
          conds(newConnect.serialize) = newConnect
        }

    }

    def onExpr(s: ir.Expression, aList: Seq[ir.Reference]): ir.Expression = s match {
      case prim: ir.DoPrim =>
        //Summary: Replaces bits(a, i, j) with (aj # ... # ai). *Currently only able to return a single bit.
        if (prim.op == PrimOps.Bits && prim.args(0).asInstanceOf[ir.Reference].name == "a") {
          aList(1)
        } else {
          //Summary: Recursively calls onExpr on each of the arguments to DoPrim
          var newPrimArgs = new ArrayBuffer[ir.Expression]()
          for (arg <- prim.args) {
            newPrimArgs = newPrimArgs :+ onExpr(arg, aList)
          }
          ir.DoPrim(prim.op, newPrimArgs, prim.consts, prim.tpe)
        }
      case ref: ir.Reference =>
        if (ref.name == "a") {
          //Summary: Replaces a (in expr i.e. rhs) with (an # ... # a0)
          ir.DoPrim(PrimOps.Cat, aList.reverse, Seq.empty, Utils.BoolType)
        } else {
          ref
        }
      case other => other
    }


    onStmt(m.body)
    conds.values.toList
  }

}
