package firrtl.range

import firrtl.ir.{DoPrim, GroundType, SIntType, UIntType, Type}
import firrtl.PrimOps._
import Range.{getRange, getWidth}

object Inference {
  def opType(prim: DoPrim): Type = {
    def infer(tpes: Seq[GroundType]): GroundType = prim.op match {
      case Sub =>
        // i:x - j:y = k:z
        val (xMin, xMax) = getRange(tpes(0))
        val (yMin, yMax) = getRange(tpes(1))
        val zmin = xMin - yMax
        val zmax = xMax - yMin
        SIntType(getWidth(zmin, zmax))
    }
    val tpes = prim.args.map(e => e.tpe match {
      case t: GroundType => t
      case _ => throw new RangeException(s"Primop ${prim.op} has a non-GroundType argument")
    })
    infer(tpes)
  }
}

// vim: set ts=4 sw=4 et:
