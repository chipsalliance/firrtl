package firrtl.range

import firrtl.ir._
import firrtl.PrimOps._
import Range.{getRange, getWidth}

object Inference {
  def allUInt(tpes: Seq[Type]): Boolean =
    tpes.find((t: Type) => t match {
      case u: UIntType => false
      case _ => true
    }).isEmpty
  def getType(width: BigInt, asUInt: Boolean): GroundType =
    if(asUInt) {
      if(width == 0) UIntType(IntWidth(0))
      else UIntType(IntWidth(width - 1))
    } else SIntType(IntWidth(width))
  def infer(tpes: Seq[GroundType], op: PrimOp): GroundType = op match {
    case Add => // i:x + j:y = k:z
      val (xMin, xMax) = getRange(tpes(0))
      val (yMin, yMax) = getRange(tpes(1))
      val zmin = xMin + yMin
      val zmax = xMax + yMax
      getType(getWidth(zmin, zmax), allUInt(tpes)) // UInt if all args are UInt
    case Sub => // i:x - j:y = k:z
      val (xMin, xMax) = getRange(tpes(0))
      val (yMin, yMax) = getRange(tpes(1))
      val zmin = xMin - yMax
      val zmax = xMax - yMin
      SIntType(IntWidth(getWidth(zmin, zmax))) // Always SInt
    case Mul => //i:x * j:y = k:z
      val (xMin, xMax) = getRange(tpes(0))
      val (yMin, yMax) = getRange(tpes(1))
      val zmin = Seq(xMax * yMax, xMax * yMin, xMin * yMax, xMin * yMin).reduce(_.min(_))
      val zmax = Seq(xMax * yMax, xMax * yMin, xMin * yMax, xMin * yMin).reduce(_.max(_))
      getType(getWidth(zmin, zmax), allUInt(tpes)) // UInt if all args are UInt
    //case Div => //i:x / j:y = k:z
    //  val (xMin, xMax) = getRange(tpes(0))
    //  val (yMin, yMax) = getRange(tpes(1))
    //  val zmin = Seq(xMax / yMax, xMax / yMin, xMin / yMax, xMin / yMin).reduce(_.min(_))
    //  val zmax = Seq(xMax / yMax, xMax / yMin, xMin / yMax, xMin / yMin).reduce(_.max(_))
    //  getType(getWidth(zmin, zmax), allUInt(tpes)) // UInt if all args are UInt
  }
  def opType(prim: DoPrim): Type = {
    val tpes = prim.args.map(e => e.tpe match {
      case t: GroundType => t
      case _ => throw new RangeException(s"Primop ${prim.op} has a non-GroundType argument")
    })
    infer(tpes, prim.op)
  }
}

// vim: set ts=4 sw=4 et:
