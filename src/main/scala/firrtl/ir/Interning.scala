// See LICENSE for license details.

package firrtl.ir

import scala.collection.mutable

/** Helper functions and classes for keeping the number of FirrtlNodes created small.
  *
  * The most important thing to note is: The firrtl compiler does not do **strict** interning,
  * i.e., only because two FirrtlNodes are not `eq`, the could still contain the same data.
  * However, in an effort to reduce the number of object used in the compiler we try to intern
  * often used expressions and types.
  *
  * Our strategy is three fold:
  * 1. We provide interning expression and type factories for use in the protobuf and firrtl parsers.
  * 2. We modify the `mapExpr` and `mapType` methods to only copy of the returned object is not `eq` to the prior value.
  * 3. We provide an object identity based cache so that, e.g., passes that work on expressions can replace
  *    all expression instances 1:1 instead of creating a new node at every place that the expression is used.
  */
object Interning {

}


/** Caches type nodes in order to reduce the number of objects created.
  * @note This class is only useful for parsers, incremental interning is not supported.
  */
class InterningTypeFactory {
  private val intWidths = mutable.HashMap[BigInt, IntWidth]()
  def makeIntWidth(width: BigInt): IntWidth = intWidths.getOrElseUpdate(width, IntWidth(width))

  private val uIntUnknown = UIntType(UnknownWidth)
  def makeUIntWithUnknownWidth: UIntType = uIntUnknown
  private val uInts = mutable.HashMap[BigInt, UIntType]()
  def makeUInt(width: BigInt): UIntType = uInts.getOrElseUpdate(width, UIntType(makeIntWidth(width)))

  private val sIntUnknown = SIntType(UnknownWidth)
  def makeSIntWithUnknownWidth: SIntType = sIntUnknown
  private val sInts = mutable.HashMap[BigInt, SIntType]()
  def makeSInt(width: BigInt): SIntType = sInts.getOrElseUpdate(width, SIntType(makeIntWidth(width)))

  private val analogUnknown = AnalogType(UnknownWidth)
  def makeAnalogWithUnknownWidth: AnalogType = analogUnknown
  private val analogs = mutable.HashMap[BigInt, AnalogType]()
  def makeAnalog(width: BigInt): AnalogType = analogs.getOrElseUpdate(width, AnalogType(makeIntWidth(width)))

  private val fixedUnknown = FixedType(UnknownWidth, UnknownWidth)
  def makeFixedWithUnknownWidthAndPoint: FixedType = fixedUnknown
  private val fixeds = mutable.HashMap[(BigInt, BigInt), FixedType]()
  def makeFixedWithUnknownWidth(point: BigInt): FixedType =
    fixeds.getOrElseUpdate((-1, point), FixedType(UnknownWidth, makeIntWidth(point)))
  def makeFixedWithUnknownPoint(width: BigInt): FixedType =
    fixeds.getOrElseUpdate((width, -1), FixedType(makeIntWidth(width), UnknownWidth))
  def makeFixed(width: BigInt, point: BigInt): FixedType =
    fixeds.getOrElseUpdate((width, point), FixedType(IntWidth(width), makeIntWidth(point)))

  private val fields = mutable.HashMap[IdAndEqKey[Type, (String, Orientation)], Field]()
  def makeField(name: String, flip: Orientation, tpe: Type): Field =
    fields.getOrElseUpdate(IdAndEqKey(tpe, (name, flip)), Field(name, flip, tpe))


  private val bundles = mutable.HashMap[IdSeqKey[Field], BundleType]()
  def makeBundle(fields: Seq[Field]): BundleType = bundles.getOrElseUpdate(IdSeqKey(fields), BundleType(fields))

  // TODO: interval types

  private val vectors = mutable.HashMap[IdAndEqKey[Type, Int], VectorType]()
  def makeVector(tpe: Type, size: Int): VectorType =
    vectors.getOrElseUpdate(IdAndEqKey(tpe, size), VectorType(tpe, size))
}

/** Caches expression nodes in order to reduce the number of objects created.
  * @note This class is only useful for parsers, incremental interning is not supported.
  * @note Since we target parsers, we generate expressions with unknown type.
  */
class InterningExpressionFactory {
  private val references = mutable.HashMap[String, Reference]()
  def makeReference(name: String): Reference = references.getOrElseUpdate(name, Reference(name))

  private val subFields = mutable.HashMap[IdAndEqKey[Expression, String], SubField]()
  def makeSubField(expr: Expression, name: String): SubField =
    subFields.getOrElseUpdate(IdAndEqKey(expr, name), SubField(expr, name, UnknownType))

  private val subIndices = mutable.HashMap[IdAndEqKey[Expression, Int], SubIndex]()
  def makeSubIndex(expr: Expression, value: Int): SubIndex =
    subIndices.getOrElseUpdate(IdAndEqKey(expr, value), SubIndex(expr, value, UnknownType))

  private val subAccesses = mutable.HashMap[IdSeqKey[Expression], SubAccess]()
  def makeSubAccess(expr: Expression, index: Expression): SubAccess =
    subAccesses.getOrElseUpdate(IdSeqKey(List(expr, index)), SubAccess(expr, index, UnknownType))

  private val muxes = mutable.HashMap[IdSeqKey[Expression], Mux]()
  def makeMux(cond: Expression, tval: Expression, fval: Expression): Mux =
    muxes.getOrElseUpdate(IdSeqKey(List(cond, tval, fval)), Mux(cond, tval, fval, UnknownType))

  private val doPrims = mutable.HashMap[IdSeqAndEqKey[Expression, (PrimOp, Seq[BigInt])], DoPrim]()
  def makeDoPrim(op: PrimOp, args: Seq[Expression], consts: Seq[BigInt]): DoPrim =
    doPrims.getOrElseUpdate(IdSeqAndEqKey(args, (op, consts)), DoPrim(op, args, consts, UnknownType))

  private val validIfs = mutable.HashMap[IdSeqKey[Expression], ValidIf]()
  def makeValifIf(cond: Expression, value: Expression): ValidIf =
    validIfs.getOrElseUpdate(IdSeqKey(Seq(cond, value)), ValidIf(cond, value, UnknownType))

  private val uIntLits = mutable.HashMap[IdAndEqKey[Width, BigInt], UIntLiteral]()
  def makeUIntLiteral(value: BigInt, width: Width = UnknownWidth): UIntLiteral =
    uIntLits.getOrElseUpdate(IdAndEqKey(width, value), UIntLiteral(value, width))

  private val sIntLits = mutable.HashMap[IdAndEqKey[Width, BigInt], SIntLiteral]()
  def makeSIntLiteral(value: BigInt, width: Width = UnknownWidth): SIntLiteral =
    sIntLits.getOrElseUpdate(IdAndEqKey(width, value), SIntLiteral(value, width))

  private val fixedLits = mutable.HashMap[IdSeqAndEqKey[Width, BigInt], FixedLiteral]()
  def makeFixedLiteral(value: BigInt, width: Width, point: Width): FixedLiteral =
    fixedLits.getOrElseUpdate(IdSeqAndEqKey(Seq(width, point), value), FixedLiteral(value, width, point))
}



private class IdSeqKey[T <: AnyRef](val e: Seq[T]) {
  override def equals(obj: Any): Boolean = obj match {
    case other : IdSeqKey[_] => e.zip(other.e).forall{ case (a,b) => a.eq(b) } && e.length == other.e.length
    case _ => false
  }
  override val hashCode: Int = e.map(System.identityHashCode).hashCode()
}
private object IdSeqKey { def apply[T <: AnyRef](e: Seq[T]): IdSeqKey[T] = new IdSeqKey[T](e) }
private[firrtl] class IdKey[I <: AnyRef](val i: I) {
  override def equals(obj: Any): Boolean = obj match {
    case other : IdKey[_] => other.i.eq(i)
    case _ => false
  }
  override val hashCode: Int = System.identityHashCode(i)
}
private[firrtl] object IdKey { def apply[I <: AnyRef](i: I): IdKey[I] = new IdKey[I](i) }
private class IdAndEqKey[I <: AnyRef, E](val i: I, val e: E) {
  override def equals(obj: Any): Boolean = obj match {
    case other : IdAndEqKey[_, _] => other.i.eq(i) && other.e == e
    case _ => false
  }
  override val hashCode: Int = (System.identityHashCode(i), e).hashCode()
}
private object IdAndEqKey {
  def apply[I <: AnyRef, E](i: I, e: E): IdAndEqKey[I, E] = new IdAndEqKey[I,E](i, e)
}
private class IdSeqAndEqKey[I <: AnyRef, E](val i: Seq[I], val e: E) {
  override def equals(obj: Any): Boolean = obj match {
    case other : IdSeqAndEqKey[_, _] =>
      i.zip(other.i).forall{ case (a,b) => a.eq(b) } && i.length == other.i.length && other.e == e
    case _ => false
  }
  override val hashCode: Int = (i.map(System.identityHashCode).hashCode(), e.hashCode()).hashCode()
}
private object IdSeqAndEqKey {
  def apply[I <: AnyRef, E](i: Seq[I], e: E): IdSeqAndEqKey[I, E] = new IdSeqAndEqKey[I,E](i, e)
}