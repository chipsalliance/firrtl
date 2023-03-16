// SPDX-License-Identifier: Apache-2.0

package firrtl.passes

import firrtl._
import firrtl.ir._
import firrtl.Utils._
import firrtl.Mappers._
import firrtl.options.Dependency

object InferTypes extends Pass {
  import CInferTypes.resolveSlice
  override def prerequisites = Dependency(ResolveKinds) +: firrtl.stage.Forms.MinimalHighForm
  override def invalidates(a: Transform) = false

  @deprecated("This should never have been public", "FIRRTL 1.3.2")
  type TypeMap = collection.mutable.LinkedHashMap[String, Type]

  private type TypeLookup = collection.mutable.HashMap[String, Type]

  def run(c: Circuit): Circuit = {
    val namespace = Namespace()

    def remove_unknowns_b(b: Bound): Bound = b match {
      case UnknownBound => VarBound(namespace.newName("b"))
      case k            => k
    }

    def remove_unknowns_w(w: Width): Width = w match {
      case UnknownWidth => VarWidth(namespace.newName("w"))
      case wx           => wx
    }

    def remove_unknowns(t: Type): Type = {
      t.map(remove_unknowns).map(remove_unknowns_w) match {
        case IntervalType(l, u, p) =>
          IntervalType(remove_unknowns_b(l), remove_unknowns_b(u), p)
        case x => x
      }
    }

    // we first need to remove the unknown widths and bounds from all ports,
    // as their type will determine the module types
    val portsKnown = c.modules.map(_.map { p: Port => p.copy(tpe = remove_unknowns(p.tpe)) })
    val mtypes = portsKnown.map(m => m.name -> module_type(m)).toMap

    def infer_types_e(types: TypeLookup)(e: Expression): Expression =
      e.map(infer_types_e(types)) match {
        case e: WRef       => e.copy(tpe = types(e.name))
        case e: WSubField  => e.copy(tpe = field_type(e.expr.tpe, e.name))
        case e: WSubIndex  => e.copy(tpe = sub_type(e.expr.tpe))
        case e: WSubAccess => e.copy(tpe = sub_type(e.expr.tpe))
        case e: DoPrim     => PrimOps.set_primop_type(e)
        case e: Mux        => e.copy(tpe = mux_type_and_widths(e.tval, e.fval))
        case e: ValidIf    => e.copy(tpe = e.value.tpe)
        case e @ (_: UIntLiteral | _: SIntLiteral) => e
        case e: WSliceNode => resolveSlice(e)
      }

    def infer_types_s(types: TypeLookup)(s: Statement): Statement = s match {
      case sx: WDefInstance =>
        val t = mtypes(sx.module)
        types(sx.name) = t
        sx.copy(tpe = t)
      case sx: DefWire =>
        val t = remove_unknowns(sx.tpe)
        types(sx.name) = t
        sx.copy(tpe = t)
      case sx: DefNode =>
        val sxx = (sx.map(infer_types_e(types))).asInstanceOf[DefNode]
        val t = remove_unknowns(sxx.value.tpe)
        types(sx.name) = t
        sxx
      case sx: DefRegister =>
        val t = remove_unknowns(sx.tpe)
        types(sx.name) = t
        sx.copy(tpe = t).map(infer_types_e(types))
      case sx: DefMemory =>
        // we need to remove the unknowns from the data type so that all ports get the same VarWidth
        val knownDataType = sx.copy(dataType = remove_unknowns(sx.dataType))
        types(sx.name) = MemPortUtils.memType(knownDataType)
        knownDataType
      case sx => sx.map(infer_types_s(types)).map(infer_types_e(types))
    }

    def infer_types_p(types: TypeLookup)(p: Port): Port = {
      val t = remove_unknowns(p.tpe)
      types(p.name) = t
      p.copy(tpe = t)
    }

    def infer_types(m: DefModule): DefModule = {
      val types = new TypeLookup
      m.map(infer_types_p(types)).map(infer_types_s(types))
    }

    c.copy(modules = portsKnown.map(infer_types))
  }
}

/** Internal node used by the parser for the `[...]` and `[... : ...]` syntax.
  *  CHIRRTL level type inference then converts this into either a `bits(..., ... , ...)`,
  *  or [[ir.SubIndex]] depending on the inferred type of the inner expression.
  */
case class WSliceNode(expr: ir.Expression, hi: Int, lo: Int) extends ir.Expression {
  override def tpe = ir.UnknownType
  override def mapExpr(f: ir.Expression => ir.Expression) = copy(expr = f(expr))
  override def mapType(f: ir.Type => ir.Type) = { f(ir.UnknownType); this }
  override def mapWidth(f:     ir.Width => ir.Width) = this
  override def foreachExpr(f:  ir.Expression => Unit): Unit = f(expr)
  override def foreachType(f:  ir.Type => Unit):       Unit = f(ir.UnknownType)
  override def foreachWidth(f: ir.Width => Unit):      Unit = ()
  override def serialize = {
    val suffix = if (hi == lo) { s"[$hi]" }
    else { s"[$hi:$lo]" }
    expr.serialize + suffix
  }
}

object CInferTypes extends Pass {

  override def prerequisites = firrtl.stage.Forms.ChirrtlForm
  override def invalidates(a: Transform) = false

  @deprecated("This should never have been public", "FIRRTL 1.3.2")
  type TypeMap = collection.mutable.LinkedHashMap[String, Type]

  private type TypeLookup = collection.mutable.HashMap[String, Type]

  /** Turns an internal slice node into either a bits primop or a sub index depending on types.
    *  This is necessary because the parser cannot disambiguate the three AST nodes since they share the same syntax.
    */
  def resolveSlice(e: WSliceNode): Expression = e.expr.tpe match {
    case _: VectorType if e.hi == e.lo =>
      SubIndex(e.expr, e.hi, sub_type(e.expr.tpe))
    case _ =>
      val op = DoPrim(PrimOps.Bits, List(e.expr), List(e.hi, e.lo), UnknownType)
      PrimOps.set_primop_type(op)
  }

  def run(c: Circuit): Circuit = {
    val mtypes = (c.modules.map(m => m.name -> module_type(m))).toMap

    def infer_types_e(types: TypeLookup)(e: Expression): Expression =
      e.map(infer_types_e(types)) match {
        case (e: Reference) => e.copy(tpe = types.getOrElse(e.name, UnknownType))
        case (e: SubField)  => e.copy(tpe = field_type(e.expr.tpe, e.name))
        case (e: SubIndex)  => e.copy(tpe = sub_type(e.expr.tpe))
        case (e: SubAccess) => e.copy(tpe = sub_type(e.expr.tpe))
        case (e: DoPrim)    => PrimOps.set_primop_type(e)
        case (e: Mux)       => e.copy(tpe = mux_type(e.tval, e.fval))
        case (e: ValidIf)   => e.copy(tpe = e.value.tpe)
        case e @ (_: UIntLiteral | _: SIntLiteral) => e
        case e: WSliceNode => resolveSlice(e)
      }

    def infer_types_s(types: TypeLookup)(s: Statement): Statement = s match {
      case sx: DefRegister =>
        types(sx.name) = sx.tpe
        sx.map(infer_types_e(types))
      case sx: DefWire =>
        types(sx.name) = sx.tpe
        sx
      case sx: DefNode =>
        val sxx = (sx.map(infer_types_e(types))).asInstanceOf[DefNode]
        types(sxx.name) = sxx.value.tpe
        sxx
      case sx: DefMemory =>
        types(sx.name) = MemPortUtils.memType(sx)
        sx
      case sx: CDefMPort =>
        val t = types.getOrElse(sx.mem, UnknownType)
        types(sx.name) = t
        sx.copy(tpe = t)
      case sx: CDefMemory =>
        types(sx.name) = sx.tpe
        sx
      case sx: DefInstance =>
        types(sx.name) = mtypes(sx.module)
        sx
      case sx => sx.map(infer_types_s(types)).map(infer_types_e(types))
    }

    def infer_types_p(types: TypeLookup)(p: Port): Port = {
      types(p.name) = p.tpe
      p
    }

    def infer_types(m: DefModule): DefModule = {
      val types = new TypeLookup
      m.map(infer_types_p(types)).map(infer_types_s(types))
    }

    c.copy(modules = c.modules.map(infer_types))
  }
}
