// See LICENSE for license details.

package firrtl
package proto

import collection.JavaConverters._

import FirrtlProtos._

case class ProtoDeserializationException(msg: String) extends FIRRTLException(msg)

// TODO Change requires to handled exceptions
object FromProto {

  private def compressStmts(stmts: Seq[ir.Statement]): ir.Statement = stmts match {
    case Seq() => ir.EmptyStmt
    case Seq(stmt) => stmt
    case multiple => ir.Block(multiple)
  }

  val convert: Map[Firrtl.Expression.PrimOp.Op, ir.PrimOp] =
    ToProto.convert.map { case (k, v) => v -> k }

  def convert(literal: Firrtl.Expression.IntegerLiteral): BigInt =
    BigInt(literal.getValue)

  def convert(uint: Firrtl.Expression.UIntLiteral): ir.UIntLiteral = {
    val width = if (uint.hasWidth) convert(uint.getWidth) else ir.UnknownWidth
    ir.UIntLiteral(convert(uint.getValue), width)
  }

  def convert(sint: Firrtl.Expression.SIntLiteral): ir.SIntLiteral = {
    val width = if (sint.hasWidth) convert(sint.getWidth) else ir.UnknownWidth
    ir.SIntLiteral(convert(sint.getValue), width)
  }

  def convert(subfield: Firrtl.Expression.SubField): ir.SubField =
    ir.SubField(convert(subfield.getExpression), subfield.getField, ir.UnknownType)

  def convert(index: Firrtl.Expression.SubIndex): ir.SubIndex =
    ir.SubIndex(convert(index.getExpression), convert(index.getIndex).toInt, ir.UnknownType)

  def convert(access: Firrtl.Expression.SubAccess): ir.SubAccess =
    ir.SubAccess(convert(access.getExpression), convert(access.getIndex), ir.UnknownType)

  def convert(primop: Firrtl.Expression.PrimOp): ir.DoPrim = {
    val args = primop.getArgList.asScala.map(convert(_))
    val consts = primop.getConstList.asScala.map(convert(_))
    ir.DoPrim(convert(primop.getOp), args, consts, ir.UnknownType)
  }

  def convert(mux: Firrtl.Expression.Mux): ir.Mux =
    ir.Mux(convert(mux.getCondition), convert(mux.getTValue), convert(mux.getFValue), ir.UnknownType)

  def convert(expr: Firrtl.Expression): ir.Expression =
    if (expr.hasReference) ir.Reference(expr.getReference.getId, ir.UnknownType)
    else if (expr.hasSubField) convert(expr.getSubField)
    else if (expr.hasSubIndex) convert(expr.getSubIndex)
    else if (expr.hasSubAccess) convert(expr.getSubAccess)
    else if (expr.hasUintLiteral) convert(expr.getUintLiteral)
    else if (expr.hasSintLiteral) convert(expr.getSintLiteral)
    else if (expr.hasPrimOp) convert(expr.getPrimOp)
    else if (expr.hasMux) convert(expr.getMux)
    else throw new Exception(s"Got $expr")

  def convert(con: Firrtl.Statement.Connect): ir.Connect =
    ir.Connect(ir.NoInfo, convert(con.getLocation), convert(con.getExpression))

  def convert(con: Firrtl.Statement.PartialConnect): ir.PartialConnect =
    ir.PartialConnect(ir.NoInfo, convert(con.getLocation), convert(con.getExpression))

  def convert(wire: Firrtl.Statement.Wire): ir.DefWire =
    ir.DefWire(ir.NoInfo, wire.getId, convert(wire.getType))

  def convert(reg: Firrtl.Statement.Register): ir.DefRegister =
    ir.DefRegister(ir.NoInfo, reg.getId, convert(reg.getType), convert(reg.getClock),
                   convert(reg.getReset), convert(reg.getInit))

  def convert(node: Firrtl.Statement.Node): ir.DefNode =
    ir.DefNode(ir.NoInfo, node.getId, convert(node.getExpression))

  def convert(inst: Firrtl.Statement.Instance): ir.DefInstance =
    ir.DefInstance(ir.NoInfo, inst.getId, inst.getModuleId)

  def convert(when: Firrtl.Statement.When): ir.Conditionally = {
    val conseq = compressStmts(when.getConsequentList.asScala.map(convert(_)))
    val alt = compressStmts(when.getOtherwiseList.asScala.map(convert(_)))
    ir.Conditionally(ir.NoInfo, convert(when.getPredicate), conseq, alt)
  }

  def convert(cmem: Firrtl.Statement.CMemory): ir.Statement = {
    val vtpe = convert(cmem.getType)
    CDefMemory(ir.NoInfo, cmem.getId, vtpe.tpe, vtpe.size, cmem.getSyncRead)
  }

  import Firrtl.Statement.MemoryPort.Direction._
  def convert(mportdir: Firrtl.Statement.MemoryPort.Direction) = mportdir match {
    case MEMORY_PORT_DIRECTION_INFER => MInfer
    case MEMORY_PORT_DIRECTION_READ => MRead
    case MEMORY_PORT_DIRECTION_WRITE => MWrite
    case MEMORY_PORT_DIRECTION_READ_WRITE => MReadWrite
  }

  def convert(port: Firrtl.Statement.MemoryPort): CDefMPort = {
    val exprs = Seq(convert(port.getMemoryIndex), convert(port.getExpression))
    CDefMPort(ir.NoInfo, port.getId, ir.UnknownType, port.getMemoryId, exprs, convert(port.getDirection))
  }

  def convert(printf: Firrtl.Statement.Printf): ir.Print = {
    val args = printf.getArgList.asScala.map(convert(_))
    val str = ir.StringLit(printf.getValue)
    ir.Print(ir.NoInfo, str, args, convert(printf.getClk), convert(printf.getEn))
  }

  def convert(stop: Firrtl.Statement.Stop): ir.Stop =
    ir.Stop(ir.NoInfo, stop.getReturnValue, convert(stop.getClk), convert(stop.getEn))

  def convert(mem: Firrtl.Statement.Memory): ir.DefMemory = {
    val dtype = convert(mem.getType)
    val rs = mem.getReaderIdList.asScala
    val ws = mem.getWriterIdList.asScala
    val rws = mem.getReadwriterIdList.asScala
    ir.DefMemory(ir.NoInfo, mem.getId, dtype, mem.getDepth, mem.getWriteLatency, mem.getReadLatency,
                 rs, ws, rws, None)
  }

  def convert(stmt: Firrtl.Statement): ir.Statement =
    if (stmt.hasNode) convert(stmt.getNode)
    else if (stmt.hasConnect) convert(stmt.getConnect)
    else if (stmt.hasPartialConnect) convert(stmt.getPartialConnect)
    else if (stmt.hasWire) convert(stmt.getWire)
    else if (stmt.hasRegister) convert(stmt.getRegister)
    else if (stmt.hasWhen) convert(stmt.getWhen)
    else if (stmt.hasInstance) convert(stmt.getInstance)
    else if (stmt.hasPrintf) convert(stmt.getPrintf)
    else if (stmt.hasStop) convert(stmt.getStop)
    else if (stmt.hasMemory) convert(stmt.getMemory)
    else if (stmt.hasIsInvalid) ir.IsInvalid(ir.NoInfo, convert(stmt.getIsInvalid.getExpression))
    else if (stmt.hasCmemory) convert(stmt.getCmemory)
    else if (stmt.hasMemoryPort) convert(stmt.getMemoryPort)
    else throw new Exception(s"Got $stmt")


  def convert(width: Firrtl.Width): ir.Width = {
    if (width.isInitialized) ir.IntWidth(width.getValue)
    else ir.UnknownWidth
  }

  def convert(ut: Firrtl.Type.UIntType): ir.UIntType = {
    val w = if (ut.hasWidth) convert(ut.getWidth) else ir.UnknownWidth
    ir.UIntType(w)
  }
  
  def convert(ut: Firrtl.Type.SIntType): ir.SIntType = {
    val w = if (ut.hasWidth) convert(ut.getWidth) else ir.UnknownWidth
    ir.SIntType(w)
  }

  def convert(field: Firrtl.Type.BundleType.Field): ir.Field = {
    val flip = if (field.getIsFlipped) ir.Flip else ir.Default
    ir.Field(field.getId, flip, convert(field.getType))
  }

  def convert(vtpe: Firrtl.Type.VectorType): ir.VectorType =
    ir.VectorType(convert(vtpe.getType), vtpe.getSize)

  def convert(tpe: Firrtl.Type): ir.Type = {
    if (tpe.hasUintType) convert(tpe.getUintType)
    else if (tpe.hasSintType) convert(tpe.getSintType)
    else if (tpe.hasClockType) ir.ClockType
    else if (tpe.hasBundleType) ir.BundleType(tpe.getBundleType.getFieldList.asScala.map(convert(_)))
    else if (tpe.hasVectorType) convert(tpe.getVectorType)
    else throw new Exception("deleteme")
  }

  def convert(dir: Firrtl.Port.Direction): ir.Direction = {
    dir match {
      case Firrtl.Port.Direction.PORT_DIRECTION_IN => ir.Input
      case Firrtl.Port.Direction.PORT_DIRECTION_OUT => ir.Output
      case unknown =>
        throw ProtoDeserializationException(s"Direction must be Input or Output, got $unknown")
    }
  }

  def convert(port: Firrtl.Port): ir.Port = {
    val dir = convert(port.getDirection)
    val tpe = convert(port.getType)
    ir.Port(ir.NoInfo, port.getId, dir, tpe)
  }

  def convert(module: Firrtl.Module.UserModule): ir.Module = {
    val name = module.getId
    val ports = module.getPortList.asScala.map(convert(_))
    val stmts = module.getStatementList.asScala.map(convert(_))
    ir.Module(ir.NoInfo, name, ports, ir.Block(stmts))
  }

  def convert(module: Firrtl.Module.ExternalModule): ir.ExtModule = {
    val name = module.getId
    val ports = module.getPortList.asScala.map(convert(_))
    ir.ExtModule(ir.NoInfo, name, ports, name, Seq.empty)
  }

  def convert(module: Firrtl.Module): ir.DefModule =
    if (module.hasUserModule) convert(module.getUserModule)
    else {
      require(module.hasExternalModule, "Module must have Module or ExtModule")
      convert(module.getExternalModule)
    }

  def convert(proto: Firrtl): ir.Circuit = {
    require(proto.getCircuitCount == 1, "Only 1 circuit is currently supported")
    val c = proto.getCircuit(0)
    require(c.getTopCount == 1, "Only 1 top is currently supported")
    val modules = c.getModuleList.asScala.map(convert(_))
    val top = c.getTop(0).getName
    ir.Circuit(ir.NoInfo, modules, top)
  }
}
