/*
Copyright (c) 2014 - 2016 The Regents of the University of
California (Regents). All Rights Reserved.  Redistribution and use in
source and binary forms, with or without modification, are permitted
provided that the following conditions are met:
   * Redistributions of source code must retain the above
     copyright notice, this list of conditions and the following
     two paragraphs of disclaimer.
   * Redistributions in binary form must reproduce the above
     copyright notice, this list of conditions and the following
     two paragraphs of disclaimer in the documentation and/or other materials
     provided with the distribution.
   * Neither the name of the Regents nor the names of its contributors
     may be used to endorse or promote products derived from this
     software without specific prior written permission.
IN NO EVENT SHALL REGENTS BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS,
ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
REGENTS HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE. THE SOFTWARE AND ACCOMPANYING DOCUMENTATION, IF
ANY, PROVIDED HEREUNDER IS PROVIDED "AS IS". REGENTS HAS NO OBLIGATION
TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
MODIFICATIONS.
*/

package firrtl

import scala.collection.Seq

// Should this be defined elsewhere?
/*
Structure containing source locator information.
Member of most Stmt case classes.
*/
trait Info
case object NoInfo extends Info {
  override def toString(): String = ""
}
case class FileInfo(info: StringLit) extends Info {
  override def toString(): String = " @[" + info.serialize + "]"
}

class FIRRTLException(str: String) extends Exception(str)

trait AST {
  def serialize: String = firrtl.Serialize.serialize(this)
}

trait HasName {
  def name: String
}
trait HasInfo {
  val info: Info
}
trait HasType {
  def tpe: Type
}
trait IsDeclaration extends HasName with HasType with HasInfo

case class StringLit(array: Array[Byte]) extends AST

trait PrimOp extends AST
case object ADD_OP extends PrimOp 
case object SUB_OP extends PrimOp
case object MUL_OP extends PrimOp
case object DIV_OP extends PrimOp
case object REM_OP extends PrimOp
case object LESS_OP extends PrimOp
case object LESS_EQ_OP extends PrimOp
case object GREATER_OP extends PrimOp
case object GREATER_EQ_OP extends PrimOp
case object EQUAL_OP extends PrimOp
case object NEQUAL_OP extends PrimOp
case object PAD_OP extends PrimOp
case object AS_UINT_OP extends PrimOp
case object AS_SINT_OP extends PrimOp
case object AS_CLOCK_OP extends PrimOp
case object SHIFT_LEFT_OP extends PrimOp
case object SHIFT_RIGHT_OP extends PrimOp
case object DYN_SHIFT_LEFT_OP extends PrimOp
case object DYN_SHIFT_RIGHT_OP extends PrimOp
case object CONVERT_OP extends PrimOp
case object NEG_OP extends PrimOp
case object NOT_OP extends PrimOp
case object AND_OP extends PrimOp
case object OR_OP extends PrimOp
case object XOR_OP extends PrimOp
case object AND_REDUCE_OP extends PrimOp
case object OR_REDUCE_OP extends PrimOp
case object XOR_REDUCE_OP extends PrimOp
case object CONCAT_OP extends PrimOp
case object BITS_SELECT_OP extends PrimOp
case object HEAD_OP extends PrimOp
case object TAIL_OP extends PrimOp

trait Expression extends AST {
  def tpe: Type
}
case class Ref(name: String, tpe: Type) extends Expression with HasName
object Ref {
  def apply(decl: IsDeclaration) = new Ref(decl.name, decl.tpe)
}
case class SubField(exp: Expression, name: String, tpe: Type) extends Expression with HasName
object SubField {
  def apply(expr: Expression, field: DefField) = new SubField(expr, field.name, field.tpe)
}
case class SubIndex(exp: Expression, value: Int, tpe: Type) extends Expression
object SubIndex {
  def apply(expr: Expression, value: Int) = {
    val tpe = expr.tpe match {
      case VectorType(tpe, size) =>
        if (value >= size) throw new FIRRTLException("SubIndex out of bounds!")
        tpe
      case _ => throw new FIRRTLException("Attempting to SubIndex non-VectorType!!!")
    }
    new SubIndex(expr, value, tpe)
  }
}
case class SubAccess(exp: Expression, index: Expression, tpe: Type) extends Expression
object SubAccess {
  def apply(expr: Expression, index: Expression) = {
    val tpe = expr.tpe match {
      case VectorType(tpe, size) => tpe
      case _ => throw new FIRRTLException("Attempting to SubAccess non-VectorType!!!")
    }
    new SubAccess(expr, index, tpe)
  }
}
case class Mux(cond: Expression, tval: Expression, fval: Expression, tpe: Type) extends Expression
object Mux {
  def apply(cond: Expression, tval: Expression, fval: Expression) = {
    val tpe = Utils.mux_type_and_widths(tval.tpe, fval.tpe)
    if (tpe.isInstanceOf[UnknownType])
      throw new FIRRTLException("Attempting to create Mux from Expressions of different type!")
    new Mux(cond, tval, fval, tpe)
  }
}
case class ValidIf(cond: Expression, value: Expression, tpe: Type) extends Expression
object ValidIf {
  def apply(cond: Expression, value: Expression) = new ValidIf(cond, value, value.tpe)
}
case class UIntValue(value: BigInt, width: Width) extends Expression {
  def tpe = UIntType(width)
}
object UIntValue {
  def apply(value: BigInt) = new UIntValue(value, IntWidth(scala.math.max(value.bitLength, 1)))
}
case class SIntValue(value: BigInt, width: Width) extends Expression {
  def tpe = SIntType(width)
}
object SIntValue {
  def apply(value: BigInt) = new SIntValue(value, IntWidth(value.bitLength + 1))
}
case class DoPrim(op: PrimOp, args: Seq[Expression], consts: Seq[BigInt], tpe: Type) extends Expression
object DoPrim {
  def apply(op: PrimOp, args: Seq[Expression], consts: Seq[BigInt]) = {
    val tpe = PrimOps.set_primop_type(new DoPrim(op, args, consts, UnknownType())).tpe
    if (tpe.isInstanceOf[UnknownType])
      throw new FIRRTLException("Attempting to create DoPrim from Expressions of different type!")
    new DoPrim(op, args, consts, tpe)
  }
}

/** Common interface for [[Field]] and [[MemPortField]] */
trait DefField extends AST with HasName {
  def name: String
  def flip: Flip
  def tpe: Type
}

// Memory Port Fields
abstract class MemPortField extends DefField {
  def toField: Field = Field(name, flip, tpe)
}
case class MemPortData(tpe: Type, flip: Flip) extends MemPortField {
  def name = "data"
}
case class MemPortAddress(tpe: Type) extends MemPortField {
  def name = "addr"
  def flip = DEFAULT
}
object MemPortAddress {
  /** Construct a [[MemPortAddress]] from depth of a [[DefMemory]] */
  def apply(memDepth: Int): MemPortAddress = {
    val width = IntWidth(scala.math.max(Utils.ceil_log2(memDepth), 1))
    MemPortAddress(UIntType(width))
  }
}
case object MemPortEnable extends MemPortField {
  def name = "en"
  def flip = DEFAULT
  def tpe = UIntType(IntWidth(1))
}
case object MemPortClock extends MemPortField {
  def name = "clk"
  def flip = DEFAULT
  def tpe = ClockType()
}
// Use constructor in companion object
case class MemPortWriteMask (tpe: Type) extends MemPortField {
  def name = "mask"
  def flip = DEFAULT
}
object MemPortWriteMask {
  /** Construct [[MemPortWriteMask]] from dataType of [[DefMemory]]
    *
    * @note Cannot override apply because of identical signature
    */
  def construct(dataType: Type): MemPortWriteMask =
    new MemPortWriteMask(Utils.create_mask(dataType))
}
case object MemPortWriteMode extends MemPortField {
  def name = "wmode"
  def flip = DEFAULT
  def tpe = UIntType(IntWidth(1))
}

abstract class MemPort extends AST {
  val name: String
  val data: MemPortData
  val addr: MemPortAddress
  val en: MemPortField
  val clk: MemPortField
  def fields: Seq[MemPortField]
  def getType: Type = BundleType(fields map (_.toField))
}
case class ReadPort(
    name: String,
    data: MemPortData,
    addr: MemPortAddress,
    en: MemPortField,
    clk: MemPortField) extends MemPort {
  val fields = Seq(data, addr, en, clk)
}
object ReadPort {
  def apply(name: String, dataType: Type, memDepth: Int) = {
    val data = MemPortData(dataType, REVERSE)
    val addr = MemPortAddress(memDepth)
    val en = MemPortEnable
    val clk = MemPortClock
    new ReadPort(name, data, addr, en, clk)
  }
}
case class WritePort(
    name: String,
    data: MemPortData,
    addr: MemPortAddress,
    en: MemPortField,
    mask: MemPortWriteMask,
    clk: MemPortField) extends MemPort {
  val fields = Seq(data, addr, en, mask, clk)
}
object WritePort {
  def apply(name: String, dataType: Type, memDepth: Int) = {
    val data = MemPortData(dataType, DEFAULT)
    val addr = MemPortAddress(memDepth)
    val en = MemPortEnable
    val mask = MemPortWriteMask.construct(dataType)
    val clk = MemPortClock
    new WritePort(name, data, addr, en, mask, clk)
  }
}
case class ReadWritePort(
    name: String,
    data: MemPortData,
    rdata: MemPortData,
    addr: MemPortAddress,
    en: MemPortField,
    mask: MemPortWriteMask,
    mode: MemPortField,
    clk: MemPortField) extends MemPort {
  val fields = Seq(data, rdata, en, addr, mask, mode, clk)
}
object ReadWritePort {
  def apply(name: String, dataType: Type, memDepth: Int) = {
    val data = MemPortData(dataType, DEFAULT)
    val rdata = MemPortData(dataType, REVERSE)
    val addr = MemPortAddress(memDepth)
    val en = MemPortEnable
    val mask = MemPortWriteMask(dataType)
    val mode = MemPortWriteMode
    val clk = MemPortClock
    new ReadWritePort(name, data, rdata, addr, en, mask, mode, clk)
  }
}

trait Stmt extends AST
case class DefWire(info: Info, name: String, tpe: Type) extends Stmt with IsDeclaration
case class DefPoison(info: Info, name: String, tpe: Type) extends Stmt with IsDeclaration
case class DefRegister(info: Info, name: String, tpe: Type, clock: Expression, reset: Expression, init: Expression) extends Stmt with IsDeclaration
case class DefInstance(info: Info, name: String, module: String, tpe: Type) extends Stmt with IsDeclaration
object DefInstance {
  def apply(info: Info, name: String, module: String, nameMap: Map[String, Module]) = {
    new DefInstance(info, name, module, nameMap(module).tpe)
  }
}
case class DefMemory(
    info: Info,
    name: String,
    data_type: Type,
    depth: Int,
    write_latency: Int,
    read_latency: Int,
    readers: Seq[String],
    writers: Seq[String],
    readwriters: Seq[String]) extends Stmt with IsDeclaration {
  def tpe: Type = {
    val addr = Field("addr",DEFAULT,UIntType(IntWidth(scala.math.max(Utils.ceil_log2(depth), 1))))
    val en = Field("en",DEFAULT,Utils.BoolType())
    val clk = Field("clk",DEFAULT,ClockType())
    val def_data = Field("data",DEFAULT,data_type)
    val rev_data = Field("data",REVERSE,data_type)
    val mask = Field("mask",DEFAULT,Utils.create_mask(data_type))
    val wmode = Field("wmode",DEFAULT,UIntType(IntWidth(1)))
    val rdata = Field("rdata",REVERSE,data_type)
    val read_type = BundleType(Seq(rev_data,addr,en,clk))
    val write_type = BundleType(Seq(def_data,mask,addr,en,clk))
    val readwrite_type = BundleType(Seq(wmode,rdata,def_data,mask,addr,en,clk))

    BundleType(readers.map(x => Field(x,REVERSE,read_type)) ++
               writers.map(x => Field(x,REVERSE,write_type)) ++
               readwriters.map(x => Field(x,REVERSE,readwrite_type)))
  }
}
case class DefNode(info: Info, name: String, value: Expression) extends Stmt with IsDeclaration {
  def tpe = value.tpe
}
case class Conditionally(info: Info, pred: Expression, conseq: Stmt, alt: Stmt) extends Stmt with HasInfo
case class Begin(stmts: Seq[Stmt]) extends Stmt
case class BulkConnect(info: Info, loc: Expression, exp: Expression) extends Stmt with HasInfo
case class Connect(info: Info, loc: Expression, exp: Expression) extends Stmt with HasInfo
case class IsInvalid(info: Info, exp: Expression) extends Stmt with HasInfo
case class Stop(info: Info, ret: Int, clk: Expression, en: Expression) extends Stmt with HasInfo
case class Print(info: Info, string: StringLit, args: Seq[Expression], clk: Expression, en: Expression) extends Stmt with HasInfo
case class Empty() extends Stmt

trait Width extends AST {
  def +(x: Width): Width = (this, x) match {
    case (a: IntWidth, b: IntWidth) => IntWidth(a.width + b.width)
    case _ => UnknownWidth()
  }
  def -(x: Width): Width = (this, x) match {
    case (a: IntWidth, b: IntWidth) => IntWidth(a.width - b.width)
    case _ => UnknownWidth()
  }
  def max(x: Width): Width = (this, x) match {
    case (a: IntWidth, b: IntWidth) => IntWidth(a.width max b.width)
    case _ => UnknownWidth()
  }
  def min(x: Width): Width = (this, x) match {
    case (a: IntWidth, b: IntWidth) => IntWidth(a.width min b.width)
    case _ => UnknownWidth()
  }
}
case class IntWidth(width: BigInt) extends Width 
case class UnknownWidth() extends Width

trait Flip extends AST
case object DEFAULT extends Flip
case object REVERSE extends Flip

case class Field(name: String, flip: Flip, tpe: Type) extends DefField

trait Type extends AST
case class UIntType(width: Width) extends Type
case class SIntType(width: Width) extends Type
case class BundleType(fields: Seq[Field]) extends Type
case class VectorType(tpe: Type, size: Int) extends Type
case class ClockType() extends Type
case class UnknownType() extends Type

trait Direction extends AST {
  def toFlip: Flip
}
case object INPUT extends Direction {
  def toFlip = REVERSE
}
case object OUTPUT extends Direction {
  def toFlip = DEFAULT
}

case class Port(info: Info, name: String, direction: Direction, tpe: Type) extends AST with IsDeclaration {
  def toField: Field = Field(name, direction.toFlip, tpe)
}

trait Module extends AST with IsDeclaration {
  val info : Info
  val name : String
  val ports : Seq[Port]
  def tpe: Type = BundleType(ports map (_.toField))
}
case class InModule(info: Info, name: String, ports: Seq[Port], body: Stmt) extends Module
case class ExModule(info: Info, name: String, ports: Seq[Port]) extends Module

case class Circuit(info: Info, modules: Seq[Module], main: String) extends AST with HasInfo

