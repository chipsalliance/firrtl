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

// Should this be defined elsewhere?
/*
Structure containing source locator information.
Member of most Statement case classes.
*/
abstract class Info
case object NoInfo extends Info {
  override def toString(): String = "NoFileInfo"
}
case class FileInfo(file: String, line: Int, column: Int) extends Info {
  override def toString(): String = s"$file@$line.$column"
}

class FIRRTLException(str: String) extends Exception(str)

abstract class FIRRTLNode {
  def serialize: String = firrtl.Serialize.serialize(this)
}

trait HasName {
  val name: String
}
trait IsDeclaration extends HasName

case class StringLit(array: Array[Byte]) extends FIRRTLNode

abstract class PrimOp extends FIRRTLNode
case object AddOp extends PrimOp
case object SubOp extends PrimOp
case object MulOp extends PrimOp
case object DivOp extends PrimOp
case object RemOp extends PrimOp
case object LtOp extends PrimOp
case object LeqOp extends PrimOp
case object GtOp extends PrimOp
case object GeqOp extends PrimOp
case object EqOp extends PrimOp
case object NeqOp extends PrimOp
case object PadOp extends PrimOp
case object AsUIntOp extends PrimOp
case object AsSIntOp extends PrimOp
case object AsClockOp extends PrimOp
case object ShlOp extends PrimOp
case object ShrOp extends PrimOp
case object DshlOp extends PrimOp
case object DshrOp extends PrimOp
case object CvtOp extends PrimOp
case object NegOp extends PrimOp
case object NotOp extends PrimOp
case object AndOp extends PrimOp
case object OrOp extends PrimOp
case object XorOp extends PrimOp
case object AndrOp extends PrimOp
case object OrrOp extends PrimOp
case object XorrOp extends PrimOp
case object CatOp extends PrimOp
case object BitsOp extends PrimOp
case object HeadOp extends PrimOp
case object TailOp extends PrimOp

abstract class Expression extends FIRRTLNode {
  def tpe: Type
}
case class Reference(name: String, tpe: Type) extends Expression with HasName
case class SubField(expr: Expression, name: String, tpe: Type) extends Expression with HasName
case class SubIndex(expr: Expression, value: Int, tpe: Type) extends Expression
case class SubAccess(expr: Expression, index: Expression, tpe: Type) extends Expression
case class Mux(cond: Expression, tval: Expression, fval: Expression, tpe: Type) extends Expression
case class ValidIf(cond: Expression, value: Expression, tpe: Type) extends Expression
abstract class Literal extends Expression {
  val value: BigInt
  val width: Width
  def tpe: Type
}
case class UIntLiteral(value: BigInt, width: Width) extends Literal {
  def tpe = UIntType(width)
}
case class SIntLiteral(value: BigInt, width: Width) extends Literal {
  def tpe = SIntType(width)
}
case class DoPrim(op: PrimOp, args: Seq[Expression], consts: Seq[BigInt], tpe: Type) extends Expression

// Mem Ports/Fields
case class MemoryPortField(name: String, orientation: Orientation, tpe: Type) extends FIRRTLNode
case class MemoryPortData(tpe: Type, orientation: Orientation) extends MemoryPortField("data", orientation, tpe)
case class MemoryPortAddress(tpe: Type) extends MemoryPortField("addr", Default, tpe)
case object MemoryPortEnable extends MemoryField("en", Default, UIntType(IntWidth(1)))
case object MemoryPortClock extends MemoryField("clk", Default, ClockType)
case class MemoryPortWriteMask(tpe: Type) extends MemoryField("mask", Default, tpe)
case object MemoryPortWriteMode extends MemoryField("wmode", Default, UIntType(IntWidth(1)))

// ********** Memory Port 1 **********
abstract class MemoryPort extends FIRRTLNode {
  val name: String
  val data: MemoryPortData
  val addr: MemoryPortAddress
  val en: MemoryPortEnable
  val clk: MemoryPortClock
}
case class Reader(
    name: String,
    data: MemoryPortData,
    addr: MemoryPortAddress,
    en: MemoryPortEnable,
    clk: MemoryPortClock) extends MemoryPort
case class Writer(
    name: String,
    data: MemoryPortData,
    mask: MemoryPortWriteMask,
    addr: MemoryPortAddress,
    en: MemoryPortEnable,
    clk: MemoryPortClock) extends MemoryPort
case class ReadWriter(
    name: String,
    data: MemoryPortData,
    mask: MemoryPortWriteMask,
    addr: MemoryPortAddress,
    en: MemoryPortEnable,
    mode: MemoryPortWriteMode,
    clk: MemoryPortClock) extends MemoryPort

// ********** Memory Port 2 **********
abstract class MemoryPortType extends FIRRTLNode
case object Reader extends MemoryPortType
case object Writer extends MemoryPortType
case object ReadWriter extends MemoryPortType
case class MemoryPort(name: String, tpe: MemoryPortType, fields: Seq[MemoryPortField]) extends FIRRTLNode

abstract class Statement extends FIRRTLNode
case class DefWire(info: Info, name: String, tpe: Type) extends Statement with IsDeclaration
case class DefRegister(
    info: Info,
    name: String,
    tpe: Type,
    clock: Expression,
    reset: Expression,
    init: Expression) extends Statement with IsDeclaration
case class DefInstance(info: Info, name: String, module: String) extends Statement with IsDeclaration
case class DefMemory(
    info: Info,
    name: String,
    dataType: Type,
    depth: Int,
    writeLatency: Int,
    readLatency: Int,
    ports: Seq[MemoryPort]) extends Statement with IsDeclaration
case class DefNode(info: Info, name: String, value: Expression) extends Statement with IsDeclaration
case class Conditionally(info: Info, pred: Expression, conseq: Statement, alt: Statement) extends Statement
case class Begin(stmts: Seq[Statement]) extends Statement
case class PartialConnect(info: Info, loc: Expression, expr: Expression) extends Statement
case class Connect(info: Info, loc: Expression, expr: Expression) extends Statement
case class IsInvalid(info: Info, expr: Expression) extends Statement
case class Stop(info: Info, ret: Int, clk: Expression, en: Expression) extends Statement
case class Print(
    info: Info,
    string: StringLit,
    args: Seq[Expression],
    clk: Expression,
    en: Expression) extends Statement
case object EmptyStatement extends Statement

abstract class Width extends FIRRTLNode {
  def +(x: Width): Width = (this, x) match {
    case (a: IntWidth, b: IntWidth) => IntWidth(a.width + b.width)
    case _ => UnknownWidth
  }
  def -(x: Width): Width = (this, x) match {
    case (a: IntWidth, b: IntWidth) => IntWidth(a.width - b.width)
    case _ => UnknownWidth
  }
  def max(x: Width): Width = (this, x) match {
    case (a: IntWidth, b: IntWidth) => IntWidth(a.width max b.width)
    case _ => UnknownWidth
  }
  def min(x: Width): Width = (this, x) match {
    case (a: IntWidth, b: IntWidth) => IntWidth(a.width min b.width)
    case _ => UnknownWidth
  }
}
case class IntWidth(width: BigInt) extends Width
case object UnknownWidth extends Width

abstract class Orientation extends FIRRTLNode
case object Default extends Orientation
case object Reverse extends Orientation

case class Field(name: String, orientation: Orientation, tpe: Type) extends FIRRTLNode with IsDeclaration // ?

abstract class Type extends FIRRTLNode
abstract class GroundType extends Type {
  val width: Width
}
abstract class AggregateType extends Type
case class UIntType(width: Width) extends GroundType
case class SIntType(width: Width) extends GroundType
case class BundleType(fields: Seq[Field]) extends AggregateType
case class VectorType(tpe: Type, size: Int) extends AggregateType
case object ClockType extends GroundType {
  def width = IntWidth(1)
}
case object UnknownType extends Type

abstract class Direction extends FIRRTLNode
case object Input extends Direction
case object Output extends Direction

case class Port(info: Info, name: String, direction: Direction, tpe: Type) extends FIRRTLNode with IsDeclaration

abstract class Module extends FIRRTLNode with IsDeclaration {
  val info : Info
  val name : String
  val ports : Seq[Port]
}
case class InModule(info: Info, name: String, ports: Seq[Port], body: Statement) extends Module
case class ExModule(info: Info, name: String, ports: Seq[Port]) extends Module

case class Circuit(info: Info, modules: Seq[Module], main: String) extends FIRRTLNode

