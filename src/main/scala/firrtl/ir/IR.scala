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
package ir

import firrtl.Utils.{max, min, pow_minus_one}

import com.typesafe.scalalogging.LazyLogging

import scala.language.postfixOps

import Utils.indent
import scala.language.postfixOps

/** Intermediate Representation */
sealed abstract class FirrtlNode {
  def serialize: String
}

sealed abstract class Info extends FirrtlNode {
  // default implementation
  def serialize: String = this.toString
}

/**
  * Primitive Operation
  */
sealed abstract class PrimOp extends FirrtlNode {
  def serialize: String = this.toString
}

// Resultant width is the same as the maximum input width
case object Addw extends PrimOp { override def toString = "addw" }
// Resultant width is the same as the maximum input width
case object Subw extends PrimOp { override def toString = "subw" }
// Resultant width is the same as input argument width
case object Dshlw extends PrimOp { override def toString = "dshlw" }
// Resultant width is the same as input argument width
case object Shlw extends PrimOp { override def toString = "shlw" }


/** Definitions and Utility functions for [[ir.PrimOp]]s */
object PrimOps extends LazyLogging {
  /** Addition */
  case object Add extends PrimOp { override def toString = "add" }
  /** Subtraction */
  case object Sub extends PrimOp { override def toString = "sub" }
  /** Multiplication */
  case object Mul extends PrimOp { override def toString = "mul" }
  /** Division */
  case object Div extends PrimOp { override def toString = "div" }
  /** Remainder */
  case object Rem extends PrimOp { override def toString = "rem" }
  /** Less Than */
  case object Lt extends PrimOp { override def toString = "lt" }
  /** Less Than Or Equal To */
  case object Leq extends PrimOp { override def toString = "leq" }
  /** Greater Than */
  case object Gt extends PrimOp { override def toString = "gt" }
  /** Greater Than Or Equal To */
  case object Geq extends PrimOp { override def toString = "geq" }
  /** Equal To */
  case object Eq extends PrimOp { override def toString = "eq" }
  /** Not Equal To */
  case object Neq extends PrimOp { override def toString = "neq" }
  /** Padding */
  case object Pad extends PrimOp { override def toString = "pad" }
  /** Interpret As UInt */
  case object AsUInt extends PrimOp { override def toString = "asUInt" }
  /** Interpret As SInt */
  case object AsSInt extends PrimOp { override def toString = "asSInt" }
  /** Interpret As Clock */
  case object AsClock extends PrimOp { override def toString = "asClock" }
  /** Static Shift Left */
  case object Shl extends PrimOp { override def toString = "shl" }
  /** Static Shift Right */
  case object Shr extends PrimOp { override def toString = "shr" }
  /** Dynamic Shift Left */
  case object Dshl extends PrimOp { override def toString = "dshl" }
  /** Dynamic Shift Right */
  case object Dshr extends PrimOp { override def toString = "dshr" }
  /** Arithmetic Convert to Signed */
  case object Cvt extends PrimOp { override def toString = "cvt" }
  /** Negate */
  case object Neg extends PrimOp { override def toString = "neg" }
  /** Bitwise Complement */
  case object Not extends PrimOp { override def toString = "not" }
  /** Bitwise And */
  case object And extends PrimOp { override def toString = "and" }
  /** Bitwise Or */
  case object Or extends PrimOp { override def toString = "or" }
  /** Bitwise Exclusive Or */
  case object Xor extends PrimOp { override def toString = "xor" }
  /** Bitwise And Reduce */
  case object Andr extends PrimOp { override def toString = "andr" }
  /** Bitwise Or Reduce */
  case object Orr extends PrimOp { override def toString = "orr" }
  /** Bitwise Exclusive Or Reduce */
  case object Xorr extends PrimOp { override def toString = "xorr" }
  /** Concatenate */
  case object Cat extends PrimOp { override def toString = "cat" }
  /** Bit Extraction */
  case object Bits extends PrimOp { override def toString = "bits" }
  /** Head */
  case object Head extends PrimOp { override def toString = "head" }
  /** Tail */
  case object Tail extends PrimOp { override def toString = "tail" }

  private lazy val builtinPrimOps: Seq[PrimOp] =
    Seq(Add, Sub, Mul, Div, Rem, Lt, Leq, Gt, Geq, Eq, Neq, Pad, AsUInt, AsSInt, AsClock, Shl, Shr,
        Dshl, Dshr, Neg, Cvt, Not, And, Or, Xor, Andr, Orr, Xorr, Cat, Bits, Head, Tail)
  private lazy val strToPrimOp: Map[String, PrimOp] = builtinPrimOps map (op => op.toString -> op) toMap

  /** Seq of String representations of [[ir.PrimOp]]s */
  lazy val listing: Seq[String] = builtinPrimOps map (_.toString)
  /** Gets the corresponding [[ir.PrimOp]] from its String representation */
  def fromString(op: String): PrimOp = strToPrimOp(op)

  // Borrowed from Stanza implementation
   def set_primop_type (e:DoPrim) : DoPrim = {
      //println-all(["Inferencing primop type: " e])
      def PLUS (w1:Width,w2:Width) : Width = (w1, w2) match {
        case (IntWidth(i), IntWidth(j)) => IntWidth(i + j)
        case _ => PlusWidth(w1,w2)
      }
      def MAX (w1:Width,w2:Width) : Width = (w1, w2) match {
        case (IntWidth(i), IntWidth(j)) => IntWidth(max(i,j))
        case _ => MaxWidth(Seq(w1,w2))
      }
      def MINUS (w1:Width,w2:Width) : Width = (w1, w2) match {
        case (IntWidth(i), IntWidth(j)) => IntWidth(i - j)
        case _ => MinusWidth(w1,w2)
      }
      def POW (w1:Width) : Width = w1 match {
        case IntWidth(i) => IntWidth(pow_minus_one(BigInt(2), i))
        case _ => ExpWidth(w1)
      }
      def MIN (w1:Width,w2:Width) : Width = (w1, w2) match {
        case (IntWidth(i), IntWidth(j)) => IntWidth(min(i,j))
        case _ => MinWidth(Seq(w1,w2))
      }
      val o = e.op
      val a = e.args
      val c = e.consts
      def t1 () = a(0).tpe
      def t2 () = a(1).tpe
      def t3 () = a(2).tpe
      def w1 () = Utils.widthBANG(a(0).tpe)
      def w2 () = Utils.widthBANG(a(1).tpe)
      def w3 () = Utils.widthBANG(a(2).tpe)
      def c1 () = IntWidth(c(0))
      def c2 () = IntWidth(c(1))
      o match {
         case Add => {
            val t = (t1(),t2()) match {
               case (t1:UIntType, t2:UIntType) => UIntType(PLUS(MAX(w1(),w2()),Utils.ONE))
               case (t1:UIntType, t2:SIntType) => SIntType(PLUS(MAX(w1(),w2()),Utils.ONE))
               case (t1:SIntType, t2:UIntType) => SIntType(PLUS(MAX(w1(),w2()),Utils.ONE))
               case (t1:SIntType, t2:SIntType) => SIntType(PLUS(MAX(w1(),w2()),Utils.ONE))
               case (t1, t2) => UnknownType
            }
            DoPrim(o,a,c,t)
         }
         case Sub => {
            val t = (t1(),t2()) match {
               case (t1:UIntType, t2:UIntType) => SIntType(PLUS(MAX(w1(),w2()),Utils.ONE))
               case (t1:UIntType, t2:SIntType) => SIntType(PLUS(MAX(w1(),w2()),Utils.ONE))
               case (t1:SIntType, t2:UIntType) => SIntType(PLUS(MAX(w1(),w2()),Utils.ONE))
               case (t1:SIntType, t2:SIntType) => SIntType(PLUS(MAX(w1(),w2()),Utils.ONE))
               case (t1, t2) => UnknownType
            }
            DoPrim(o,a,c,t)
         }
         case Mul => {
            val t = (t1(),t2()) match {
               case (t1:UIntType, t2:UIntType) => UIntType(PLUS(w1(),w2()))
               case (t1:UIntType, t2:SIntType) => SIntType(PLUS(w1(),w2()))
               case (t1:SIntType, t2:UIntType) => SIntType(PLUS(w1(),w2()))
               case (t1:SIntType, t2:SIntType) => SIntType(PLUS(w1(),w2()))
               case (t1, t2) => UnknownType
            }
            DoPrim(o,a,c,t)
         }
         case Div => {
            val t = (t1(),t2()) match {
               case (t1:UIntType, t2:UIntType) => UIntType(w1())
               case (t1:UIntType, t2:SIntType) => SIntType(PLUS(w1(),Utils.ONE))
               case (t1:SIntType, t2:UIntType) => SIntType(w1())
               case (t1:SIntType, t2:SIntType) => SIntType(PLUS(w1(),Utils.ONE))
               case (t1, t2) => UnknownType
            }
            DoPrim(o,a,c,t)
         }
         case Rem => {
            val t = (t1(),t2()) match {
               case (t1:UIntType, t2:UIntType) => UIntType(MIN(w1(),w2()))
               case (t1:UIntType, t2:SIntType) => UIntType(MIN(w1(),w2()))
               case (t1:SIntType, t2:UIntType) => SIntType(MIN(w1(),PLUS(w2(),Utils.ONE)))
               case (t1:SIntType, t2:SIntType) => SIntType(MIN(w1(),w2()))
               case (t1, t2) => UnknownType
            }
            DoPrim(o,a,c,t)
         }
         case Lt => {
            val t = (t1(),t2()) match {
               case (t1:UIntType, t2:UIntType) => Utils.BoolType
               case (t1:SIntType, t2:UIntType) => Utils.BoolType
               case (t1:UIntType, t2:SIntType) => Utils.BoolType
               case (t1:SIntType, t2:SIntType) => Utils.BoolType
               case (t1, t2) => UnknownType
            }
            DoPrim(o,a,c,t)
         }
         case Leq => {
            val t = (t1(),t2()) match {
               case (t1:UIntType, t2:UIntType) => Utils.BoolType
               case (t1:SIntType, t2:UIntType) => Utils.BoolType
               case (t1:UIntType, t2:SIntType) => Utils.BoolType
               case (t1:SIntType, t2:SIntType) => Utils.BoolType
               case (t1, t2) => UnknownType
            }
            DoPrim(o,a,c,t)
         }
         case Gt => {
            val t = (t1(),t2()) match {
               case (t1:UIntType, t2:UIntType) => Utils.BoolType
               case (t1:SIntType, t2:UIntType) => Utils.BoolType
               case (t1:UIntType, t2:SIntType) => Utils.BoolType
               case (t1:SIntType, t2:SIntType) => Utils.BoolType
               case (t1, t2) => UnknownType
            }
            DoPrim(o,a,c,t)
         }
         case Geq => {
            val t = (t1(),t2()) match {
               case (t1:UIntType, t2:UIntType) => Utils.BoolType
               case (t1:SIntType, t2:UIntType) => Utils.BoolType
               case (t1:UIntType, t2:SIntType) => Utils.BoolType
               case (t1:SIntType, t2:SIntType) => Utils.BoolType
               case (t1, t2) => UnknownType
            }
            DoPrim(o,a,c,t)
         }
         case Eq => {
            val t = (t1(),t2()) match {
               case (t1:UIntType, t2:UIntType) => Utils.BoolType
               case (t1:SIntType, t2:UIntType) => Utils.BoolType
               case (t1:UIntType, t2:SIntType) => Utils.BoolType
               case (t1:SIntType, t2:SIntType) => Utils.BoolType
               case (t1, t2) => UnknownType
            }
            DoPrim(o,a,c,t)
         }
         case Neq => {
            val t = (t1(),t2()) match {
               case (t1:UIntType, t2:UIntType) => Utils.BoolType
               case (t1:SIntType, t2:UIntType) => Utils.BoolType
               case (t1:UIntType, t2:SIntType) => Utils.BoolType
               case (t1:SIntType, t2:SIntType) => Utils.BoolType
               case (t1, t2) => UnknownType
            }
            DoPrim(o,a,c,t)
         }
         case Pad => {
            val t = (t1()) match {
               case (t1:UIntType) => UIntType(MAX(w1(),c1()))
               case (t1:SIntType) => SIntType(MAX(w1(),c1()))
               case (t1) => UnknownType
            }
            DoPrim(o,a,c,t)
         }
         case AsUInt => {
            val t = (t1()) match {
               case (t1:UIntType) => UIntType(w1())
               case (t1:SIntType) => UIntType(w1())
               case ClockType => UIntType(Utils.ONE)
               case (t1) => UnknownType
            }
            DoPrim(o,a,c,t)
         }
         case AsSInt => {
            val t = (t1()) match {
               case (t1:UIntType) => SIntType(w1())
               case (t1:SIntType) => SIntType(w1())
               case ClockType => SIntType(Utils.ONE)
               case (t1) => UnknownType
            }
            DoPrim(o,a,c,t)
         }
         case AsClock => {
            val t = (t1()) match {
               case (t1:UIntType) => ClockType
               case (t1:SIntType) => ClockType
               case ClockType => ClockType
               case (t1) => UnknownType
            }
            DoPrim(o,a,c,t)
         }
         case Shl => {
            val t = (t1()) match {
               case (t1:UIntType) => UIntType(PLUS(w1(),c1()))
               case (t1:SIntType) => SIntType(PLUS(w1(),c1()))
               case (t1) => UnknownType
            }
            DoPrim(o,a,c,t)
         }
         case Shr => {
            val t = (t1()) match {
               case (t1:UIntType) => UIntType(MAX(MINUS(w1(),c1()),Utils.ONE))
               case (t1:SIntType) => SIntType(MAX(MINUS(w1(),c1()),Utils.ONE))
               case (t1) => UnknownType
            }
            DoPrim(o,a,c,t)
         }
         case Dshl => {
            val t = (t1()) match {
               case (t1:UIntType) => UIntType(PLUS(w1(),POW(w2())))
               case (t1:SIntType) => SIntType(PLUS(w1(),POW(w2())))
               case (t1) => UnknownType
            }
            DoPrim(o,a,c,t)
         }
         case Dshr => {
            val t = (t1()) match {
               case (t1:UIntType) => UIntType(w1())
               case (t1:SIntType) => SIntType(w1())
               case (t1) => UnknownType
            }
            DoPrim(o,a,c,t)
         }
         case Cvt => {
            val t = (t1()) match {
               case (t1:UIntType) => SIntType(PLUS(w1(),Utils.ONE))
               case (t1:SIntType) => SIntType(w1())
               case (t1) => UnknownType
            }
            DoPrim(o,a,c,t)
         }
         case Neg => {
            val t = (t1()) match {
               case (t1:UIntType) => SIntType(PLUS(w1(),Utils.ONE))
               case (t1:SIntType) => SIntType(PLUS(w1(),Utils.ONE))
               case (t1) => UnknownType
            }
            DoPrim(o,a,c,t)
         }
         case Not => {
            val t = (t1()) match {
               case (t1:UIntType) => UIntType(w1())
               case (t1:SIntType) => UIntType(w1())
               case (t1) => UnknownType
            }
            DoPrim(o,a,c,t)
         }
         case And => {
            val t = (t1(),t2()) match {
               case (_:SIntType|_:UIntType, _:SIntType|_:UIntType) => UIntType(MAX(w1(),w2()))
               case (t1,t2) => UnknownType
            }
            DoPrim(o,a,c,t)
         }
         case Or => {
            val t = (t1(),t2()) match {
               case (_:SIntType|_:UIntType, _:SIntType|_:UIntType) => UIntType(MAX(w1(),w2()))
               case (t1,t2) => UnknownType
            }
            DoPrim(o,a,c,t)
         }
         case Xor => {
            val t = (t1(),t2()) match {
               case (_:SIntType|_:UIntType, _:SIntType|_:UIntType) => UIntType(MAX(w1(),w2()))
               case (t1,t2) => UnknownType
            }
            DoPrim(o,a,c,t)
         }
         case Andr => {
            val t = (t1()) match {
               case (_:UIntType|_:SIntType) => Utils.BoolType
               case (t1) => UnknownType
            }
            DoPrim(o,a,c,t)
         }
         case Orr => {
            val t = (t1()) match {
               case (_:UIntType|_:SIntType) => Utils.BoolType
               case (t1) => UnknownType
            }
            DoPrim(o,a,c,t)
         }
         case Xorr => {
            val t = (t1()) match {
               case (_:UIntType|_:SIntType) => Utils.BoolType
               case (t1) => UnknownType
            }
            DoPrim(o,a,c,t)
         }
         case Cat => {
            val t = (t1(),t2()) match {
               case (_:UIntType|_:SIntType,_:UIntType|_:SIntType) => UIntType(PLUS(w1(),w2()))
               case (t1, t2) => UnknownType
            }
            DoPrim(o,a,c,t)
         }
         case Bits => {
            val t = (t1()) match {
               case (_:UIntType|_:SIntType) => UIntType(PLUS(MINUS(c1(),c2()),Utils.ONE))
               case (t1) => UnknownType
            }
            DoPrim(o,a,c,t)
         }
         case Head => {
            val t = (t1()) match {
               case (_:UIntType|_:SIntType) => UIntType(c1())
               case (t1) => UnknownType
            }
            DoPrim(o,a,c,t)
         }
         case Tail => {
            val t = (t1()) match {
               case (_:UIntType|_:SIntType) => UIntType(MINUS(w1(),c1()))
               case (t1) => UnknownType
            }
            DoPrim(o,a,c,t)
         }
     }
   }
}
 
sealed case class VRandom(width: BigInt) extends Expression {
  def tpe = UIntType(IntWidth(width))
  def nWords = (width + 31) / 32
  def realWidth = nWords * 32
  def serialize: String = "RANDOM"
}

sealed abstract class MPortDir extends FirrtlNode
case object MInfer extends MPortDir {
  def serialize: String = "infer"
}
case object MRead extends MPortDir {
  def serialize: String = "read"
}
case object MWrite extends MPortDir {
  def serialize: String = "write"
}
case object MReadWrite extends MPortDir {
  def serialize: String = "rdwr"
}

case object NoInfo extends Info {
  override def toString: String = ""
}
sealed case class FileInfo(info: StringLit) extends Info {
  override def toString: String = " @[" + info.serialize + "]"
}

sealed trait HasName {
  val name: String
}
sealed trait HasInfo {
  val info: Info
}
sealed trait IsDeclaration extends HasName with HasInfo

sealed case class StringLit(array: Array[Byte]) extends FirrtlNode {
  def serialize: String = FIRRTLStringLitHandler.escape(this)
}


sealed abstract class Expression extends FirrtlNode {
  def tpe: Type
}
sealed case class Reference(name: String, tpe: Type) extends Expression with HasName {
  def serialize: String = name
}
sealed case class SubField(expr: Expression, name: String, tpe: Type) extends Expression with HasName {
  def serialize: String = s"${expr.serialize}.$name"
}
sealed case class SubIndex(expr: Expression, value: Int, tpe: Type) extends Expression {
  def serialize: String = s"${expr.serialize}[$value]"
}
sealed case class SubAccess(expr: Expression, index: Expression, tpe: Type) extends Expression {
  def serialize: String = s"${expr.serialize}[${index.serialize}]"
}
sealed case class Mux(cond: Expression, tval: Expression, fval: Expression, tpe: Type) extends Expression {
  def serialize: String = s"mux(${cond.serialize}, ${tval.serialize}, ${fval.serialize})"
}
sealed case class ValidIf(cond: Expression, value: Expression, tpe: Type) extends Expression {
  def serialize: String = s"validif(${cond.serialize}, ${value.serialize})"
}
sealed case class WRef(name: String, tpe: Type, kind: Kind, gender: Gender) extends Expression {
  def serialize: String = name
}
sealed case class WSubField(exp: Expression, name: String, tpe: Type, gender: Gender) extends Expression {
  def serialize: String = s"${exp.serialize}.$name"
}
sealed case class WSubIndex(exp: Expression, value: Int, tpe: Type, gender: Gender) extends Expression {
  def serialize: String = s"${exp.serialize}[$value]"
}
sealed case class WSubAccess(exp: Expression, index: Expression, tpe: Type, gender: Gender) extends Expression {
  def serialize: String = s"${exp.serialize}[${index.serialize}]"
}
sealed case class WVoid() extends Expression {
  def tpe = UnknownType
  def serialize: String = "VOID"
}
sealed case class WInvalid() extends Expression {
  def tpe = UnknownType
  def serialize: String = "INVALID"
}
// Useful for splitting then remerging references
case object EmptyExpression extends Expression {
  def tpe = UnknownType
  def serialize: String = "EMPTY"
}
sealed case class WDefInstance(info: Info, name: String, module: String, tpe: Type) extends Statement with IsDeclaration {
  def serialize: String = s"inst $name of $module" + info.serialize
}

sealed abstract class Literal extends Expression {
  val value: BigInt
  val width: Width
}
sealed case class UIntLiteral(value: BigInt, width: Width) extends Literal {
  def tpe = UIntType(width)
  def serialize = s"UInt${width.serialize}(" + Utils.serialize(value) + ")"
}
sealed case class SIntLiteral(value: BigInt, width: Width) extends Literal {
  def tpe = SIntType(width)
  def serialize = s"SInt${width.serialize}(" + Utils.serialize(value) + ")"
}
sealed case class DoPrim(op: PrimOp, args: Seq[Expression], consts: Seq[BigInt], tpe: Type) extends Expression {
  def serialize: String = op.serialize + "(" +
    (args.map(_.serialize) ++ consts.map(_.toString)).mkString(", ") + ")"
}

sealed abstract class Statement extends FirrtlNode
sealed case class DefWire(info: Info, name: String, tpe: Type) extends Statement with IsDeclaration {
  def serialize: String = s"wire $name : ${tpe.serialize}" + info.serialize
}
sealed case class DefRegister(
    info: Info,
    name: String,
    tpe: Type,
    clock: Expression,
    reset: Expression,
    init: Expression) extends Statement with IsDeclaration {
  def serialize: String =
    s"reg $name : ${tpe.serialize}, ${clock.serialize} with :" +
    indent("\n" + s"reset => (${reset.serialize}, ${init.serialize})" + info.serialize)

}
sealed case class DefInstance(info: Info, name: String, module: String) extends Statement with IsDeclaration {
  def serialize: String = s"inst $name of $module" + info.serialize
}
sealed case class DefMemory(
    info: Info,
    name: String,
    dataType: Type,
    depth: Int,
    writeLatency: Int,
    readLatency: Int,
    readers: Seq[String],
    writers: Seq[String],
    readwriters: Seq[String],
    // TODO: handle read-under-write
    readUnderWrite: Option[String] = None) extends Statement with IsDeclaration {
  def serialize: String =
    s"mem $name :" + info.serialize +
    indent(
      (Seq("\ndata-type => " + dataType.serialize,
          "depth => " + depth,
          "read-latency => " + readLatency,
          "write-latency => " + writeLatency) ++
          (readers map ("reader => " + _)) ++
          (writers map ("writer => " + _)) ++
          (readwriters map ("readwriter => " + _)) ++
       Seq("read-under-write => undefined")) mkString "\n")
}
sealed case class DefNode(info: Info, name: String, value: Expression) extends Statement with IsDeclaration {
  def serialize: String = s"node $name = ${value.serialize}" + info.serialize
}
sealed case class Conditionally(
    info: Info,
    pred: Expression,
    conseq: Statement,
    alt: Statement) extends Statement with HasInfo {
  def serialize: String =
    s"when ${pred.serialize} :" + info.serialize +
    indent("\n" + conseq.serialize) +
    (if (alt == EmptyStmt) ""
    else "\nelse :" + indent("\n" + alt.serialize))
}
sealed case class Block(stmts: Seq[Statement]) extends Statement {
  def serialize: String = stmts map (_.serialize) mkString "\n"
}
sealed case class PartialConnect(info: Info, loc: Expression, expr: Expression) extends Statement with HasInfo {
  def serialize: String =  s"${loc.serialize} <- ${expr.serialize}" + info.serialize
}
sealed case class Connect(info: Info, loc: Expression, expr: Expression) extends Statement with HasInfo {
  def serialize: String =  s"${loc.serialize} <= ${expr.serialize}" + info.serialize
}
sealed case class IsInvalid(info: Info, expr: Expression) extends Statement with HasInfo {
  def serialize: String =  s"${expr.serialize} is invalid" + info.serialize
}
sealed case class Stop(info: Info, ret: Int, clk: Expression, en: Expression) extends Statement with HasInfo {
  def serialize: String = s"stop(${clk.serialize}, ${en.serialize}, $ret)" + info.serialize
}
sealed case class Print(
    info: Info,
    string: StringLit,
    args: Seq[Expression],
    clk: Expression,
    en: Expression) extends Statement with HasInfo {
  def serialize: String = {
    val strs = Seq(clk.serialize, en.serialize, ("\"" + string.serialize + "\"")) ++
               (args map (_.serialize))
    "printf(" + (strs mkString ", ") + ")" + info.serialize
  }
}
case object EmptyStmt extends Statement {
  def serialize: String = "skip"
}

sealed case class CDefMemory(
    info: Info,
    name: String,
    tpe: Type,
    size: Int,
    seq: Boolean) extends Statement {
  def serialize: String = (if (seq) "smem" else "cmem") +
    s" $name : ${tpe.serialize} [$size]" + info.serialize
}
sealed case class CDefMPort(info: Info,
    name: String,
    tpe: Type,
    mem: String,
    exps: Seq[Expression],
    direction: MPortDir) extends Statement {
  def serialize: String = {
    val dir = direction.serialize
    s"$dir mport $name = $mem[${exps(0).serialize}], ${exps(1).serialize}" + info.serialize
  }
}

sealed abstract class Width extends FirrtlNode {
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
sealed case class VarWidth(name: String) extends Width {
  def serialize: String = name
}
sealed case class PlusWidth(arg1: Width, arg2: Width) extends Width {
  def serialize: String = "(" + arg1.serialize + " + " + arg2.serialize + ")"
}
sealed case class MinusWidth(arg1: Width, arg2: Width) extends Width {
  def serialize: String = "(" + arg1.serialize + " - " + arg2.serialize + ")"
}
sealed case class MaxWidth(args: Seq[Width]) extends Width {
  def serialize: String = args map (_.serialize) mkString ("max(", ", ", ")")
}
sealed case class MinWidth(args: Seq[Width]) extends Width {
  def serialize: String = args map (_.serialize) mkString ("min(", ", ", ")")
}
sealed case class ExpWidth(arg1: Width) extends Width {
  def serialize: String = "exp(" + arg1.serialize + " )"
}

/** Positive Integer Bit Width of a [[GroundType]] */
sealed case class IntWidth(width: BigInt) extends Width {
  def serialize: String = s"<$width>"
}
case object UnknownWidth extends Width {
  def serialize: String = ""
}

/** Orientation of [[Field]] */
sealed abstract class Orientation extends FirrtlNode
case object Default extends Orientation {
  def serialize: String = ""
}
case object Flip extends Orientation {
  def serialize: String = "flip "
}

/** Field of [[BundleType]] */
sealed case class Field(name: String, flip: Orientation, tpe: Type) extends FirrtlNode with HasName {
  def serialize: String = flip.serialize + name + " : " + tpe.serialize
}

sealed abstract class Type extends FirrtlNode
sealed abstract class GroundType extends Type {
  val width: Width
}
sealed abstract class AggregateType extends Type
sealed case class UIntType(width: Width) extends GroundType {
  def serialize: String = "UInt" + width.serialize
}
sealed case class SIntType(width: Width) extends GroundType {
  def serialize: String = "SInt" + width.serialize
}
sealed case class BundleType(fields: Seq[Field]) extends AggregateType {
  def serialize: String = "{ " + (fields map (_.serialize) mkString ", ") + "}"
}
sealed case class VectorType(tpe: Type, size: Int) extends AggregateType {
  def serialize: String = tpe.serialize + s"[$size]"
}
case object ClockType extends GroundType {
  val width = IntWidth(1)
  def serialize: String = "Clock"
}
case object UnknownType extends Type {
  def serialize: String = "?"
}

/** [[Port]] Direction */
sealed abstract class Direction extends FirrtlNode
case object Input extends Direction {
  def serialize: String = "input"
}
case object Output extends Direction {
  def serialize: String = "output"
}

/** [[DefModule]] Port */
sealed case class Port(
    info: Info,
    name: String,
    direction: Direction,
    tpe: Type) extends FirrtlNode with IsDeclaration {
  def serialize: String = s"${direction.serialize} $name : ${tpe.serialize}" + info.serialize
}

/** Base class for modules */
sealed abstract class DefModule extends FirrtlNode with IsDeclaration {
  val info : Info
  val name : String
  val ports : Seq[Port]
  protected def serializeHeader(tpe: String): String =
    s"$tpe $name :" + info.serialize +
    indent(ports map ("\n" + _.serialize) mkString) + "\n"
}
/** Internal Module
  *
  * An instantiable hardware block
  */
sealed case class Module(info: Info, name: String, ports: Seq[Port], body: Statement) extends DefModule {
  def serialize: String = serializeHeader("module") + indent("\n" + body.serialize)
}
/** External Module
  *
  * Generally used for Verilog black boxes
  */
sealed case class ExtModule(info: Info, name: String, ports: Seq[Port]) extends DefModule {
  def serialize: String = serializeHeader("extmodule")
}

sealed case class Circuit(info: Info, modules: Seq[DefModule], main: String) extends FirrtlNode with HasInfo {
  def serialize: String =
    s"circuit $main :" + info.serialize +
    (modules map ("\n" + _.serialize) map indent mkString "\n") + "\n"
}
