package firrtl

import firrtl.ir._
import firrtl.Utils._
import firrtl.Mappers._
import firrtl.PrimOps._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object CompoundExprEmitter {
  def genVerilogReferenceName(expr: Expression): Option[String] = {
    expr match {
      case Reference(name, tpe) => Some(name)
      case SubField(expr, name, tpe) =>
        genVerilogReferenceName(expr) match {
          case Some(exprName) => Some(exprName + "_" + name)
          case None => None
        }
      case SubIndex(expr, value, tpe) =>
        genVerilogReferenceName(expr) match {
          case Some(exprName) => Some(exprName + "_" + value)
          case None => None
        }
      case _ => //return None for expression types that don't have matching verilog reference names
        None
    }
  }

  /**
   * Checks if expr is emittable by current new emitter framework. Remove when
   * more complete new emitter is put into place.
   */
  def checkExprEmittable(
    expr: Expression
  ): Boolean = {
    expr match {
      case DoPrim(op, args, consts, tpe) => {
        op match {
          // TODO(b/38357475): remove when twigg's inline verilog refactor is checked in
          case Rem => false
          case _ => {
            args.foldLeft(true)(
              (result, arg) => result && checkExprEmittable(arg)
            )
          }
        }
      }
      case Reference(name, tpe) => true
      case subField: SubField => {
        genVerilogReferenceName(subField) match {
          case Some(name) => true
          case None => false
        }
      }
      case subIndex: SubIndex => {
        genVerilogReferenceName(subIndex) match {
          case Some(name) => true
          case None => false
        }
      }
      case UIntLiteral(value, width) => true
      case SIntLiteral(value, width) => true
      case _ => false
    }
  }

  def genVerilog(
    expr: Expression,
    resultText: StringBuilder
  ): Unit = {
    expr match {
      case doprim: DoPrim => {
        genVerilog(doprim, resultText)
      }
      case reference: Reference => {
        genVerilog(reference, resultText)
      }
      case subField: SubField => {
        genVerilog(subField, resultText)
      }
      case subIndex: SubIndex => {
        genVerilog(subIndex, resultText)
      }
      case (uintLiteral: UIntLiteral) => {
        genVerilog(uintLiteral, resultText)
      }
      case (sintLiteral: SIntLiteral) => {
        genVerilog(sintLiteral, resultText)
      }
      case _ => {
        throw new IllegalArgumentException("CompoundExprEmitter cannot yet handle expr: " + expr)
      }
    }
  }

  def genVerilog(
    doprim: DoPrim,
    resultText: StringBuilder
  ): Unit = {
    def getArg0 () : Expression = doprim.args(0)
    def getArg1 () : Expression = doprim.args(1)
    def getArg2 () : Expression = doprim.args(2)
    def getConst0 () : BigInt = doprim.consts(0)
    def getConst1 () : BigInt = doprim.consts(1)

    def genBinOp (
      arg0: Expression,
      opString: String,
      arg1: Expression
    ): Unit = {
      resultText.append("(")
      genVerilog(arg0, resultText)
      resultText.append(opString)
      genVerilog(arg1, resultText)
      resultText.append(")")
    }

    def genBinOpSigned (
      arg0: Expression,
      opString: String,
      arg1: Expression
    ): Unit = {
      val signed = doprim.args.find(x => tpe(x).typeof[SIntType])
      if (signed != None) {
        tpe(arg0) match {
          case (t:SIntType) => resultText.append("$signed(")
          case (t:UIntType) => resultText.append("$signed({1'b0,")
        }
      }
      resultText.append("(")
      genVerilog(arg0, resultText)
      if (signed != None) resultText.append(")")
      resultText.append(opString)
      if (signed != None) {
        tpe(arg1) match {
          case (t:SIntType) => resultText.append("$signed(")
          case (t:UIntType) => resultText.append("$signed({1'b0,")
        }
      }
      genVerilog(arg1, resultText)
      if (signed != None) resultText.append(")")
      resultText.append(")")
    }

    def genUnOp (
      opString: String,
      arg: Expression
    ): Unit = {
      resultText.append(opString)
      resultText.append("(")
      genVerilog(arg, resultText)
      resultText.append(")")
    }

    def genSysCall (
      sysCallName: String,
      arg: Expression
    ): Unit = {
      resultText.append(sysCallName)
      resultText.append("(")
      genVerilog(getArg0(), resultText)
      resultText.append(")")
    }
    doprim.op match {
      case Add => genBinOpSigned(getArg0(), " + ", getArg1())
      case Addw => genBinOpSigned(getArg0(), " + ", getArg1())
      case Sub => genBinOpSigned(getArg0(), " - ", getArg1())
      case Subw => genBinOpSigned(getArg0(), " - ", getArg1())
      case Mul => genBinOpSigned(getArg0(), " * ", getArg1())
      case Div => genBinOpSigned(getArg0(), " / ", getArg1())
      case Rem => genBinOpSigned(getArg0(), " % ", getArg1())
      case Lt => genBinOpSigned(getArg0(), " < ", getArg1())
      case Leq => genBinOpSigned(getArg0(), " <= ", getArg1())
      case Gt => genBinOpSigned(getArg0(), " > ", getArg1())
      case Geq => genBinOpSigned(getArg0(), " >= ", getArg1())
      case Eq => genBinOpSigned(getArg0(), " == ", getArg1())
      case Neq => genBinOpSigned(getArg0(), " != ", getArg1())
      case Pad => {
        val originalWidth = long_BANG(tpe(getArg0()))
        val padWidth = (getConst0() - originalWidth)
        if (originalWidth == 0) {
          genVerilog(getArg0(), resultText)
        } else doprim.tpe match {
          // Either sign extend or zero extend.
          case t: SIntType => {
            // If width == 1, don't extract bit
            if (originalWidth == 1) {
              resultText.append("{" + getConst0() + "{")
              genVerilog(getArg0(), resultText)
              resultText.append("}}")
            } else {
              resultText.append("{{" + padWidth + "{")
              genVerilog(getArg0(), resultText)
              resultText.append("[" + (originalWidth - 1) + "]}},")
              genVerilog(getArg0(), resultText)
              resultText.append("}")
            }
          }
          case _ => {
            resultText.append("{{" + padWidth + "'d0}, ")
            genVerilog(getArg0(), resultText)
            resultText.append("}")
          }
        }
      }
      case AsUInt => genSysCall("$unsigned", getArg0())
      case AsSInt => genSysCall("$signed",getArg0())
      case AsClock => genSysCall("$unsigned",getArg0())
      case Dshlw => genBinOp(getArg0(), " << ", getArg1())
      case Dshl => genBinOp(getArg0(), " << ", getArg1())
      case Dshr => {
        (doprim.tpe) match {
          case (t:SIntType) => genBinOp(getArg0(), " >>> ", getArg1())
          case (t) => genBinOp(getArg0(), " >> ", getArg1())
        }
      }
      case Shlw => {
        genVerilog(getArg0(), resultText)
        resultText.append(" << " + getConst0())
      }
      case Shl => {
        genVerilog(getArg0(), resultText)
        resultText.append(" << " + getConst0())
      }
      case Shr => {
        if (getConst0 >= long_BANG(tpe(getArg0))) {
          error("Verilog emitter does not support SHIFT_RIGHT >= arg width")
        } else {
          genVerilog(getArg0(), resultText)
          resultText.append("[" + (long_BANG(tpe(getArg0())) - 1) + ":" + getConst0() + "]")
        }
      }
      case Neg => {
        resultText.append("-{")
        genVerilog(getArg0(), resultText)
        resultText.append("}")
      }
      case Cvt => {
        tpe(getArg0()) match {
          case (t:UIntType) => {
            resultText.append("{1'b0,")
            genVerilog(getArg0(), resultText)
            resultText.append("}")
          }
          case (t:SIntType) => genVerilog(getArg0(), resultText)
        }
      }
      case Not => genUnOp("~ ",getArg0())
      case And => genBinOp(getArg0(), " & ", getArg1())
      case Or => genBinOp(getArg0(), " | ", getArg1())
      case Xor => genBinOp(getArg0(), " ^ ", getArg1())
      case Andr => genUnOp("&", getArg0())
      case Orr => genUnOp("|", getArg0())
      case Xorr => genUnOp("^", getArg0())
      case Cat => {
        resultText.append("{")
        genVerilog(getArg0(), resultText)
        resultText.append(",")
        genVerilog(getArg1(), resultText)
        resultText.append("}")
      }
      case Bits => {
        // If selecting zeroth bit and single-bit wire, just emit the wire
        if (getConst0() == 0 && getConst1() == 0 && long_BANG(tpe(getArg0())) == 1) {
          genVerilog(getArg0(), resultText)
        } else if (getConst0() == getConst1()) {
          genVerilog(getArg0(), resultText)
          resultText.append("[")
          resultText.append(getConst0())
          resultText.append("]")
        } else {
          genVerilog(getArg0(), resultText)
          resultText.append("[")
          resultText.append(getConst0())
          resultText.append(":")
          resultText.append(getConst1())
          resultText.append("]")
        }
      }
      case Head => {
        val width = long_BANG(tpe(getArg0()))
        val highIndex = width - 1
        val lowIndex= width - getConst0()
        genVerilog(getArg0(), resultText)
        resultText.append("[" + highIndex + ":" + lowIndex + "]")
      }
      case Tail => {
        val width = long_BANG(tpe(getArg0()))
        val lowIndex = width - getConst0() - 1
        genVerilog(getArg0(), resultText)
        resultText.append("[" + lowIndex + ":" + 0 + "]")
      }
    }
  }

  def genVerilog(
    reference: Reference,
    resultText: StringBuilder
  ): Unit = {
    resultText.append(reference.name)
  }

  def genVerilog(
    subField: SubField,
    resultText: StringBuilder
  ): Unit = {
    genVerilogReferenceName(subField) match {
      case Some(name) => resultText.append(name)
      case None =>
        throw new IllegalArgumentException(
          "CompoundExprEmitter encountered illegal SubField expr: " + subField
        )
    }
  }

  def genVerilog(
    subIndex: SubIndex,
    resultText: StringBuilder
  ): Unit = {
    genVerilogReferenceName(subIndex) match {
      case Some(name) => resultText.append(name)
      case None =>
        throw new IllegalArgumentException(
          "CompoundExprEmitter encountered illegal SubIndex expr: " + subIndex
        )
    }
  }

  def genVerilog(
    uintLiteral: UIntLiteral,
    resultText: StringBuilder
  ): Unit = {
    val valueStr = uintLiteral.value.toString(16)
    resultText.append(long_BANG(tpe(uintLiteral)).toString + "'h" + valueStr)
  }

  def genVerilog(
    sintLiteral: SIntLiteral,
    resultText: StringBuilder
  ): Unit = {
    val valueStr = sintLiteral.value.toString(16)
    resultText.append(long_BANG(tpe(sintLiteral)).toString + "'sh" + valueStr)
  }
}
