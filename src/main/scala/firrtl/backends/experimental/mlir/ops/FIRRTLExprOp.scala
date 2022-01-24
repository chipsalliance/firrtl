// SPDX-License-Identifier: Apache-2.0
// Author: Jiuyang Liu <liu@jiuyang.me>

// This files is based on circt/include/circt/Dialect/HW/FIRRTLExprOp.td

package firrtl.backends.experimental.mlir.ops


trait FIRRTLExprOp extends Op

case class ConstantOp(result: ValueAndType, value: BigInt) extends FIRRTLExprOp
case class SpecialConstantOp(result: ValueAndType, value: BigInt) extends FIRRTLExprOp
case class SubfieldOp(result: ValueAndType, input: ValueAndType, fieldIndex: BigInt) extends FIRRTLExprOp
case class SubindexOp(result: ValueAndType, input: ValueAndType, index: BigInt) extends FIRRTLExprOp
case class SubaccessOp(result: ValueAndType, input: ValueAndType, index: ValueAndType) extends FIRRTLExprOp

trait PrimOp extends FIRRTLExprOp
trait BinaryPrimOp extends PrimOp
case class AddPrimOp(result: ValueAndType, lhs: ValueAndType, rhs: ValueAndType) extends BinaryPrimOp
case class SubPrimOp(result: ValueAndType, lhs: ValueAndType, rhs: ValueAndType) extends BinaryPrimOp
case class MulPrimOp(result: ValueAndType, lhs: ValueAndType, rhs: ValueAndType) extends BinaryPrimOp
case class DivPrimOp(result: ValueAndType, lhs: ValueAndType, rhs: ValueAndType) extends BinaryPrimOp
case class RemPrimOp(result: ValueAndType, lhs: ValueAndType, rhs: ValueAndType) extends BinaryPrimOp
case class AndPrimOp(result: ValueAndType, lhs: ValueAndType, rhs: ValueAndType) extends BinaryPrimOp
case class OrPrimOp(result: ValueAndType, lhs: ValueAndType, rhs: ValueAndType) extends BinaryPrimOp
case class XorPrimOp(result: ValueAndType, lhs: ValueAndType, rhs: ValueAndType) extends BinaryPrimOp
case class LEQPrimOp(result: ValueAndType, lhs: ValueAndType, rhs: ValueAndType) extends BinaryPrimOp
case class LTPrimOp(result: ValueAndType, lhs: ValueAndType, rhs: ValueAndType) extends BinaryPrimOp
case class GEQPrimOp(result: ValueAndType, lhs: ValueAndType, rhs: ValueAndType) extends BinaryPrimOp
case class GTPrimOp(result: ValueAndType, lhs: ValueAndType, rhs: ValueAndType) extends BinaryPrimOp
case class EQPrimOp(result: ValueAndType, lhs: ValueAndType, rhs: ValueAndType) extends BinaryPrimOp
case class NEQPrimOp(result: ValueAndType, lhs: ValueAndType, rhs: ValueAndType) extends BinaryPrimOp
case class CatPrimOp(result: ValueAndType, lhs: ValueAndType, rhs: ValueAndType) extends BinaryPrimOp
case class DShlPrimOp(result: ValueAndType, lhs: ValueAndType, rhs: ValueAndType) extends BinaryPrimOp
case class DShrPrimOp(result: ValueAndType, lhs: ValueAndType, rhs: ValueAndType) extends BinaryPrimOp

trait UnaryPrimOp extends PrimOp
case class AsSIntPrimOp(result: ValueAndType, input: ValueAndType) extends UnaryPrimOp
case class AsUIntPrimOp(result: ValueAndType, input: ValueAndType) extends UnaryPrimOp
case class AsAsyncResetPrimOp(result: ValueAndType, input: ValueAndType) extends UnaryPrimOp
case class AsClockPrimOp(result: ValueAndType, input: ValueAndType) extends UnaryPrimOp
case class CvtPrimOp(result: ValueAndType, input: ValueAndType) extends UnaryPrimOp
case class NegPrimOp(result: ValueAndType, input: ValueAndType) extends UnaryPrimOp
case class NotPrimOp(result: ValueAndType, input: ValueAndType) extends UnaryPrimOp
case class AndRPrimOp(result: ValueAndType, input: ValueAndType) extends UnaryPrimOp
case class OrRPrimOp(result: ValueAndType, input: ValueAndType) extends UnaryPrimOp
case class XorRPrimOp(result: ValueAndType, input: ValueAndType) extends UnaryPrimOp

trait BinaryIntPrimOp extends PrimOp
case class HeadPrimOp(result: ValueAndType, input: ValueAndType, amount: BigInt) extends BinaryPrimOp
case class TailPrimOp(result: ValueAndType, input: ValueAndType, amount: BigInt) extends BinaryPrimOp
case class ShlPrimOp(result: ValueAndType, input: ValueAndType, amount: BigInt) extends BinaryPrimOp
case class ShrPrimOp(result: ValueAndType, input: ValueAndType, amount: BigInt) extends BinaryPrimOp
case class PadPrimOp(result: ValueAndType, input: ValueAndType, amount: BigInt) extends BinaryPrimOp

case class BitsPrimOp(result: ValueAndType, input: ValueAndType, hi: BigInt, low: BigInt) extends PrimOp
case class MuxPrimOp(result: ValueAndType, sel: ValueAndType, high: ValueAndType, low: ValueAndType) extends PrimOp

