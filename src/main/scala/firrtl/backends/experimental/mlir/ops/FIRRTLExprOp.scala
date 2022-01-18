// SPDX-License-Identifier: Apache-2.0
// Author: Jiuyang Liu <liu@jiuyang.me>

// This files is based on circt/include/circt/Dialect/HW/FIRRTLExprOp.td

package firrtl.backends.experimental.mlir.ops

import firrtl.backends.experimental.mlir._

trait FIRRTLExprOp extends Op

trait ConstantOp extends FIRRTLOp

trait SpecialConstantOp extends FIRRTLOp

trait InvalidValueOp extends FIRRTLOp

/**
  * BundleType:$input
  * I32Attr:$fieldIndex
  */
case class SubfieldOp(input: BundleType, fieldIndex: I32Attr) extends FIRRTLExprOp

// TODO
trait IndexConstraint

/**
  * FIRRTLType:$input, I32Attr:$index
  */
case class SubindexOp(input: FIRRTLType, index: I32Attr) extends FIRRTLExprOp

case class SubaccessOp(input: FIRRTLType, index: UIntType) extends FIRRTLExprOp

// Primitive Operations

trait PrimOp extends FIRRTLExprOp

// Binary Operations
trait BinaryPrimOp extends PrimOp

trait IntBinaryPrimOp extends BinaryPrimOp

trait MulPrimOp extends IntBinaryPrimOp
trait DivPrimOp extends IntBinaryPrimOp
trait RemPrimOp extends IntBinaryPrimOp
trait AndPrimOp extends IntBinaryPrimOp
trait OrPrimOp extends IntBinaryPrimOp
trait XorPrimOp extends IntBinaryPrimOp
trait LEQPrimOp extends IntBinaryPrimOp
trait LTPrimOp extends IntBinaryPrimOp
trait GEQPrimOp extends IntBinaryPrimOp
trait GTPrimOp extends IntBinaryPrimOp
trait EQPrimOp extends IntBinaryPrimOp
trait NEQPrimOp extends IntBinaryPrimOp
trait CatPrimOp extends IntBinaryPrimOp
trait DshlPrimOp extends IntBinaryPrimOp
trait DShlwPrimOp extends IntBinaryPrimOp
trait DShrPrimOp extends IntBinaryPrimOp

trait UnaryPrimOp extends PrimOp
trait AsSIntPrimOp extends UnaryPrimOp
trait AsUIntPrimOp extends UnaryPrimOp
trait AsAsyncResetPrimOp extends UnaryPrimOp
trait AsClockPrimOp extends UnaryPrimOp
trait CvtPrimOp extends UnaryPrimOp
trait NegPrimOp extends UnaryPrimOp
trait NotPrimOp extends UnaryPrimOp
trait AndRPrimOp extends UnaryPrimOp
trait OrRPrimOp extends UnaryPrimOp
trait XorRPrimOp extends UnaryPrimOp

trait BitsPrimOp extends PrimOp
trait HeadPrimOp extends PrimOp
trait MuxPrimOp extends PrimOp
trait PadPrimOp extends PrimOp
trait ShiftPrimOp extends PrimOp
trait ShlPrimOp extends ShiftPrimOp
trait ShrPrimOp extends ShiftPrimOp
trait TailPrimOp extends PrimOp

// TODO
trait HasCustomSSAName

trait VerbatimExprOp extends PrimOp
trait VerbatimWireOp extends PrimOp

trait HWStructCastOp extends FIRRTLOp
trait BitCastOp extends FIRRTLOp