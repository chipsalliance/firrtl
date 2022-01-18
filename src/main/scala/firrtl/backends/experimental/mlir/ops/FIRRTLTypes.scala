// SPDX-License-Identifier: Apache-2.0
// Author: Jiuyang Liu <liu@jiuyang.me>

// This files is based on
// circt/include/circt/Dialect/HW/FIRRTLTypes.td
// circt/include/circt/Dialect/FIRRTLTypes.td
//

package firrtl.backends.experimental.mlir.ops

import firrtl.backends.experimental.mlir._

/**
  * @todo implement value based on .h
  */
trait FIRRTLType extends Type

class ClockType() extends FIRRTLType
class IntType extends FIRRTLType
/** -1 for unknown width */
class SIntType(val width: BigInt) extends IntType
/** -1 for unknown width */
class UIntType(val width: BigInt) extends IntType
class UInt1Type extends UIntType(1)
/** -1 for unknown width */
class AnalogType(val width: BigInt) extends FIRRTLType

class BundleElement(val name: String, val isFlip: Boolean, val tpe: FIRRTLType)
/** */
class BundleType(val elements: Seq[BundleElement]) extends FIRRTLType
class FVectorType(val elements: Seq[FIRRTLType]) extends FIRRTLType
class AsyncResetType extends FIRRTLType
class ResetType extends FIRRTLType
class PassiveType extends FIRRTLType

// FIRRTL Types Predicates
// Scala doesn't have dependent type
// which make impossible to pattern match a UInt<1> like what MLIR does.
object Predicates {
  def OneBitType(tpe:         FIRRTLType): Boolean = false
  def AnyResetType(tpe:       FIRRTLType): Boolean = false
  def UIntSIntClockType(tpe:  FIRRTLType): Boolean = false
  def OneBitCastableType(tpe: FIRRTLType): Boolean = false
}

object RUW_Undefined extends RUWAttr {
  def name = "Undefined"
  def value = 0
}
object RUW_Old extends RUWAttr {
  def name = "Old"
  def value = 1
}
object RUW_New extends RUWAttr {
  def name = "New"
  def value = 2
}
trait RUWAttr extends Enum
object MemDir_Infer extends MemDirAttr {
  def name = "Infer"
  def value = 0
}
object MemDir_Read extends MemDirAttr {
  def name = "Read"
  def value = 1
}
object MemDir_Write extends MemDirAttr {
  def name = "Write"
  def value = 2
}
object MemDir_ReadWrite extends MemDirAttr {
  def name = "ReadWrite"
  def value = 3
}

trait MemDirAttr extends Enum
