// SPDX-License-Identifier: Apache-2.0
// Author: Jiuyang Liu <liu@jiuyang.me>

// This files is based on circt/include/circt/Dialect/HW/FIRRTLStatements.td

package firrtl.backends.experimental.mlir.ops

import firrtl.backends.experimental.mlir._

/**
  * Variadic<AnalogType>:$operands
  */
case class AttachOp(operands: Variadic[AnalogType]) extends FIRRTLOp

/**
  * FIRRTLType:$dest
  * FIRRTLType:$src
  */
case class ConnectOp(dest: FIRRTLType, src: FIRRTLType) extends FIRRTLOp

/**
  * FIRRTLType:$dest
  * FIRRTLType:$src
  */
case class PartialConnectOp(dest: FIRRTLType, src: FIRRTLType) extends FIRRTLOp

/** ClockType:$clock
  * UInt1Type:$cond
  * StrAttr:$formatString
  * Variadic<FIRRTLType>:$operands
  * StrAttr:$name
  */
case class PrintFOp(clock: ClockType, cond: UInt1Type, formatSting: StrAttr, operands: Variadic[FIRRTLType], name:StrAttr) extends FIRRTLOp
/** */
case class SkipOp() extends FIRRTLOp
/** ClockType:$clock
  * UInt1Type:$cond
  * I32Attr:$exitCode
  * StrAttr:$name
  */
case class StopOp(clock: ClockType, cond: UInt1Type, name: StrAttr) extends FIRRTLOp

case object AtPosEdge extends EventControlAttr{
  def name: String = "posedge"
  def value: Int = 0
}
case object AtNegEdge extends EventControlAttr {
  def name: String = "negedge"
  def value: Int = 1
}

case object AtEdge extends EventControlAttr {
  def name: String = "edge"
  def value: Int = 2
}
trait EventControlAttr extends Enum

/** ClockType:$clock,
  * UInt1Type:$predicate,
  * UInt1Type:$enable,
  * StrAttr:$message,
  * Variadic<AnyType>:$operands,
  * StrAttr:$name,
  * DefaultValuedAttr<BoolAttr,"false">:$isConcurrent,
  * DefaultValuedAttr<EventControlAttr,"EventControl::AtPosEdge">:$eventControl
  */
trait VerifOp

case class AssertOp(clock: ClockType,
                    predicate: UInt1Type,
                    enable: UInt1Type,
                    message: StrAttr,
                    operands: Variadic[AnyType],
                    isConcurrent: DefaultValuedAttr[BoolAttr],
                    eventControl: EventControlAttr) extends VerifOp

case class AssumeOp(clock: ClockType,
                    predicate: UInt1Type,
                    enable: UInt1Type,
                    message: StrAttr,
                    operands: Variadic[AnyType],
                    isConcurrent: DefaultValuedAttr[BoolAttr],
                    eventControl: EventControlAttr) extends VerifOp

case class CoverOp(clock: ClockType,
                    predicate: UInt1Type,
                    enable: UInt1Type,
                    message: StrAttr,
                    operands: Variadic[AnyType],
                    isConcurrent: DefaultValuedAttr[BoolAttr],
                    eventControl: EventControlAttr) extends VerifOp

/** UInt1Type:$condition
  *
  * SizedRegion<1>:$thenRegion
  * AnyRegion:$elseRegion
  *
  * @todo how to take care about region.
  */
case class WhenOp(thenRegion: Block, elseRegion: Seq[Block]) extends FIRRTLOp
case class ForceOp(dest: FIRRTLType, src: FIRRTLType) extends FIRRTLOp
/**
  * SymbolNameAttr:$inner_sym
  * Variadic<FIRRTLType>:$operands)
  *
  */
case class ProbeOp(inner_sym: SymbolNameAttr, operands: Variadic[FIRRTLType]) extends FIRRTLOp
