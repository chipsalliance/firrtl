// SPDX-License-Identifier: Apache-2.0
// Author: Jiuyang Liu <liu@jiuyang.me>

// This files is based on circt/include/circt/Dialect/HW/CHIRRTL.td

package firrtl.backends.experimental.mlir.ops

import firrtl.backends.experimental.mlir._

trait CHIRRTLOp extends Op
/**
  * firrtl::FIRRTLType":$elementType
  * unsigned":$numElements
  *
  */
case class CMemoryType(elementType: FIRRTLType, numElements: unsigned) extends Type {
  canCastToUnsigned(numElements)
}

case class CMemoryPortType() extends Type

/**
  *
  * StrAttr:$name
  * AnnotationArrayAttr:$annotations
  * OptionalAttr<SymbolNameAttr>:$inner_sym
  *
  */
case class CombMemOp(name: String, annotations: AnnotationArrayAttr, inner_sym: OptionalAttr[String]) extends CHIRRTLOp

/**
  * RUWAttr:$ruw
  * StrAttr:$name
  * AnnotationArrayAttr:$annotations
  * OptionalAttr<SymbolNameAttr>:$inner_sym
  *
  */
case class SeqMemOp(ruw: RUWAttr, name: StringRef, annotations: ArrayAttr, inner_sym: StringAttr) extends CHIRRTLOp

/**
  * CMemoryType:$memory
  * MemDirAttr:$direction
  * StrAttr:$name
  * AnnotationArrayAttr:$annotations
  *
  */
case class MemoryPortOp(memory: CMemoryType, direction: MemDirAttr, name: StrAttr, annotations: AnnotationArrayAttr) extends CHIRRTLOp

/**
  * CMemoryPortType:$port
  * IntType:$index
  * ClockType:$clock
  */
case class MemoryPortAccessOp(port: CMemoryPortType, index: IntType, clock: ClockType) extends CHIRRTLOp

