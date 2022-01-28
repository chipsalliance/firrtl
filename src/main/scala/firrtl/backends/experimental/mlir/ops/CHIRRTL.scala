// SPDX-License-Identifier: Apache-2.0
// Author: Jiuyang Liu <liu@jiuyang.me>

// This files is based on circt/include/circt/Dialect/HW/CHIRRTL.td

package firrtl.backends.experimental.mlir.ops

import firrtl.backends.experimental.mlir._

trait CHIRRTLOp extends Op

case class CMemoryType(elementType: FIRRTLType, numElements: BigInt) extends FIRRTLType

case class CombMemOp(result: ValueAndType) extends CHIRRTLOp

case class SeqMemOp(result: ValueAndType, ruw: RUWAttr) extends CHIRRTLOp

case class MemoryPortOp(memory: ValueAndType, data: ValueAndType, port: ValueAndType, direction: MemDirAttr)
    extends CHIRRTLOp

case class MemoryPortAccessOp(port: ValueAndType, index: ValueAndType, clock: ValueAndType) extends CHIRRTLOp
