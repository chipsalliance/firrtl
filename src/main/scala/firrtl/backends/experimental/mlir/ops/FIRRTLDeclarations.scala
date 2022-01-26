// SPDX-License-Identifier: Apache-2.0
// Author: Jiuyang Liu <liu@jiuyang.me>

// This files is based on circt/include/circt/Dialect/HW/FIRRTLExprOp.td

package firrtl.backends.experimental.mlir.ops

case class InstanceOp(results: Seq[ValueAndType],instanceName: String, moduleName: String) extends FIRRTLOp
case class NodeOp(result: ValueAndType) extends FIRRTLOp
case class WireOp(result: ValueAndType) extends FIRRTLOp
case class RegOp(result: ValueAndType, clock: ValueAndType) extends FIRRTLOp
case class RegResetOp(result: ValueAndType, clock: ValueAndType, reset: ValueAndType, init: ValueAndType) extends FIRRTLOp
// Actually there is no MemOp in CHIRRTL