// SPDX-License-Identifier: Apache-2.0
// Author: Jiuyang Liu <liu@jiuyang.me>

// This files is based on circt/include/circt/Dialect/HW/???.td

package firrtl.backends.experimental.mlir.ops

case class CircuitOp(name: String, body: Region) extends FIRRTLOp

case class FModuleOp(name: String, ports: Seq[PortInfo], body: Region) extends FIRRTLOp

case class FExtModuleOp(name: String, port: Seq[PortInfo]) extends FIRRTLOp