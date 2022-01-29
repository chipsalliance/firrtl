// SPDX-License-Identifier: Apache-2.0
// Author: Jiuyang Liu <liu@jiuyang.me>

// This files is based on circt/include/circt/Dialect/HW/???.td

package firrtl.backends.experimental.mlir.ops

case class CircuitOp(name: String, body: Region) extends FIRRTLOp

trait ModuleOp extends FIRRTLOp {
  val name: String
  val ports: Seq[PortInfo]
}

case class FModuleOp(name: String, ports: Seq[PortInfo], body: Region) extends ModuleOp

// TODO: parameters
case class FExtModuleOp(name: String, ports: Seq[PortInfo]) extends ModuleOp