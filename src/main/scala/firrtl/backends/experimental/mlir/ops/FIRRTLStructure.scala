// SPDX-License-Identifier: Apache-2.0
// Author: Jiuyang Liu <liu@jiuyang.me>

// This files is based on circt/include/circt/Dialect/HW/???.td

package firrtl.backends.experimental.mlir.ops
import firrtl.backends.experimental.mlir.Serializer

case class CircuitOp(name: String, body: Region) extends FIRRTLOp {
  def serialize = {
    implicit val b = StringBuilder.newBuilder
    implicit val indent = 0
    Serializer.write(this)
    b.toString
  }
}

trait ModuleOp extends FIRRTLOp {
  val name: String
  val ports: Seq[PortInfo]
}

case class FModuleOp(name: String, ports: Seq[PortInfo], body: Region) extends ModuleOp

// TODO: parameters
case class FExtModuleOp(name: String, ports: Seq[PortInfo]) extends ModuleOp