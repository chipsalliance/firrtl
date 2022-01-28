// SPDX-License-Identifier: Apache-2.0
// Author: Jiuyang Liu <liu@jiuyang.me>

// This files is based on circt/include/circt/Dialect/HW/FIRRTLStatements.td

package firrtl.backends.experimental.mlir.ops

case class AttachOp(arguments: Seq[ValueAndType]) extends FIRRTLOp
case class ConnectOp(dest: ValueAndType, src: ValueAndType) extends FIRRTLOp
case class PartialConnectOp(dest: ValueAndType, src: ValueAndType) extends FIRRTLOp
case class PrintFOp(
  clock:       ValueAndType,
  cond:        ValueAndType,
  operands:    Seq[ValueAndType],
  formatSting: String)
    extends FIRRTLOp
case class SkipOp() extends FIRRTLOp
case class StopOp(clock: ValueAndType, cond: ValueAndType, exitCode: Int, name: String) extends FIRRTLOp

trait VerifOp extends Op {
  val name:      String
  val clock:     ValueAndType
  val predicate: ValueAndType
  val enable:    ValueAndType
  val message:   String
}

case class AssertOp(
  clock:     ValueAndType,
  predicate: ValueAndType,
  enable:    ValueAndType,
  message:   String)
    extends VerifOp {
  val name = "assert"
}

case class AssumeOp(
  clock:     ValueAndType,
  predicate: ValueAndType,
  enable:    ValueAndType,
  message:   String)
    extends VerifOp {
  val name = "assume"
}

case class CoverOp(
  clock:     ValueAndType,
  predicate: ValueAndType,
  enable:    ValueAndType,
  message:   String)
    extends VerifOp {
  val name = "cover"
}

case class WhenOp(condition: ValueAndType, thenRegion: Region, elseRegion: Region) extends FIRRTLOp
