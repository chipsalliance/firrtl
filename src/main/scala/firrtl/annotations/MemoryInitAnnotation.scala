// See LICENSE for license details.

package firrtl.annotations

import firrtl.{MemoryEmissionOption, MemoryInitValue}

/**
 * Represents the initial value of the annotated memory.
 * While not supported on normal ASIC flows, it can be useful for simulation and FPGA flows.
 * This annotation is consumed by the verilog emitter.
 */
case class MemoryInitAnnotation(target: ReferenceTarget, value: MemoryInitValue) extends
  SingleTargetAnnotation[ReferenceTarget] with MemoryEmissionOption {
  override def duplicate(n: ReferenceTarget): Annotation = copy(n)
  override def initValue: MemoryInitValue = value
}
