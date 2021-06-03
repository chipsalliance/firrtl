// SPDX-License-Identifier: Apache-2.0

package firrtl
package annotations

/**
  * Transform all registers connected to the targeted AsyncReset tree into bitstream preset registers
  * Impacts all registers connected to any child (cross module) of the target AsyncReset
  *
  * @param target ReferenceTarget to an AsyncReset
  */
case class PresetAnnotation(target: ReferenceTarget)
    extends SingleTargetAnnotation[ReferenceTarget]
    with firrtl.transforms.DontTouchAllTargets {
  override def duplicate(n: ReferenceTarget) = this.copy(target = n)
}

/**
  * Transform the targeted asynchronously-reset Reg into a bitstream preset Reg
  * Thus you can use this annotation in order to initialize a register
  * at the beginning of simulation or through the FPGA bit-stream to its `init` value.
  *
  * The register must fulfil the following requirements:
  * - the reset signal is `UInt(0)`
  * - the `init` value is a Literal
  *
  * @param target ReferenceTarget to a Reg
  */
case class PresetRegAnnotation(
  target: ReferenceTarget)
    extends SingleTargetAnnotation[ReferenceTarget]
    with RegisterEmissionOption {
  def duplicate(n: ReferenceTarget) = this.copy(target = n)
  override def useInitAsPreset = true
  override def disableRandomization = true
}
