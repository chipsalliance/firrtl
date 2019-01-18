// See LICENSE for license details.

package firrtl
package annotations

trait EmissionOptions

trait RegisterEmissionOptions extends EmissionOptions {
  def useInitAsPreset : Boolean
  def disableRandomization : Boolean
}

abstract class LocalAnnotations extends SingleTargetAnnotation[Target]

case class PresetRegAnnotation(
    target: Target
) extends LocalAnnotations with RegisterEmissionOptions {
  def duplicate(n: Target) = this.copy(target = n)
  def useInitAsPreset = true
  def disableRandomization = true
}
