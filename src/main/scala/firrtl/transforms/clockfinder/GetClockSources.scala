// See LICENSE for license details.

package firrtl.transforms.clockfinder

import firrtl.RenameMap
import firrtl.annotations.{Annotation, CompleteTarget}

/** Consumed by [[ClockFinderTransform]], records a given set of signals for which to find their clock sources
  *
  * @param targets
  */
case class GetClockSources(targets: Seq[CompleteTarget]) extends Annotation {
  override def update(renames: RenameMap): Seq[Annotation] = {
    val newTargets = targets.flatMap(renames(_))
    Seq(GetClockSources(newTargets))
  }
}
