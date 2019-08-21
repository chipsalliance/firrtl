// See LICENSE for license details.

package firrtl.stage.transforms

import firrtl.{Transform, UnknownForm}
import firrtl.options.PreservesAll
import firrtl.transforms.IdentityTransform

/** Defines a hook for other [[Transform]]s to define as a prerequisite. In-tree, optional [[Transform]]s then define
  * this hook as a dependent. This enables behavior whereby the custom/out-of-tree transform runs at a specific point in
  * a sequence of transforms instead of after specific transforms (prerequisites). This narrowly addresses the situation
  * where the transforms before the specific point may not known.
  */
class HookTransform extends IdentityTransform(UnknownForm) with PreservesAll[Transform] {
  override final val dependents = Seq.empty
}
