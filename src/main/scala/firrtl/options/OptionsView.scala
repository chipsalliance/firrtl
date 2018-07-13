// See LICENSE for license details

package firrtl.options

import firrtl.AnnotationSeq

/** Type class defining a "view" of an [[AnnotationSeq]] */
trait OptionsView[T] {
  def view(implicit options: AnnotationSeq): Option[T]
}

/** A shim to manage multiple "views" of an [[AnnotationSeq]] */
object Viewer {
  def view[T](implicit optionsView: OptionsView[T], options: AnnotationSeq): Option[T] = optionsView.view
}
