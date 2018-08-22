// See LICENSE for license details.

package firrtl.options

import firrtl.{AnnotationSeq, Transform}
import scopt.OptionParser

/** Indicates that this class/object includes options (but does not add
  * these as a registered class */
trait HasScoptOptions {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit
}

/** A [[Transform]] that includes options that should be exposed at the
  * top level.
  *
  * @note To complete registration, include an entry in
  * src/main/resources/META-INF/services/firrtl.options.RegisteredTransform */
trait RegisteredTransform extends HasScoptOptions { this: Transform => }

/** A class that includes options that should be exposed as a group at the
  * top level.
  *
  * @note To complete registration, include an entry in
  * src/main/resources/META-INF/services/firrtl.options.RegisteredLibrary */
trait RegisteredLibrary extends HasScoptOptions {
  def name: String
}
