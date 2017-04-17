
package firrtl
package transforms

import firrtl.annotations._
import firrtl.passes.PassException

/** A component that should be preserved
  *
  * DCE treats the component as a top-level sink of the circuit
  */
object DontTouchAnnotation {
  private val marker = "DONTtouch!"
  def apply(target: ComponentName): Annotation = Annotation(target, classOf[Transform], "DONTtouch!")

  def unapply(a: Annotation): Option[ComponentName] = a match {
    case Annotation(component: ComponentName, _, "DONTtouch!") => Some(component)
    case _ => None
  }

  class DontTouchNotFoundException(module: String, component: String) extends PassException(
    s"Component marked DONT Touch ($module.$component) not found!\n" +
    "Perhaps it is an aggregate type? Currently only leaf components are supported.\n" +
    "Otherwise it was probably accidentally deleted. Please file an issue on Github."
  )

  def errorNotFound(module: String, component: String) =
    throw new DontTouchNotFoundException(module, component)
}
