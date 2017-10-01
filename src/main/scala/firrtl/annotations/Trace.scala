// See LICENSE for license details.

package firrtl
package annotations

import scala.util.{Try, Success, Failure}

/** A component that should be traced
  *
  * The compiler will track name changes and other changes to annotated signals
  */
object TraceAnnotation {
  val marker = "trace!"
  def parse(target: String): Try[Annotation] = {
    val split = target.split("\\.", 3)
    if (split.size == 3 && split.forall(_.size > 0)) {
      Success(apply(ComponentName(split(2), ModuleName(split(1), CircuitName(split(0))))))
    }
    else Failure(new Exception("We can only trace Module relative signals: <circuit>.<module>.<signal>"))
  }
  def apply(target: ComponentName): Annotation = Annotation(target, classOf[Transform], marker)

  def unapply(a: Annotation): Option[ComponentName] = a match {
    case Annotation(component: ComponentName, _, value) if value == marker => Some(component)
    case _ => None
  }
}

