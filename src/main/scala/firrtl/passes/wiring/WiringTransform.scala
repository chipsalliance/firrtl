// See LICENSE for license details.

package firrtl.passes
package wiring

import firrtl._
import firrtl.ir._
import firrtl.Utils._
import firrtl.Mappers._
import scala.collection.mutable
import firrtl.annotations._
import WiringUtils._

/** A Source Component
  */
object SourceAnnotation {
  def apply(target: ComponentName, pin: String): Annotation = Annotation(target, classOf[WiringTransform], s"source $pin")

  private val matcher = "source (.+)".r
  def unapply(a: Annotation): Option[(ComponentName, String)] = a match {
    case Annotation(ComponentName(n, m), _, matcher(pin)) => Some((ComponentName(n, m), pin))
    case _ => None
  }
}

/** A Sink Module or Component
  */
object SinkAnnotation {
  def apply(target: Named, pin: String): Annotation = Annotation(target, classOf[WiringTransform], s"sink $pin")

  private val matcher = "sink (.+)".r
  def unapply(a: Annotation): Option[(Named, String)] = a match {
    case Annotation(ModuleName(n, c), _, matcher(pin)) => Some((ModuleName(n, c), pin))
    case Annotation(ComponentName(n, m), _, matcher(pin)) => Some((ComponentName(n, m), pin))
    case _ => None
  }
}

/** Wires Sources to Sinks
  *
  * Sinks are wired to their closest source through their lowest
  * common ancestor (LCA). Verbosely, this modifies the circuit in
  * the following ways:
  *   - Adds a pin to each sink module
  *   - Punches ports up from source signals to the LCA of each sink
  *   - Punches ports down from LCAs to each sink module
  *   - Wires sources up to LCA, sinks down from LCA, and across each LCA
  *
  * @throws WiringException if a sink is equidistant to two sources
  */
class WiringTransform extends Transform {
  def inputForm = MidForm
  def outputForm = HighForm
  def transforms(wis: Seq[WiringInfo]) = Seq(new Wiring(wis))
  def execute(state: CircuitState): CircuitState = getMyAnnotations(state) match {
    case Nil => state
    case p =>
      val sinks = mutable.HashMap[String, Seq[Named]]()
      val sources = mutable.HashMap[String, Named]()
      p.foreach {
        case SinkAnnotation(m, pin) =>
          sinks(pin) = sinks.getOrElse(pin, Seq.empty) :+ m
        case SourceAnnotation(c, pin) =>
          sources(pin) = c
      }
      (sources.size, sinks.size) match {
        case (0, p) => state
        case (s, p) if (p > 0) =>
          val wis = sources.foldLeft(Seq[WiringInfo]()) { case (seq, (pin, source)) =>
            seq :+ WiringInfo(source, sinks(pin), pin)
          }
          transforms(wis).foldLeft(state) { (in, xform) => xform.runTransform(in) }
        case _ => error("Wrong number of sources or sinks!")
      }
  }
}
