// See LICENSE for license details.

package firrtl
package passes
package memlib
import ir._
import Annotations._
import wiring._

class CreateMemoryAnnotations(reader: Option[YamlFileReader]) extends Transform {
  def inputForm = MidForm
  def outputForm = MidForm
  override def name = "Create Memory Annotations"
  def execute(state: CircuitState): CircuitState = reader match {
    case None => state
    case Some(r) =>
      import CustomYAMLProtocol._
      val configs = r.parse[Config]
      val cN = CircuitName(state.circuit.main)
      val (as, pins) = configs.foldLeft((Seq.empty[Annotation], Seq.empty[String])) { case ((annos, pins), config) =>
        val top = TopAnnotation(ModuleName(config.top.name, cN), config.pin.name)
        val source = SourceAnnotation(ComponentName(config.source.name, ModuleName(config.source.module, cN)), config.pin.name)
        (annos ++ Seq(top, source), pins :+ config.pin.name)
      }
      state.copy(annotations = Some(AnnotationMap(as :+ PinAnnotation(cN, pins.toSeq))))
  }
}
