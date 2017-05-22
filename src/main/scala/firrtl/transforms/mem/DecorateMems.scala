// See LICENSE for license details.

package firrtl.transforms.mem

import firrtl.annotations._
import firrtl.transforms.wiring.{SourceAnnotation, TopAnnotation}
import firrtl.{AnnotationMap, CircuitState, MidForm, Transform}

class CreateMemoryAnnotations(reader: Option[YamlFileReader]) extends Transform {
  def inputForm = MidForm
  def outputForm = MidForm
  def execute(state: CircuitState): CircuitState = reader match {
    case None => state
    case Some(r) =>
      import firrtl.transforms.mem.CustomYAMLProtocol._
      val configs = r.parse[Config]
      val cN = CircuitName(state.circuit.main)
      val oldAnnos = state.annotations.getOrElse(AnnotationMap(Seq.empty)).annotations
      val (as, pins) = configs.foldLeft((oldAnnos, Seq.empty[String])) { case ((annos, pins), config) =>
        val top = TopAnnotation(ModuleName(config.top.name, cN), config.pin.name)
        val source = SourceAnnotation(ComponentName(config.source.name, ModuleName(config.source.module, cN)), config.pin.name)
        (annos ++ Seq(top, source), pins :+ config.pin.name)
      }
      state.copy(annotations = Some(AnnotationMap(as :+ PinAnnotation(cN, pins.toSeq))))
  }
}
