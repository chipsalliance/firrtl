package firrtl
package passes
package memlib
import ir._
import Annotations._
import wiring._

class CreateMemoryAnnotations(reader: Option[YamlFileReader], replaceID: TransID, wiringID: TransID) extends Transform {
  def name = "Create Memory Annotations"
  def execute(c: Circuit, map: AnnotationMap): TransformResult = reader match {
    case None => TransformResult(c)
    case Some(r) =>
      import CustomYAMLProtocol._
      val config = r.parse[Config].head
      val cN = CircuitName(c.main)
      val top = TopAnnotation(ModuleName(config.top.name, cN), wiringID)
      val source = SourceAnnotation(ComponentName(config.source.name, ModuleName(config.source.module, cN)), wiringID)
      val pin = PinAnnotation(cN, replaceID, config.pin.name)
      TransformResult(c, None, Some(AnnotationMap(Seq(top, source, pin))))
  }
}


// vim: set ts=4 sw=4 et:
