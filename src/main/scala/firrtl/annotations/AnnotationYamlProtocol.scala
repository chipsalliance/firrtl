// See LICENSE for license details.

package firrtl
package annotations

import net.jcazevedo.moultingyaml._

object AnnotationYamlProtocol extends DefaultYamlProtocol {
  // bottom depends on top
  implicit object AnnotationYamlFormat extends YamlFormat[Annotation] {
    def write(a: Annotation) = YamlObject(
      YamlString("targetString") -> YamlString(a.targetString),
      YamlString("transformClass") -> YamlString(a.transformClass),
      YamlString("value") -> YamlString(a.value)
    )

    def read(yamlValue: YamlValue): Annotation = {
      yamlValue.asYamlObject.getFields(
        YamlString("targetString"),
        YamlString("transformClass"),
        YamlString("value")) match {
          case Seq(YamlString(targetString), YamlString(transformClass), YamlString(value)) =>
              Annotation(
                toTarget(targetString), Class.forName(transformClass).asInstanceOf[Class[_ <: Transform]], value)
          case _ => deserializationError("Color expected")
      }
    }
    def toTarget(string: String): Named = string.split('.').toSeq match {
      case Seq(c) => CircuitName(c)
      case Seq(c, m) => ModuleName(m, CircuitName(c))
      case Nil => Utils.error("BAD")
      case s => ComponentName(s.drop(2).mkString("."), ModuleName(s.tail.head, CircuitName(s.head)))
    }
  }
}
