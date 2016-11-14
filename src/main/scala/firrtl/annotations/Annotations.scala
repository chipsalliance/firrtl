// See LICENSE for license details.

package firrtl
package annotations

import firrtl.ir._

import scala.collection.mutable
import java.io.Writer

import net.jcazevedo.moultingyaml._
import AnnotationUtils._



object AnnotationYAMLProtocol extends DefaultYamlProtocol {
  // bottom depends on top
  implicit object AnnotationYamlFormat extends YamlFormat[Annotation] {
    def write(a: Annotation) =
      YamlArray(
        YamlString(a.targetString),
        YamlString(a.transformClass),
        YamlString(a.value))

    def read(value: YamlValue) = {
      value.asYamlObject.getFields(
        YamlString("targetString"),
        YamlString("transformClass"),
        YamlString("value")) match {
        case Seq(
          YamlString(targetString),
          YamlString(transformClass),
          YamlString(value)) =>
          new Annotation(toTarget(targetString), Class.forName(transformClass).asInstanceOf[Class[_ <: Transform]], value)
        case _ => deserializationError("Color expected")
      }
    }
    def toTarget(string: String) = string.split('.').toSeq match {
      case Seq(c) => CircuitName(c)
      case Seq(c, m) => ModuleName(m, CircuitName(c))
      case Nil => error("BAD")
      case s => ComponentName(s.drop(2).mkString("."), ModuleName(s(1), CircuitName(s(0))))
    }
  }
}

/**
 * Named classes associate an annotation with a component in a Firrtl circuit
 */

trait Named {
  def name: String
  def serialize: String
}

case class CircuitName(name: String) extends Named {
  if(!validModuleName(name)) throw AnnotationException(s"Illegal circuit name: $name")
  def serialize: String = name
}

case class ModuleName(name: String, circuit: CircuitName) extends Named {
  if(!validModuleName(name)) throw AnnotationException(s"Illegal module name: $name")
  def serialize: String = name + "." + circuit.serialize
}

case class ComponentName(name: String, module: ModuleName) extends Named {
  if(!validComponentName(name)) throw AnnotationException(s"Illegal component name: $name")
  def expr: Expression = toExp(name)
  def serialize: String = name + "." + module.serialize
}

case class AnnotationException(message: String) extends Exception(message)


case class Annotation(target: Named, transform: Class[_ <: Transform], value: String) {
  val targetString: String = target.serialize
  val transformClass: String = transform.getName
  def serialize: String = this.toString
  def update(tos: Seq[Named]): Seq[Annotation] = {
    check(target, tos, this)
    propagate(target, tos, duplicate)
  }
  def propagate(from: Named, tos: Seq[Named], dup: Named=>Annotation): Seq[Annotation] = tos.map(dup(_))
  def check(from: Named, tos: Seq[Named], which: Annotation): Unit = {}
  def duplicate(n: Named) = new Annotation(n, transform, value)
  //def transform: Class[_ <: Transform] = 
}

object AnnotationUtils {
  /** Returns true if a valid Module name */
  val SerializedModuleName = """([a-zA-Z_][a-zA-Z_0-9~!@#$%^*\-+=?/]*)""".r
  def validModuleName(s: String): Boolean = s match {
    case SerializedModuleName(name) => true
    case _ => false
  }

  /** Returns true if a valid component/subcomponent name */
  val SerializedComponentName = """([a-zA-Z_][a-zA-Z_0-9\[\]\.~!@#$%^*\-+=?/]*)""".r
  def validComponentName(s: String): Boolean = s match {
    case SerializedComponentName(name) => true
    case _ => false
  }

  /** Tokenizes a string with '[', ']', '.' as tokens, e.g.:
   *  "foo.bar[boo.far]" becomes Seq("foo" "." "bar" "[" "boo" "." "far" "]")
   */
  def tokenize(s: String): Seq[String] = s.find(c => "[].".contains(c)) match {
    case Some(_) =>
      val i = s.indexWhere(c => "[].".contains(c))
      Seq(s.slice(0, i), s(i).toString) ++ tokenize(s.drop(i + 1))
    case None => Seq(s)
  }

  /** Given a serialized component/subcomponent reference, subindex, subaccess,
   *  or subfield, return the corresponding IR expression.
   */
  def toExp(s: String): Expression = {
    def parse(tokens: Seq[String]): Expression = {
      val DecPattern = """([1-9]\d*)""".r
      def findClose(tokens: Seq[String], index: Int, nOpen: Int): Seq[String] =
        if(index >= tokens.size) {
          error("Cannot find closing bracket ]")
        } else tokens(index) match {
          case "[" => findClose(tokens, index + 1, nOpen + 1)
          case "]" if nOpen == 1 => tokens.slice(1, index)
          case _ => findClose(tokens, index + 1, nOpen)
        }
      def buildup(e: Expression, tokens: Seq[String]): Expression = tokens match {
        case "[" :: tail =>
          val indexOrAccess = findClose(tokens, 0, 0)
          indexOrAccess.head match {
            case DecPattern(d) => SubIndex(e, d.toInt, UnknownType)
            case _ => buildup(SubAccess(e, parse(indexOrAccess), UnknownType), tokens.slice(1, indexOrAccess.size))
          }
        case "." :: tail =>
          buildup(SubField(e, tokens(1), UnknownType), tokens.drop(2))
        case Nil => e
      }
      val root = Reference(tokens.head, UnknownType)
      buildup(root, tokens.tail)
    }
    if(validComponentName(s)) {
      parse(tokenize(s))
    } else error(s"Cannot convert $s into an expression.")
  }
}

