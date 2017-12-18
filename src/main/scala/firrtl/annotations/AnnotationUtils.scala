// See LICENSE for license details.

package firrtl
package annotations

import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.native.Serialization
import org.json4s.native.Serialization.{read, write, writePretty}

import net.jcazevedo.moultingyaml._
import firrtl.annotations.AnnotationYamlProtocol._

import firrtl.ir._
import firrtl.Utils.error

//finalState.annotations.map { annos =>
//  val tags = annos.annotations.map(_.getClass).distinct
//  implicit val formats = AnnotationUtils.jsonFormat(tags)
//  outputFile.write(writePretty(annos.annotations))
//}

//if (annotationFileName.endsWith(".anno")) {
//  val annotationsYaml = io.Source.fromFile(annotationFile).getLines().mkString("\n").parseYaml
//  val annotationArray = annotationsYaml.convertTo[Array[LegacyAnnotation]]
//  optionsManager.firrtlOptions = firrtlConfig.copy(annotations = firrtlConfig.annotations ++ annotationArray)
//} else if (annotationFileName.endsWith(".json")) {
//  import org.json4s._
//  import org.json4s.native.JsonMethods.parse
//  val text = io.Source.fromFile(annotationFile).getLines().mkString("\n")
//  val parsed = parse(text)
//  val annos = parsed match { case JArray(objs) => objs }
//  val classes = annos.map({ case JObject(("class", JString(c)) :: tail) => c }).distinct
//  val loaded = classes.map(Class.forName(_).asInstanceOf[Class[_ <: Annotation]])
//  implicit val formats = AnnotationUtils.jsonFormat(loaded)
//  println(read[List[Annotation]](text))
//}

// TODO move to another file
class ClassSerializer extends CustomSerializer[Class[_ <: Transform]](format => (
  {
    case JString(s) => Class.forName(s).asInstanceOf[Class[_ <: Transform]]
  },
  {
    case x: Class[_] => JString(x.getName)
  }
))
class NamedSerializer extends CustomSerializer[Named](format => (
  {
    case JString(s) => NamedSerializer.toNamed(s)
  },
  {
    case named: Named => JString(named.serialize)
  }
))
object NamedSerializer {
  def toNamed(string: String): Named = string.split("""\.""", -1).toSeq match {
    case Seq(c) => CircuitName(c)
    case Seq(c, m) => ModuleName(m, CircuitName(c))
    case Nil => Utils.error("BAD")
    case s =>
      val componentString = s.drop(2).mkString(".")
      ComponentName(componentString, ModuleName(s.tail.head, CircuitName(s.head)))
  }
}

object AnnotationUtils {
  def toYaml(a: LegacyAnnotation): String = a.toYaml.prettyPrint
  def fromYaml(s: String): LegacyAnnotation = s.parseYaml.convertTo[LegacyAnnotation]

  /** Construct Json formatter for annotations */
  def jsonFormat(tags: Seq[Class[_ <: Annotation]]) = {
    Serialization.formats(FullTypeHints(tags.toList)).withTypeHintFieldName("class") +
      new ClassSerializer + new NamedSerializer
  }

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
      s.slice(0, i) match {
        case "" => Seq(s(i).toString) ++ tokenize(s.drop(i + 1))
        case x => Seq(x, s(i).toString) ++ tokenize(s.drop(i + 1))
      }
    case None if s == "" => Nil
    case None => Seq(s)
  }

  def toNamed(s: String): Named = tokenize(s) match {
    case Seq(n) => CircuitName(n)
    case Seq(c, m) => ModuleName(m, CircuitName(c))
    case Seq(c, m) => ModuleName(m, CircuitName(c))
    case Seq(c, m, x) => ComponentName(x, ModuleName(m, CircuitName(c)))
  }

  /** Given a serialized component/subcomponent reference, subindex, subaccess,
   *  or subfield, return the corresponding IR expression.
   *  E.g. "foo.bar" becomes SubField(Reference("foo", UnknownType), "bar", UnknownType)
   */
  def toExp(s: String): Expression = {
    def parse(tokens: Seq[String]): Expression = {
      val DecPattern = """([1-9]\d*)""".r
      def findClose(tokens: Seq[String], index: Int, nOpen: Int): Seq[String] = {
        if(index >= tokens.size) {
          error("Cannot find closing bracket ]")
        } else tokens(index) match {
          case "[" => findClose(tokens, index + 1, nOpen + 1)
          case "]" if nOpen == 1 => tokens.slice(1, index)
          case "]" => findClose(tokens, index + 1, nOpen - 1)
          case _ => findClose(tokens, index + 1, nOpen)
        }
      }
      def buildup(e: Expression, tokens: Seq[String]): Expression = tokens match {
        case "[" :: tail =>
          val indexOrAccess = findClose(tokens, 0, 0)
          val exp = indexOrAccess.head match {
            case DecPattern(d) => SubIndex(e, d.toInt, UnknownType)
            case _ => SubAccess(e, parse(indexOrAccess), UnknownType)
          }
          buildup(exp, tokens.drop(2 + indexOrAccess.size))
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

