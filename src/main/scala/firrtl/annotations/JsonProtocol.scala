
package firrtl
package annotations

import scala.util.{Try, Failure}

import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.native.Serialization
import org.json4s.native.Serialization.{read, write, writePretty}

import firrtl.ir._
import firrtl.Utils.error

object JsonProtocol {
  class TransformClassSerializer extends CustomSerializer[Class[_ <: Transform]](format => (
    { case JString(s) => Class.forName(s).asInstanceOf[Class[_ <: Transform]] },
    { case x: Class[_] => JString(x.getName) }
  ))
  // TODO Reduce boilerplate?
  class NamedSerializer extends CustomSerializer[Named](format => (
    { case JString(s) => AnnotationUtils.toNamed(s) },
    { case named: Named => JString(named.serialize) }
  ))
  class CircuitNameSerializer extends CustomSerializer[CircuitName](format => (
    { case JString(s) => AnnotationUtils.toNamed(s).asInstanceOf[CircuitName] },
    { case named: CircuitName => JString(named.serialize) }
  ))
  class ModuleNameSerializer extends CustomSerializer[ModuleName](format => (
    { case JString(s) => AnnotationUtils.toNamed(s).asInstanceOf[ModuleName] },
    { case named: ModuleName => JString(named.serialize) }
  ))
  class ComponentNameSerializer extends CustomSerializer[ComponentName](format => (
    { case JString(s) => AnnotationUtils.toNamed(s).asInstanceOf[ComponentName] },
    { case named: ComponentName => JString(named.serialize) }
  ))

  /** Construct Json formatter for annotations */
  def jsonFormat(tags: Seq[Class[_ <: Annotation]]) = {
    Serialization.formats(FullTypeHints(tags.toList)).withTypeHintFieldName("class") +
      new TransformClassSerializer + new NamedSerializer + new CircuitNameSerializer +
      new ModuleNameSerializer + new ComponentNameSerializer
  }

  /** Serialize annotations to a String for emission */
  def serialize(annos: Seq[Annotation]): String = serializeTry(annos).get

  def serializeTry(annos: Seq[Annotation]): Try[String] = {
    val annosx: Seq[Annotation] = annos.flatMap {
      case a: Unserializable => a._toJsonSerializable
      case DeletedAnnotation(_, _: Unserializable) => Seq()
      case a => Seq(a)
    }
    val tags = annosx.map(_.getClass).distinct
    implicit val formats = jsonFormat(tags)
    Try(writePretty(annosx))
  }

  def deserialize(in: JsonInput): Seq[Annotation] = deserializeTry(in).get

  def deserializeTry(in: JsonInput): Try[Seq[Annotation]] = Try({
    val parsed = parse(in)
    val annos = parsed match {
      case JArray(objs) => objs
      case x => throw new InvalidAnnotationJSONException(
        s"Annotations must be serialized as a JArray, got ${x.getClass.getSimpleName} instead!")
    }
    // Gather classes so we can deserialize arbitrary Annotations
    val classes = annos.map({
      case JObject(("class", JString(c)) :: tail) => c
      case obj => throw new InvalidAnnotationJSONException(s"Expected field 'class' not found! $obj")
    }).distinct
    val loaded = classes.map(Class.forName(_).asInstanceOf[Class[_ <: Annotation]])
    implicit val formats = jsonFormat(loaded)
    read[List[Annotation]](in)
  }).recoverWith {
    // Translate some generic errors to specific ones
    case e: java.lang.ClassNotFoundException =>
      Failure(new AnnotationClassNotFoundException(e.getMessage))
    case e: org.json4s.ParserUtil.ParseException =>
      Failure(new InvalidAnnotationJSONException(e.getMessage))
  }.recoverWith { // If the input is a file, wrap in InvalidAnnotationFileException
    case e => in match {
      case FileInput(file) =>
        /* This occurs during scopt parsing, however, scopt will only print the
         * first error message and does not properly deal with nested
         * exceptions. Hence, the better processing below of wrapping the
         * annotation with an [[InvalidAnnotationFileException]] is
         * currently disabled for improved user understanding of the
         * error. */
        /* Failure(new InvalidAnnotationFileException(file, e)) */
        Failure(e)
      case _ => Failure(e)
    }
  }
}
