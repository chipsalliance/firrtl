
package firrtl
package annotations

import scala.util.{Try, Failure}
import java.io.File

trait HasSerializationHints {
  // For serialization of complicated constructor arguments, let the annotation
  // writer specify additional type hints for relevant classes that might be
  // contained within
  def typeHints: Seq[Class[_]]
}

object JsonProtocol {

  /** Serialize annotations to a String for emission */
  def serialize(annos: Seq[Annotation]): String = serializeTry(annos).get

  def serializeTry(annos: Seq[Annotation]): Try[String] =
    Failure(throw new Exception("json4s removed"))

  def deserialize(in: File): Seq[Annotation] = deserializeTry(in).get

  def deserializeTry(in: File): Try[Seq[Annotation]] =
    Failure(throw new Exception("json4s removed"))
}
