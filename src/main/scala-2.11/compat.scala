package firrtl

import scala.collection.mutable.{ArrayBuffer}
import firrtl.annotations.{ ReferenceTarget, PresetAnnotation }
import firrtl.transforms.PropagatePresetAnnotations

object compat {
  val JavaConverters = collection.JavaConverters

  /**
    * Instance wrappers. Use AnyVal to avoid extra JVM allocations
    * @param data
    */
  object wrappers {
  implicit class ArrSeqWrapper[A](private val data: ArrayBuffer[A]) extends AnyVal {
      def wrap():Seq[A] = data
    }

  implicit class ArrISeqWrapper[A](private val data: ArrayBuffer[A]) extends AnyVal {
    def wrap():IndexedSeq[A] = data.toIndexedSeq
  }

  implicit class MapViewWrapper[A,B](private val data:Map[A,B]) extends AnyVal {
    def wrap():Map[A,B] = data
  }

  // Dummy types, not used 
  trait IterWrapper[A,B]

  }

  object Annos {
    def annos(cs:CircuitState, presetAnnos:Seq[PresetAnnotation]) = cs.annotations.to[ArrayBuffer] -- presetAnnos
  }

}
