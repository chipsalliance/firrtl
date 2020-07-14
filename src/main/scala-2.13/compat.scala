package firrtl

import scala.collection.mutable.{ArrayBuffer}
import firrtl.annotations.{ ReferenceTarget, PresetAnnotation }
import scala.collection.MapView
import scala.collection.immutable.Iterable

object compat {
  val JavaConverters = scala.jdk.CollectionConverters

  /**
    * Instance wrappers. Use AnyVal to avoid extra JVM allocations
    * @param data
    */
  object wrappers {
  
    implicit class ArrSeqWrapper[A](private val data: ArrayBuffer[A]) extends AnyVal {
      def wrap():Seq[A] = data.toSeq
    }

    implicit class ArrISeqWrapper[A](private val data : ArrayBuffer[A]) extends AnyVal {
      def wrap() : IndexedSeq[A] = IndexedSeq.from(data)
    }

    implicit class MapViewWrapper[A,B](private val data:MapView[A,B]) extends AnyVal {
      def wrap():Map[A,B] = data.toMap
    }

    implicit class IterWrapper[A,B](private val data : Iterable[(A,B)]) extends AnyVal {
      def wrap() : Map[A,B] = Map.from(data)
    }
  }

  object instances {
  /** 
    * Store Emission option per Target
    * Guarantee only one emission option per Target 
    */
    private[firrtl] class EmissionOptionMap[V <: EmissionOption](val df : V) extends collection.mutable.WeakHashMap[ReferenceTarget, V] {
      override def default(key: ReferenceTarget) = df
    }
  }

  object Annos {
    def annos(cs:CircuitState, presetAnnos:Seq[PresetAnnotation]) = cs.annotations.to(ArrayBuffer)  --= ArrayBuffer.from(presetAnnos)
  }
}
