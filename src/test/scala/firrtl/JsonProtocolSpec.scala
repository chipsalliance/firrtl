// See LICENSE for license details.

package firrtlTests

import org.scalatest.FlatSpec
import org.json4s._
import org.json4s.native.JsonMethods._

import firrtl.annotations.{NoTargetAnnotation, JsonProtocol, InvalidAnnotationJSONException, HasSerializationHints, Annotation}

object JsonProtocolTestClasses {
  trait Parent

  case class ChildA(foo: Int) extends Parent
  case class ChildB(bar: String) extends Parent
  case class PolymorphicParameterAnnotation(param: Parent) extends NoTargetAnnotation
  case class PolymorphicParameterAnnotationWithTypeHints(param: Parent) extends NoTargetAnnotation with HasSerializationHints {
    def typeHints = Seq(param.getClass)
  }

  case class TypeParameterizedAnnotation[T](param: T) extends NoTargetAnnotation
  case class TypeParameterizedAnnotationWithTypeHints[T](param: T) extends NoTargetAnnotation with HasSerializationHints {
    def typeHints = Seq(param.getClass)
  }
}

import JsonProtocolTestClasses._

class JsonProtocolSpec extends FlatSpec {
  def serializeAndDeserialize(anno: Annotation): Annotation = {
    val serializedAnno = JsonProtocol.serialize(Seq(anno))
    JsonProtocol.deserialize(serializedAnno).head
  }

  "Annotations with polymorphic parameters" should "not serialize and deserialize without type hints" in {
    val anno = PolymorphicParameterAnnotation(ChildA(1))
    assertThrows[InvalidAnnotationJSONException] {
      serializeAndDeserialize(anno)
    }
  }

  it should "serialize and deserialize with type hints" in {
    val anno = PolymorphicParameterAnnotationWithTypeHints(ChildA(1))
    val deserAnno = serializeAndDeserialize(anno)
    assert(anno == deserAnno)

    val anno2 = PolymorphicParameterAnnotationWithTypeHints(ChildB("Test"))
    val deserAnno2 = serializeAndDeserialize(anno2)
    assert(anno2 == deserAnno2)
  }

  "Annotations with non-primitive type parameters" should "not serialize and deserialize without type hints" in {
    val anno = TypeParameterizedAnnotation(ChildA(1))
    val deserAnno = serializeAndDeserialize(anno)
    assert (anno != deserAnno)
  }
  it should "serialize and deserialize with type hints" in {
    val anno = TypeParameterizedAnnotationWithTypeHints(ChildA(1))
    val deserAnno = serializeAndDeserialize(anno)
    assert (anno == deserAnno)
  }
}
