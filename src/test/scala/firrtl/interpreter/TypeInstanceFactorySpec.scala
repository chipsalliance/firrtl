package firrtl.interpreter

import firrtl._
import org.scalatest.{ShouldMatchers, FlatSpec}

/**
  * Created by chick on 4/21/16.
  */
class TypeInstanceFactorySpec extends FlatSpec with ShouldMatchers {
  behavior of "TypeInstanceFactory"

  it should "concrete types are initialized with value zero" in {
    for(width <- 0 to 100) {
      val ui = TypeInstanceFactory(UIntType(IntWidth(width)))
      ui.value should be (0)
      ui.width should be (IntWidth(width))

      val si = TypeInstanceFactory(SIntType(IntWidth(width)))
      si.value should be (0)
      si.width should be (IntWidth(width))
    }
  }
  it should "throw exception on other types" in {
    intercept[InterpreterException] {
      TypeInstanceFactory(BundleType(Seq()))
    }
    intercept[InterpreterException] {
      TypeInstanceFactory(VectorType(UIntType(IntWidth(10)), 10))
    }
    intercept[InterpreterException] {
      TypeInstanceFactory(UnknownType())
    }
  }
}
