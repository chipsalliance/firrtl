package firrtl.interpreter

import firrtl._
import org.scalatest.{ShouldMatchers, FlatSpec}

/**
  * Created by chick on 4/21/16.
  */
class TypeInstanceFactorySpec extends FlatSpec with ShouldMatchers {
  behavior of "TypeInstanceFactory"

  it should "create types of proper size for UIntTypes" in {
    for(width <- 0 to 100) {
      val ui = TypeInstanceFactory(UIntType(IntWidth(width)))
      ui.min should be (0)
      ui.max should be (math.pow(2.0, width.toDouble).toInt)
    }
  }
  it should "create types of proper size for SIntTypes" in {
    for(width <- 0 to 100) {
      val ui = TypeInstanceFactory(SIntType(IntWidth(width)))
      ui.min should be (-math.pow(2.0, width.toDouble).toInt)
      ui.max should be (math.pow(2.0, width.toDouble).toInt)
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
