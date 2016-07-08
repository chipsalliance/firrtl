package firrtlTests.range
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import firrtl.range.Range._
import firrtl.range.Value
import firrtl.ir.{IntWidth, SIntType, UIntType}

class ceilLog2Tests extends FlatSpec with Matchers {
  "ceilLog2 of BigInt(0)" should "be 0" in {
    ceilLog2(BigInt(0)) should be (0);
  }
  "ceilLog2 of BigInt(1)" should "have a width of 1" in {
    ceilLog2(BigInt(1)) should be (1);
  }
  "ceilLog2 of BigInt(2)" should "have a width of 2" in {
    ceilLog2(BigInt(2)) should be (2);
  }
  "ceilLog2 of BigInt(4)" should "have a width of 3" in {
    ceilLog2(BigInt(4)) should be (3);
  }
  "ceilLog2 of BigInt(15)" should "have a width of 4" in {
    ceilLog2(BigInt(15)) should be (4);
  }
}

class getWidthTests extends FlatSpec with Matchers {
  "Width required for [-1,0]" should "be 1" in {
    getWidth(Value(-1), Value(0)) should be (IntWidth(1));
  }
  "Width required for [0,1]" should "be 2" in {
    getWidth(Value(0), Value(1)) should be (IntWidth(2));
  }
  "Width required for [-1,1]" should "be 2" in {
    getWidth(Value(-1), Value(1)) should be (IntWidth(2));
  }
  "Width required for [-1,2]" should "be 3" in {
    getWidth(Value(-1), Value(2)) should be (IntWidth(3));
  }
  "Width required for [-4,3]" should "be 3" in {
    getWidth(Value(-4), Value(3)) should be (IntWidth(3));
  }
  "Width required for [-9,7]" should "be 5" in {
    getWidth(Value(-9), Value(7)) should be (IntWidth(5));
  }
}

class getMinTests extends FlatSpec with Matchers {
  "Minimum of UInt<0>" should "be 0" in {
    getMin(UIntType(IntWidth(0))) should be (Value(0));
  }
  "Minimum of UInt<1>" should "be -1" in {
    getMin(UIntType(IntWidth(1))) should be (Value(0));
  }
  "Minimum of SInt<0>" should "be 0" in {
    getMin(SIntType(IntWidth(0))) should be (Value(0));
  }
  "Minimum of SInt<1>" should "be -1" in {
    getMin(SIntType(IntWidth(1))) should be (Value(-1));
  }
  "Minimum of SInt<4>" should "be -8" in {
    getMin(SIntType(IntWidth(4))) should be (Value(-8));
  }
}

class getMaxTests extends FlatSpec with Matchers {
  "Maximum of UInt<0>" should "be 0" in {
    getMax(UIntType(IntWidth(0))) should be (Value(0));
  }
  "Maximum of UInt<1>" should "be 1" in {
    getMax(UIntType(IntWidth(1))) should be (Value(1));
  }
  "Maximum of UInt<2>" should "be 3" in {
    getMax(UIntType(IntWidth(2))) should be (Value(3));
  }
  "Maximum of UInt<5>" should "be 31" in {
    getMax(UIntType(IntWidth(5))) should be (Value(31));
  }
  "Maximum of SInt<0>" should "be 0" in {
    getMax(SIntType(IntWidth(0))) should be (Value(0));
  }
  "Maximum of SInt<1>" should "be 0" in {
    getMax(SIntType(IntWidth(1))) should be (Value(0));
  }
  "Maximum of SInt<2>" should "be 1" in {
    getMax(SIntType(IntWidth(2))) should be (Value(1));
  }
  "Maximum of SInt<5>" should "be 15" in {
    getMax(SIntType(IntWidth(5))) should be (Value(15));
  }
}

// vim: set ts=4 sw=4 et:
