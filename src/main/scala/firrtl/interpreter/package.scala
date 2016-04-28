package firrtl

/**
  * Created by chick on 4/28/16.
  */
package object interpreter {
  val Big0 = BigInt(0)
  val Big1 = BigInt(1)
  val DangerShiftSize = 27
  val BitsRequiredOverflowSizeBigInt = Big1 << DangerShiftSize

  val random = util.Random
  random.setSeed(0L)
  def randomBigInt(width: Int): BigInt = {
    BigInt(width, random)
  }

  def boolToInt(condition: Boolean): Int = if(condition) 1 else 0
  def boolToBigInt(condition: Boolean): BigInt = if(condition) 1 else 0
  def widthToInt(width: Width): Int = width.asInstanceOf[IntWidth].width.toInt
  def ceilingLog2(x: Int): Int = scala.math.ceil(scala.math.log(x) / scala.math.log(2)).toInt

  /**
    * give the minimum number required to hold @num adding one for sign as necessary
    *
    * @param num the number that must be contained
    * @return
    */
  def requiredBits(num: BigInt): Int = {
//    if(num > BitsRequiredOverflowSizeBigInt) throw new InterpreterException(s"Error:requiredBits num $num > $BitsRequiredOverflowSizeBigInt")
    if(num < 2) 1 + (if(num < 0) 1 else 0)
    else if(num > BitsRequiredOverflowSizeBigInt) {
      var width = DangerShiftSize
      var comparison = Big1 << width
      while(comparison <= num) {
        width += 1
        comparison <<= 1
      }
      width
    }
    else {
      val a = num.abs.toDouble + 1.0
      (scala.math.ceil(scala.math.log(a) / scala.math.log(2)) + (if(num < 0) 1.0 else 0.0)).toInt
    }
  }
}
