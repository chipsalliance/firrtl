// See LICENSE for license details.

package firrtlTests.interval

import firrtl.{CircuitState, ChirrtlForm, LowFirrtlCompiler, Parser, AnnotationMap}
import firrtl.ir.{Closed, Open, KnownBound}
import scala.math.BigDecimal.RoundingMode._
import firrtl.Parser.IgnoreInfo
import firrtlTests.FirrtlFlatSpec

class IntervalMathSpec extends FirrtlFlatSpec {
  //def range(lower: KnownBound, upper: KnownBound, point: Int): Seq[BigDecimal] = {
  //  val prec = 1/Math.pow(2, point.toDouble)
  //  val minAdjusted = lower match {
  //    case Open(a) => (a / prec) match {
  //      case x if x == 0 => x + prec // add precision for open lower bound
  //      case x => x.setScale(0, DOWN) * prec
  //    }
  //    case Closed(a) => (a / prec).setScale(0, UP) * prec
  //  }
  //  val maxAdjusted = upper match {
  //    case Open(a) => (a / prec) match {
  //      case x if x == 0 => x - prec // subtract precision for open upper bound
  //      case x => x.setScale(0, DOWN) * prec
  //    }
  //    case Closed(a) => (a / prec).setScale(0, UP) * prec
  //  }
  //  Range.BigDecimal(minAdjusted, maxAdjusted, prec)
  //}
  def width(point: Int, lower: KnownBound, upper: KnownBound): Int = {
    def resize(value: BigDecimal): BigDecimal = value * Math.pow(2, point)
    def calcWidth(value: BigInt): Int = value match {
      case v if(v == 0) => 0
      case v if(v > 0) => firrtl.Utils.ceilLog2(v) + 1
      case v if(v == -1) => 1
      case v if(v < -1) => firrtl.Utils.ceilLog2(-v - 1) + 1 //e.g. v = -2 -> 1
    }
    val resizedMin = lower match {
      case Open(x) => resize(x) match {
        case v if v.scale == 0 => v + 1
        case v => v.setScale(0, UP)
      }
      case Closed(x) => resize(x) match {
        case v if v.scale == 0 => v
        case v => v.setScale(0, DOWN)
      }
    }
    val resizedMax = upper match {
      case Open(x) => resize(x) match {
        case v if v.scale == 0 => v - 1
        case v => v.setScale(0, DOWN)
      }
      case Closed(x) => resize(x) match {
        case v if v.scale == 0 => v
        case v => v.setScale(0, UP)
      }
    }
    Math.max(calcWidth(resizedMin.toBigInt), calcWidth(resizedMax.toBigInt))
  }
  val SumPattern    = """.*output sum.*<(\d+)>.*""".r
  //val ProductPattern    = """.*output product.*<(\d+)>.*""".r
  //val DifferencePattern = """.*output difference.*<(\d+)>.*""".r

  val AssignPattern     = """\s*(\w+) <= (\w+)\((.*)\)\s*""".r

  for {
    lb1        <- Seq("[", "(")
    lv1        <- Range.Double(-2.0, 1.0, 0.5)
    uv1        <- Range.Double(lv1, 1.0, 0.5)
    ub1        <- {
      (lv1 == uv1, lb1) match {
        case (true, "[") => Seq("]")
        case _ => Seq("]", ")")
      }
    }
    bp1        <- 0 to 1
    lb2        <- Seq("[", "(")
    lv2        <- Range.Double(-2.0, 1.0, 0.5)
    uv2        <- Range.Double(lv2, 1.0, 0.5)
    ub2        <- {
      (lv2 == uv2, lb2) match {
        case (true, "[") => Seq("]")
        case _ => Seq("]", ")")
      }
    }
    bp2        <- 0 to 1
  } {
    def config = s"$lb1$lv1,$uv1$ub1.$bp1 and $lb2$lv2,$uv2$ub2.$bp2"
    println(s"On $config")

    s"Configuration $config" should "pass" in {

      val input =
        s"""circuit Unit :
        |  module Unit :
        |    input  in1 : Interval$lb1$lv1, $uv1$ub1.$bp1
        |    input  in2 : Interval$lb2$lv2, $uv2$ub2.$bp2
        |    output sum        : Interval
        |    sum        <= add(in2, in1)
        |    """.stripMargin
        //|    output product    : Interval
        //|    output difference : Interval
        //|    product    <= mul(a, b)
        //|    difference <= sub(a, b)

      val lowerer = new LowFirrtlCompiler
      val res = lowerer.compileAndEmit(CircuitState(parse(input), ChirrtlForm))
      val output = res.getEmittedCircuit.value split "\n"
      def getBound(bound: String, value: Double): KnownBound = bound match {
        case "[" => Closed(BigDecimal(value))
        case "]" => Closed(BigDecimal(value))
        case "(" => Open(BigDecimal(value))
        case ")" => Open(BigDecimal(value))
      }
      for (line <- output) {
        line match {
          case SumPattern(varWidth)     =>
            assert(varWidth.toInt == width(Math.max(bp1.toInt, bp2.toInt), getBound(lb1, lv1) + getBound(lb2, lv2), getBound(ub1, uv1) + getBound(ub2, uv2)))
          //case AssignPattern(varName, operation, args) =>
          //  varName match {
          //    case "sum" =>
          //      assert(operation === "add", s"var sum should be result of an add in $line")
          //      if (binaryPoint1 > binaryPoint2) {
          //        assert(!args.contains("shl(a"), s"$config first arg should be just a in $line")
          //        assert(args.contains(s"shl(b, ${binaryPoint1 - binaryPoint2})"),
          //          s"$config second arg incorrect in $line")
          //      } else if (binaryPoint1 < binaryPoint2) {
          //        assert(args.contains(s"shl(a, ${(binaryPoint1 - binaryPoint2).abs})"),
          //          s"$config second arg incorrect in $line")
          //        assert(!args.contains("shl(b"), s"$config second arg should be just b in $line")
          //      } else {
          //        assert(!args.contains("shl(a"), s"$config first arg should be just a in $line")
          //        assert(!args.contains("shl(b"), s"$config second arg should be just b in $line")
          //      }
          //    case "product" =>
          //      assert(operation === "mul", s"var sum should be result of an add in $line")
          //      assert(!args.contains("shl(a"), s"$config first arg should be just a in $line")
          //      assert(!args.contains("shl(b"), s"$config second arg should be just b in $line")
          //    case "difference" =>
          //      assert(operation === "sub", s"var difference should be result of an sub in $line")
          //      if (binaryPoint1 > binaryPoint2) {
          //        assert(!args.contains("shl(a"), s"$config first arg should be just a in $line")
          //        assert(args.contains(s"shl(b, ${binaryPoint1 - binaryPoint2})"),
          //          s"$config second arg incorrect in $line")
          //      } else if (binaryPoint1 < binaryPoint2) {
          //        assert(args.contains(s"shl(a, ${(binaryPoint1 - binaryPoint2).abs})"),
          //          s"$config second arg incorrect in $line")
          //        assert(!args.contains("shl(b"), s"$config second arg should be just b in $line")
          //      } else {
          //        assert(!args.contains("shl(a"), s"$config first arg should be just a in $line")
          //        assert(!args.contains("shl(b"), s"$config second arg should be just b in $line")
          //      }
          //    case _ =>
          //  }
          case _ =>
        }
      }
    }
  }
}


// vim: set ts=4 sw=4 et:
