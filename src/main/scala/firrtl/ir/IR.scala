// See LICENSE for license details.

package firrtl
package ir

import Utils.{indent, trim, dec2string}
import scala.math.BigDecimal.RoundingMode._
import scala.collection.mutable
import passes.{IsConstrainable, IsKnown, IsVar}

/** Intermediate Representation */
abstract class FirrtlNode {
  def serialize: String
}

abstract class Info extends FirrtlNode {
  // default implementation
  def serialize: String = this.toString
  def ++(that: Info): Info
}
case object NoInfo extends Info {
  override def toString: String = ""
  def ++(that: Info): Info = that
}
case class FileInfo(info: StringLit) extends Info {
  override def toString: String = " @[" + info.serialize + "]"
  def ++(that: Info): Info = MultiInfo(Seq(this, that))
}
case class MultiInfo(infos: Seq[Info]) extends Info {
  private def collectStringLits(info: Info): Seq[StringLit] = info match {
    case FileInfo(lit) => Seq(lit)
    case MultiInfo(seq) => seq flatMap collectStringLits
    case NoInfo => Seq.empty
  }
  override def toString: String =
    collectStringLits(this).map(_.serialize).mkString(" @[", " ", "]")
  def ++(that: Info): Info = MultiInfo(Seq(this, that))
}

trait HasName {
  val name: String
}
trait HasInfo {
  val info: Info
}
trait IsDeclaration extends HasName with HasInfo

case class StringLit(array: Array[Byte]) extends FirrtlNode {
  def serialize: String = FIRRTLStringLitHandler.escape(this)
}

/** Primitive Operation
  *
  * See [[PrimOps]]
  */
abstract class PrimOp extends FirrtlNode {
  def serialize: String = this.toString
  def apply(args: Any*): DoPrim = {
    val groups = args.groupBy {
      case x: Expression => "exp"
      case x: BigInt => "int"
      case x: Int => "int"
      case other => "other"
    }
    val exprs = groups.getOrElse("exp", Nil).collect {
      case e: Expression => e
    }
    val consts = groups.getOrElse("int", Nil).map {
      _ match {
        case i: BigInt => i
        case i: Int => BigInt(i)
      }
    }
    groups.get("other") match {
      case None =>
      case Some(x) => sys.error(s"Shouldn't be here: $x")
    }
    DoPrim(this, exprs, consts, UnknownType)
  }
}

abstract class Expression extends FirrtlNode {
  def tpe: Type
  def mapExpr(f: Expression => Expression): Expression
  def mapType(f: Type => Type): Expression
  def mapWidth(f: Width => Width): Expression
}
case class Reference(name: String, tpe: Type) extends Expression with HasName {
  def serialize: String = name
  def mapExpr(f: Expression => Expression): Expression = this
  def mapType(f: Type => Type): Expression = this.copy(tpe = f(tpe))
  def mapWidth(f: Width => Width): Expression = this
}
case class SubField(expr: Expression, name: String, tpe: Type) extends Expression with HasName {
  def serialize: String = s"${expr.serialize}.$name"
  def mapExpr(f: Expression => Expression): Expression = this.copy(expr = f(expr))
  def mapType(f: Type => Type): Expression = this.copy(tpe = f(tpe))
  def mapWidth(f: Width => Width): Expression = this
}
case class SubIndex(expr: Expression, value: Int, tpe: Type) extends Expression {
  def serialize: String = s"${expr.serialize}[$value]"
  def mapExpr(f: Expression => Expression): Expression = this.copy(expr = f(expr))
  def mapType(f: Type => Type): Expression = this.copy(tpe = f(tpe))
  def mapWidth(f: Width => Width): Expression = this
}
case class SubAccess(expr: Expression, index: Expression, tpe: Type) extends Expression {
  def serialize: String = s"${expr.serialize}[${index.serialize}]"
  def mapExpr(f: Expression => Expression): Expression =
    this.copy(expr = f(expr), index = f(index))
  def mapType(f: Type => Type): Expression = this.copy(tpe = f(tpe))
  def mapWidth(f: Width => Width): Expression = this
}
case class Mux(cond: Expression, tval: Expression, fval: Expression, tpe: Type = UnknownType) extends Expression {
  def serialize: String = s"mux(${cond.serialize}, ${tval.serialize}, ${fval.serialize})"
  def mapExpr(f: Expression => Expression): Expression = Mux(f(cond), f(tval), f(fval), tpe)
  def mapType(f: Type => Type): Expression = this.copy(tpe = f(tpe))
  def mapWidth(f: Width => Width): Expression = this
}
case class ValidIf(cond: Expression, value: Expression, tpe: Type) extends Expression {
  def serialize: String = s"validif(${cond.serialize}, ${value.serialize})"
  def mapExpr(f: Expression => Expression): Expression = ValidIf(f(cond), f(value), tpe)
  def mapType(f: Type => Type): Expression = this.copy(tpe = f(tpe))
  def mapWidth(f: Width => Width): Expression = this
}
abstract class Literal extends Expression {
  val value: BigInt
  val width: Width
}
case class UIntLiteral(value: BigInt, width: Width) extends Literal {
  def tpe = UIntType(width)
  def serialize = s"""UInt${width.serialize}("h""" + value.toString(16)+ """")"""
  def mapExpr(f: Expression => Expression): Expression = this
  def mapType(f: Type => Type): Expression = this
  def mapWidth(f: Width => Width): Expression = UIntLiteral(value, f(width))
}
case class SIntLiteral(value: BigInt, width: Width) extends Literal {
  def tpe = SIntType(width)
  def serialize = s"""SInt${width.serialize}("h""" + value.toString(16)+ """")"""
  def mapExpr(f: Expression => Expression): Expression = this
  def mapType(f: Type => Type): Expression = this
  def mapWidth(f: Width => Width): Expression = SIntLiteral(value, f(width))
}
case class FixedLiteral(value: BigInt, width: Width, point: Width) extends Literal {
  def tpe = FixedType(width, point)
  def serialize = {
    val pstring = if(point == UnknownWidth) "" else s"<${point.serialize}>"
    s"""Fixed${width.serialize}$pstring("h${value.toString(16)}")"""
  }
  def mapExpr(f: Expression => Expression): Expression = this
  def mapType(f: Type => Type): Expression = this
  def mapWidth(f: Width => Width): Expression = FixedLiteral(value, f(width), f(point))
}
case class DoPrim(op: PrimOp, args: Seq[Expression], consts: Seq[BigInt], tpe: Type) extends Expression {
  def serialize: String = op.serialize + "(" +
    (args.map(_.serialize) ++ consts.map(_.toString)).mkString(", ") + ")"
  def mapExpr(f: Expression => Expression): Expression = this.copy(args = args map f)
  def mapType(f: Type => Type): Expression = this.copy(tpe = f(tpe))
  def mapWidth(f: Width => Width): Expression = this
}

abstract class Statement extends FirrtlNode {
  def mapStmt(f: Statement => Statement): Statement
  def mapExpr(f: Expression => Expression): Statement
  def mapType(f: Type => Type): Statement
  def mapString(f: String => String): Statement
}
case class DefWire(info: Info, name: String, tpe: Type) extends Statement with IsDeclaration {
  def serialize: String = s"wire $name : ${tpe.serialize}" + info.serialize
  def mapStmt(f: Statement => Statement): Statement = this
  def mapExpr(f: Expression => Expression): Statement = this
  def mapType(f: Type => Type): Statement = DefWire(info, name, f(tpe))
  def mapString(f: String => String): Statement = DefWire(info, f(name), tpe)
}
case class DefRegister(
    info: Info,
    name: String,
    tpe: Type,
    clock: Expression,
    reset: Expression,
    init: Expression) extends Statement with IsDeclaration {
  def serialize: String =
    s"reg $name : ${tpe.serialize}, ${clock.serialize} with :" +
    indent("\n" + s"reset => (${reset.serialize}, ${init.serialize})" + info.serialize)
  def mapStmt(f: Statement => Statement): Statement = this
  def mapExpr(f: Expression => Expression): Statement =
    DefRegister(info, name, tpe, f(clock), f(reset), f(init))
  def mapType(f: Type => Type): Statement = this.copy(tpe = f(tpe))
  def mapString(f: String => String): Statement = this.copy(name = f(name))

}
case class DefInstance(info: Info, name: String, module: String) extends Statement with IsDeclaration {
  def serialize: String = s"inst $name of $module" + info.serialize
  def mapStmt(f: Statement => Statement): Statement = this
  def mapExpr(f: Expression => Expression): Statement = this
  def mapType(f: Type => Type): Statement = this
  def mapString(f: String => String): Statement = DefInstance(info, f(name), module)
}
case class DefMemory(
    info: Info,
    name: String,
    dataType: Type,
    depth: Int,
    writeLatency: Int,
    readLatency: Int,
    readers: Seq[String],
    writers: Seq[String],
    readwriters: Seq[String],
    // TODO: handle read-under-write
    readUnderWrite: Option[String] = None) extends Statement with IsDeclaration {
  def serialize: String =
    s"mem $name :" + info.serialize +
    indent(
      (Seq("\ndata-type => " + dataType.serialize,
          "depth => " + depth,
          "read-latency => " + readLatency,
          "write-latency => " + writeLatency) ++
          (readers map ("reader => " + _)) ++
          (writers map ("writer => " + _)) ++
          (readwriters map ("readwriter => " + _)) ++
       Seq("read-under-write => undefined")) mkString "\n")
  def mapStmt(f: Statement => Statement): Statement = this
  def mapExpr(f: Expression => Expression): Statement = this
  def mapType(f: Type => Type): Statement = this.copy(dataType = f(dataType))
  def mapString(f: String => String): Statement = this.copy(name = f(name))
}
case class DefNode(info: Info, name: String, value: Expression) extends Statement with IsDeclaration {
  def serialize: String = s"node $name = ${value.serialize}" + info.serialize
  def mapStmt(f: Statement => Statement): Statement = this
  def mapExpr(f: Expression => Expression): Statement = DefNode(info, name, f(value))
  def mapType(f: Type => Type): Statement = this
  def mapString(f: String => String): Statement = DefNode(info, f(name), value)
}
case class Conditionally(
    info: Info,
    pred: Expression,
    conseq: Statement,
    alt: Statement) extends Statement with HasInfo {
  def serialize: String =
    s"when ${pred.serialize} :" + info.serialize +
    indent("\n" + conseq.serialize) +
    (if (alt == EmptyStmt) ""
    else "\nelse :" + indent("\n" + alt.serialize))
  def mapStmt(f: Statement => Statement): Statement = Conditionally(info, pred, f(conseq), f(alt))
  def mapExpr(f: Expression => Expression): Statement = Conditionally(info, f(pred), conseq, alt)
  def mapType(f: Type => Type): Statement = this
  def mapString(f: String => String): Statement = this
}
case class Block(stmts: Seq[Statement]) extends Statement {
  def serialize: String = stmts map (_.serialize) mkString "\n"
  def mapStmt(f: Statement => Statement): Statement = Block(stmts map f)
  def mapExpr(f: Expression => Expression): Statement = this
  def mapType(f: Type => Type): Statement = this
  def mapString(f: String => String): Statement = this
}
case class PartialConnect(info: Info, loc: Expression, expr: Expression) extends Statement with HasInfo {
  def serialize: String =  s"${loc.serialize} <- ${expr.serialize}" + info.serialize
  def mapStmt(f: Statement => Statement): Statement = this
  def mapExpr(f: Expression => Expression): Statement = PartialConnect(info, f(loc), f(expr))
  def mapType(f: Type => Type): Statement = this
  def mapString(f: String => String): Statement = this
}
case class Connect(info: Info, loc: Expression, expr: Expression) extends Statement with HasInfo {
  def serialize: String =  s"${loc.serialize} <= ${expr.serialize}" + info.serialize
  def mapStmt(f: Statement => Statement): Statement = this
  def mapExpr(f: Expression => Expression): Statement = Connect(info, f(loc), f(expr))
  def mapType(f: Type => Type): Statement = this
  def mapString(f: String => String): Statement = this
}
case class IsInvalid(info: Info, expr: Expression) extends Statement with HasInfo {
  def serialize: String =  s"${expr.serialize} is invalid" + info.serialize
  def mapStmt(f: Statement => Statement): Statement = this
  def mapExpr(f: Expression => Expression): Statement = IsInvalid(info, f(expr))
  def mapType(f: Type => Type): Statement = this
  def mapString(f: String => String): Statement = this
}
case class Attach(info: Info, exprs: Seq[Expression]) extends Statement with HasInfo {
  def serialize: String = "attach " + exprs.map(_.serialize).mkString("(", ", ", ")")
  def mapStmt(f: Statement => Statement): Statement = this
  def mapExpr(f: Expression => Expression): Statement = Attach(info, exprs map f)
  def mapType(f: Type => Type): Statement = this
  def mapString(f: String => String): Statement = this
}
case class Stop(info: Info, ret: Int, clk: Expression, en: Expression) extends Statement with HasInfo {
  def serialize: String = s"stop(${clk.serialize}, ${en.serialize}, $ret)" + info.serialize
  def mapStmt(f: Statement => Statement): Statement = this
  def mapExpr(f: Expression => Expression): Statement = Stop(info, ret, f(clk), f(en))
  def mapType(f: Type => Type): Statement = this
  def mapString(f: String => String): Statement = this
}
case class Print(
    info: Info,
    string: StringLit,
    args: Seq[Expression],
    clk: Expression,
    en: Expression) extends Statement with HasInfo {
  def serialize: String = {
    val strs = Seq(clk.serialize, en.serialize, "\"" + string.serialize + "\"") ++
               (args map (_.serialize))
    "printf(" + (strs mkString ", ") + ")" + info.serialize
  }
  def mapStmt(f: Statement => Statement): Statement = this
  def mapExpr(f: Expression => Expression): Statement = Print(info, string, args map f, f(clk), f(en))
  def mapType(f: Type => Type): Statement = this
  def mapString(f: String => String): Statement = this
}
case object EmptyStmt extends Statement {
  def serialize: String = "skip"
  def mapStmt(f: Statement => Statement): Statement = this
  def mapExpr(f: Expression => Expression): Statement = this
  def mapType(f: Type => Type): Statement = this
  def mapString(f: String => String): Statement = this
}

abstract class Width extends FirrtlNode {
  def +(x: Width): Width = (this, x) match {
    case (a: IntWidth, b: IntWidth) => IntWidth(a.width + b.width)
    case _ => UnknownWidth
  }
  def -(x: Width): Width = (this, x) match {
    case (a: IntWidth, b: IntWidth) => IntWidth(a.width - b.width)
    case _ => UnknownWidth
  }
  def max(x: Width): Width = (this, x) match {
    case (a: IntWidth, b: IntWidth) => IntWidth(a.width max b.width)
    case _ => UnknownWidth
  }
  def min(x: Width): Width = (this, x) match {
    case (a: IntWidth, b: IntWidth) => IntWidth(a.width min b.width)
    case _ => UnknownWidth
  }
  def get: BigInt = sys.error(s"Cannot call get on $this!")
}
/** Positive Integer Bit Width of a [[GroundType]] */
object IntWidth {
  private val maxCached = 1024
  private val cache = new Array[IntWidth](maxCached + 1)
  def apply(width: BigInt): IntWidth = {
    if (0 <= width && width <= maxCached) {
      val i = width.toInt
      var w = cache(i)
      if (w eq null) {
        w = new IntWidth(width)
        cache(i) = w
      }
      w
    } else new IntWidth(width)
  }
  // For pattern matching
  def unapply(w: IntWidth): Option[BigInt] = Some(w.width)
}
class IntWidth(val width: BigInt) extends Width with Product {
  def serialize: String = s"<$width>"
  override def equals(that: Any) = that match {
    case w: IntWidth => width == w.width
    case _ => false
  }
  override def hashCode = width.toInt
  override def productPrefix = "IntWidth"
  override def toString = s"$productPrefix($width)"
  def copy(width: BigInt = width) = IntWidth(width)
  def canEqual(that: Any) = that.isInstanceOf[Width]
  def productArity = 1
  def productElement(int: Int) = int match {
    case 0 => width
    case _ => throw new IndexOutOfBoundsException
  }
  override def get: BigInt = width
}
case object UnknownWidth extends Width {
  def serialize: String = ""
}
case class CalcWidth(arg: IsConstrainable) extends Width with IsConstrainable {
  def serialize: String = s"calcw(${arg.serialize})"
  def map(f: IsConstrainable=>IsConstrainable): IsConstrainable = f(arg)
  override def reduce(): IsConstrainable = arg
}
case class VarWidth(name: String) extends Width with IsVar {
  override def serialize: String = s"<$name>"
}

/** Orientation of [[Field]] */
abstract class Orientation extends FirrtlNode
case object Default extends Orientation {
  def serialize: String = ""
}
case object Flip extends Orientation {
  def serialize: String = "flip "
}

/** Field of [[BundleType]] */
case class Field(name: String, flip: Orientation, tpe: Type) extends FirrtlNode with HasName {
  def serialize: String = flip.serialize + name + " : " + tpe.serialize
}


/** Bounds of [[IntervalType]] */

trait Bound extends IsConstrainable {
  def serialize: String
}
case object UnknownBound extends Bound {
  def serialize: String = "?"
  def map(f: IsConstrainable=>IsConstrainable): IsConstrainable = this
}
case class CalcBound(arg: IsConstrainable) extends Bound {
  def serialize: String = s"calcb(${arg.serialize})"
  def map(f: IsConstrainable=>IsConstrainable): IsConstrainable = f(arg)
  override def reduce(): IsConstrainable = arg
}
case class VarBound(name: String) extends IsVar with Bound
object KnownBound {
  def unapply(b: IsConstrainable): Option[BigDecimal] = b match {
    case k: IsKnown => Some(k.value)
    case _ => None
  }
  def unapply(b: Bound): Option[BigDecimal] = b match {
    case k: IsKnown => Some(k.value)
    case _ => None
  }
}
case class Open(value: BigDecimal) extends IsKnown with Bound {
  def serialize = s"o($value)"
  def +(that: IsKnown): IsKnown = Open(value + that.value)
  def *(that: IsKnown): IsKnown = that match {
    case Closed(x) if x == 0 => Closed(x)
    case _ => Open(value * that.value)
  }
  def min(that: IsKnown): IsKnown = if(value < that.value) this else that
  def max(that: IsKnown): IsKnown = if(value > that.value) this else that
  def neg: IsKnown = Open(-value)
  def floor: IsKnown = Open(value.setScale(0, BigDecimal.RoundingMode.FLOOR)) 
  def pow: IsKnown = if(value.isBinaryDouble) Open(1 << value.toInt) else sys.error("Shouldn't be here")
}
case class Closed(value: BigDecimal) extends IsKnown with Bound {
  def serialize = s"c($value)"
  def +(that: IsKnown): IsKnown = that match {
    case Open(x) => Open(value + x)
    case Closed(x) => Closed(value + x)
  }
  def *(that: IsKnown): IsKnown = that match {
    case IsKnown(x) if value == 0 => Closed(0)
    case Open(x) => Open(value * x)
    case Closed(x) => Closed(value * x)
  }
  def min(that: IsKnown): IsKnown = if(value <= that.value) this else that
  def max(that: IsKnown): IsKnown = if(value >= that.value) this else that
  def neg: IsKnown = Closed(-value)
  def floor: IsKnown = Closed(value.setScale(0, BigDecimal.RoundingMode.FLOOR))
  def pow: IsKnown = if(value.isBinaryDouble) Closed(1 << value.toInt) else sys.error("Shouldn't be here")
}

/** Types of [[FirrtlNode]] */
abstract class Type extends FirrtlNode {
  def mapType(f: Type => Type): Type
  def mapWidth(f: Width => Width): Type
}
abstract class GroundType extends Type {
  val width: Width
  def mapType(f: Type => Type): Type = this
}
object GroundType {
  def unapply(ground: GroundType): Option[Width] = Some(ground.width)
}
abstract class AggregateType extends Type {
  def mapWidth(f: Width => Width): Type = this
}
case class UIntType(width: Width) extends GroundType {
  def serialize: String = "UInt" + width.serialize
  def mapWidth(f: Width => Width): Type = UIntType(f(width))
}
case class SIntType(width: Width) extends GroundType {
  def serialize: String = "SInt" + width.serialize
  def mapWidth(f: Width => Width): Type = SIntType(f(width))
}
case class FixedType(width: Width, point: Width) extends GroundType {
  override def serialize: String = {
    val pstring = if(point == UnknownWidth) "" else s"<${point.serialize}>"
    s"Fixed${width.serialize}$pstring"
  }
  def mapWidth(f: Width => Width): Type = FixedType(f(width), f(point))
}
case class IntervalType(lower: Bound, upper: Bound, point: Width) extends GroundType {
  override def serialize: String = {
    val lowerString = lower match {
      case Open(l)      => s"(${dec2string(l)}, "
      case Closed(l)    => s"[${dec2string(l)}, "
      case UnknownBound => s"[?, "
      case _  => s"[?, "
    }
    val upperString = upper match {
      case Open(u)      => s"${dec2string(u)})"
      case Closed(u)    => s"${dec2string(u)}]"
      case UnknownBound => s"?]"
      case _  => s"?]"
    }
    val bounds = (lower, upper) match {
      case (k1: IsKnown, k2: IsKnown) => lowerString + upperString
      case _ => ""
    }
    val pointString = point match {
      case IntWidth(i)  => "." + i.toString
      case _ => ""
    }
    "Interval" + bounds + pointString
  }
  lazy val prec = BigDecimal(1) / BigDecimal(1 << point.get.toInt)
  lazy val min = lower.optimize match {
    case Open(a) => (a / prec) match {
      case x if trim(x).isWhole => a + prec // add precision for open lower bound i.e. (-4 -> [3 for bp = 0
      case x => x.setScale(0, CEILING) * prec // Deal with unreprsentable bound representations (finite BP) -- new closed form l > original l
    }
    //case Closed(a) => (a / prec).setScale(0, FLOOR) * prec
    case Closed(a) => (a / prec).setScale(0, CEILING) * prec
  }
  lazy val max = upper.optimize match {
    case Open(a) => (a / prec) match {
      case x if trim(x).isWhole => a - prec // subtract precision for open upper bound
      case x => x.setScale(0, FLOOR) * prec
    }
    //case Closed(a) => (a / prec).setScale(0, CEILING) * prec
    case Closed(a) => (a / prec).setScale(0, FLOOR) * prec
  }
  lazy val minAdjusted = min * BigDecimal(1 << point.get.toInt) match {
    case x if trim(x).isWhole | x.doubleValue == 0.0 => x.toBigInt
    case x => sys.error(s"MinAdjusted should be a whole number: $x. Min is $min. BP is ${point.get.toInt}. Precision is $prec. Lower is ${lower}.")
  }
  lazy val maxAdjusted = max * BigDecimal(1 << point.get.toInt) match {
    case x if trim(x).isWhole => x.toBigInt
    case x => sys.error(s"MaxAdjusted should be a whole number: $x")
  }
  lazy val width: Width = (point, lower, upper) match {
    case (IntWidth(i), l: IsKnown, u: IsKnown) =>
      def resize(value: BigDecimal): BigDecimal = value * BigDecimal(1 << i.toInt)
      val resizedMin = l match {
        case Open(x) => resize(x) match {
          case v if trim(v).isWhole => v + 1
          case v => v.setScale(0, CEILING)
        }
        case Closed(x) => resize(x) match {
          case v if trim(v).isWhole => v
          case v => v.setScale(0, FLOOR)
          //case v => v.setScale(0, CEILING)
        }
      }
      val resizedMax = u match {
        case Open(x) => resize(x) match {
          case v if trim(v).isWhole => v - 1
          case v => v.setScale(0, FLOOR)
        }
        case Closed(x) => resize(x) match {
          case v if trim(v).isWhole => v
          case v => v.setScale(0, CEILING)
          //case v => v.setScale(0, FLOOR)
        }
      }
      //val r = range
      //println(r)
      //if(r.get.size > 0) {
      //  val rh = resize(r.get.head)
      //  val rl = resize(r.get.last)
      //  assert(rh == resizedMin && rl == resizedMax, s"Min of $this, $r: $rh != $resizedMin\nMax of $this, $r: $rl != $resizedMax")
      //}
      IntWidth(Math.max(Utils.getSIntWidth(resizedMin.toBigInt), Utils.getSIntWidth(resizedMax.toBigInt)))
    case _ => UnknownWidth
  }

  lazy val range: Option[Seq[BigDecimal]] = (lower, upper, point) match {
    case (l: IsKnown, u: IsKnown, p: IntWidth) =>
      if(min > max) Some(Nil) else Some(Range.BigDecimal(min, max, prec))
    case _ => None
  }
  def mapWidth(f: Width => Width): Type = this.copy(point = f(point))
}

case class BundleType(fields: Seq[Field]) extends AggregateType {
  def serialize: String = "{ " + (fields map (_.serialize) mkString ", ") + "}"
  def mapType(f: Type => Type): Type =
    BundleType(fields map (x => x.copy(tpe = f(x.tpe))))
}
case class VectorType(tpe: Type, size: Int) extends AggregateType {
  def serialize: String = tpe.serialize + s"[$size]"
  def mapType(f: Type => Type): Type = this.copy(tpe = f(tpe))
}
case object ClockType extends GroundType {
  val width = IntWidth(1)
  def serialize: String = "Clock"
  def mapWidth(f: Width => Width): Type = this
}
case class AnalogType(width: Width) extends GroundType {
  def serialize: String = "Analog" + width.serialize
  def mapWidth(f: Width => Width): Type = AnalogType(f(width))
}
case object UnknownType extends Type {
  def serialize: String = "?"
  def mapType(f: Type => Type): Type = this
  def mapWidth(f: Width => Width): Type = this
}

/** [[Port]] Direction */
abstract class Direction extends FirrtlNode
case object Input extends Direction {
  def serialize: String = "input"
}
case object Output extends Direction {
  def serialize: String = "output"
}

/** [[DefModule]] Port */
case class Port(
    info: Info,
    name: String,
    direction: Direction,
    tpe: Type) extends FirrtlNode with IsDeclaration {
  def serialize: String = s"${direction.serialize} $name : ${tpe.serialize}" + info.serialize
  def mapType(f: Type => Type): Port = Port(info, name, direction, f(tpe))
}

/** Parameters for external modules */
sealed abstract class Param extends FirrtlNode {
  def name: String
  def serialize: String = s"parameter $name = "
}
/** Integer (of any width) Parameter */
case class IntParam(name: String, value: BigInt) extends Param {
  override def serialize: String = super.serialize + value
}
/** IEEE Double Precision Parameter (for Verilog real) */
case class DoubleParam(name: String, value: Double) extends Param {
  override def serialize: String = super.serialize + value
}
/** String Parameter */
case class StringParam(name: String, value: StringLit) extends Param {
  override def serialize: String = super.serialize + value.serialize
}
/** Raw String Parameter
  * Useful for Verilog type parameters
  * @note Firrtl doesn't guarantee anything about this String being legal in any backend
  */
case class RawStringParam(name: String, value: String) extends Param {
  override def serialize: String = super.serialize + s"'$value'"
}

/** Base class for modules */
abstract class DefModule extends FirrtlNode with IsDeclaration {
  val info : Info
  val name : String
  val ports : Seq[Port]
  protected def serializeHeader(tpe: String): String =
    s"$tpe $name :${info.serialize}${indent(ports.map("\n" + _.serialize).mkString)}\n"
  def mapStmt(f: Statement => Statement): DefModule
  def mapPort(f: Port => Port): DefModule
  def mapString(f: String => String): DefModule
}
/** Internal Module
  *
  * An instantiable hardware block
  */
case class Module(info: Info, name: String, ports: Seq[Port], body: Statement) extends DefModule {
  def serialize: String = serializeHeader("module") + indent("\n" + body.serialize)
  def mapStmt(f: Statement => Statement): DefModule = this.copy(body = f(body))
  def mapPort(f: Port => Port): DefModule = this.copy(ports = ports map f)
  def mapString(f: String => String): DefModule = this.copy(name = f(name))
}
/** External Module
  *
  * Generally used for Verilog black boxes
  * @param defname Defined name of the external module (ie. the name Firrtl will emit)
  */
case class ExtModule(
    info: Info,
    name: String,
    ports: Seq[Port],
    defname: String,
    params: Seq[Param]) extends DefModule {
  def serialize: String = serializeHeader("extmodule") +
    indent(s"\ndefname = $defname\n" + params.map(_.serialize).mkString("\n"))
  def mapStmt(f: Statement => Statement): DefModule = this
  def mapPort(f: Port => Port): DefModule = this.copy(ports = ports map f)
  def mapString(f: String => String): DefModule = this.copy(name = f(name))
}

case class Circuit(info: Info, modules: Seq[DefModule], main: String) extends FirrtlNode with HasInfo {
  def serialize: String =
    s"circuit $main :" + info.serialize +
    (modules map ("\n" + _.serialize) map indent mkString "\n") + "\n"
  def mapModule(f: DefModule => DefModule): Circuit = this.copy(modules = modules map f)
  def mapString(f: String => String): Circuit = this.copy(main = f(main))
}
