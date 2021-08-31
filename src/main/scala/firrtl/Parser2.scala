package firrtl

import logger.LazyLogging
import firrtl.ir._
import firrtl.Utils.time
import org.parboiled2.ParserInput.StringBasedParserInput
import org.parboiled2._
import shapeless.HNil

import scala.util.{Failure, Success, Try}

class Calculator(val input: ParserInput) extends Parser {
  def InputLine = rule { Expression ~ EOI }

  def Expression: Rule1[Int] = rule {
    Term ~ zeroOrMore(
      '+' ~ Term ~> ((_: Int) + _)
    | '-' ~ Term ~> ((_: Int) - _))
  }

  def Term = rule {
    Factor ~ zeroOrMore(
      '*' ~ Factor ~> ((_: Int) * _)
    | '/' ~ Factor ~> ((_: Int) / _))
  }

  def Factor = rule { Number | Parens }

  def Parens = rule { '(' ~ Expression ~ ')' }

  def Number = rule { capture(Digits) ~> (_.toInt) }

  def Digits = rule { oneOrMore(CharPredicate.Digit) }
}

class FirrtlParser(val input: ParserInput) extends Parser {
  import FirrtlParser._

  def Connect: Rule1[Connect] = rule { (Ref ~ ws("<=") ~ Ref) ~> ((r1, r2) => ir.Connect(NoInfo, r1, r2)) }

  def Top = rule { Expr ~ EOI }

  // ********** Expression Parsing **********

  def Expr: Rule1[Expression] = rule {
      ExprLit |
      Mux |
      DoPrim |
      ValidIf |
      Ref
  }

  val mkExprLit: (String, Option[BigInt], BigInt) => Expression = ((t, i, l) => {
    val width = i.map(IntWidth(_)).getOrElse(UnknownWidth)
    if (t == "U") UIntLiteral(l, width) else SIntLiteral(l, width)
  })
  def ExprLit: Rule1[Expression] = rule {
    (capture(ch('U') | 'S') ~ "Int" ~ ('<' ~ Number ~ '>').? ~ '(' ~ HexLit ~ ')') ~> mkExprLit
  }

  val mkMux: (Expression, Expression, Expression) => Expression = (p, e1, e2) => ir.Mux(p, e1, e2)
  def Mux: Rule1[Expression] = rule {
    "mux" ~ ws('(') ~ Expr ~ ws(',') ~ Expr ~ ws(',') ~ Expr ~ ws(')') ~> mkMux
  }

  val mkDoPrim: (String, Expression, Seq[Expression], Seq[BigInt]) => Expression =
    (op, eh, etail, lits) => ir.DoPrim(PrimOps.fromString(op), eh +: etail, lits, UnknownType)
  def DoPrim: Rule1[Expression] = rule {
    capture(PrimOp) ~ ws('(') ~ Expr ~ zeroOrMore(ws(',') ~ Expr) ~ zeroOrMore(ws(',') ~ Number) ~ ws(')') ~> mkDoPrim
  }

  val mkValidIf: (Expression, Expression) => Expression = (c, e) => ir.ValidIf(c, e, e.tpe)
  def ValidIf: Rule1[Expression] = rule {
    "validif" ~ WS ~ '(' ~ WS ~ Expr ~ WS ~ ',' ~ WS ~ Expr ~ WS ~ ')' ~> mkValidIf
  }

  // TODO can we get this from PrimOps.listing?
  def PrimOp: Rule0 = rule {
    "add" | "sub" | "mul" | "div" | "rem" | "lt" | "leq" | "gt" | "geq" | "eq" | "neq" |
      "pad" | "asUInt" | "asAsyncReset" | "asSInt" | "asClock" | "asFixedPoint" | "asInterval" |
      "shl" | "shr" | "dshl" | "dshr" | "cvt" | "neg" | "not" | "and" | "or" | "xor" |
      "andr" | "orr" | "xorr" | "cat" | "bits" | "head" | "tail" | "incp" | "decp" |
      "setp" | "wrap" | "clip" | "squz"
  }

  // ********** Reference Parsing **********
  // References are recursive objects so parsing involves composion functions that accept
  // an inner Expression in order to create the outer Expression
  val mkRef: String => Reference = x => Reference(x)
  val mkSubField: String => Expression => ir.SubField = n => e => ir.SubField(e, n)
  val mkSubIndex: BigInt => Expression => ir.SubIndex = n => e => ir.SubIndex(e, n.toInt, UnknownType)
  val mkSubAccess: Expression => Expression => ir.SubAccess = i => e => ir.SubAccess(e, i, UnknownType)

  val nestExpr: (Expression, Option[Expression => Expression]) => Expression =
    (inner, of) => of.map(_(inner)).getOrElse(inner)
  val nestExprs: (Expression => Expression, Option[Expression => Expression]) => Expression => Expression =
    (getInner, optOuter) => e => {
      val inner = getInner(e)
      optOuter.map(_(inner)).getOrElse(inner)
    }

  def Ref: Rule1[Expression] = rule {
    (Id ~> mkRef ~ optional(SubRef)) ~> nestExpr
  }

  def SubRef: Rule1[ir.Expression => ir.Expression] = rule {
    (
      ('.' ~ Id) ~> mkSubField
      | ('[' ~ Number ~ ']') ~> mkSubIndex
      | ('[' ~ Expr ~ ']') ~> mkSubAccess
    ) ~ optional(SubRef) ~> nestExprs
  }

  def Id: Rule1[String] = rule { capture(oneOrMore(Characters)) }

  def Number: Rule1[BigInt] = rule { capture(oneOrMore(Numbers)) ~> ((x: String) => BigInt(x)) }

  val mkHexLit: String => BigInt = x => BigInt(x, 16)
  def HexLit: Rule1[BigInt] = rule {
    '"' ~ 'h' ~ optional( ch('+') | '-' ) ~ capture(oneOrMore(HexDigit)) ~ '"' ~> mkHexLit
  }

  def WS = rule { quiet(zeroOrMore(WhiteSpaceChar)) }

  def ws(c: Char): Rule0 = rule { (WS ~ c ~ WS).named(c.toString) }

  // TODO is this what we want? On both sides?
  def ws(s: String) = rule { (WS ~ s ~ WS).named(s) }
}

object FirrtlParser {
  // TODO taken from:
  // https://github.com/sirthias/parboiled2/blob/master/examples/src/main/scala/org/parboiled2/examples/JsonParser.scala
  val WhiteSpaceChar      = CharPredicate(" \n\r\t\f")
  val QuoteBackslash      = CharPredicate("\"\\")
  val QuoteSlashBackSlash = QuoteBackslash ++ "/"
  val HexDigit = CharPredicate.HexDigit

  val Characters = CharPredicate.Alpha
  val Numbers = CharPredicate.Digit
}

class CustomParserInput(string: String) extends StringBasedParserInput(string: String) {
  override def charAt(ix: Int) = {
    val res = super.charAt(ix)
    println(s"Asking for charAt $ix, Got $res")
    res
  }
}

object Parser2Main extends App {

  //println(new Calculator("1+1").InputLine.run())

  //val input = "bar[z.x.y[add(foo, UInt(1))].z]"
  //val input = "sub[add(a, b)]"
  //val input = "add(add.sub[div(a, b)].foo, bar)"
  val input = "validif(cond, foo)"

  val parser = new FirrtlParser(new CustomParserInput(input))
  parser.Top.run() match {
    case Success(res) => println(s"Success! $res")
    case Failure(err: ParseError) =>
      val msg = parser.formatError(err, new ErrorFormatter(showTraces = true))
      println(msg)
  }
}

