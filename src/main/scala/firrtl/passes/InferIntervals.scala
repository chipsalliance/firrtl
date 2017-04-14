// See LICENSE for license details.

package firrtl.passes

// Datastructures

import firrtl._
import firrtl.ir._
import firrtl.Utils._
import firrtl.Mappers._
import InferIntervalsUtils._
import collection.immutable.ListMap

case class Bigger(loc: Interval, exp: Interval) {
  def serialize: String = s"${loc.serialize} encompasses ${exp.serialize}"
}
case class IVar(name: String) extends Interval {
  def serialize: String = s"$name"
  def map(f: Interval=>Interval): Interval = this
}
object IAdd {
  def apply(x: Interval, y: Interval): Interval = (x, y) match {
    case (IVal(a, b), IVal(c, d)) => IVal(a + c, b + d)
    // Reorder adds
    case (IAdd(a: IVal, x), b: IVal) => IAdd(IAdd(a, b), x)
    case (IAdd(x, a: IVal), b: IVal) => IAdd(IAdd(a, b), x)
    case (b: IVal, IAdd(a: IVal, x)) => IAdd(IAdd(a, b), x)
    case (b: IVal, IAdd(x, a: IVal)) => IAdd(IAdd(a, b), x)
    // Reorder subs
    case (ISub(a: IVal, x), b: IVal) => ISub(IAdd(a, b), x)
    case (ISub(x, a: IVal), b: IVal) => IAdd(ISub(b, a), x)
    case (b: IVal, ISub(a: IVal, x)) => ISub(IAdd(a, b), x)
    case (b: IVal, ISub(x, a: IVal)) => IAdd(ISub(b, a), x)
    // Add zero
    case (IVal(a, b), x) if a == b && a == BigInt(0) => x
    case (x, IVal(a, b)) if a == b && a == BigInt(0) => x
    case _ => new IAdd(x, y)
  }
  def unapply(i: IAdd): Option[(Interval, Interval)] = Some((i.x, i.y))
}
class IAdd(val x: Interval, val y: Interval) extends Interval {
  def serialize: String = s"(${x.serialize} + ${y.serialize})"
  def map(f: Interval=>Interval): Interval = IAdd(f(x), f(y))
}
object INeg {
  def apply(x: Interval): Interval = ISub(IVal(BigInt(0), BigInt(0)), x)
}
object ISub {
  def apply(x: Interval, y: Interval): Interval = (x, y) match {
    case (IVal(a, b), IVal(c, d)) => IVal(a - d, b - c)
    // Reorder adds
    case (IAdd(a: IVal, x), b: IVal) => IAdd(ISub(a, b), x)
    case (IAdd(x, a: IVal), b: IVal) => IAdd(ISub(a, b), x)
    case (b: IVal, IAdd(a: IVal, x)) => ISub(ISub(b, a), x)
    case (b: IVal, IAdd(x, a: IVal)) => ISub(ISub(b, a), x)
    // Reorder subs
    case (ISub(a: IVal, x), b: IVal) => ISub(ISub(a, b), x)
    case (ISub(x, a: IVal), b: IVal) => IAdd(INeg(IAdd(a, b)), x)
    case (b: IVal, ISub(a: IVal, x)) => IAdd(ISub(b, a), x)
    case (b: IVal, ISub(x, a: IVal)) => ISub(IAdd(a, b), x)
    // Sub zero
    case (x, IVal(a, b)) if a == b && a == BigInt(0) => x
    case _ => new ISub(x, y)
  }
  def unapply(i: ISub): Option[(Interval, Interval)] = Some((i.x, i.y))
}
class ISub(val x: Interval, val y: Interval) extends Interval {
  def serialize: String = s"(${x.serialize} - ${y.serialize})"
  def map(f: Interval=>Interval): Interval = ISub(f(x), f(y))
}
object IMul {
  def apply(x: Interval, y: Interval): Interval = (x, y) match {
    case (IVal(a, b), IVal(c, d)) => IVal(a * c, b * d)
    // Reorder muls
    case (IMul(a: IVal, x), b: IVal) => IMul(IMul(a, b), x)
    case (IMul(x, a: IVal), b: IVal) => IMul(IMul(a, b), x)
    case (b: IVal, IMul(a: IVal, x)) => IMul(IMul(a, b), x)
    case (b: IVal, IMul(x, a: IVal)) => IMul(IMul(a, b), x)
    // Reorder divs
    case (IDiv(a: IVal, x), b: IVal) => IDiv(IMul(a, b), x)
    case (IDiv(x, a: IVal), b: IVal) => IMul(IDiv(b, a), x)
    case (b: IVal, IDiv(a: IVal, x)) => IDiv(IMul(a, b), x)
    case (b: IVal, IDiv(x, a: IVal)) => IMul(IDiv(b, a), x)
    // Mul zero
    case (IVal(a, b), x) if a == b && a == BigInt(0) => IVal(BigInt(0), BigInt(0))
    case (x, IVal(a, b)) if a == b && a == BigInt(0) => IVal(BigInt(0), BigInt(0))
    case (IVal(a, b), x) if a == b && a == BigInt(1) => x
    case (x, IVal(a, b)) if a == b && a == BigInt(1) => x
    case _ => new IMul(x, y)
  }
  def unapply(i: IMul): Option[(Interval, Interval)] = Some((i.x, i.y))
}
class IMul(val x: Interval, val y: Interval) extends Interval {
  def serialize: String = s"(${x.serialize} * ${y.serialize})"
  def map(f: Interval=>Interval): Interval = IMul(f(x), f(y))
}
object IDiv {
  def div(x: BigInt, y: BigInt): BigInt = if(y == BigInt(0)) x else x / y
  def apply(x: Interval, y: Interval): Interval = (x, y) match {
    case (IVal(a, b), IVal(c, d)) =>
      val values = Seq(div(a, c), div(a, d), div(b, c), div(b, d))
      IVal(values.reduce(_ min _), values.reduce(_ max _))
    // Reorder muls
    case (IMul(a: IVal, x), b: IVal) => IMul(IDiv(a, b), x)
    case (IMul(x, a: IVal), b: IVal) => IMul(IDiv(a, b), x)
    case (b: IVal, IMul(a: IVal, x)) => IDiv(IDiv(b, a), x)
    case (b: IVal, IMul(x, a: IVal)) => IDiv(IDiv(b, a), x)
    // Reorder divs
    case (IDiv(a: IVal, x), b: IVal) => IDiv(IDiv(a, b), x)
    case (IDiv(x, a: IVal), b: IVal) => IDiv(x, IMul(b, a))
    case (b: IVal, IDiv(a: IVal, x)) => IMul(IDiv(b, a), x)
    case (b: IVal, IDiv(x, a: IVal)) => IDiv(IMul(b, a), x)
    // Div zero
    case (IVal(a, b), x) if a == b && a == BigInt(0) => IVal(BigInt(0), BigInt(0))
    case (x, IVal(a, b)) if a <= BigInt(1) && b <= BigInt(1) && a >= BigInt(0) && b >= BigInt(0) => x
    case _ => new IDiv(x, y)
  }
  def unapply(i: IDiv): Option[(Interval, Interval)] = Some((i.x, i.y))
}
class IDiv(val x: Interval, val y: Interval) extends Interval {
  def serialize: String = s"(${x.serialize} / ${y.serialize})"
  def map(f: Interval=>Interval): Interval = IDiv(f(x), f(y))
}
object IRem {
  def mod(x: BigInt, y: BigInt): BigInt = if(y == BigInt(0)) BigInt(0) else x mod y
  def apply(x: Interval, y: Interval): Interval = (x, y) match {
    case (IVal(a, b), IVal(c, d)) =>
      val pairs = (a until (b + 1)).flatMap(x => (c until (d + 1)).map((_, x))).map {
        case (x, y) => mod(x, y)
      }
      IVal(pairs.reduce(_ min _),pairs.reduce(_ max _))
    case _ => new IRem(x, y)
  }
  def unapply(i: IRem): Option[(Interval, Interval)] = Some((i.x, i.y))
}
class IRem(val x: Interval, val y: Interval) extends Interval {
  def serialize: String = s"(${x.serialize} % ${y.serialize})"
  def map(f: Interval=>Interval): Interval = IRem(f(x), f(y))
}
object IWrap {
  def wrap(x: BigInt, hi: BigInt, lo: BigInt): BigInt = ((x - lo) mod (hi - lo + 1)) + lo
  def apply(x: Interval, hi: BigInt, lo: BigInt): Interval = x match {
    case IVal(a, b) =>
      val values = (a until (b + 1)).map { y => wrap(y, hi, lo) }
      IVal(values.reduce(_ min _), values.reduce(_ max _))
    case _ => new IWrap(x, hi, lo)
  }
  def unapply(i: IWrap): Option[(Interval, BigInt, BigInt)] = Some((i.x, i.hi, i.lo))
}
class IWrap(val x: Interval, val hi: BigInt, val lo: BigInt) extends Interval {
  def serialize: String = s"(${x.serialize} wrap($hi, $lo))"
  def map(f: Interval=>Interval): Interval = IWrap(f(x), hi, lo)
}
object ISat {
  def sat(x: BigInt, hi: BigInt, lo: BigInt): BigInt = if(x >= hi) hi else if(x <= lo) lo else x
  def apply(x: Interval, hi: BigInt, lo: BigInt): Interval = x match {
    case IVal(a, b) =>
      val values = (a until (b + 1)).map { y => sat(y, hi, lo) }
      IVal(values.reduce(_ min _), values.reduce(_ max _))
    case _ => new ISat(x, hi, lo)
  }
  def unapply(i: ISat): Option[(Interval, BigInt, BigInt)] = Some((i.x, i.hi, i.lo))
}
class ISat(val x: Interval, val hi: BigInt, val lo: BigInt) extends Interval {
  def serialize: String = s"(${x.serialize} sat($hi, $lo))"
  def map(f: Interval=>Interval): Interval = ISat(f(x), hi, lo)
}
object IMax {
  def apply(is: Seq[Interval]): Interval = {
    val flattened = is.flatMap { i =>
      i match {
        case IMax(seq) => seq
        case x => Seq(x)
      }
    }
    val (seq, imax) = flattened.foldLeft((Seq[Interval](), IVal(BigInt(0), BigInt(0)))) { case ((seq, IVal(a, b)), next) =>
      next match {
        case IVal(x, y) => (seq, IVal(min(a, x), max(b, y)))
        case _ => (seq :+ next, IVal(a, b))
      }
    }
    seq match {
      case Nil => imax
      case _ => new IMax(seq :+ imax)
    }
  }
  def unapply(i: IMax): Option[Seq[Interval]] = Some(i.is)
}
class IMax(val is: Seq[Interval]) extends Interval {
  def serialize: String = is map (_.serialize) mkString ("max(", ", ", ")")
  def map(f: Interval=>Interval): Interval = IMax(is map f)
}

object InferIntervalsUtils {

  type ConstraintMap = collection.mutable.LinkedHashMap[String, Interval]

  def uniquify(ls: Seq[Bigger]): ListMap[String, Interval] = {
    ls.foldLeft(ListMap[String, Interval]()) { (h, g) => 
      g.loc match {
        case w: IVar => h get w.name match {
          case None => h + (w.name -> g.exp)
          case Some(p) => h + (w.name -> IMax(Seq(g.exp, p)))
        }
        case w: IVal => h
        case _ => throwInternalError
      }
    }
  }

  def simplify(w: Interval): Interval = {
    //val opts = Seq.empty
    //opts.foldLeft(w) { (width, opt) => opt(width) }
    w
  }

  def substitute(h: ConstraintMap)(w: Interval): Interval = {
    simplify(w) map substitute(h) match {
      case wxx: IVar => h get wxx.name match {
        case None => wxx
        case Some(p) =>
          val t = simplify(substitute(h)(p))
          h(wxx.name) = t
          t
      }
      case wxx => wxx
    }
  }

  def backwardSubstitution(h: ConstraintMap)(w: Interval): Interval = {
    w map backwardSubstitution(h) match {
      case wx: IVar => h getOrElse (wx.name, wx)
      case wx => wx
    }
  }

  def removeCycle(n: String)(w: Interval): Interval = w match {
    case IMax(is) => IMax(is filter { 
      case IVar(name) => n != name
      case x => true
    })
    case _ => w
  }

  def hasIVar(n: String)(w: Interval): Boolean = {
    var has = false
    def rec(w: Interval): Interval = {
      w match {
        case wx: IVar if wx.name == n => has = true
        case _ =>
      }
      w map rec
    }
    rec(w)
    has
  }
 
  def solveConstraints(l: Seq[Bigger]): ConstraintMap = {
    //; Forward solve
    //; Returns a solved list where each constraint undergoes:
    //;  1) Continuous Solving (using triangular solving)
    //;  2) Remove Cycles
    //;  3) Move to solved if not self-recursive
    val u = uniquify(l)
    
    //println("======== UNIQUE CONSTRAINTS ========")
    //for (x <- u) { println(s"${x._1}: ${x._2.serialize}") }
    //println("====================================")
 
    val f = new ConstraintMap
    val o = collection.mutable.ArrayBuffer[String]()
    for ((n, e) <- u) {
      //println("==== SOLUTIONS TABLE ====")
      //for (x <- f) println(x)
      //println("=========================")

      val e_sub = simplify(substitute(f)(e))

      //println("Solving " + n + " => " + e)
      //println("After Substitute: " + n + " => " + e_sub)
      //println("==== SOLUTIONS TABLE (Post Substitute) ====")
      //for (x <- f) println(x)
      //println("=========================")

      val ex = removeCycle(n)(e_sub)

      //println("After Remove Cycle: " + n + " => " + ex)
      if (!hasIVar(n)(ex)) {
        //println("Not rec!: " + n + " => " + ex)
        //println("Adding [" + n + "=>" + ex + "] to Solutions Table")
        f(n) = ex
        o += n
      }
    }
 
    //println("Forward Solved Constraints")
    //for (x <- f) println(x)
 
    //; Backwards Solve
    val b = new ConstraintMap
    for (i <- (o.size - 1) to 0 by -1) {
      val n = o(i) // Should visit `o` backward
      /*
      println("SOLVE BACK: [" + n + " => " + f(n) + "]")
      println("==== SOLUTIONS TABLE ====")
      for (x <- b) println(x)
      println("=========================")
      */
      val ex = simplify(backwardSubstitution(b)(f(n)))
      /*
      println("BACK RETURN: [" + n + " => " + ex + "]")
      */
      b(n) = ex
      /*
      println("==== SOLUTIONS TABLE (Post backsolve) ====")
      for (x <- b) println(x)
      println("=========================")
      */
    }
    b
  }
  def evaluate(h: ConstraintMap)(w: Interval): Interval = {
    def map2(a: Option[BigInt], b: Option[BigInt], f: (BigInt,BigInt) => BigInt): Option[BigInt] =
       for (a_num <- a; b_num <- b) yield f(a_num, b_num)
    def reduceOptions(l: Seq[Option[BigInt]], f: (BigInt,BigInt) => BigInt): Option[BigInt] =
       l.reduce(map2(_, _, f))

    // This function shouldn't be necessary
    // Added as protection in case a constraint accidentally uses MinWidth/MaxWidth
    // without any actual Widths. This should be elevated to an earlier error
    def forceNonEmpty(in: Seq[Option[BigInt]], default: Option[BigInt]): Seq[Option[BigInt]] =
      if (in.isEmpty) Seq(default)
      else in

    def solve(w: Interval): Interval = w map solve match {
      case IVar(name) => h.get(name) match {
        case Some(IVar(_)) => IUnknown //(azidar): not sure why we don't recurse...
        case Some(i) => solve(i)
        case None => IUnknown
      }
      case IVal(x, y) => IVal(x, y)
      case _: IMax|_: IAdd|_: ISub|_: IMul|_: IDiv|_: IRem => IUnknown
      case wx => println(wx); throwInternalError
    }

    solve(w)
  }
}
     
object InferIntervals extends Pass {
  private def getTypeConstraints(t1: Type, t2: Type): Seq[Bigger] = (t1,t2) match {
    case (t1: IntervalType, t2: IntervalType) => Seq(Bigger(t1.interval, t2.interval))
    case (t1: BundleType, t2: BundleType) =>
      (t1.fields zip t2.fields foldLeft Seq[Bigger]()){case (res, (f1, f2)) =>
        res ++ (f1.flip match {
          case Default => getTypeConstraints(f1.tpe, f2.tpe)
          case Flip => getTypeConstraints(f2.tpe, f1.tpe)
        })
      }
    case (t1: VectorType, t2: VectorType) => getTypeConstraints(t1.tpe, t2.tpe)
    case (_, _) => Nil
  }
  def run (c: Circuit): Circuit = {
    val v = collection.mutable.ArrayBuffer[Bigger]()

    def getStmtConstraints(sx: Statement): Statement = {
      sx match {
        case (s: Connect) =>
          val n = get_size(s.loc.tpe)
          val locs = create_exps(s.loc)
          val exps = create_exps(s.expr)
          v ++= ((locs zip exps).zipWithIndex flatMap {case ((locx, expx), i) =>
            get_flip(s.loc.tpe, i, Default) match {
              case Default => getTypeConstraints(locx.tpe, expx.tpe)
              case Flip => getTypeConstraints(expx.tpe, locx.tpe)
            }
          })
          s
        case (s: PartialConnect) =>
          val ls = get_valid_points(s.loc.tpe, s.expr.tpe, Default, Default)
          val locs = create_exps(s.loc)
          val exps = create_exps(s.expr)
          v ++= (ls flatMap {case (x, y) =>
            val locx = locs(x)
            val expx = exps(y)
            get_flip(s.loc.tpe, x, Default) match {
              case Default => getTypeConstraints(locx.tpe, expx.tpe)
              case Flip => getTypeConstraints(expx.tpe, locx.tpe)
            }
          })
          s
        case (s: DefRegister) =>
          v ++= getTypeConstraints(s.tpe, s.init.tpe)
          s
        case s => s map getStmtConstraints
      }
    }

    c.modules foreach (_ map getStmtConstraints)

    //println("======== ALL CONSTRAINTS ========")
    //for(x <- v) println(x.serialize)
    //println("=================================")
    val h = solveConstraints(v)
    //println("======== SOLVED CONSTRAINTS ========")
    //for(x <- h) println(x)
    //println("====================================")


    def replaceTypeInterval(t: Type): Type = t map replaceTypeInterval match {
      case IntervalType(IVar(name)) => IntervalType(evaluate(h)(IVar(name)))
      case tx => tx
    }

    def replaceStmtInterval(s: Statement): Statement =
      s map replaceStmtInterval map replaceTypeInterval

    def replacePortInterval(p: Port): Port =
      Port(p.info, p.name, p.direction, replaceTypeInterval(p.tpe))
  
    InferTypes.run(c.copy(modules = c.modules map (_
      map replacePortInterval
      map replaceStmtInterval)))
  }
}
