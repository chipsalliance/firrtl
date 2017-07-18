// See LICENSE for license details.

package firrtl.passes

// Datastructures
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.ListMap

import firrtl._
import firrtl.ir._
import firrtl.Utils._
import firrtl.Mappers._

class InferBinaryPoints extends Pass {
  type ConstraintMap = collection.mutable.LinkedHashMap[String, Width]

  def run (c: Circuit): Circuit = {
    val v = ArrayBuffer[WGeq]()

    def get_constraints_t(t1: Type, t2: Type): Seq[WGeq] = (t1,t2) match {
      case (FixedType(_, p1), FixedType(_, p2)) => Seq(WGeq(p1,p2))
      case (IntervalType(_, _, p1), IntervalType(_, _, p2)) => Seq(WGeq(p1,p2))
      case (t1: BundleType, t2: BundleType) =>
        (t1.fields zip t2.fields foldLeft Seq[WGeq]()){case (res, (f1, f2)) =>
          res ++ (f1.flip match {
            case Default => get_constraints_t(f1.tpe, f2.tpe)
            case Flip => get_constraints_t(f2.tpe, f1.tpe)
          })
        }
      case (t1: VectorType, t2: VectorType) => get_constraints_t(t1.tpe, t2.tpe)
      case (_, _) => Nil
    }

    def get_constraints_declared_type (t: Type): Type = t match {
      case FixedType(_, p) => 
        v += WGeq(p,IntWidth(0))
        t
      case IntervalType(_, _, p) => 
        v += WGeq(p, IntWidth(0))
        t
      case _ => t map get_constraints_declared_type
    }

    def get_constraints_s(s: Statement): Statement = {
      s map get_constraints_declared_type match {
        case (s: Connect) =>
          val n = get_size(s.loc.tpe)
          val locs = create_exps(s.loc)
          val exps = create_exps(s.expr)
          v ++= ((locs zip exps).zipWithIndex flatMap {case ((locx, expx), i) =>
            get_flip(s.loc.tpe, i, Default) match {
              case Default => get_constraints_t(locx.tpe, expx.tpe)//WGeq(getWidth(locx), getWidth(expx))
              case Flip => get_constraints_t(expx.tpe, locx.tpe)//WGeq(getWidth(expx), getWidth(locx))
            }
          })
        case (s: PartialConnect) =>
          val ls = get_valid_points(s.loc.tpe, s.expr.tpe, Default, Default)
          val locs = create_exps(s.loc)
          val exps = create_exps(s.expr)
          v ++= (ls flatMap {case (x, y) =>
            val locx = locs(x)
            val expx = exps(y)
            get_flip(s.loc.tpe, x, Default) match {
              case Default => get_constraints_t(locx.tpe, expx.tpe)//WGeq(getWidth(locx), getWidth(expx))
              case Flip => get_constraints_t(expx.tpe, locx.tpe)//WGeq(getWidth(expx), getWidth(locx))
            }
          })
        case _ =>
      }
      s map get_constraints_s
    }

    c.modules foreach (_ map get_constraints_s)
    c.modules foreach (_.ports foreach {p => get_constraints_declared_type(p.tpe)})

    //println("======== ALL CONSTRAINTS ========")
    //for(x <- v) println(x)
    //println("=================================")
    val h = InferWidths.solve_constraints(v)
    //println("======== SOLVED CONSTRAINTS ========")
    //for(x <- h) println(x)
    //println("====================================")

    def evaluate(w: Width): Width = {
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

      def solve(w: Width): Option[BigInt] = w match {
        case wx: VarWidth =>
          for{
            v <- h.get(wx.name) if !v.isInstanceOf[VarWidth]
            result <- solve(v)
          } yield result
        case wx: MaxWidth => reduceOptions(forceNonEmpty(wx.args.map(solve), Some(BigInt(0))), max)
        case wx: MinWidth => reduceOptions(forceNonEmpty(wx.args.map(solve), None), min)
        case wx: PlusWidth => map2(solve(wx.arg1), solve(wx.arg2), {_ + _})
        case wx: MinusWidth => map2(solve(wx.arg1), solve(wx.arg2), {_ - _})
        case wx: ExpWidth => map2(Some(BigInt(2)), solve(wx.arg1), pow_minus_one)
        case wx: IntWidth => Some(wx.width)
        case wx => println(wx); error("Shouldn't be here"); None;
      }

      solve(w) match {
        case None => w
        case Some(s) => IntWidth(s)
      }
    }

    def reduce_var_widths_w(w: Width): Width = {
      //println-all-debug(["REPLACE: " w])
      evaluate(w)
      //println-all-debug(["WITH: " wx])
    }

    def reduce_var_widths_t(t: Type): Type = {
      t map reduce_var_widths_t map reduce_var_widths_w
    }

    def reduce_var_widths_s(s: Statement): Statement = {
      s map reduce_var_widths_s map reduce_var_widths_t
    }

    def reduce_var_widths_p(p: Port): Port = {
      Port(p.info, p.name, p.direction, reduce_var_widths_t(p.tpe))
    } 
  
    InferTypes.run(c.copy(modules = c.modules map (_
      map reduce_var_widths_p
      map reduce_var_widths_s)))
  }
}
