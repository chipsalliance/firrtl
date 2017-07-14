// See LICENSE for license details.

package firrtl.passes

// Datastructures
import scala.collection.mutable
import scala.collection.immutable.ListMap

import firrtl._
import firrtl.ir._
import firrtl.Utils._
import firrtl.Mappers._

trait Constraint[T<:IsConstrainable[T]] {
  def left: String
  def right: T
  def geq: Boolean
}
case class BGeq(left: String, right: Bound) extends Constraint[Bound] {
  val geq = true
}
case class BLeq(left: String, right: Bound) extends Constraint[Bound] {
  val geq = false
}

abstract class ConstraintSolver[T<:IsConstrainable[T]] {
  /** Unimplemented Members */
  def genMax(args: T*): T
  def genMin(args: T*): T
  def genVar(name: String): T

  /** Constraint Solver */
  val constraints = mutable.ArrayBuffer[Constraint[T]]()
  def add(c: Constraint[T]) = constraints += c

  type ConstraintMap = mutable.HashMap[String, (T, Boolean)]

  private def mergeConstraints(constraints: Seq[Constraint[T]]): Seq[Constraint[T]] = {
    val geqMap = mutable.HashMap[String, T]()
    val leqMap = mutable.HashMap[String, T]()
    constraints.foreach { c =>
      c match {
        case c: Constraint[T] if (c.geq == true)  && (leqMap.contains(c.left)) => sys.error("Bad Constraint")
        case c: Constraint[T] if (c.geq == true)  && (geqMap.contains(c.left)) => geqMap(c.left) = genMax(geqMap(c.left), c.right)
        case c: Constraint[T] if (c.geq == true)                               => geqMap(c.left) = c.right
        case c: Constraint[T] if (c.geq == false) && (geqMap.contains(c.left)) => sys.error("Bad Constraint")
        case c: Constraint[T] if (c.geq == false) && (leqMap.contains(c.left)) => leqMap(c.left) = genMin(leqMap(c.left), c.right)
        case c: Constraint[T] if (c.geq == false)                              => leqMap(c.left) = c.right
      }
    }
    constraints.toSeq
  }
  private def substitute(h: ConstraintMap)(t: T): T = {
    val tOptimized = t.optimize()
    tOptimized map substitute(h) match {
      case isVar: IsVar => h get isVar.name match {
        case None => isVar.asInstanceOf[T]
        case Some((p, geq)) =>
          val newT = substitute(h)(p)
          val newTOptimized = newT.optimize()
          h(isVar.name) = (newTOptimized, geq)
          newTOptimized
      }
      case other => other
    }
  }

  private def backwardSubstitution(h: ConstraintMap)(t: T): T = {
    t map backwardSubstitution(h) match {
      case isVar: IsVar => h.get(isVar.name) match {
        case None => isVar.asInstanceOf[T]
        case Some((p, geq)) => p
      }
      case other => other
    }
  }

  private def removeCycle(n: String, geq: Boolean)(t: T): T = t match {
    case isMax: IsMax[T] => genMax(isMax.children collect {
      case isVar: IsVar if isVar.name != n => isVar.asInstanceOf[T]
      //TODO think about this case
      //case MinusWidth(VarWidth(name), IntWidth(i)) if ((i >= 0) && (n == name)) => false
      case other => other
    }:_*)
    // TODO think about this case
    //case wx: MinusWidth => wx.arg1 match {
    //  case v: VarWidth if n == v.name => v
    //  case v => wx
    //}
    case other => other
  }

  //private def lessEqThan(name: String)(t: T): Boolean = t.optimize() match {
  //  case isMax: IsMax[T] => genMax(isMax.children.filter(!lessEqThan(name)))
  //  case isAdd: IsAdd[T] => isAdd.children match {
  //    Seq(isVar: IsVar[T], isNeg: IsNeg[T]) =>
  //  }
  //}

  private def hasVar(n: String)(t: T): Boolean = {
    var has = false
    def rec(t: T): T = {
      t match {
        case isVar: IsVar if isVar.name == n => has = true
        case _ =>
      }
      t map rec
    }
    rec(t)
    has
  }

  def solve(): ConstraintMap = {
    // Combines constraints on the same VarWidth into the same constraint
 
    // Forward solve
    // Returns a solved list where each constraint undergoes:
    //  1) Continuous Solving (using triangular solving)
    //  2) Remove Cycles
    //  3) Move to solved if not self-recursive
    val uniqueConstraints = mergeConstraints(constraints.toSeq)
    val forwardConstraintMap = new ConstraintMap
    val orderedVars = mutable.ArrayBuffer[String]()
    for (constraint <- uniqueConstraints) {
      //TODO: Risky if used improperly... need to check whether substitution from a leq to a geq is negated (always).
      val subbedRight = substitute(forwardConstraintMap)(constraint.right).optimize()
      val name = constraint.left
      val finishedRight = removeCycle(name, constraint.geq)(subbedRight)
      if (!hasVar(name)(finishedRight)) {
        forwardConstraintMap(name) = (finishedRight, constraint.geq)
        orderedVars += name
      }
    }
 
    // Backwards Solve
    val backwardConstraintMap = new ConstraintMap
    for (i <- (orderedVars.size - 1) to 0 by -1) {
      val name = orderedVars(i) // Should visit `orderedVars` backward
      val (forwardRight, forwardGeq) = forwardConstraintMap(name)
      val solvedRight = backwardSubstitution(backwardConstraintMap)(forwardRight).optimize()
      backwardConstraintMap(name) = (solvedRight, forwardGeq)
    }
    backwardConstraintMap
  }
}

class BoundConstraintSolver extends ConstraintSolver[Bound] {
  def genMax(args: Bound*): Bound = MaxBound(args:_*)
  def genMin(args: Bound*): Bound = MinBound(args:_*)
  def genVar(name: String): Bound = VarBound(name)
}

//object InferIntervals extends Pass {
//  type ConstraintMap = collection.mutable.LinkedHashMap[String, Width]
//
//     
//  def run (c: Circuit): Circuit = {
//    val v = ArrayBuffer[WGeq]()
//
//    def get_constraints_t(t1: Type, t2: Type): Seq[WGeq] = (t1,t2) match {
//      case (t1: UIntType, t2: UIntType) => Nil
//      case (t1: SIntType, t2: SIntType) => Nil
//      case (ClockType, ClockType) => Nil
//      case (FixedType(w1, p1), FixedType(w2, p2)) => Nil
//      case (IntervalType(l1, u1, p1), IntervalType(l1, u1, p2)) => Nil
//      case (AnalogType(w1), AnalogType(w2)) => Seq(WGeq(w1,w2), WGeq(w2,w1))
//      case (t1: BundleType, t2: BundleType) =>
//        (t1.fields zip t2.fields foldLeft Seq[WGeq]()){case (res, (f1, f2)) =>
//          res ++ (f1.flip match {
//            case Default => get_constraints_t(f1.tpe, f2.tpe)
//            case Flip => get_constraints_t(f2.tpe, f1.tpe)
//          })
//        }
//      case (t1: VectorType, t2: VectorType) => get_constraints_t(t1.tpe, t2.tpe)
//    }
//
//    def get_constraints_e(e: Expression): Expression = {
//      e match {
//        case (e: Mux) => v ++= Seq(
//          WGeq(getWidth(e.cond), IntWidth(1)),
//          WGeq(IntWidth(1), getWidth(e.cond))
//        )
//        case _ =>
//      }
//      e map get_constraints_e
//    }
//
//    def get_constraints_declared_type (t: Type): Type = t match {
//      case FixedType(_, p) => 
//        v += WGeq(p,IntWidth(0))
//        t
//      case _ => t map get_constraints_declared_type
//    }
//
//    def get_constraints_s(s: Statement): Statement = {
//      s map get_constraints_declared_type match {
//        case (s: Connect) =>
//          val n = get_size(s.loc.tpe)
//          val locs = create_exps(s.loc)
//          val exps = create_exps(s.expr)
//          v ++= ((locs zip exps).zipWithIndex flatMap {case ((locx, expx), i) =>
//            get_flip(s.loc.tpe, i, Default) match {
//              case Default => get_constraints_t(locx.tpe, expx.tpe)//WGeq(getWidth(locx), getWidth(expx))
//              case Flip => get_constraints_t(expx.tpe, locx.tpe)//WGeq(getWidth(expx), getWidth(locx))
//            }
//          })
//        case (s: PartialConnect) =>
//          val ls = get_valid_points(s.loc.tpe, s.expr.tpe, Default, Default)
//          val locs = create_exps(s.loc)
//          val exps = create_exps(s.expr)
//          v ++= (ls flatMap {case (x, y) =>
//            val locx = locs(x)
//            val expx = exps(y)
//            get_flip(s.loc.tpe, x, Default) match {
//              case Default => get_constraints_t(locx.tpe, expx.tpe)//WGeq(getWidth(locx), getWidth(expx))
//              case Flip => get_constraints_t(expx.tpe, locx.tpe)//WGeq(getWidth(expx), getWidth(locx))
//            }
//          })
//        case (s: DefRegister) => v ++= (
//           get_constraints_t(s.reset.tpe, UIntType(IntWidth(1))) ++
//           get_constraints_t(UIntType(IntWidth(1)), s.reset.tpe) ++ 
//           get_constraints_t(s.tpe, s.init.tpe))
//        case (s:Conditionally) => v ++= 
//           get_constraints_t(s.pred.tpe, UIntType(IntWidth(1))) ++
//           get_constraints_t(UIntType(IntWidth(1)), s.pred.tpe)
//        case Attach(_, exprs) =>
//          // All widths must be equal
//          val widths = exprs map (e => getWidth(e.tpe))
//          v ++= widths.tail map (WGeq(widths.head, _))
//        case _ =>
//      }
//      s map get_constraints_e map get_constraints_s
//    }
//
//    c.modules foreach (_ map get_constraints_s)
//    c.modules foreach (_.ports foreach {p => get_constraints_declared_type(p.tpe)})
//
//    //println("======== ALL CONSTRAINTS ========")
//    //for(x <- v) println(x)
//    //println("=================================")
//    val h = solve_constraints(v)
//    //println("======== SOLVED CONSTRAINTS ========")
//    //for(x <- h) println(x)
//    //println("====================================")
//
//    def evaluate(w: Width): Width = {
//      def map2(a: Option[BigInt], b: Option[BigInt], f: (BigInt,BigInt) => BigInt): Option[BigInt] =
//         for (a_num <- a; b_num <- b) yield f(a_num, b_num)
//      def reduceOptions(l: Seq[Option[BigInt]], f: (BigInt,BigInt) => BigInt): Option[BigInt] =
//         l.reduce(map2(_, _, f))
//
//      // This function shouldn't be necessary
//      // Added as protection in case a constraint accidentally uses MinWidth/MaxWidth
//      // without any actual Widths. This should be elevated to an earlier error
//      def forceNonEmpty(in: Seq[Option[BigInt]], default: Option[BigInt]): Seq[Option[BigInt]] =
//        if (in.isEmpty) Seq(default)
//        else in
//
//      def solve(w: Width): Option[BigInt] = w match {
//        case wx: VarWidth =>
//          for{
//            v <- h.get(wx.name) if !v.isInstanceOf[VarWidth]
//            result <- solve(v)
//          } yield result
//        case wx: MaxWidth => reduceOptions(forceNonEmpty(wx.args.map(solve), Some(BigInt(0))), max)
//        case wx: MinWidth => reduceOptions(forceNonEmpty(wx.args.map(solve), None), min)
//        case wx: PlusWidth => map2(solve(wx.arg1), solve(wx.arg2), {_ + _})
//        case wx: MinusWidth => map2(solve(wx.arg1), solve(wx.arg2), {_ - _})
//        case wx: ExpWidth => map2(Some(BigInt(2)), solve(wx.arg1), pow_minus_one)
//        case wx: IntWidth => Some(wx.width)
//        case wx => println(wx); error("Shouldn't be here"); None;
//      }
//
//      solve(w) match {
//        case None => w
//        case Some(s) => IntWidth(s)
//      }
//    }
//
//    def reduce_var_widths_w(w: Width): Width = {
//      //println-all-debug(["REPLACE: " w])
//      evaluate(w)
//      //println-all-debug(["WITH: " wx])
//    }
//
//    def reduce_var_widths_t(t: Type): Type = {
//      t map reduce_var_widths_t map reduce_var_widths_w
//    }
//
//    def reduce_var_widths_s(s: Statement): Statement = {
//      s map reduce_var_widths_s map reduce_var_widths_t
//    }
//
//    def reduce_var_widths_p(p: Port): Port = {
//      Port(p.info, p.name, p.direction, reduce_var_widths_t(p.tpe))
//    } 
//  
//    InferTypes.run(c.copy(modules = c.modules map (_
//      map reduce_var_widths_p
//      map reduce_var_widths_s)))
//  }
//}
