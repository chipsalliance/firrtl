// See LICENSE for license details.

package firrtl.passes

// Datastructures
import scala.collection.mutable
import scala.collection.immutable.ListMap

import firrtl._
import firrtl.ir._
import firrtl.Utils._
import firrtl.Mappers._

class InferIntervals extends Pass {
  private val constraintSolver = new ConstraintSolver()
  private def addTypeConstraints(t1: Type, t2: Type): Unit = (t1,t2) match {
    case (IntervalType(l1, u1, p1), IntervalType(l2, u2, p2)) =>
      //println(t1)
      //println(t2)
      constraintSolver.addLeq(l1, l2)
      constraintSolver.addGeq(u1, u2)
    case (t1: BundleType, t2: BundleType) =>
      (t1.fields zip t2.fields) foreach { case (f1, f2) =>
        (f1.flip, f2.flip) match {
          case (Default, Default) => addTypeConstraints(f1.tpe, f2.tpe)
          case (Flip, Flip) => addTypeConstraints(f2.tpe, f1.tpe)
          case _ => sys.error("Shouldn't be here")
        }
      }
    case (t1: VectorType, t2: VectorType) => addTypeConstraints(t1.tpe, t2.tpe)
    case _ =>
  }
  private def addDecConstraints (t: Type): Type = t match {
    case IntervalType(l, u, p) =>
      constraintSolver.addGeq(u, l)
      t
    case _ => t map addDecConstraints
  }
  private def addStmtConstraints(s: Statement): Statement = s match {
    case c: Connect =>
      val n = get_size(c.loc.tpe)
      val locs = create_exps(c.loc)
      val exps = create_exps(c.expr)
      (locs zip exps).zipWithIndex foreach { case ((loc, exp), i) =>
        get_flip(c.loc.tpe, i, Default) match {
          case Default => addTypeConstraints(loc.tpe, exp.tpe)
          case Flip => addTypeConstraints(exp.tpe, loc.tpe)
        }
      }
      c
    case pc: PartialConnect =>
      val ls = get_valid_points(pc.loc.tpe, pc.expr.tpe, Default, Default)
      val locs = create_exps(pc.loc)
      val exps = create_exps(pc.expr)
      ls foreach { case (x, y) =>
        val loc = locs(x)
        val exp = exps(y)
        get_flip(pc.loc.tpe, x, Default) match {
          case Default => addTypeConstraints(loc.tpe, exp.tpe)
          case Flip => addTypeConstraints(exp.tpe, loc.tpe)
        }
      }
      pc
    case x => x map addStmtConstraints map addDecConstraints
  }
  private def fixTypeBounds(t: Type): Type = {
    t map fixTypeBounds match {
      case IntervalType(l, u, p) => 
        val (lx, ux) = (constraintSolver.get(l), constraintSolver.get(u)) match {
          case (Some(x: Bound), Some(y: Bound)) => (x, y)
          case (None, None) => (l, u)
          case x => sys.error(s"Shouldn't be here: $x")
        }
        IntervalType(lx, ux, p)
      case x => x
    }
  }
  private def fixStmtBounds(s: Statement): Statement = s map fixStmtBounds map fixTypeBounds
  private def fixPortBounds(p: Port): Port = {
    Port(p.info, p.name, p.direction, fixTypeBounds(p.tpe))
  } 

  def run (c: Circuit): Circuit = {

    c.modules foreach (_ map addStmtConstraints)
    c.modules foreach (_.ports foreach {p => addDecConstraints(p.tpe)})
    //println("Initial Constraints!")
    //println(constraintSolver.serializeConstraints)

    constraintSolver.solve()
    //println("Solved Constraints!")
    //println(constraintSolver.serializeSolutions)
    InferTypes.run(c.copy(modules = c.modules map (_
      map fixPortBounds
      map fixStmtBounds)))
  }
}
