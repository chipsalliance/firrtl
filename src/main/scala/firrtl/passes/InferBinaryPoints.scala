// See LICENSE for license details.

package firrtl.passes

// Datastructures
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.ListMap

import firrtl._
import firrtl.ir._
import firrtl.Utils._
import firrtl.Mappers._
import firrtl.PrimOps.constraint2width

class InferBinaryPoints extends Pass {
  private val constraintSolver = new ConstraintSolver()

  private def addTypeConstraints(t1: Type, t2: Type): Unit = (t1,t2) match {
    case (FixedType(_, p1), FixedType(_, p2)) => constraintSolver.addGeq(p1, p2)
    case (IntervalType(l1, u1, p1), IntervalType(l2, u2, p2)) => constraintSolver.addGeq(p1, p2)
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
    case IntervalType(_, _, p) =>
      constraintSolver.addGeq(p, Closed(0))
      t
    case FixedType(_, p) =>
      constraintSolver.addGeq(p, Closed(0))
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
  private def fixTypeBP(t: Type): Type = t map fixTypeBP match {
    case IntervalType(l, u, p) => 
      val px = constraintSolver.get(p) match {
        case Some(Closed(x)) if x.isWhole => IntWidth(x.toBigInt)
        case None => p
        case _ => sys.error("Shouldn't be here")
      }
      IntervalType(l, u, px)
    case FixedType(w, p) => 
      val px = constraintSolver.get(p) match {
        case Some(Closed(x)) if x.isWhole => IntWidth(x.toBigInt)
        case None => p
        case _ => sys.error("Shouldn't be here")
      }
      FixedType(w, px)
    case x => x
  }
  private def fixStmtBP(s: Statement): Statement = s map fixStmtBP map fixTypeBP
  private def fixPortBP(p: Port): Port = {
    Port(p.info, p.name, p.direction, fixTypeBP(p.tpe))
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
      map fixPortBP
      map fixStmtBP)))
  }
}
