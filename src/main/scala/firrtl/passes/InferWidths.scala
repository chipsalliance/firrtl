// See LICENSE for license details.

package firrtl.passes

// Datastructures
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.ListMap

import firrtl._
import firrtl.ir._
import firrtl.Utils._
import firrtl.Mappers._
import firrtl.PrimOps.{constraint2width, width2constraint}

class InferWidths extends Pass {
  private val constraintSolver = new ConstraintSolver()

  private def addTypeConstraints(t1: Type, t2: Type): Unit = (t1,t2) match {
    case (UIntType(w1), UIntType(w2)) => constraintSolver.addGeq(w1, w2)
    case (SIntType(w1), SIntType(w2)) => constraintSolver.addGeq(w1, w2)
    case (ClockType, ClockType) =>
    case (FixedType(w1, p1), FixedType(w2, p2)) => constraintSolver.addGeq(w1, w2)
    case (_: IntervalType, _: IntervalType) => Nil
    case (AnalogType(w1), AnalogType(w2)) =>
      constraintSolver.addGeq(w1, w2)
      constraintSolver.addGeq(w2, w1)
    case (t1: BundleType, t2: BundleType) =>
      (t1.fields zip t2.fields) foreach { case (f1, f2) =>
        (f1.flip, f2.flip) match {
          case (Default, Default) => addTypeConstraints(f1.tpe, f2.tpe)
          case (Flip, Flip) => addTypeConstraints(f2.tpe, f1.tpe)
          case _ => sys.error("Shouldn't be here")
        }
      }
    case (t1: VectorType, t2: VectorType) => addTypeConstraints(t1.tpe, t2.tpe)
  }
  private def addExpConstraints(e: Expression): Expression = e map addExpConstraints match {
    case m@Mux(p, tVal, fVal, t) =>
      constraintSolver.addGeq(getWidth(p), Closed(1))
      m
    case other => other
  }
  private def addDecConstraints(t: Type): Type = t match {
    case FixedType(w, _) =>
      constraintSolver.addGeq(w, Closed(0))
      t
    case _ => t map addDecConstraints
  }
  private def addStmtConstraints(s: Statement): Statement = s map addDecConstraints match {
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
    case r: DefRegister =>
       addTypeConstraints(r.reset.tpe, UIntType(IntWidth(1)))
       addTypeConstraints(r.tpe, r.init.tpe)
       r
    case a@Attach(_, exprs) =>
      // All widths must be equal
      val widths = exprs map (e => getWidth(e.tpe))
      val maxWidth = IsMax(widths.map(width2constraint):_*)
      widths.foreach { w =>
        constraintSolver.addGeq(w, maxWidth)
      }
      a
    case c: Conditionally =>
       addTypeConstraints(c.pred.tpe, UIntType(IntWidth(1)))
       c map addStmtConstraints
    case x => x map addStmtConstraints
  }
  private def fixWidth(w: Width): Width = constraintSolver.get(w) match {
    case Some(Closed(x)) if x.isWhole => IntWidth(x.toBigInt)
    case None => w
    case _ => sys.error("Shouldn't be here")
  }
  private def fixType(t: Type): Type = t map fixType map fixWidth
  private def fixStmt(s: Statement): Statement = s map fixStmt map fixType
  private def fixPort(p: Port): Port = {
    Port(p.info, p.name, p.direction, fixType(p.tpe))
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
      map fixPort
      map fixStmt)))
  }
}
