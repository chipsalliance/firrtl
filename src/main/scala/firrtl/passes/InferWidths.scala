// See LICENSE for license details.

package firrtl.passes

// Datastructures
import firrtl._
import firrtl.ir._
import firrtl.Utils._
import firrtl.Mappers._
import firrtl.Implicits.width2constraint
import firrtl.annotations.{CircuitTarget, ModuleTarget, ReferenceTarget, Target}
import firrtl.constraint.{ConstraintSolver, IsMax}

object InferWidths {
  def apply(): InferWidths = new InferWidths()
  def run(c: Circuit): Circuit = new InferWidths().run(c)
  def execute(state: CircuitState): CircuitState = new InferWidths().execute(state)
}

/** Infers the widths of all signals with unknown widths
  *
  * Is a global width inference algorithm
  * - Instances of the same module with unknown input port widths will be assigned the
  *   largest width of all assignments to each of its instance ports
  * - If you don't want the global inference behavior, then be sure to define all your input widths
  *
  * Infers the smallest width is larger than all assigned widths to a signal
  * - Note that this means that dummy assignments that are overwritten by last-connect-semantics
  *   can still influence width inference
  * - E.g.
  *   wire x: UInt
  *   x <= UInt<5>(15)
  *   x <= UInt<1>(1)
  *
  *   Since width inference occurs before lowering, it infers x's width to be 5 but with an assignment of UInt(1):
  *
  *   wire x: UInt<5>
  *   x <= UInt<1>(1)
  *
  * Uses firrtl.constraint package to infer widths
  */
class InferWidths extends Pass {
  private val constraintSolver = new ConstraintSolver()

  private def addTypeConstraints(r1: ReferenceTarget, r2: ReferenceTarget)(t1: Type, t2: Type): Unit = (t1,t2) match {
    case (UIntType(w1), UIntType(w2)) => constraintSolver.addGeq(r1.prettyPrint(""), r2.prettyPrint(""))(w1, w2)
    case (SIntType(w1), SIntType(w2)) => constraintSolver.addGeq(r1.prettyPrint(""), r2.prettyPrint(""))(w1, w2)
    case (ClockType, ClockType) =>
    case (FixedType(w1, p1), FixedType(w2, p2)) =>
      constraintSolver.addGeq(r1.prettyPrint(""), r2.prettyPrint(""))(p1, p2)
      constraintSolver.addGeq(r1.prettyPrint(""), r2.prettyPrint(""))(w1, w2)
    case (IntervalType(l1, u1, p1), IntervalType(l2, u2, p2)) =>
      constraintSolver.addGeq(r1.prettyPrint(""), r2.prettyPrint(""))(p1, p2)
      constraintSolver.addLeq(r1.prettyPrint(""), r2.prettyPrint(""))(l1, l2)
      constraintSolver.addGeq(r1.prettyPrint(""), r2.prettyPrint(""))(u1, u2)
    case (AnalogType(w1), AnalogType(w2)) =>
      constraintSolver.addGeq(r1.prettyPrint(""), r2.prettyPrint(""))(w1, w2)
      constraintSolver.addGeq(r1.prettyPrint(""), r2.prettyPrint(""))(w2, w1)
    case (t1: BundleType, t2: BundleType) =>
      (t1.fields zip t2.fields) foreach { case (f1, f2) =>
        (f1.flip, f2.flip) match {
          case (Default, Default) => addTypeConstraints(r1.field(f1.name), r2.field(f2.name))(f1.tpe, f2.tpe)
          case (Flip, Flip) => addTypeConstraints(r2.field(f2.name), r1.field(f1.name))(f2.tpe, f1.tpe)
          case _ => sys.error("Shouldn't be here")
        }
      }
    case (t1: VectorType, t2: VectorType) => addTypeConstraints(r1.index(0), r2.index(0))(t1.tpe, t2.tpe)
  }
  private def addExpConstraints(e: Expression): Expression = e map addExpConstraints match {
    case m@Mux(p, tVal, fVal, t) =>
      constraintSolver.addGeq("mux predicate", "1.W")(getWidth(p), Closed(1))
      m
    case other => other
  }
  private def addStmtConstraints(mt: ModuleTarget)(s: Statement): Statement = s map addExpConstraints match {
    case c: Connect =>
      val n = get_size(c.loc.tpe)
      val locs = create_exps(c.loc)
      val exps = create_exps(c.expr)
      (locs zip exps).zipWithIndex foreach { case ((loc, exp), i) =>
        get_flip(c.loc.tpe, i, Default) match {
          case Default => addTypeConstraints(Target.asTarget(mt)(loc), Target.asTarget(mt)(exp))(loc.tpe, exp.tpe)
          case Flip => addTypeConstraints(Target.asTarget(mt)(exp), Target.asTarget(mt)(loc))(exp.tpe, loc.tpe)
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
          case Default => addTypeConstraints(Target.asTarget(mt)(loc), Target.asTarget(mt)(exp))(loc.tpe, exp.tpe)
          case Flip => addTypeConstraints(Target.asTarget(mt)(exp), Target.asTarget(mt)(loc))(exp.tpe, loc.tpe)
        }
      }
      pc
    case r: DefRegister =>
       addTypeConstraints(Target.asTarget(mt)(r.reset), mt.ref("1"))(r.reset.tpe, UIntType(IntWidth(1)))
       addTypeConstraints(mt.ref(r.name), Target.asTarget(mt)(r.init))(r.tpe, r.init.tpe)
       r
    case a@Attach(_, exprs) =>
      val widths = exprs map (e => (e, getWidth(e.tpe)))
      val maxWidth = IsMax(widths.map(x => width2constraint(x._2)))
      widths.foreach { case (e, w) =>
        constraintSolver.addGeq(Target.asTarget(mt)(e).prettyPrint(""), mt.ref(a.serialize).prettyPrint(""))(w, maxWidth)
      }
      a
    case c: Conditionally =>
       addTypeConstraints(Target.asTarget(mt)(c.pred), mt.ref("1.W"))(c.pred.tpe, UIntType(IntWidth(1)))
       c map addStmtConstraints(mt)
    case x => x map addStmtConstraints(mt)
  }
  private def fixWidth(w: Width): Width = constraintSolver.get(w) match {
    case Some(Closed(x)) if trim(x).isWhole => IntWidth(x.toBigInt)
    case None => w
    case _ => sys.error("Shouldn't be here")
  }
  private def fixType(t: Type): Type = t map fixType map fixWidth match {
    case IntervalType(l, u, p) => 
      val (lx, ux) = (constraintSolver.get(l), constraintSolver.get(u)) match {
        case (Some(x: Bound), Some(y: Bound)) => (x, y)
        case (None, None) => (l, u)
        case x => sys.error(s"Shouldn't be here: $x")
      }
      IntervalType(lx, ux, fixWidth(p))
    case FixedType(w, p) => FixedType(w, fixWidth(p))
    case x => x
  }
  private def fixStmt(s: Statement): Statement = s map fixStmt map fixType
  private def fixPort(p: Port): Port = {
    Port(p.info, p.name, p.direction, fixType(p.tpe))
  }

  def run (c: Circuit): Circuit = {
    val ct = CircuitTarget(c.main)
    c.modules foreach ( m => m map addStmtConstraints(ct.module(m.name)))
    constraintSolver.solve()
    val ret = InferTypes.run(c.copy(modules = c.modules map (_
      map fixPort
      map fixStmt)))
    constraintSolver.clear()
    ret
  }
}
