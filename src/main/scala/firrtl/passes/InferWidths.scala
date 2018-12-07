// See LICENSE for license details.

package firrtl.passes

// Datastructures
import firrtl._
import firrtl.ir._
import firrtl.Utils._
import firrtl.Mappers._
import firrtl.Implicits.width2constraint
import firrtl.constraint.{ConstraintSolver, IsMax}

object InferWidths {
  def apply(): InferWidths = new InferWidths()
  def run(c: Circuit): Circuit = new InferWidths().run(c)
  def execute(state: CircuitState): CircuitState = new InferWidths().execute(state)
}

class InferWidths extends Pass {
  private val constraintSolver = new ConstraintSolver()

  private def addTypeConstraints(t1: Type, t2: Type): Unit = (t1,t2) match {
    case (UIntType(w1), UIntType(w2)) => constraintSolver.addGeq(w1, w2)
    case (SIntType(w1), SIntType(w2)) => constraintSolver.addGeq(w1, w2)
    case (ClockType, ClockType) =>
    case (FixedType(w1, p1), FixedType(w2, p2)) =>
      constraintSolver.addGeq(p1, p2)
      constraintSolver.addGeq(w1, w2)
    case (IntervalType(l1, u1, p1), IntervalType(l2, u2, p2)) =>
      constraintSolver.addGeq(p1, p2)
      constraintSolver.addLeq(l1, l2)
      constraintSolver.addGeq(u1, u2)
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
  private def addStmtConstraints(s: Statement): Statement = s map addExpConstraints match {
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
//<<<<<<< HEAD
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
/*=======
     
  def run (c: Circuit): Circuit = {
    val v = ArrayBuffer[WGeq]()

    def get_constraints_t(t1: Type, t2: Type): Seq[WGeq] = (t1,t2) match {
      case (t1: UIntType, t2: UIntType) => Seq(WGeq(t1.width, t2.width))
      case (t1: SIntType, t2: SIntType) => Seq(WGeq(t1.width, t2.width))
      case (ClockType, ClockType) => Nil
      case (FixedType(w1, p1), FixedType(w2, p2)) => Seq(WGeq(w1,w2), WGeq(p1,p2))
      case (AnalogType(w1), AnalogType(w2)) => Seq(WGeq(w1,w2), WGeq(w2,w1))
      case (t1: BundleType, t2: BundleType) =>
        (t1.fields zip t2.fields foldLeft Seq[WGeq]()){case (res, (f1, f2)) =>
          res ++ (f1.flip match {
            case Default => get_constraints_t(f1.tpe, f2.tpe)
            case Flip => get_constraints_t(f2.tpe, f1.tpe)
          })
        }
      case (t1: VectorType, t2: VectorType) => get_constraints_t(t1.tpe, t2.tpe)
    }

    def get_constraints_e(e: Expression): Expression = {
      e match {
        case (e: Mux) => v ++= Seq(
          WGeq(getWidth(e.cond), IntWidth(1)),
          WGeq(IntWidth(1), getWidth(e.cond))
        )
        case _ =>
      }
      e map get_constraints_e
    }

    def get_constraints_declared_type (t: Type): Type = t match {
      case FixedType(_, p) => 
        v += WGeq(p,IntWidth(0))
        t
      case _ => t map get_constraints_declared_type
    }

    def get_constraints_s(s: Statement): Statement = {
      s map get_constraints_declared_type match {
        case (s: Connect) =>
          val n = get_size(s.loc.tpe)
          val locs = create_exps(s.loc)
          val exps = create_exps(s.expr)
          v ++= locs.zip(exps).flatMap { case (locx, expx) =>
            to_flip(gender(locx)) match {
              case Default => get_constraints_t(locx.tpe, expx.tpe)//WGeq(getWidth(locx), getWidth(expx))
              case Flip => get_constraints_t(expx.tpe, locx.tpe)//WGeq(getWidth(expx), getWidth(locx))
            }
          }
        case (s: PartialConnect) =>
          val ls = get_valid_points(s.loc.tpe, s.expr.tpe, Default, Default)
          val locs = create_exps(s.loc)
          val exps = create_exps(s.expr)
          v ++= (ls flatMap {case (x, y) =>
            val locx = locs(x)
            val expx = exps(y)
            to_flip(gender(locx)) match {
              case Default => get_constraints_t(locx.tpe, expx.tpe)//WGeq(getWidth(locx), getWidth(expx))
              case Flip => get_constraints_t(expx.tpe, locx.tpe)//WGeq(getWidth(expx), getWidth(locx))
            }
          })
        case (s: DefRegister) => v ++= (
           get_constraints_t(s.reset.tpe, UIntType(IntWidth(1))) ++
           get_constraints_t(UIntType(IntWidth(1)), s.reset.tpe) ++ 
           get_constraints_t(s.tpe, s.init.tpe))
        case (s:Conditionally) => v ++= 
           get_constraints_t(s.pred.tpe, UIntType(IntWidth(1))) ++
           get_constraints_t(UIntType(IntWidth(1)), s.pred.tpe)
        case Attach(_, exprs) =>
          // All widths must be equal
          val widths = exprs map (e => getWidth(e.tpe))
          v ++= widths.tail map (WGeq(widths.head, _))
        case _ =>
      }
      s map get_constraints_e map get_constraints_s
    }

    c.modules foreach (_ map get_constraints_s)
    c.modules foreach (_.ports foreach {p => get_constraints_declared_type(p.tpe)})

    //println("======== ALL CONSTRAINTS ========")
    //for(x <- v) println(x)
    //println("=================================")
    val h = solve_constraints(v)
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
        case wx => throwInternalError(s"solve: shouldn't be here - %$wx")
      }

      solve(w) match {
        case None => w
        case Some(s) => IntWidth(s)
>>>>>>> b90589f5cd9d4048ada2a05d5225874791546170*/
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
    c.modules foreach (_ map addStmtConstraints)
      constraintSolver.solve()
    InferTypes.run(c.copy(modules = c.modules map (_
      map fixPort
      map fixStmt)))
  }
}
