// See LICENSE for license details.

package firrtl.passes

// Datastructures
import scala.collection.mutable
import scala.collection.immutable.ListMap

import firrtl._
import firrtl.ir._
import firrtl.Utils._
import firrtl.Mappers._

trait Constraint {
  def left: String
  def right: IsConstrainable
  def geq: Boolean
}
case class GEQ(left: String, right: IsConstrainable) extends Constraint {
  val geq = true
  override def toString: String = s"$left >= ${right.serialize}"
}
case class LEQ(left: String, right: IsConstrainable) extends Constraint {
  val geq = false
  override def toString: String = s"$left <= ${right.serialize}"
}

trait IsConstrainable {
  def serialize: String
  def map(f: IsConstrainable=>IsConstrainable): IsConstrainable
  def children: Seq[IsConstrainable] = {
    val kids = mutable.ArrayBuffer[IsConstrainable]()
    def f(b: IsConstrainable): IsConstrainable = {
      kids += b
      b
    }
    map(f)
    kids.toSeq
  }
  def optimize(): IsConstrainable = map(_.optimize()).reduce()
  def op(b1: IsKnown, b2: IsKnown): IsConstrainable = sys.error("Shouldn't be here")
  def gen(children: Seq[IsConstrainable]): IsConstrainable = (this, children) match {
    case (_: IsVar, Nil)              => this
    case (_: IsKnown, Nil)            => this
    case (_: IsKnown, _)              => sys.error("Shouldn't be here")
    case (_: IsAdd, x)                => IsAdd(x:_*)
    case (_: IsMul, x)                => IsMul(x:_*)
    case (_: IsNeg, x) if x.size != 1 => sys.error("Shouldn't be here")
    case (_: IsNeg, x)                => IsNeg(x.head)
    case (_: IsMax, x)                => IsMax(x:_*)
    case (_: IsMin, x)                => IsMin(x:_*)
    case (_: IsVar, x)                => sys.error("Shouldn't be here")
    case (_, Nil)                     => sys.error("Shouldn't be here")
    case _                            => sys.error("Shouldn't be here")
  }
  def reduce(): IsConstrainable = {
    val newBounds = children.flatMap{ 
      _ match {
        case a if a.getClass == this.getClass => a.children
        case x => Seq(x)
      }
    }
    val groups = newBounds.groupBy {
      case x: IsKnown => "known"
      case x: IsMax   => "max"
      case x: IsMin   => "min"
      case x             => "other"
    }
    def genericOp(b1: IsConstrainable, b2: IsConstrainable): IsConstrainable = (b1, b2) match {
      case (k1: IsKnown, k2: IsKnown) => op(k1, k2)
      case _ => sys.error("Shouldn't be here")
    }
    val known = groups.get("known") match {
      case None => Nil
      case Some(seq) => Seq(seq.reduce(genericOp))
    }
    val max = groups.get("max") match {
      case None => Nil
      case Some(seq) => seq
    }
    val min = groups.get("min") match {
      case None => Nil
      case Some(seq) => seq
    }
    val others = groups.get("other") match {
      case None => Nil
      case Some(seq) => seq
    }
    (max, min, known, others) match {
      case (Nil, Nil, Nil, Nil) => this
      case (Nil, Nil, _, _) if known.size + others.size == 1 => (known ++ others).head
      case (Nil, Nil, _, _) => gen(known ++ others)
      case (Seq(x), Nil, _, _) => IsMax(x.children.map { c => gen(Seq(c) ++ known ++ others).reduce() }:_*)
      case (Nil, Seq(x), _, _) => IsMin(x.children.map { c => gen(Seq(c) ++ known ++ others).reduce() }:_*)
      case (_, _, _, _) => gen(known ++ others ++ max ++ min)
    }
  }
}
trait IsKnown extends IsConstrainable {
  val value: BigDecimal
  def +(that: IsKnown): IsKnown
  def *(that: IsKnown): IsKnown
  def max(that: IsKnown): IsKnown
  def min(that: IsKnown): IsKnown
  def neg: IsKnown

  // Unnecessary members
  def map(f: IsConstrainable=>IsConstrainable): IsConstrainable = this
}
object IsKnown {
  def unapply(b: IsConstrainable): Option[BigDecimal] = b match {
    case k: IsKnown => Some(k.value)
    case _ => None
  }
}
case class IsAdd(override val children: IsConstrainable*) extends IsConstrainable {
  override def op(b1: IsKnown, b2: IsKnown): IsConstrainable = b1 + b2
  def serialize: String = "(" + children.map(_.serialize).mkString(" + ") + ")"
  def map(f: IsConstrainable=>IsConstrainable): IsConstrainable = IsAdd(children.map(f):_*)
}
case class IsMul(override val children: IsConstrainable*) extends IsConstrainable {
  override def op(b1: IsKnown, b2: IsKnown): IsConstrainable = b1 * b2
  def map(f: IsConstrainable=>IsConstrainable): IsConstrainable = IsMul(children.map(f):_*)
  def serialize: String = "(" + children.map(_.serialize).mkString(" * ") + ")"
}
case class IsNeg(child: IsConstrainable) extends IsConstrainable {
  override def reduce(): IsConstrainable = child match {
    case k: IsKnown => k.neg
    case IsAdd(children) => IsAdd(children.map { b => IsNeg(b).reduce }).reduce
    case IsMul(children) => IsMul(children.map { b => IsNeg(b).reduce }).reduce
    case IsNeg(child) => child
    case IsMax(children) => IsMin(children.map {b => IsNeg(b).reduce }).reduce
    case IsMin(children) => IsMax(children.map {b => IsNeg(b).reduce }).reduce
  }
  def map(f: IsConstrainable=>IsConstrainable): IsConstrainable = IsNeg(f(child))
  def serialize: String = "(-" + child.serialize + ")"
}
case class IsMax(override val children: IsConstrainable*) extends IsConstrainable {
  override def op(b1: IsKnown, b2: IsKnown): IsConstrainable = b1 max b2
  def serialize: String = "max(" + children.map(_.serialize).mkString(", ") + ")"
  def map(f: IsConstrainable=>IsConstrainable): IsConstrainable = IsMax(children.map(f):_*)
}
case class IsMin(override val children: IsConstrainable*) extends IsConstrainable {
  override def op(b1: IsKnown, b2: IsKnown): IsConstrainable = b1 min b2
  def serialize: String = "min(" + children.map(_.serialize).mkString(", ") + ")"
  def map(f: IsConstrainable=>IsConstrainable): IsConstrainable = IsMin(children.map(f):_*)
}
case class IsVar(name: String) extends IsConstrainable {
  def serialize: String = name
  def map(f: IsConstrainable=>IsConstrainable): IsConstrainable = this
}

class ConstraintSolver {
  def addGeq(big: IsConstrainable, small: IsConstrainable): Unit = (big, small) match {
    case (CalcBound(IsVar(name)), _) => add(GEQ(name, small))
    case (IsKnown(u), IsKnown(l)) if l > u => println("BAD!")
    case _ =>
  }
  def addLeq(small: IsConstrainable, big: IsConstrainable): Unit = (small, big) match {
    case (CalcBound(IsVar(name)), _) => add(LEQ(name, big))
    case (IsKnown(l), IsKnown(u)) if u < l => println("BAD!")
    case _ =>
  }
  def get(b: IsConstrainable): IsConstrainable = {
    val name = b match {
      case CalcBound(IsVar(name)) => name
      //case VarWidth(name) => name
      case x => ""
    }
    solvedConstraintMap.get(name) match {
      case None => b
      case Some((k: IsKnown, _)) => k
      case Some(_) => b
    }
  }

  /** Internal State and Accessors */
  private def genConst(left: String, right: IsConstrainable, geq: Boolean): Constraint = geq match {
    case true => GEQ(left, right)
    case false => LEQ(left, right)
  }
  // initial constraints
  val constraints = mutable.ArrayBuffer[Constraint]()
  def add(c: Constraint) = constraints += c
  def serializeConstraints: String = constraints.mkString("\n")

  // solved constraints
  type ConstraintMap = mutable.HashMap[String, (IsConstrainable, Boolean)]
  val solvedConstraintMap = new ConstraintMap()
  def serializeSolutions: String = solvedConstraintMap.map{
    case (k, (v, true))  => s"$k >= ${v.serialize}"
    case (k, (v, false)) => s"$k <= ${v.serialize}"
  }.mkString("\n")

  /** Constraint Solver */

  private def mergeConstraints(constraints: Seq[Constraint]): Seq[Constraint] = {
    val mergedMap = mutable.HashMap[String, Constraint]()
    constraints.foreach { c =>
      c match {
        case c if (c.geq == true)  && mergedMap.contains(c.left) => mergedMap(c.left) = genConst(c.left, IsMax(mergedMap(c.left).right, c.right), true)
        case c if (c.geq == false) && mergedMap.contains(c.left) => mergedMap(c.left) = genConst(c.left, IsMin(mergedMap(c.left).right, c.right), false)
        case c                                                   => mergedMap(c.left) = c
      }
    }
    mergedMap.values.toSeq
  }
  private def substitute(h: ConstraintMap)(t: IsConstrainable): IsConstrainable = {
    val tOptimized = t.optimize()
    //println(s"tOptimized: $tOptimized")
    val x = tOptimized map substitute(h)
    //println("SDFL:")
    x match {
      case isVar: IsVar => h get isVar.name match {
        case None => isVar.asInstanceOf[IsConstrainable]
        case Some((p, geq)) =>
          //println("HERE 2")
          val newT = substitute(h)(p)
          val newTOptimized = newT.optimize()
          //println("HERE 3")
          h(isVar.name) = (newTOptimized, geq)
          newTOptimized
      }
      case other => other
    }
  }

  private def backwardSubstitution(h: ConstraintMap)(t: IsConstrainable): IsConstrainable = {
    t map backwardSubstitution(h) match {
      case isVar: IsVar => h.get(isVar.name) match {
        case None => isVar
        case Some((p, geq)) => p
      }
      case other => other
    }
  }

  private def removeCycle(n: String, geq: Boolean)(t: IsConstrainable): IsConstrainable = if(geq) removeGeqCycle(n)(t) else removeLeqCycle(n)(t)

  private def removeLeqCycle(name: String)(t: IsConstrainable): IsConstrainable = t match {
    case x if greaterEqThan(name)(x) => IsVar(name)
    case isMin: IsMin => IsMin(isMin.children.filter{ c => !greaterEqThan(name)(c)}:_*)
    case x => x
  }
  private def greaterEqThan(name: String)(t: IsConstrainable): Boolean = t.optimize() match {
    case isMin: IsMin => isMin.children.map(greaterEqThan(name)).reduce(_ && _)
    case isAdd: IsAdd => isAdd.children match {
      case Seq(isVar: IsVar, isVal: IsKnown) if (isVar.name == name) && (isVal.value >= 0) => true
      case Seq(isVal: IsKnown, isVar: IsVar) if (isVar.name == name) && (isVal.value >= 0) => true
      case _ => false
    }
    case isMul: IsMul => isMul.children match {
      case Seq(isVar: IsVar, isVal: IsKnown) if (isVar.name == name) && (isVal.value >= 0) => true
      case Seq(isVal: IsKnown, isVar: IsVar) if (isVar.name == name) && (isVal.value >= 0) => true
      case _ => false
    }
    case isVar: IsVar if isVar.name == name => true
    case _ => false
  }

  private def removeGeqCycle(name: String)(t: IsConstrainable): IsConstrainable = t match {
    case x if lessEqThan(name)(x) => IsVar(name)
    case isMax: IsMax => IsMax(isMax.children.filter{c => !lessEqThan(name)(c)}:_*)
    case x => x
  }
  private def lessEqThan(name: String)(t: IsConstrainable): Boolean = t.optimize() match {
    case isMax: IsMax => isMax.children.map(lessEqThan(name)).reduce(_ && _)
    case isAdd: IsAdd => isAdd.children match {
      case Seq(isVar: IsVar, isVal: IsKnown) if (isVar.name == name) && (isVal.value <= 0) => true
      case Seq(isVal: IsKnown, isVar: IsVar) if (isVar.name == name) && (isVal.value <= 0) => true
      case _ => false
    }
    case isMul: IsMul => isMul.children match {
      case Seq(isVar: IsVar, isVal: IsKnown) if (isVar.name == name) && (isVal.value <= 0) => true
      case Seq(isVal: IsKnown, isVar: IsVar) if (isVar.name == name) && (isVal.value <= 0) => true
      case _ => false
    }
    case isVar: IsVar if isVar.name == name => true
    case isNeg: IsNeg => isNeg.children match {
      case Seq(isVar: IsVar) if isVar == name => true
      case _ => false
    }
    case _ => false
  }

  private def hasVar(n: String)(t: IsConstrainable): Boolean = {
    var has = false
    def rec(t: IsConstrainable): IsConstrainable = {
      t match {
        case isVar: IsVar if isVar.name == n => has = true
        case _ =>
      }
      t map rec
    }
    rec(t)
    has
  }
  def check(): Seq[Constraint] = {
    val checkMap = new mutable.HashMap[String, Constraint]()
    constraints.foldLeft(Seq[Constraint]()) { (seq, c) =>
      checkMap.get(c.left) match {
        case None =>
          checkMap(c.left) = c
          seq ++ Nil
        case Some(x) if x.geq != c.geq => seq ++ Seq(x, c)
        case Some(x) => seq ++ Nil
      }
    }
  }

  def solve() = {
    // Check that all constraints per name are either geq or leq, but not both
    val illegals = check()
    if (illegals != Nil) sys.error("Illegal constraints!: $illegals")

    // Combines constraints on the same VarWidth into the same constraint
    val uniqueConstraints = mergeConstraints(constraints.toSeq)
    //println("Unique Constraints!")
    //println(uniqueConstraints.mkString("\n"))

    // Forward solve
    // Returns a solved list where each constraint undergoes:
    //  1) Continuous Solving (using triangular solving)
    //  2) Remove Cycles
    //  3) Move to solved if not self-recursive
    val forwardConstraintMap = new ConstraintMap
    val orderedVars = mutable.ArrayBuffer[String]()
    for (constraint <- uniqueConstraints) {
      //TODO: Risky if used improperly... need to check whether substitution from a leq to a geq is negated (always).
      //println(s" Forward solving: $constraint")
      val subbedRight = substitute(forwardConstraintMap)(constraint.right)
      //println(s" subbedRight: $subbedRight")
      val optSubbedRight = subbedRight.optimize()
      //println(s" optSubbedRight: $subbedRight")
      val name = constraint.left
      val finishedRight = removeCycle(name, constraint.geq)(subbedRight)
      if (!hasVar(name)(finishedRight)) {
        forwardConstraintMap(name) = (finishedRight, constraint.geq)
        orderedVars += name
      }
    }
    //println("Forward Constraints!")
    //println(forwardConstraintMap.mkString("\n"))
 
    // Backwards Solve
    for (i <- (orderedVars.size - 1) to 0 by -1) {
      val name = orderedVars(i) // Should visit `orderedVars` backward
      val (forwardRight, forwardGeq) = forwardConstraintMap(name)
      val solvedRight = backwardSubstitution(solvedConstraintMap)(forwardRight).optimize()
      //println("HERE 4")
      solvedConstraintMap(name) = (solvedRight, forwardGeq)
    }
  }
}


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
          case (x: Bound, y: Bound) => (x, y)
          case _ => sys.error("Shouldn't be here")
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
