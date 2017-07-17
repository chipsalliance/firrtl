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
abstract class ConstraintSolver[T<:IsConstrainable[T]] {
  /** Unimplemented Members */
  def genConst(left: String, right: T, geq: Boolean): Constraint[T]
  def genMax(args: T*): T
  def genMin(args: T*): T
  def genVar(name: String): T

  /** Internal State and Accessors */
  // initial constraints
  val constraints = mutable.ArrayBuffer[Constraint[T]]()
  def add(c: Constraint[T]) = constraints += c
  def serializeConstraints: String = constraints.mkString("\n")

  // solved constraints
  type ConstraintMap = mutable.HashMap[String, (T, Boolean)]
  val solvedConstraintMap = new ConstraintMap()
  def serializeSolutions: String = solvedConstraintMap.map{
    case (k, (v, true))  => s"$k >= ${v.serialize}"
    case (k, (v, false)) => s"$k <= ${v.serialize}"
  }.mkString("\n")

  /** Constraint Solver */

  private def mergeConstraints(constraints: Seq[Constraint[T]]): Seq[Constraint[T]] = {
    val mergedMap = mutable.HashMap[String, Constraint[T]]()
    constraints.foreach { c =>
      c match {
        case c if (c.geq == true)  && mergedMap.contains(c.left) => mergedMap(c.left) = genConst(c.left, genMax(mergedMap(c.left).right, c.right), true)
        case c if (c.geq == false) && mergedMap.contains(c.left) => mergedMap(c.left) = genConst(c.left, genMin(mergedMap(c.left).right, c.right), false)
        case c                                                   => mergedMap(c.left) = c
      }
    }
    mergedMap.values.toSeq
  }
  private def substitute(h: ConstraintMap)(t: T): T = {
    val tOptimized = t.optimize()
    //println(s"tOptimized: $tOptimized")
    val x = tOptimized map substitute(h)
    //println("SDFL:")
    x match {
      case isVar: IsVar => h get isVar.name match {
        case None => isVar.asInstanceOf[T]
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

  private def backwardSubstitution(h: ConstraintMap)(t: T): T = {
    t map backwardSubstitution(h) match {
      case isVar: IsVar => h.get(isVar.name) match {
        case None => isVar.asInstanceOf[T]
        case Some((p, geq)) => p
      }
      case other => other
    }
  }

  private def removeCycle(n: String, geq: Boolean)(t: T): T = if(geq) removeGeqCycle(n)(t) else removeLeqCycle(n)(t)

  private def removeLeqCycle(name: String)(t: T): T = t match {
    case x if greaterEqThan(name)(x) => genVar(name)
    case isMin: IsMin[T] => genMin(isMin.children.filter{ c => !greaterEqThan(name)(c)}:_*)
    case x => x
  }
  private def greaterEqThan(name: String)(t: T): Boolean = t.optimize() match {
    case isMin: IsMin[T] => isMin.children.map(greaterEqThan(name)).reduce(_ && _)
    case isAdd: IsAdd[T] => isAdd.children match {
      case Seq(isVar: IsVar, isVal: IsVal) if (isVar.name == name) && (isVal.value >= 0) => true
      case Seq(isVal: IsVal, isVar: IsVar) if (isVar.name == name) && (isVal.value >= 0) => true
      case _ => false
    }
    case isMul: IsMul[T] => isMul.children match {
      case Seq(isVar: IsVar, isVal: IsVal) if (isVar.name == name) && (isVal.value >= 0) => true
      case Seq(isVal: IsVal, isVar: IsVar) if (isVar.name == name) && (isVal.value >= 0) => true
      case _ => false
    }
    case isVar: IsVar if isVar.name == name => true
    case _ => false
  }

  private def removeGeqCycle(name: String)(t: T): T = t match {
    case x if lessEqThan(name)(x) => genVar(name)
    case isMax: IsMax[T] => genMax(isMax.children.filter{c => !lessEqThan(name)(c)}:_*)
    case x => x
  }
  private def lessEqThan(name: String)(t: T): Boolean = t.optimize() match {
    case isMax: IsMax[T] => isMax.children.map(lessEqThan(name)).reduce(_ && _)
    case isAdd: IsAdd[T] => isAdd.children match {
      case Seq(isVar: IsVar, isVal: IsVal) if (isVar.name == name) && (isVal.value <= 0) => true
      case Seq(isVal: IsVal, isVar: IsVar) if (isVar.name == name) && (isVal.value <= 0) => true
      case _ => false
    }
    case isMul: IsMul[T] => isMul.children match {
      case Seq(isVar: IsVar, isVal: IsVal) if (isVar.name == name) && (isVal.value <= 0) => true
      case Seq(isVal: IsVal, isVar: IsVar) if (isVar.name == name) && (isVal.value <= 0) => true
      case _ => false
    }
    case isVar: IsVar if isVar.name == name => true
    case isNeg: IsNeg[T] => isNeg.children match {
      case Seq(isVar: IsVar) if isVar == name => true
      case _ => false
    }
    case _ => false
  }

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
  def check(): Seq[Constraint[T]] = {
    val checkMap = new mutable.HashMap[String, Constraint[T]]()
    constraints.foldLeft(Seq[Constraint[T]]()) { (seq, c) =>
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

class BoundConstraintSolver extends ConstraintSolver[Bound] {
  def genMax(args: Bound*): Bound = MaxBound(args:_*)
  def genMin(args: Bound*): Bound = MinBound(args:_*)
  def genVar(name: String): Bound = VarBound(name)
  def genConst(left: String, right: Bound, geq: Boolean): Constraint[Bound] = geq match {
    case true => BGeq(left, right)
    case false => BLeq(left, right)
  }
  def addGeq(big: Bound, small: Bound): Unit = (big, small) match {
    case (VarBound(name), _) => add(BGeq(name, small))
    case (KnownBound(u), KnownBound(l)) if l > u => println("BAD!")
    case _ =>
  }
  def addLeq(small: Bound, big: Bound): Unit = (small, big) match {
    case (VarBound(name), _) => add(BLeq(name, big))
    case (KnownBound(l), KnownBound(u)) if u < l => println("BAD!")
    case _ =>
  }
  def get(b: Bound): Bound = b match {
    case VarBound(name) => solvedConstraintMap.get(name) match {
      case None => b
      case Some((k: KnownBound, _)) => k
      case Some(_) => b
    }
    case x => x
  }
}

case class BGeq(left: String, right: Bound) extends Constraint[Bound] {
  val geq = true
  override def toString: String = s"$left >= ${right.serialize}"
}
case class BLeq(left: String, right: Bound) extends Constraint[Bound] {
  val geq = false
  override def toString: String = s"$left <= ${right.serialize}"
}


class InferIntervals extends Pass {
  private val constraintSolver = new BoundConstraintSolver()
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
        val lx = constraintSolver.get(l)
        val ux = constraintSolver.get(u)
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
