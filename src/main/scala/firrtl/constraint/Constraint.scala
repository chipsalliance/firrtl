// See LICENSE for license details.

package firrtl.constraint

import firrtl.ir._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** Trait for all Constraint Solver expressions */
trait Constraint {
  def serialize: String
  def map(f: Constraint => Constraint): Constraint
  val children: Vector[Constraint]
  def reduce(): Constraint
}

trait MultiAry extends Constraint {
  def op(a: IsKnown, b: IsKnown): IsKnown
  def merge(b1: Option[IsKnown], b2: Option[IsKnown]): Option[IsKnown] = (b1, b2) match {
    case (Some(x), Some(y)) => Some(op(x, y))
    case (_, y: Some[_]) => y
    case (x: Some[_], _) => x
    case _ => None
  }
}

object IsKnown {
  def unapply(b: Constraint): Option[BigDecimal] = b match {
    case k: IsKnown => Some(k.value)
    case _ => None
  }
}

/** Constant values must extend this trait see [[firrtl.ir.Closed and firrtl.ir.Open]] */
trait IsKnown extends Constraint {
  val value: BigDecimal

  /** Addition */
  def +(that: IsKnown): IsKnown

  /** Multiplication */
  def *(that: IsKnown): IsKnown

  /** Max */
  def max(that: IsKnown): IsKnown

  /** Min */
  def min(that: IsKnown): IsKnown

  /** Negate */
  def neg: IsKnown

  /** 2 << value */
  def pow: IsKnown

  /** Floor */
  def floor: IsKnown

  override def map(f: Constraint=>Constraint): Constraint = this

  val children: Vector[Constraint] = Vector.empty[Constraint]

  def reduce(): IsKnown = this
}

object IsAdd {
  def apply(left: Constraint, right: Constraint): Constraint = (left, right) match {
    case (l: IsKnown, r: IsKnown) => l + r
    case _ => apply(Seq(left, right))
  }
  def apply(children: Seq[Constraint]): Constraint = {
    children.foldLeft(new IsAdd(None, Vector(), Vector(), Vector())) { (add, c) =>
      add.addChild(c)
    }.reduce()
  }
}

// Is case class because writing tests is easier due to equality is not object equality
case class IsAdd private (known: Option[IsKnown],
                          maxs: Vector[IsMax],
                          mins: Vector[IsMin],
                          others: Vector[Constraint]) extends Constraint with MultiAry {

  def op(b1: IsKnown, b2: IsKnown): IsKnown = b1 + b2

  lazy val children: Vector[Constraint] = {
    if(known.nonEmpty) known.get +: (maxs ++ mins ++ others) else maxs ++ mins ++ others
  }

  def addChild(x: Constraint): IsAdd = x match {
    case k: IsKnown => new IsAdd(merge(Some(k), known), maxs, mins, others)
    case add: IsAdd => new IsAdd(merge(known, add.known), maxs ++ add.maxs, mins ++ add.mins, others ++ add.others)
    case max: IsMax => new IsAdd(known, maxs :+ max, mins, others)
    case min: IsMin => new IsAdd(known, maxs, mins :+ min, others)
    case other      => new IsAdd(known, maxs, mins, others :+ other)
  }

  override def serialize: String = "(" + children.map(_.serialize).mkString(" + ") + ")"

  override def map(f: Constraint=>Constraint): Constraint = IsAdd(children.map(f))

  def reduce(): Constraint = {
    if(children.size == 1) children.head else {
      (known, maxs, mins, others) match {
        case (Some(k), _, _, _) if k.value == 0         => new IsAdd(None, maxs, mins, others).reduce()
        case (Some(k), Vector(max), Vector(), Vector()) => max.map { o => IsAdd(k, o) }.reduce()
        case (Some(k), Vector(), Vector(min), Vector()) => min.map { o => IsAdd(k, o) }.reduce()
        case _ => this
      }
    }
  }
}

object IsMax {
  def apply(left: Constraint, right: Constraint): Constraint = (left, right) match {
    case (l: IsKnown, r: IsKnown) => l max r
    case _ => apply(Seq(left, right))
  }
  def apply(children: Seq[Constraint]): Constraint = {
    val x = children.foldLeft(new IsMax(None, Vector(), Vector())) { (add, c) =>
      add.addChild(c)
    }
    x.reduce()
  }
}

case class IsMax private[constraint](known: Option[IsKnown],
                                     mins: Vector[IsMin],
                                     others: Vector[Constraint]
                                    ) extends MultiAry {

  def op(b1: IsKnown, b2: IsKnown): IsKnown = b1 max b2

  override def serialize: String = "max(" + children.map(_.serialize).mkString(", ") + ")"

  override def map(f: Constraint=>Constraint): Constraint = IsMax(children.map(f))

  lazy val children: Vector[Constraint] = {
    if(known.nonEmpty) known.get +: (mins ++ others) else mins ++ others
  }

  def reduce(): Constraint = {
    if(children.size == 1) children.head else {
      (known, mins, others) match {
        case (Some(IsKnown(a)), _, _) =>
          // Eliminate minimums who have a known minimum value which is smaller than known maximum value
          val filteredMins = mins.filter {
            case IsMin(Some(IsKnown(i)), _, _) if i <= a => false
            case other => true
          }
          // If a successful filter, rerun reduce
          val newMax = new IsMax(known, filteredMins, others)
          if(filteredMins.size != mins.size) {
            newMax.reduce()
          } else newMax
        case _ => this
      }
    }
  }

  def addChild(x: Constraint): IsMax = x match {
    case k: IsKnown => new IsMax(known = merge(Some(k), known), mins, others)
    case max: IsMax => new IsMax(known = merge(known, max.known), max.mins ++ mins, others ++ max.others)
    case min: IsMin => new IsMax(known, mins :+ min, others)
    case other      => new IsMax(known, mins, others :+ other)
  }
}



object IsMin {
  def apply(left: Constraint, right: Constraint): Constraint = (left, right) match {
    case (l: IsKnown, r: IsKnown) => l min r
    case _ => apply(Seq(left, right))
  }
  def apply(children: Seq[Constraint]): Constraint = {
    children.foldLeft(new IsMin(None, Vector(), Vector())) { (add, c) =>
      add.addChild(c)
    }.reduce()
  }
}

case class IsMin private[constraint](known: Option[IsKnown],
                                     maxs: Vector[IsMax],
                                     others: Vector[Constraint]
                                    ) extends MultiAry {

  def op(b1: IsKnown, b2: IsKnown): IsKnown = b1 min b2

  override def serialize: String = "min(" + children.map(_.serialize).mkString(", ") + ")"

  override def map(f: Constraint=>Constraint): Constraint = IsMin(children.map(f))

  lazy val children: Vector[Constraint] = {
    if(known.nonEmpty) known.get +: (maxs ++ others) else maxs ++ others
  }

  def reduce(): Constraint = {
    if(children.size == 1) children.head else {
      (known, maxs, others) match {
        case (Some(IsKnown(i)), _, _) =>
          // Eliminate maximums who have a known maximum value which is larger than known minimum value
          val filteredMaxs = maxs.filter {
            case IsMax(Some(IsKnown(a)), _, _) if a >= i => false
            case other => true
          }
          // If a successful filter, rerun reduce
          val newMin = new IsMin(known, filteredMaxs, others)
          if(filteredMaxs.size != maxs.size) {
            newMin.reduce()
          } else newMin
        case _ => this
      }
    }
  }

  def addChild(x: Constraint): IsMin = x match {
    case k: IsKnown => new IsMin(merge(Some(k), known), maxs, others)
    case max: IsMax => new IsMin(known, maxs :+ max, others)
    case min: IsMin => new IsMin(merge(min.known, known), maxs ++ min.maxs, others ++ min.others)
    case other      => new IsMin(known, maxs, others :+ other)
  }
}



object IsMul {
  def apply(left: Constraint, right: Constraint): Constraint = (left, right) match {
    case (l: IsKnown, r: IsKnown) => l * r
    case _ => apply(Seq(left, right))
  }
  def apply(children: Seq[Constraint]): Constraint = {
    children.foldLeft(new IsMul(None, Vector())) { (add, c) =>
      add.addChild(c)
    }.reduce()
  }
}

case class IsMul private (known: Option[IsKnown], others: Vector[Constraint]) extends MultiAry {

  def op(b1: IsKnown, b2: IsKnown): IsKnown = b1 * b2

  lazy val children: Vector[Constraint] = if(known.nonEmpty) known.get +: others else others

  def addChild(x: Constraint): IsMul = x match {
    case k: IsKnown => new IsMul(known = merge(Some(k), known), others)
    case mul: IsMul => new IsMul(merge(known, mul.known), others ++ mul.others)
    case other      => new IsMul(known, others :+ other)
  }

  override def reduce(): Constraint = {
    if(children.size == 1) children.head else {
      (known, others) match {
        case (Some(Closed(x)), _) if x == BigDecimal(1)   => new IsMul(None, others).reduce()
        case (Some(Closed(x)), _) if x == BigDecimal(0)   => Closed(0)
        case (Some(Closed(x)), Vector(m: IsMax)) if x > 0 =>
          IsMax(m.children.map { c => IsMul(Closed(x), c) })
        case (Some(Closed(x)), Vector(m: IsMax)) if x < 0 =>
          IsMin(m.children.map { c => IsMul(Closed(x), c) })
        case (Some(Closed(x)), Vector(m: IsMin)) if x > 0 =>
          IsMin(m.children.map { c => IsMul(Closed(x), c) })
        case (Some(Closed(x)), Vector(m: IsMin)) if x < 0 =>
          IsMax(m.children.map { c => IsMul(Closed(x), c) })
        case _ => this
      }
    }
  }

  override def map(f: Constraint=>Constraint): Constraint = IsMul(children.map(f))

  override def serialize: String = "(" + children.map(_.serialize).mkString(" * ") + ")"
}



object IsNeg {
  def apply(child: Constraint): Constraint = new IsNeg(child, 0).reduce()
}

// Dummy arg is to get around weird Scala issue that can't differentiate between a
//   private constructor and public apply that share the same arguments
case class IsNeg private (child: Constraint, dummyArg: Int) extends Constraint {
  override def reduce(): Constraint = child match {
    case k: IsKnown => k.neg
    case x: IsAdd => IsAdd(x.children.map { b => IsNeg(b) })
    case x: IsMul => IsMul(Seq(IsNeg(x.children.head)) ++ x.children.tail)
    case x: IsNeg => x.child
    case x: IsPow => this
    // -[max(a, b)] -> min[-a, -b]
    case x: IsMax => IsMin(x.children.map { b => IsNeg(b) })
    case x: IsMin => IsMax(x.children.map { b => IsNeg(b) })
    case x: IsVar => this
    case _ => this
  }

  lazy val children = Vector(child)

  override def map(f: Constraint=>Constraint): Constraint = IsNeg(f(child))

  override def serialize: String = "(-" + child.serialize + ")"
}




object IsPow { def apply(child: Constraint): Constraint = new IsPow(child).reduce() }

class IsPow private (val child: Constraint) extends Constraint {
  override def reduce(): Constraint = child match {
    case k: IsKnown => k.pow
    // 2^(a + b) -> 2^a * 2^b
    case x: IsAdd => IsMul(x.children.map { b => IsPow(b)})
    case x: IsMul => this
    case x: IsNeg => this
    case x: IsPow => this
    // 2^(max(a, b)) -> max(2^a, 2^b) since two is always positive, so a, b control magnitude
    case x: IsMax => IsMax(x.children.map {b => IsPow(b)})
    case x: IsMin => IsMin(x.children.map {b => IsPow(b)})
    case x: IsVar => this
    case _ => this
  }
  val children = Vector(child)

  override def map(f: Constraint=>Constraint): Constraint = IsPow(f(child))

  override def serialize: String = "(2^" + child.serialize + ")"
}




object IsFloor { def apply(child: Constraint): Constraint = new IsFloor(child).reduce() }

class IsFloor private (val child: Constraint) extends Constraint {

  override def reduce(): Constraint = child match {
    case k: IsKnown => k.floor
    case x: IsAdd => this
    case x: IsMul => this
    case x: IsNeg => this
    case x: IsPow => this
    // floor(max(a, b)) -> max(floor(a), floor(b))
    case x: IsMax => IsMax(x.children.map {b => IsFloor(b)})
    case x: IsMin => IsMin(x.children.map {b => IsFloor(b)})
    case x: IsVar => this
    // floor(floor(x)) -> floor(x)
    case x: IsFloor => x
    case _ => this
  }
  val children = Vector(child)

  override def map(f: Constraint=>Constraint): Constraint = IsFloor(f(child))

  override def serialize: String = "floor(" + child.serialize + ")"
}


object IsVar {
  def unapply(i: Constraint): Option[String] = i match {
    case i: IsVar => Some(i.name)
    case _ => None
  }
}

/** Extend to be a constraint variable */
trait IsVar extends Constraint {

  def name: String

  override def serialize: String = name

  override def map(f: Constraint=>Constraint): Constraint = this

  override def reduce() = this

  val children = Vector()
}

case class VarCon(name: String) extends IsVar
