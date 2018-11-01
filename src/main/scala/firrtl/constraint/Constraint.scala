// See LICENSE for license details.

package firrtl.constraint

import firrtl.ir._

import scala.collection.mutable

object Constraint {
  /** Returns all children, including children of children who share the same class as this
    * @param constraint Constraint to return collapsed children constraints
    * @return
    */
  def collapseChildren(constraint: Constraint): Seq[Constraint] = {
    constraint.getChildren.foldLeft(Vector.empty[Constraint]){ (vec, c) =>
      c match {
        case a if a.getClass == constraint.getClass => vec ++ a.getChildren
        case x => vec :+ x
      }
    }
  }

  /** Groups constraints into [[IsKnown]], [[IsMax]], [[IsMin]], and [[Constraint]] seqs
    * [[IsKnown]] Seq is reduced with binOp
    * @param constraints
    * @param binOp
    * @return
    */
  def collect(constraints: Seq[Constraint], binOp: (IsKnown, IsKnown) => IsKnown):
      (Seq[IsKnown], Seq[IsMax], Seq[IsMin], Seq[Constraint]) = {
    val knowns = mutable.ArrayBuffer[IsKnown]()
    val maxs = mutable.ArrayBuffer[IsMax]()
    val mins = mutable.ArrayBuffer[IsMin]()
    val others = mutable.ArrayBuffer[Constraint]()
    constraints.foreach {
      case c: IsKnown => knowns += c
      case c: IsMax => maxs += c
      case c: IsMin => mins += c
      case c => others += c
    }
    val reducedKnown = knowns.foldLeft(Seq.empty[IsKnown]) { (col, k) =>
      col match {
        case Nil => Seq(k)
        case Seq(x) => Seq(binOp(x, k))
      }
    }
    (reducedKnown, maxs, mins, others)
  }

  /** Generic simplification of a constraint, only for [[IsAdd]], [[IsMax]], and [[IsMin]]
    * @param c constraint
    * @param binOp reduce operator
    * @param gen constructor for a [[Constraint]] of the same class as c
    * @return
    */
  private [constraint] def reduce(c: Constraint, binOp: (IsKnown, IsKnown)=>IsKnown, gen: Seq[Constraint]=>Constraint): Constraint = {
    val (known, max, min, others) = collect(collapseChildren(c), binOp)
    (max, min, known, others) match {
      case (Nil, Nil, Nil, Nil) => c
      case (Nil, Nil, _, _) if known.size + others.size == 1 => (known ++ others).head
      case (Nil, Nil, _, _) if known ++ others == c.getChildren => c
      case (Nil, Nil, _, _) => gen(known ++ others)
      case (Seq(x), Nil, _, _) => IsMax(x.children.map { c => gen(Seq(c) ++ known ++ others) }:_*)
      case (Nil, Seq(x), _, _) => IsMin(x.children.map { c => gen(Seq(c) ++ known ++ others) }:_*)
      case _ if (max ++ min ++ known ++ others).toSet == c.getChildren.toSet => c
      case _ => gen(known ++ others ++ max ++ min)
    }
  }
}

/** Trait for all Constraint Solver expressions */
trait Constraint {
  def serialize: String
  def map(f: Constraint=>Constraint): Constraint
  def getChildren: Seq[Constraint] = {
    val kids = mutable.ArrayBuffer[Constraint]()
    def f(b: Constraint): Constraint = {
      kids += b
      b
    }
    map(f)
    kids
  }
  def reduce(): Constraint
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

  override def reduce() = this

  override def map(f: Constraint=>Constraint): Constraint = this
}



object IsAdd { def apply(children: Constraint*): Constraint = new IsAdd(children:_*).reduce() }

class IsAdd private (val children: Constraint*) extends Constraint {

  def op(b1: IsKnown, b2: IsKnown): IsKnown = b1 + b2

  override def serialize: String = "(" + children.map(_.serialize).mkString(" + ") + ")"

  override def map(f: Constraint=>Constraint): Constraint = IsAdd(children.map(f):_*)

  override def getChildren = children

  override def reduce(): Constraint = Constraint.reduce(this, op, (children: Seq[Constraint]) => IsAdd(children:_*))
}



object IsMul { def apply(children: Constraint*): Constraint = new IsMul(children:_*).reduce() }

class IsMul private (val children: Constraint*) extends Constraint {

  def op(b1: IsKnown, b2: IsKnown): IsKnown = b1 * b2

  override def reduce(): Constraint = {
    val (known, max, min, rest) = Constraint.collect(Constraint.collapseChildren(this), op)
    val others = max ++ min ++ rest
    (known, others) match {
      case (Nil, Nil) => this
      case (_, _) if known.size + others.size == 1 => (known ++ others).head
      case (Seq(Closed(x)), _) if x == BigDecimal(1) => if(others.size == 1) others.head else IsMul(others:_*)
      case (Seq(Closed(x)), _) if x == BigDecimal(0) => Closed(0)
      case _ if known ++ others == children => this
      case _ =>
        val args = known ++ others
        args.slice(2, args.size).foldLeft(IsMul(args(0), args(1))) { case (m, a) => IsMul(m, a) }
    }
  }

  override def getChildren = children

  override def map(f: Constraint=>Constraint): Constraint = IsMul(children.map(f):_*)

  override def serialize: String = "(" + children.map(_.serialize).mkString(" * ") + ")"
}



object IsNeg { def apply(child: Constraint): Constraint = new IsNeg(child).reduce() }

class IsNeg private (val child: Constraint) extends Constraint {
  override def reduce(): Constraint = child match {
    case k: IsKnown => k.neg
    case x: IsAdd => IsAdd(x.children.map { b => IsNeg(b) }:_*)
    case x: IsMul => IsMul(Seq(IsNeg(x.children.head)) ++ x.children.tail:_*)
    case x: IsNeg => x.child
    case x: IsPow => this
    // -[max(a, b)] -> min[-a, -b]
    case x: IsMax => IsMin(x.children.map { b => IsNeg(b) }:_*)
    case x: IsMin => IsMax(x.children.map { b => IsNeg(b) }:_*)
    case x: IsVar => this
    case _ => this
  }

  override def map(f: Constraint=>Constraint): Constraint = IsNeg(f(child))
  override def serialize: String = "(-" + child.serialize + ")"
}




object IsPow { def apply(child: Constraint): Constraint = new IsPow(child).reduce() }

class IsPow private (val child: Constraint) extends Constraint {
  override def reduce(): Constraint = child match {
    case k: IsKnown => k.pow
    // 2^(a + b) -> 2^a * 2^b
    case x: IsAdd => IsMul(x.children.map { b => IsPow(b)}:_*)
    case x: IsMul => this
    case x: IsNeg => this
    case x: IsPow => this
    // 2^(max(a, b)) -> max(2^a, 2^b) since two is always positive, so a, b control magnitude
    case x: IsMax => IsMax(x.children.map {b => IsPow(b)}:_*)
    case x: IsMin => IsMin(x.children.map {b => IsPow(b)}:_*)
    case x: IsVar => this
    case _ => this
  }

  override def map(f: Constraint=>Constraint): Constraint = IsPow(f(child))

  override def serialize: String = "(2^" + child.serialize + ")"
}



object IsMax { def apply(children: Constraint*): Constraint = new IsMax(children:_*).reduce() }

class IsMax private (val children: Constraint*) extends Constraint {

  def op(b1: IsKnown, b2: IsKnown): IsKnown = b1 max b2

  override def serialize: String = "max(" + children.map(_.serialize).mkString(", ") + ")"

  override def map(f: Constraint=>Constraint): Constraint = IsMax(children.map(f):_*)

  override def getChildren = children

  override def reduce(): Constraint = Constraint.reduce(this, op, (children:Seq[Constraint]) => IsMax(children:_*))
}



object IsMin { def apply(children: Constraint*): Constraint = new IsMin(children:_*).reduce() }

class IsMin private (val children: Constraint*) extends Constraint {

  def op(b1: IsKnown, b2: IsKnown): IsKnown = b1 min b2

  override def serialize: String = "min(" + children.map(_.serialize).mkString(", ") + ")"

  override def map(f: Constraint=>Constraint): Constraint = IsMin(children.map(f):_*)

  override def getChildren = children

  override def reduce(): Constraint = Constraint.reduce(this, op, (children:Seq[Constraint]) => IsMin(children:_*))
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
    case x: IsMax => IsMax(x.children.map {b => IsFloor(b)}:_*)
    case x: IsMin => IsMin(x.children.map {b => IsFloor(b)}:_*)
    case x: IsVar => this
    // floor(floor(x)) -> floor(x)
    case x: IsFloor => x
    case _ => this
  }

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
}

case class VarCon(name: String) extends IsVar
