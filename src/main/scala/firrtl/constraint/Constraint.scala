// See LICENSE for license details.

package firrtl.constraint

import firrtl.ir._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Constraint {
  /** Returns all children, including children of children who share the same class as this
    * @param constraint Constraint to return collapsed children constraints
    * @return
    */
  //def collapseChildren(constraint: Constraint): Seq[Constraint] = {
  //  constraint.getChildren.foldLeft(ListBuffer.empty[Constraint]){ (vec, c) =>
  //    c match {
  //      case a if a.getClass == constraint.getClass => vec ++ a.getChildren
  //      case x => vec :+ x
  //    }
  //  }
  //}

  /** Groups constraints into [[IsKnown]], [[IsMax]], [[IsMin]], and [[Constraint]] seqs
    * [[IsKnown]] Seq is reduced with binOp
    * @param constraints
    * @param binOp
    * @return
    */
  //def collect(constraints: Seq[Constraint], binOp: (IsKnown, IsKnown) => IsKnown):
  //    (Seq[IsKnown], Seq[IsMax], Seq[IsMin], Seq[Constraint]) = {
  //  val knowns = mutable.ArrayBuffer[IsKnown]()
  //  val maxs = mutable.ArrayBuffer[IsMax]()
  //  val mins = mutable.ArrayBuffer[IsMin]()
  //  val others = mutable.ArrayBuffer[Constraint]()
  //  constraints.foreach {
  //    case c: IsKnown => knowns += c
  //    case c: IsMax => maxs += c
  //    case c: IsMin => mins += c
  //    case c => others += c
  //  }
  //  val reducedKnown = knowns.foldLeft(Seq.empty[IsKnown]) { (col, k) =>
  //    col match {
  //      case Nil => Seq(k)
  //      case Seq(x) => Seq(binOp(x, k))
  //    }
  //  }
  //  (reducedKnown, maxs, mins, others)
  //}


  /** Generic simplification of a constraint, only for [[IsAdd]], [[IsMax]], and [[IsMin]]
    * @param c constraint
    * @param binOp reduce operator
    * @param gen constructor for a [[Constraint]] of the same class as c
    * @return
    */
  //private [constraint] def reduce(c: Constraint, binOp: (IsKnown, IsKnown)=>IsKnown, gen: Seq[Constraint]=>Constraint): Constraint = {
  //  val (known, max, min, others) = collect(collapseChildren(c), binOp)
  //  (max, min, known, others) match {
  //    case (Nil, Nil, Nil, Nil) => c
  //    case (Nil, Nil, _, _) if known.size + others.size == 1 => (known ++ others).head
  //    case (Nil, Nil, _, _) if known ++ others == c.getChildren => c
  //    case (Nil, Nil, _, _) => gen(known ++ others)
  //    case (Seq(x), Nil, _, _) => IsMax(x.children.map { c => gen(Seq(c) ++ known ++ others) }:_*)
  //    case (Nil, Seq(x), _, _) => IsMin(x.children.map { c => gen(Seq(c) ++ known ++ others) }:_*)
  //    case _ if (max ++ min ++ known ++ others).toSet == c.getChildren.toSet => c
  //    case _ => gen(known ++ others ++ max ++ min)
  //  }
  //}
}

/** Trait for all Constraint Solver expressions */
trait Constraint {
  def serialize: String
  def map(f: Constraint=>Constraint): Constraint
  val children: Seq[Constraint]
  //def apply(known: Seq[IsKnown], maxOpt: Option[IsMax], minOpt: Option[IsMin], others: Seq[Constraint]): this.type

  //val known: Option[IsKnown] = None
  //val maxs: Seq[IsMax] = Nil
  //val mins: Seq[IsMin] = Nil
  //val others: Seq[Constraint] = Nil

  //val children: Seq[Constraint] = maxOpt.iterator.toSeq ++ minOpt.iterator ++ known ++ others

  //def addChild(x: Constraint): this.type

  def reduce(): Constraint

  //def reduce(): Constraint

  /** Generic simplification of a constraint, only for [[IsAdd]], [[IsMax]], and [[IsMin]]
    * @param c constraint
    * @param binOp reduce operator
    * @param gen constructor for a [[Constraint]] of the same class as c
    * @return
    */
  //private [constraint] def reduce(gen: Seq[Constraint] => Constraint): Constraint = {
  //  (maxOpt, minOpt) match {
  //    case (Some(x), None) => IsMax(x.children.map { c => apply(known, Seq(c) ++ known ++ others) }:_*)
  //    case (None, Some(x)) => IsMin(x.children.map { c => apply(Seq(c) ++ known ++ others) }:_*)
  //    case _ => this
  //  }
  //}
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

  val children = Nil
  def reduce() = this
}



object IsAdd {
  def apply(left: Constraint, right: Constraint): Constraint = (left, right) match {
    case (l: IsKnown, r: IsKnown) => l + r
    case _ => apply(Seq(left, right))
  }
  def apply(children: Seq[Constraint]): Constraint = {
    children.foldLeft(new IsAdd(None, Nil, Nil, Nil)) { (add, c) =>
      add.addChild(c)
    }.reduce()
  }
}

case class IsAdd private (val known: Option[IsKnown],
                     val maxs: Seq[IsMax],
                     val mins: Seq[IsMin],
                     val others: Seq[Constraint]) extends Constraint with MultiAry {

  def op(b1: IsKnown, b2: IsKnown): IsKnown = b1 + b2

  val children = if(known.nonEmpty) known.get +: (maxs ++ mins ++ others) else (maxs ++ mins ++ others)

  def addChild(x: Constraint): IsAdd = x match {
    case k: IsKnown => new IsAdd(known = merge(Some(k), known), maxs, mins, others)
    case add: IsAdd => new IsAdd(merge(known, add.known), maxs ++ add.maxs, mins ++ add.mins, others ++ add.others)
    case max: IsMax =>  new IsAdd(known, max +: maxs, mins, others)
    case min: IsMin =>  new IsAdd(known, maxs, min +: mins, others)
    case other => new IsAdd(known, maxs, mins, other +: others)
  }

  override def serialize: String = "(" + children.map(_.serialize).mkString(" + ") + ")"

  override def map(f: Constraint=>Constraint): Constraint = IsAdd(children.map(f))

  def reduce(): Constraint = (known, maxs, mins, others) match {
    case (Some(k), Nil, Nil, Nil) => k
    case (_, Seq(x), Seq(), Seq()) =>
      val maxOthers = x.others.foldLeft(new IsAdd(known, Nil, Nil, others))((a, o) => a.addChild(o)).reduce()
      val maxKnown = merge(known, x.known)
      new IsMax(maxKnown, Nil, Seq(maxOthers)).reduce()
    case (_, Seq(), Seq(x), Seq()) =>
      val minOthers = x.others.foldLeft(new IsAdd(known, Nil, Nil, others))((a, o) => a.addChild(o)).reduce()
      val minKnown = merge(known, x.known)
      new IsMin(minKnown, Nil, Seq(minOthers)).reduce()
    case _ => this
  }
}

object IsMax {
  def apply(left: Constraint, right: Constraint): Constraint = (left, right) match {
    case (l: IsKnown, r: IsKnown) => l max r
    case _ => apply(Seq(left, right))
  }
  def apply(children: Seq[Constraint]): Constraint = {
    children.foldLeft(new IsMax(None, Nil, Nil)) { (add, c) =>
      add.addChild(c)
    }.reduce()
  }
  //private [constraint] def reduce(gen: Seq[Constraint] => Constraint): Constraint = {
  //  (maxOpt, minOpt) match {
  //    case (Some(x), None) => IsMax(x.children.map { c => apply(known, Seq(c) ++ known ++ others) }:_*)
  //    case (None, Some(x)) => IsMin(x.children.map { c => apply(Seq(c) ++ known ++ others) }:_*)
  //    case _ => this
  //  }
  //}
}

case class IsMax private[constraint] (val known: Option[IsKnown],
                     val mins: Seq[IsMin],
                     val others: Seq[Constraint]
                    ) extends MultiAry {

  def op(b1: IsKnown, b2: IsKnown): IsKnown = b1 max b2

  override def serialize: String = "max(" + children.map(_.serialize).mkString(", ") + ")"

  override def map(f: Constraint=>Constraint): Constraint = IsMax(children.map(f))

  val children = if(known.nonEmpty) known.get +: (mins ++ others) else (mins ++ others)

  def reduce(): Constraint = (known, mins, others) match {
    case (Some(k), Seq(), Seq()) => k
    case (None, Seq(m), Seq()) => m
    case (None, Seq(), Seq(o)) => o
    case _ => this
  }

  def addChild(x: Constraint): IsMax = x match {
    case k: IsKnown => new IsMax(known = merge(Some(k), known), mins, others)
    case max: IsMax =>  new IsMax(known = merge(known, max.known), max.mins ++ mins, others ++ max.others)
    case min: IsMin =>  new IsMax(known, min +: mins, others)
    case other => new IsMax(known, mins, other +: others)
  }
}



object IsMin {
  def apply(left: Constraint, right: Constraint): Constraint = (left, right) match {
    case (l: IsKnown, r: IsKnown) => l min r
    case _ => apply(Seq(left, right))
  }
  def apply(children: Seq[Constraint]): Constraint = {
    children.foldLeft(new IsMin(None, Nil, Nil)) { (add, c) =>
      add.addChild(c)
    }.reduce()
  }
}

case class IsMin private[constraint] (val known: Option[IsKnown],
                     val maxs: Seq[IsMax],
                     val others: Seq[Constraint]
                    ) extends MultiAry {

  def op(b1: IsKnown, b2: IsKnown): IsKnown = b1 min b2

  override def serialize: String = "min(" + children.map(_.serialize).mkString(", ") + ")"

  override def map(f: Constraint=>Constraint): Constraint = IsMin(children.map(f))

  val children = if(known.nonEmpty) known.get +: (maxs ++ others) else (maxs ++ others)

  def reduce(): Constraint = (known, maxs, others) match {
    case (Some(k), Seq(), Seq()) => k
    case (None, Seq(m), Seq()) => m
    case (None, Seq(), Seq(o)) => o
    case _ => this
  }

  def addChild(x: Constraint): IsMin = x match {
    case k: IsKnown => new IsMin(merge(Some(k), known), maxs, others)
    case max: IsMax =>  new IsMin(known, max +: maxs, others)
    case min: IsMin =>  new IsMin(merge(min.known, known), maxs ++ min.maxs, others ++ min.others)
    case other => new IsMin(known, maxs, other +: others)
  }
}



object IsMul {
  def apply(left: Constraint, right: Constraint): Constraint = (left, right) match {
    case (l: IsKnown, r: IsKnown) => l * r
    case _ => apply(Seq(left, right))
  }
  def apply(children: Seq[Constraint]): Constraint = {
    children.foldLeft(new IsMul(None, Nil)) { (add, c) =>
      add.addChild(c)
    }.reduce()
  }
}

case class IsMul private (val known: Option[IsKnown], val others: Seq[Constraint]) extends MultiAry {

  def op(b1: IsKnown, b2: IsKnown): IsKnown = b1 * b2

  val children = if(known.nonEmpty) known.get +: others else others

  def addChild(x: Constraint): IsMul = x match {
    case k: IsKnown => new IsMul(known = merge(Some(k), known), others)
    case mul: IsMul => new IsMul(merge(known, mul.known), others ++ mul.others)
    case other => new IsMul(known, other +: others)
  }

  override def reduce(): Constraint = {
    (known, others) match {
      case (Some(k), Nil) => k
      case (None, Seq(x)) => x
      case (Some(Closed(x)), _) if x == BigDecimal(1) => if(others.size == 1) others.head else new IsMul(None, others)
      case (Some(Closed(x)), _) if x == BigDecimal(0) => Closed(0)
      case _ => this
      //TODO why do I do this?
      //case _ =>
      //  val args = known ++ others
      //  args.slice(2, args.size).foldLeft(IsMul(args(0), args(1))) { case (m, a) => IsMul(m, a) }
    }
  }

  override def map(f: Constraint=>Constraint): Constraint = IsMul(children.map(f))

  override def serialize: String = "(" + children.map(_.serialize).mkString(" * ") + ")"
}



object IsNeg {
  def apply(child: Constraint): Constraint = new IsNeg(child).reduce()
}

case class IsNeg private (child: Constraint) extends Constraint {
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

  val children = Seq(child)

  override def map(f: Constraint=>Constraint): Constraint = IsNeg(f(child))
  override def serialize: String = "(-" + child.serialize + ")"
}




object IsPow { def apply(child: Constraint): Constraint = new IsPow(child).reduce() }

case class IsPow private (val child: Constraint) extends Constraint {
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
  val children = Seq(child)

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
  val children = Seq(child)

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

  val children = Seq()
}

case class VarCon(name: String) extends IsVar
