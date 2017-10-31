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
    case (_: IsFloor, x) if x.size != 1 => sys.error("Shouldn't be here")
    case (_: IsFloor, x)                => IsFloor(x.head)
    case (_: IsNeg, x) if x.size != 1 => sys.error("Shouldn't be here")
    case (_: IsNeg, x)                => IsNeg(x.head)
    case (_: IsPow, x) if x.size != 1 => sys.error("Shouldn't be here")
    case (_: IsPow, x)                => IsPow(x.head)
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
  def pow: IsKnown
  def floor: IsKnown

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
    case x: IsAdd => IsAdd(x.children.map { b => IsNeg(b).reduce }:_*).reduce
    case x: IsMul => IsMul(Seq(IsNeg(x.children.head).reduce()) ++ x.children.tail:_*).reduce()
    case x: IsNeg => x.child
    case x: IsPow => this
    // -[max(a, b)] -> min[-a, -b]
    case x: IsMax => IsMin(x.children.map {b => IsNeg(b).reduce }:_*).reduce
    case x: IsMin => IsMax(x.children.map {b => IsNeg(b).reduce }:_*).reduce
    case x: IsVar => this
    case _ => this
  }
  def map(f: IsConstrainable=>IsConstrainable): IsConstrainable = IsNeg(f(child))
  def serialize: String = "(-" + child.serialize + ")"
}
case class IsPow(child: IsConstrainable) extends IsConstrainable {
  override def reduce(): IsConstrainable = child match {
    case k: IsKnown => k.pow
    // 2^(a + b) -> 2^a * 2^b
    case x: IsAdd => IsMul(x.children.map { b => IsPow(b).reduce }:_*).reduce
    case x: IsMul => this
    case x: IsNeg => this
    case x: IsPow => this
    // 2^(max(a, b)) -> max(2^a, 2^b) since two is always positive, so a, b control magnitude
    case x: IsMax => IsMax(x.children.map {b => IsPow(b).reduce }:_*).reduce
    case x: IsMin => IsMin(x.children.map {b => IsPow(b).reduce }:_*).reduce
    case x: IsVar => this
    case _ => this
  }
  def map(f: IsConstrainable=>IsConstrainable): IsConstrainable = IsPow(f(child))
  def serialize: String = "(2^" + child.serialize + ")"
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
trait IsVar extends IsConstrainable {
  def name: String
  def serialize: String = name
  def map(f: IsConstrainable=>IsConstrainable): IsConstrainable = this
}
case class VarCon(name: String) extends IsVar
object IsVar {
  //def apply(name: String) = new IsVar(name)
  def unapply(i: IsConstrainable): Option[String] = i match {
    case i: IsVar => Some(i.name)
    case _ => None
  }
}
case class IsFloor(child: IsConstrainable) extends IsConstrainable {
  override def reduce(): IsConstrainable = child match {
    case k: IsKnown => k.floor
    case x: IsAdd => this
    case x: IsMul => this
    case x: IsNeg => this
    case x: IsPow => this
    // floor(max(a, b)) -> max(floor(a), floor(b))
    case x: IsMax => IsMax(x.children.map {b => IsFloor(b).reduce }:_*).reduce
    case x: IsMin => IsMin(x.children.map {b => IsFloor(b).reduce }:_*).reduce
    case x: IsVar => this
    // floor(floor(x)) -> floor(x)
    case x: IsFloor => x
    case _ => this
  }
  def map(f: IsConstrainable=>IsConstrainable): IsConstrainable = IsFloor(f(child))
  def serialize: String = "floor(" + child.serialize + ")"
}