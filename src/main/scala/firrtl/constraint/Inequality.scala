// See LICENSE for license details.

package firrtl.constraint

/**
  *
  */
trait Inequality {
  def left: String
  def right: Constraint
  def geq: Boolean
}
case class GreaterOrEqual(left: String, right: Constraint) extends Inequality {
  val geq = true
  override def toString: String = s"$left >= ${right.serialize}"
}
case class LesserOrEqual(left: String, right: Constraint) extends Inequality {
  val geq = false
  override def toString: String = s"$left <= ${right.serialize}"
}


