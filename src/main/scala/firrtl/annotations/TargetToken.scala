// See LICENSE for license details.

package firrtl.annotations

/** Building block to represent a [[Target]] of a FIRRTL component */
sealed trait TargetToken {
  def value: Any
  def canBeAfter(optT: Option[TargetToken]): Boolean
}

trait SubTargetToken extends TargetToken {
  import TargetToken._
  override def canBeAfter(optT: Option[TargetToken]): Boolean = optT match {
    case Some(_: Ref) => true
    case Some(_: Index) => true
    case Some(_: Field) => true
    case Some(Clock) => true
    case Some(Init) => true
    case Some(Reset) => true
    case Some(other: CustomToken) => other.checkWhitelistedFollower(this)
    case None => false
  }
}

/** Object containing all core [[TargetToken]] subclasses */
case object TargetToken {
  case class Instance(value: String) extends TargetToken {
    override def canBeAfter(optT: Option[TargetToken]): Boolean = optT match {
      case None => true
      case Some(_: OfModule) => true
      case Some(other: CustomToken) => other.checkWhitelistedFollower(this)
    }
  }
  case class OfModule(value: String) extends TargetToken {
    override def canBeAfter(optT: Option[TargetToken]): Boolean = optT match {
      case None => false
      case Some(_: Instance) => true
      case Some(other: CustomToken) => other.checkWhitelistedFollower(this)
    }
  }
  case class Ref(value: String) extends TargetToken {
    override def canBeAfter(optT: Option[TargetToken]): Boolean = optT match {
      case None => true
      case Some(_: Instance) => true
      case Some(_: OfModule) => true
      case Some(other: CustomToken) => other.checkWhitelistedFollower(this)
    }
  }
  case class Index(value: Int) extends SubTargetToken
  case class Field(value: String) extends SubTargetToken
  case object Clock extends SubTargetToken { val value = "" }
  case object Init extends SubTargetToken { val value = "" }
  case object Reset extends SubTargetToken { val value = "" }
  abstract class CustomToken extends TargetToken {
    def checkWhitelistedFollower(follower: TargetToken): Boolean
    def checkWhitelistedLeader(leader: TargetToken): Boolean
  }
}

