// See LICENSE for license details.

package firrtl.analyses

import firrtl.annotations.TargetToken._
import firrtl.annotations._

import scala.collection.mutable


/** An Unnamed Custom Token, used to represent simulation statements, doprims, and literals */
sealed trait UnnamedToken extends CustomToken {
  override def checkWhitelistedFollower(follower: TargetToken): Boolean = false

  override def checkWhitelistedLeader(leader: TargetToken): Boolean = false

  override def canBeAfter(optT: Option[TargetToken]): Boolean = optT match {
    case None => true
    case Some(_: OfModule) => true
    case Some(other: CustomToken) => other.checkWhitelistedFollower(this)
    case _ => false
  }

  val tag: Int
}

/** Used for obtaining a tag for a given label
  *
  * E.g. Used for constructing unnamed tokens
  */
class TokenTagger {
  private val counterMap = mutable.HashMap[String, Int]()
  def getTag(label: String): Int = {
    val tag = counterMap.getOrElse(label, 0)
    counterMap(label) = tag + 1
    tag
  }

  def getRef(label: String): String = {
    "@" + label + "#" + getTag(label)
  }
}

case class PrintToken(tag: Int) extends UnnamedToken { override val value: String = tag.toString }

case class StopToken(tag: Int) extends UnnamedToken { override val value: String = tag.toString }

case class OpToken(op: String, tag: Int) extends UnnamedToken { override val value: String = op + "$" + tag }

//TODO(azidar): Create a supported LitTarget
case class LitToken(literal: BigInt, tag: Int) extends UnnamedToken { override val value: String = Int.toString }
