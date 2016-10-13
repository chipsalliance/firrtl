package firrtl.passes
package memlib

import firrtl.ir._

/** Maps annotation (String) to value (Any)
  * Used to pass annotations in graph between passes
  */
case class AppendableInfo(fields: Map[String, Any]) extends Info {
  def append(a: Map[String, Any]) = this.copy(fields = fields ++ a)
  def append(a: (String, Any)): AppendableInfo = append(Map(a))
  def get(f: String) = fields.get(f)
  override def equals(b: Any) = b match {
    case i: AppendableInfo => fields - "info" == i.fields - "info"
    case _ => false
  }
}

object AppendableUtils {
  def appendInfo[T <: Info](info: T, add: Map[String, Any]): AppendableInfo = info match {
    case i: AppendableInfo => i.append(add)
    case _ => AppendableInfo(fields = add + ("info" -> info))
  }
  def appendInfo[T <: Info](info: T, add: (String, Any)): AppendableInfo = appendInfo(info, Map(add))
  def getInfo[T <: Info](info: T, k: String): Option[Any] = info match {
    case i: AppendableInfo => i.get(k)
    case _ => None
  }
  def containsInfo[T <: Info](info: T, k: String): Boolean = info match {
    case i: AppendableInfo => i.fields.contains(k)
    case _ => false
  }
}

