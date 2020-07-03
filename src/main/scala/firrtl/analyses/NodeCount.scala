// See LICENSE for license details.

package firrtl
package analyses

import ir._
import scala.annotation.tailrec
import scala.collection.JavaConverters._

/** This is not intended to be used as a metric for the size of an actual
  *   circuit, but rather to debug the compiler itself
  * Works because all FirrtlNodes implement Product (case classes do)
  */
class NodeCount private (node: FirrtlNode) {

  // Counts the number of unique objects in a Firrtl graph
  // There is no IdentityHashSet
  private val identityMap = new java.util.IdentityHashMap[Any, Boolean]()
  NodeCount.count(node, identityMap)

  /** Number of nodes that are referentially unique
    *
    * !(a eq b)
    */
  def unique: Long = identityMap.size

  /** Number of nodes in this NodeCount that are NOT present in that NodeCount */
  def uniqueFrom(that: NodeCount): Long =
    this.identityMap.keySet.asScala.count(!that.identityMap.containsKey(_))
}

object NodeCount {
  def apply(node: FirrtlNode) = new NodeCount(node)
  private def count(node: FirrtlNode, identityMap: java.util.IdentityHashMap[Any, Boolean]): Unit = {
    val stack = scala.collection.mutable.ArrayStack[Any]()
    stack.push(node)
    while(stack.nonEmpty) {
      val node = stack.pop()
      require(node.isInstanceOf[Product] || !node.isInstanceOf[FirrtlNode],
        "Unexpected FirrtlNode that does not implement Product!")
      if (!identityMap.containsKey(node)) {
        identityMap.put(node, true)
        node match { // FirrtlNodes are Products
          case p: Product => p.productIterator.foreach(stack.push)
          case i: Iterable[Any] => i.foreach(stack.push)
          case _ => List.empty
        }
      }
    }
  }
}
