// See LICENSE for license details.

package firrtl

import scala.collection.mutable
import firrtl.ir._
import Mappers._

// Temporary global object
// Note that this is very different than Namespace because it is just a mapping
// Whenever one wants to generate a new name, they should still use Namespace (for now at least)
// TODO Add to CircuitState
object IdNamespace {

  // Begin with a tempNamePrefix in namespace so we always have a number suffix
  private val idMap = mutable.LongMap.empty[String]
  private val nameMap = mutable.HashMap.empty[String, Long]
  private var n = 0L

  def lookupName(id: Id): String = idMap(id)
  def lookupId(name: String): Id = nameMap(name)
  /** lookup and allocate if not present */
  // TODO Are updates to maps critical sections?
  def allocate(name: String): Id =
    if (nameMap.contains(name)) nameMap(name)
    else this.synchronized {
      val id = n
      n += 1
      idMap(id) = name
      nameMap(name) = id
      id
    }
}

