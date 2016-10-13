// See LICENSE for license details.

package firrtl.passes
package memlib

import firrtl._
import firrtl.ir._
import firrtl.Utils._
import firrtl.Mappers._
import AnalysisUtils._
import MemPortUtils._
import MemTransformUtils._
import AppendableUtils._


/** Changes memory port names to standard port names (i.e. RW0 instead T_408)
 *  Adds annotation "ref" to memories that exactly match (except name) another memory
 *
 *  TODO(shunshou): Module namespace?
 */
object UpdateDuplicateMemMacros extends Pass {

  def name = "Update Duplicate Mem Macros"

  /** Renames memory ports to a standard naming scheme:
   *    - R0, R1, ... for each read port
   *    - W0, W1, ... for each write port
   *    - RW0, RW1, ... for each readwrite port
   */
  def createMemProto(m: DefMemory): DefMemory = {
    val rports = m.readers.indices map (i => s"R$i")
    val wports = m.writers.indices map (i => s"W$i")
    val rwports = m.readwriters.indices map (i => s"RW$i")
    m copy (readers = rports, writers = wports, readwriters = rwports)
  }

  /** Maps the serialized form of all memory port field names to the
   *    corresponding new memory port field Expression.
   *  E.g.:
   *    - ("m.read.addr") becomes (m.R0.addr)
   */
  def getMemPortMap(m: DefMemory): MemPortMap = {
    val memPortMap = new MemPortMap
    val defaultFields = Seq("addr", "en", "clk")
    val rFields = defaultFields :+ "data"
    val wFields = rFields :+ "mask"
    val rwFields = defaultFields ++ Seq("wmode", "wdata", "rdata", "wmask")

    def updateMemPortMap(ports: Seq[String], fields: Seq[String], newPortKind: String): Unit =
      for ((p, i) <- ports.zipWithIndex; f <- fields) {
        val newPort = createSubField(createRef(m.name), newPortKind + i)
        val field = createSubField(newPort, f)
        memPortMap(s"${m.name}.$p.$f") = field
      }
    updateMemPortMap(m.readers, rFields, "R")
    updateMemPortMap(m.writers, wFields, "W")
    updateMemPortMap(m.readwriters, rwFields, "RW")
    memPortMap
  }

  /** Replaces candidate memories with memories with standard port names
    * If a candidate memory is identical except for name to another, add an
    *   annotation that references the name of the other memory.
    * Does not update the references (this is done via updateStmtRefs)
    */
  def updateMemStmts(uniqueMems: Memories,
                     memPortMap: MemPortMap)
                     (s: Statement): Statement = s match {
    case m: DefMemory if containsInfo(m.info, "useMacro") => 
      val updatedMem = createMemProto(m)
      memPortMap ++= getMemPortMap(m)
      uniqueMems find (x => eqMems(x, updatedMem)) match {
        case None =>
          uniqueMems += updatedMem
          updatedMem
        case Some(proto) =>
          updatedMem copy (info = appendInfo(updatedMem.info, "ref" -> proto.name))
      }
    case s => s map updateMemStmts(uniqueMems, memPortMap)
  }

  /** Replaces candidate memories and their references with standard port names
   */
  def updateMemMods(m: DefModule) = {
    val uniqueMems = new Memories
    val memPortMap = new MemPortMap
    (m map updateMemStmts(uniqueMems, memPortMap)
       map updateStmtRefs(memPortMap))
  }

  def run(c: Circuit) = c copy (modules = c.modules map updateMemMods)
}
