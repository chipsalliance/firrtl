// See LICENSE for license details.

package firrtl.passes
package memlib
import firrtl._
import firrtl.ir._
import AnalysisUtils.eqMems
import firrtl.Mappers._
import firrtl.annotations._

/** A component, e.g. register etc. Must be declared only once under the TopAnnotation */
case class NoDedupMemAnnotation(target: ComponentName) extends SingleTargetAnnotation[ComponentName] {
  def duplicate(n: ComponentName) = NoDedupMemAnnotation(n)
}

/** Resolves annotation ref to memories that exactly match (except name) another memory
 */
class ResolveMemoryReference extends Transform {
  def inputForm = MidForm
  def outputForm = MidForm

  type AnnotatedMemories = collection.mutable.ArrayBuffer[(String, DefAnnotatedMemory)]

  private def found(map: Map[String, Set[String]], key: String, value: String): Boolean =
    map.get(key).map(_.contains(value)).getOrElse(false)

  /** If a candidate memory is identical except for name to another, add an
    *   annotation that references the name of the other memory.
    */
  def updateMemStmts(mname: String,
                     uniqueMems: AnnotatedMemories,
                     noDedupMap: Map[String, Set[String]])
                    (s: Statement): Statement = s match {
    case m: DefAnnotatedMemory =>
      uniqueMems.find { case (mname2, m2) =>
        !found(noDedupMap, mname2, m2.name) &&
        !found(noDedupMap, mname, m.name) &&
        eqMems(m2, m)
      } match {
        case None =>
          uniqueMems += (mname -> m)
          m
        case Some((module, proto)) => m.copy(memRef = Some(module -> proto.name))
      }
    case s => s.map(updateMemStmts(mname, uniqueMems, noDedupMap))
  }

  def run(c: Circuit, noDedupMap: Map[String, Set[String]]) = {
    val uniqueMems = new AnnotatedMemories
    val modulesx = c.modules.map(m => m.map(updateMemStmts(m.name, uniqueMems, noDedupMap)))
    c.copy(modules = modulesx)
  }
  def execute(state: CircuitState): CircuitState = {
    val noDedups = state.annotations.collect {
      case NoDedupMemAnnotation(ComponentName(cn, ModuleName(mn, _))) => mn -> cn
    }
    val noDedupMap: Map[String, Set[String]] = noDedups.groupBy(_._1).mapValues(_.map(_._2).toSet)
    state.copy(circuit = run(state.circuit, noDedupMap))
  }
}
