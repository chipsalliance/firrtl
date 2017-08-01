// See LICENSE for license details.

package firrtl.passes

import scala.collection.mutable
import firrtl.PrimOps._
import firrtl.ir._
import firrtl._
import firrtl.Mappers._
import firrtl.Utils.{sub_type, module_type, field_type, max, error, getUIntWidth}
import Implicits.{int2WInt, bigint2WInt}

/** Replaces IntervalType with SIntType, three AST walks:
  * 1) Align binary points
  *    - adds shift operators to primop args and connections
  *    - does not affect declaration- or inferred-types
  * 2) Replace declaration IntervalType's with SIntType's
  *    - for each declaration:
  *      a. remove non-zero binary points
  *      b. remove open bounds
  *      c. replace with SIntType
  * 3) Run InferTypes
  */
class TrimIntervals extends Pass {
  def run(c: Circuit): Circuit = {
    InferTypes.run(c map replaceModuleInterval)
  }
  /* Replace interval types */
  private def replaceModuleInterval(m: DefModule): DefModule = m map replaceStmtInterval map replacePortInterval
  private def replaceStmtInterval(s: Statement): Statement = s map replaceTypeInterval map replaceStmtInterval
  private def replacePortInterval(p: Port): Port = p map replaceTypeInterval
  private def replaceTypeInterval(t: Type): Type = t match {
    case i@IntervalType(l: IsKnown, u: IsKnown, IntWidth(p)) => IntervalType(Closed(i.min), Closed(i.max), IntWidth(p))
    case i: IntervalType => i
    case v => v map replaceTypeInterval
  }
}

// vim: set ts=4 sw=4 et:
