package firrtl.passes.memlib

import scala.collection.mutable
import firrtl._
import firrtl.ir._
import firrtl.passes.Pass
import firrtl.Mappers._
import firrtl.Utils.inline

class OptimizeClockMux extends Pass {
  def run(c: Circuit): Circuit = c map onModule

  private def onModule(m: DefModule): DefModule = m map optimize
  private val zero = BigInt(0)

  /** Maps (memory, port) to (loc, enable, clock) */
  type MemInfo = mutable.HashMap[(String, String), Expression]

  /** Optimizes away memory port clocks who are muxed on the same enable */
  def optimize(s: Statement): Statement = {
    val memEnables = new MemInfo()
    val nodeMap = new mutable.HashMap[String, Expression]()

    def analyze(s: Statement): Statement = s match {
      case x@Connect(_, loc@WSubField(WSubField(WRef(mem, _, MemKind, _), p, _, _), "en", _, _), expr) =>
        memEnables((mem, p)) = expr
        x
      case x@DefNode(_, name, value) =>
        nodeMap(name) = value
        x
      case other => other map analyze
    }

    /** Populates memEnables */
    analyze(s)

    /** Replaces clock mux with clock signal, if enables match */
    def fixup(s: Statement): Statement = s match {
      case x@Connect(_, loc  @ WSubField(WSubField(WRef(mem, _, MemKind, _), p, _, _), "clk", _, _), expr) =>
        val optClkExpr = inline(nodeMap)(expr)
        val optEnable = inline(nodeMap)(memEnables((mem, p)))
        val difference = Utils.diff(optClkExpr, optEnable)
        val clocksWhenEnableNotZero = difference.foldLeft(Set.empty[Expression]) { case (set, (clkExpr, enable)) =>
          enable match {
            case UIntLiteral(v, _) if v == BigInt(0) => set
            case other => set + clkExpr
          }
        }
        if(clocksWhenEnableNotZero.size == 1)
          Connect(x.info, loc, clocksWhenEnableNotZero.head)
        else x
      case other => other map fixup
    }

    /** Returns fixed statement */
    fixup(s)
  }
}
