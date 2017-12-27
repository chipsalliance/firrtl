package firrtl.passes.memlib

import scala.collection.mutable
import firrtl._
import firrtl.ir._
import firrtl.passes.Pass
import firrtl.Mappers._
import firrtl.PrimOps.AsClock
import firrtl.WrappedExpression.weq

class OptimizeClockMux extends Pass {
  def run(c: Circuit): Circuit = c map onModule

  private def onModule(m: DefModule): DefModule = m map optimize
  private val zero = BigInt(0)

  /** Maps (memory, port) to (loc, enable, clock) */
  type MemInfo = mutable.HashMap[(String, String), Expression]

  /** Optimizes away memory port clocks who are muxed on the same enable */
  def optimize(s: Statement): Statement = {
    val memEnables = new MemInfo()
    //val memClocks = new MemInfo()
    def analyze(s: Statement): Statement = s match {
      case x@Connect(_, loc@WSubField(WSubField(WRef(mem, _, MemKind, _), p, _, _), "en", _, _), expr) =>
        memEnables((mem, p)) = expr
        x
      case other => other map analyze
    }

    /** Populates memEnables */
    analyze(s)

    /** Replaces clock mux with clock signal, if enables match */
    def fixup(s: Statement): Statement = s match {
      case x@Connect(_, loc  @ WSubField(WSubField(WRef(mem, _, MemKind, _), p, _, _), "clk", _, _),
                        expr @ Mux(cond, clock, DoPrim(AsClock, Seq(UIntLiteral(`zero`, _)), _, _), _)
                    ) =>
        val constProp = new transforms.ConstantPropagation()
        val optEnable = constProp.optimize(memEnables((mem, p)))
        val optCond = constProp.optimize(cond)
        if(weq(optEnable, optCond)) Connect(x.info, loc, clock) else x
      case other => other map fixup
    }

    /** Returns fixed statement */
    fixup(s)
  }
}
