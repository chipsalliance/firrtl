// See LICENSE for license details.

package firrtlTests

import java.io._
import org.scalatest._
import org.scalatest.prop._
import firrtl._
import firrtl.ir._
import firrtl.passes._
import firrtl.transforms._
import FirrtlCheckers._

class MemSpec extends FirrtlPropSpec {

  property("Zero-ported mems should be supported!") {
    runFirrtlTest("ZeroPortMem", "/features")
  }

  property("Mems with zero-width elements should be supported!") {
    runFirrtlTest("ZeroWidthMem", "/features")
  }

  property("Writing to mems with bundle literals should work") {
    val passes = Seq(
      CheckChirrtl,
      CInferTypes,
      CInferMDir,
      RemoveCHIRRTL,
      ToWorkingIR,
      CheckHighForm,
      ResolveKinds,
      ResolveGenders,
      InferTypes,
      CheckTypes,
      ExpandConnects,
      LowerTypes,
    )
    val input =
     """circuit Unit :
       |  module Unit :
       |    input clock : Clock
       |    output i : { a: { b: UInt<32> }, c: UInt<32> }
       |    output o : { a: { b: UInt<32> }, c: UInt<32> }
       |    input wZero: UInt<1>
       |    input waddr: UInt<4>
       |    input raddr: UInt<4>
       |
       |    cmem ram : { a: { b: UInt<32> }, c: UInt<32> }[16]
       |
       |    infer mport r = ram[raddr], clock
       |    o <= r
       |    infer mport w = ram[waddr], clock
       |    w <= i
       |    when wZero :
       |      w <= { a: { b: UInt<32>("h0") }, c: UInt<32>("h0") }
       |    """.stripMargin
    val check =
     """circuit Unit :
       |  module Unit :
       |    input clock : Clock
       |    output i : { a: { b: UInt<32> }, c: UInt<32> }
       |    output o : { a: { b: UInt<32> }, c: UInt<32> }
       |    input wZero: UInt<1>
       |    input waddr: UInt<4>
       |    input raddr: UInt<4>
       |
       |    cmem ram : { a: { b: UInt<32> }, c: UInt<32> }[16]
       |
       |    infer mport r = ram[raddr], clock
       |    o <= r
       |    infer mport w = ram[waddr], clock
       |    w <= i
       |    when wZero :
       |      w.a.b <= UInt<32>("h0")
       |      w.c <= UInt<32>("h0")
       |    """.stripMargin
    val iResult = passes.foldLeft(CircuitState(parse(input), UnknownForm)) {
      (c: CircuitState, p: Transform) => p.runTransform(c)
    }
    val cResult = passes.foldLeft(CircuitState(parse(check), UnknownForm)) {
      (c: CircuitState, p: Transform) => p.runTransform(c)
    }
    def removeMask(s: String) =
      s.split("\n") filter { l => !(l contains "mask") } mkString "\n"
    val iWriter = new StringWriter()
    val cWriter = new StringWriter()
    (new HighFirrtlEmitter).emit(iResult, iWriter)
    (new HighFirrtlEmitter).emit(cResult, cWriter)
    val iCircuit = parse(removeMask(iWriter.toString()))
    val cCircuit = parse(removeMask(cWriter.toString()))
    iCircuit should be (cCircuit)
  }

  property("Writing to mems with a vector of bundle literals should work") {
    val passes = Seq(
      CheckChirrtl,
      CInferTypes,
      CInferMDir,
      RemoveCHIRRTL,
      ToWorkingIR,
      CheckHighForm,
      ResolveKinds,
      ResolveGenders,
      InferTypes,
      CheckTypes,
      ReplaceAccesses,
      ExpandConnects,
      LowerTypes,
    )
    val input =
     """circuit Unit :
       |  module Unit :
       |    input clock : Clock
       |    input i : { a: { b: UInt<32> }, c: UInt<32> }[3]
       |    output o : { a: { b: UInt<32> }, c: UInt<32> }[3]
       |    input wZero: UInt<1>
       |    input waddr: UInt<4>
       |    input raddr: UInt<4>
       |
       |    cmem ram : { a: { b: UInt<32> }, c: UInt<32> }[3][16]
       |
       |    infer mport r = ram[raddr], clock
       |    o <= r
       |    infer mport w = ram[waddr], clock
       |    w <= i
       |    when wZero :
       |      w <= [ { a: { b: UInt<32>("h0") }, c: UInt<32>("h0") }, { a: { b: UInt<32>("h1") }, c: UInt<32>("h1") }, { a: { b: UInt<32>("h4") }, c: UInt<32>("h4") }]
       |    """.stripMargin
    val check =
     """circuit Unit :
       |  module Unit :
       |    input clock : Clock
       |    input i : { a: { b: UInt<32> }, c: UInt<32> }[3]
       |    output o : { a: { b: UInt<32> }, c: UInt<32> }[3]
       |    input wZero: UInt<1>
       |    input waddr: UInt<4>
       |    input raddr: UInt<4>
       |
       |    cmem ram : { a: { b: UInt<32> }, c: UInt<32> }[3][16]
       |
       |    infer mport r = ram[raddr], clock
       |    o <= r
       |    infer mport w = ram[waddr], clock
       |    w <= i
       |    when wZero :
       |      w[0].a.b <= UInt<32>("h0")
       |      w[0].c <= UInt<32>("h0")
       |      w[1].a.b <= UInt<32>("h1")
       |      w[1].c <= UInt<32>("h1")
       |      w[2].a.b <= UInt<32>("h4")
       |      w[2].c <= UInt<32>("h4")
       |    """.stripMargin
    val iResult = passes.foldLeft(CircuitState(parse(input), UnknownForm)) {
      (c: CircuitState, p: Transform) => p.runTransform(c)
    }
    println("Input done")
    val cResult = passes.foldLeft(CircuitState(parse(check), UnknownForm)) {
      (c: CircuitState, p: Transform) => p.runTransform(c)
    }
    println("Output done")
    // All the assignments to addr, clk, mask, en, etc. will be in different orders
    // Get rid of them, leaving just data
    def removeJunk(s: String) =
      s.split("\n") filter { l => !(
        (l contains "mask") ||
        (l contains "invalid") ||
        (l contains ".addr") ||
        (l contains ".clk") ||
        (l contains ".en") ||
        (l contains ".mask")
      ) } mkString "\n"
    val iWriter = new StringWriter()
    val cWriter = new StringWriter()
    (new HighFirrtlEmitter).emit(iResult, iWriter)
    (new HighFirrtlEmitter).emit(cResult, cWriter)
    val iCircuit = parse(removeJunk(iWriter.toString()))
    val cCircuit = parse(removeJunk(cWriter.toString()))
    iCircuit should be (cCircuit)
  }

  ignore("Writing to mems with a bundle of vector literals should work") {
    val passes = Seq(
      CheckChirrtl,
      CInferTypes,
      CInferMDir,
      RemoveCHIRRTL,
      ToWorkingIR,
      CheckHighForm,
      ResolveKinds,
      ResolveGenders,
      InferTypes,
      CheckTypes,
      ReplaceAccesses,
      ExpandConnects,
      LowerTypes,
    )
    val input =
     """circuit Unit :
       |  module Unit :
       |    input clock : Clock
       |    input i : { a: { b: UInt<32>[3] }, c: UInt<32> }
       |    output o : { a: { b: UInt<32>[3] }, c: UInt<32> }
       |    input wZero: UInt<1>
       |    input waddr: UInt<4>
       |    input raddr: UInt<4>
       |
       |    cmem ram : { a: { b: UInt<32>[3] }, c: UInt<32> }[16]
       |
       |    infer mport r = ram[raddr], clock
       |    o <= r
       |    infer mport w = ram[waddr], clock
       |    w <= i
       |    when wZero :
       |      w <= { a: { b: [ UInt<32>("h0"), UInt<32>("h3"), UInt<32>("h6")] }, c: UInt<32>("h1") }
       |    """.stripMargin
    val check =
     """circuit Unit :
       |  module Unit :
       |    input clock : Clock
       |    input i : { a: { b: UInt<32>[3] }, c: UInt<32> }
       |    output o : { a: { b: UInt<32>[3] }, c: UInt<32> }
       |    input wZero: UInt<1>
       |    input waddr: UInt<4>
       |    input raddr: UInt<4>
       |
       |    cmem ram : { a: { b: UInt<32>[3] }, c: UInt<32> }[16]
       |
       |    infer mport r = ram[raddr], clock
       |    o <= r
       |    infer mport w = ram[waddr], clock
       |    w <= i
       |    when wZero :
       |      w.a.b[0] <= UInt<32>("h0")
       |      w.a.b[1] <= UInt<32>("h3")
       |      w.a.b[2] <= UInt<32>("h6")
       |      w.c <= UInt<32>("h1")
       |    """.stripMargin
    val iResult = passes.foldLeft(CircuitState(parse(input), UnknownForm)) {
      (c: CircuitState, p: Transform) => p.runTransform(c)
    }
    println("Input done")
    val cResult = passes.foldLeft(CircuitState(parse(check), UnknownForm)) {
      (c: CircuitState, p: Transform) => p.runTransform(c)
    }
    println("Output done")
    // All the assignments to addr, clk, mask, en, etc. will be in different orders
    // Get rid of them, leaving just data
    def removeJunk(s: String) =
      s.split("\n") filter { l => !(
        (l contains "mask") ||
        (l contains "invalid") ||
        (l contains ".addr") ||
        (l contains ".clk") ||
        (l contains ".en") ||
        (l contains ".mask")
      ) } mkString "\n"
    val iWriter = new StringWriter()
    val cWriter = new StringWriter()
    (new HighFirrtlEmitter).emit(iResult, iWriter)
    (new HighFirrtlEmitter).emit(cResult, cWriter)
    val iCircuit = parse(removeJunk(iWriter.toString()))
    val cCircuit = parse(removeJunk(cWriter.toString()))
    iCircuit should be (cCircuit)
  }
}

