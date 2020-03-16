// See LICENSE for license details.

package firrtlTests

import firrtl._
import firrtl.passes._
import FirrtlCheckers._

import java.io.StringWriter

class MemSpec extends FirrtlPropSpec with FirrtlMatchers {

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
      LowerTypes
    )
    val input =
     """circuit Unit :
       |  module Unit :
       |    input clock : Clock
       |    input i : { a: { b: UInt<32> }, c: UInt<32> }
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
       |    input i : { a: { b: UInt<32> }, c: UInt<32> }
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
      LowerTypes
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

  property("Writing to mems with a bundle of vector literals should work") {
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
      LowerTypes
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

  property("Very large memories should be supported") {
    val addrWidth = 65
    val memSize = BigInt(1) << addrWidth
    val input =
      s"""
         |circuit Test :
         |  module Test :
         |    input clock : Clock
         |    input raddr : UInt<$addrWidth>
         |    output rdata : UInt<8>
         |    input wdata : UInt<8>
         |    input waddr : UInt<$addrWidth>
         |    input wen : UInt<1>
         |
         |    mem m :
         |      data-type => UInt<8>
         |      depth => $memSize
         |      reader => r
         |      writer => w
         |      read-latency => 1
         |      write-latency => 1
         |      read-under-write => undefined
         |    rdata <= m.r.data
         |    m.r.addr <= raddr
         |    m.r.en <= UInt(1)
         |    m.r.clk <= clock
         |    m.w.addr <= waddr
         |    m.w.data <= wdata
         |    m.w.en <= wen
         |    m.w.clk <= clock
         |    m.w.mask <= UInt(1)
       """.stripMargin
    val result = (new VerilogCompiler).compileAndEmit(CircuitState(parse(input), ChirrtlForm, List.empty))
    // TODO Not great that it includes the sparse comment for VCS
    result should containLine (s"reg /* sparse */ [7:0] m [0:$addrWidth'd${memSize-1}];")
  }

  property("Very large CHIRRTL memories should be supported") {
    val addrWidth = 65
    val memSize = BigInt(1) << addrWidth
    val input =
      s"""
         |circuit Test :
         |  module Test :
         |    input clock : Clock
         |    input raddr : UInt<$addrWidth>
         |    output rdata : UInt<8>
         |    input wdata : UInt<8>
         |    input waddr : UInt<$addrWidth>
         |    input wen : UInt<1>
         |
         |    cmem m : UInt<8>[$memSize]
         |    read mport r = m[raddr], clock
         |    rdata <= r
         |    write mport w = m[waddr], clock
         |    when wen :
         |      w <= wdata
       """.stripMargin
    val result = (new VerilogCompiler).compileAndEmit(CircuitState(parse(input), ChirrtlForm, List.empty))
    // TODO Not great that it includes the sparse comment for VCS
    result should containLine (s"reg /* sparse */ [7:0] m [0:$addrWidth'd${memSize-1}];")
  }
}

