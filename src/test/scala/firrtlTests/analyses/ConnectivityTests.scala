package firrtlTests
package analyses

//import firrtl.analyses.{Connectivity}
//import firrtl.{ChirrtlForm, CircuitState, HighFirrtlCompiler, LowFirrtlCompiler, MiddleFirrtlCompiler}
//import firrtl.annotations.{CircuitName, ComponentName, ModuleName}
//import org.scalatest.FlatSpec

//class ConnectivityTests extends FirrtlFlatSpec {
//
//  "Connectivity" should "work" ignore {
//    val c = parse("""
//                    |circuit Test:
//                    |  module Test :
//                    |    input in : UInt<8>
//                    |    output out : UInt<8>
//                    |    inst a1 of A
//                    |    inst a2 of A
//                    |    a1.in <= in
//                    |    a2.in <= a1.out
//                    |    out <= a2.out
//                    |  module A :
//                    |    input in: UInt<8>
//                    |    output out: UInt<8>
//                    |    out <= in
//                    |""".stripMargin)
//    val state = CircuitState(c, ChirrtlForm)
//    val compiled = (new MiddleFirrtlCompiler).compileAndEmit(state, List.empty)
//    val connectivity = Connectivity(compiled)
//    connectivity.registerPaths()
//  }
//
//  "Connectivity" should "with register example" ignore {
//    val c = parse("""
//                    |circuit Test:
//                    |  module Test :
//                    |    input in : UInt<8>
//                    |    input clk: Clock
//                    |    output out : UInt<8>
//                    |    inst a1 of A
//                    |    a1.clk <= asClock(UInt(1))
//                    |    inst a2 of A
//                    |    a2.clk <= clk
//                    |    inst b1 of B
//                    |    b1.clkin <= UInt(1)
//                    |    inst b2 of B
//                    |    b2.clkin <= UInt(1)
//                    |    inst c1 of C
//                    |    c1.clk <= clk
//                    |    inst c2 of C
//                    |    c2.clk <= clk
//                    |    a1.in <= in
//                    |    a2.in <= a1.out
//                    |    b1.in <= a2.out
//                    |    b2.in <= b1.out
//                    |    c1.in <= b2.out
//                    |    c2.in <= c1.out
//                    |    out <= c2.out
//                    |  module A :
//                    |    input in: UInt<8>
//                    |    input clk: Clock
//                    |    output out: UInt<8>
//                    |    reg r : UInt<8>, clk
//                    |    r <= in
//                    |    out <= r
//                    |  module B :
//                    |    input in: UInt<8>
//                    |    input clkin: UInt<1>
//                    |    output out: UInt<8>
//                    |    reg r : UInt<8>, asClock(clkin)
//                    |    r <= in
//                    |    out <= r
//                    |  module C :
//                    |    input in: UInt<8>
//                    |    input clk: Clock
//                    |    output out: UInt<8>
//                    |    inst clkdiv of CLKDIV
//                    |    clkdiv.clk <= clk
//                    |    reg r : UInt<8>, clkdiv.clk_2
//                    |    r <= in
//                    |    out <= r
//                    |  extmodule CLKDIV:
//                    |    input clk: Clock
//                    |    output clk_2: Clock
//                    |""".stripMargin)
//    val state = CircuitState(c, ChirrtlForm)
//    val compiled = (new MiddleFirrtlCompiler).compileAndEmit(state, List.empty)
//    val connectivity = Connectivity(compiled)
//    val clockSources = connectivity.clockSources()
//    clockSources foreach println
//
//  }
//}
//
//object Main extends FirrtlFlatSpec with App {
//
//    val c = parse("""
//                    |circuit Test:
//                    |  module Test :
//                    |    input in : UInt<8>
//                    |    input clk: Clock
//                    |    output out : UInt<8>
//                    |    inst a1 of A
//                    |    a1.clk <= asClock(UInt(1))
//                    |    inst a2 of A
//                    |    a2.clk <= clk
//                    |    inst b1 of B
//                    |    b1.clkin <= UInt(1)
//                    |    inst b2 of B
//                    |    b2.clkin <= UInt(1)
//                    |    inst c1 of C
//                    |    c1.clk <= clk
//                    |    inst c2 of C
//                    |    c2.clk <= clk
//                    |    a1.in <= in
//                    |    a2.in <= a1.out
//                    |    b1.in <= a2.out
//                    |    b2.in <= b1.out
//                    |    c1.in <= b2.out
//                    |    c2.in <= c1.out
//                    |    out <= c2.out
//                    |  module A :
//                    |    input in: UInt<8>
//                    |    input clk: Clock
//                    |    output out: UInt<8>
//                    |    reg r : UInt<8>, clk
//                    |    r <= in
//                    |    out <= r
//                    |  module B :
//                    |    input in: UInt<8>
//                    |    input clkin: UInt<1>
//                    |    output out: UInt<8>
//                    |    reg r : UInt<8>, asClock(clkin)
//                    |    r <= in
//                    |    out <= r
//                    |  module C :
//                    |    input in: UInt<8>
//                    |    input clk: Clock
//                    |    output out: UInt<8>
//                    |    inst clkdiv of CLKDIV
//                    |    clkdiv.clk <= clk
//                    |    reg r : UInt<8>, clkdiv.clk_2
//                    |    r <= in
//                    |    out <= r
//                    |  extmodule CLKDIV:
//                    |    input clk: Clock
//                    |    output clk_2: Clock
//                    |""".stripMargin)
//    val state = CircuitState(c, ChirrtlForm)
//    val compiled = (new MiddleFirrtlCompiler).compileAndEmit(state, List.empty)
//    val connectivity = Connectivity(compiled)
//    val clockSources = connectivity.clockSources()
//    clockSources foreach println
//
//}
