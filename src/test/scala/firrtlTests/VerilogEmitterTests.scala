// See LICENSE for license details.

package firrtlTests

import java.io._
import org.scalatest._
import org.scalatest.prop._
import firrtl._
import firrtl.Annotations._
import firrtl.ir.Circuit
import firrtl.passes._
import firrtl.Parser.IgnoreInfo

trait VerilogSpec extends FirrtlFlatSpec {
  def executeTest(input: String, expected: Seq[String], compiler: Compiler) = {
    val writer = new StringWriter()
    compiler.compile(CircuitState(parse(input), ChirrtlForm), writer)
    val output = writer.toString()
    //println(output)
    val lines = output.split("\n") map normalized
    expected foreach { e =>
      lines should contain(e)
    }
  }
}

class DoPrimVerilog extends VerilogSpec {
  "Xorr" should "emit correctly" in {
    val compiler = new VerilogCompiler
    val input =
      """circuit Xorr : 
        |  module Xorr : 
        |    input a: UInt<4>
        |    output b: UInt<1>
        |    b <= xorr(a)""".stripMargin
    val check = 
      """module Xorr(
        |  input  [3:0] a,
        |  output  b
        |);
        |  assign b = ^a;
        |endmodule
        |""".stripMargin.split("\n") map normalized
    executeTest(input, check, compiler)
  }

  "Andr" should "emit correctly" in {
    val compiler = new VerilogCompiler
    val input =
      """circuit Andr : 
        |  module Andr : 
        |    input a: UInt<4>
        |    output b: UInt<1>
        |    b <= andr(a)""".stripMargin
    val check = 
      """module Andr(
        |  input  [3:0] a,
        |  output  b
        |);
        |  assign b = &a;
        |endmodule
        |""".stripMargin.split("\n") map normalized
    executeTest(input, check, compiler)
  }

  "Orr" should "emit correctly" in {
    val compiler = new VerilogCompiler
    val input =
      """circuit Orr : 
        |  module Orr : 
        |    input a: UInt<4>
        |    output b: UInt<1>
        |    b <= orr(a)""".stripMargin
    val check = 
      """module Orr(
        |  input  [3:0] a,
        |  output  b
        |);
        |  assign b = |a;
        |endmodule
        |""".stripMargin.split("\n") map normalized
    executeTest(input, check, compiler)
  }
}

class RegisterSpec extends VerilogSpec {
  "Register" should "emit correctly" in {
    val compiler = new VerilogCompiler
    val input =
      """circuit Register : 
        |  module Register : 
        |    input clock: Clock
        |    input reset: UInt<1>
        |    input data: UInt<32>
        |    output delay: UInt<32>
        |    reg r: UInt<32>, clock with:
        |      reset => (reset, UInt(0))
        |    delay <= r
        |    r <= data
        """.stripMargin
    val check = 
      """module Register(
        |  input   clock,
        |  input   reset,
        |  input  [31:0] data,
        |  output [31:0] delay
        |);
        |  reg [31:0] r;
        |  assign delay = r;
        |  always @(posedge clock) begin
        |    if (reset) begin
        |      r <= 32'h0;
        |    end else begin
        |      r <= data;
        |    end
        |  end
        |    if (reset) begin
        |      r <= 32'h0;
        |    end else begin
        |      r <= data;
        |    end
        |endmodule""".stripMargin.split("\n") map normalized
    executeTest(input, check, compiler)
  }
}

class MemoryVerilog extends VerilogSpec {
  "Memory" should "emit correctly" in {
    val compiler = new VerilogCompiler
    val input =
      """circuit Orr : 
        |  module Orr : 
        |    input clk0: Clock
        |    input clk1: Clock
        |    input clk2: Clock
        |    input r0addr: UInt<5>
        |    input r1addr: UInt<5>
        |    input waddr: UInt<5>
        |    input wdata: UInt<4>
        |    output r0data: UInt<32>
        |    output r1data: UInt<32>
        |    mem m :
        |      depth => 30
        |      data-type => UInt<32>
        |      reader => r0
        |      reader => r1
        |      writer => w
        |      read-latency => 1
        |      write-latency => 1
        |      read-under-write => undefined
        |    m.r0.clk <= clk0
        |    m.r0.addr <= r0addr
        |    m.r0.en <= UInt(1)
        |    r0data <= m.r0.data
        |    m.r1.clk <= clk1
        |    m.r1.addr <= r1addr
        |    m.r1.en <= UInt(1)
        |    r1data <= m.r1.data
        |    m.w.clk <= clk2
        |    m.w.addr <= waddr
        |    m.w.en <= UInt(1)
        |    m.w.mask <= UInt(1)
        |    m.w.data <= wdata
        """.stripMargin
    val check = 
      """module Orr(
        |  input  [3:0] a,
        |  output  b
        |);
        |  assign b = |a;
        |endmodule
        |""".stripMargin.split("\n") map normalized
    executeTest(input, check, compiler)
  }
}
