// See LICENSE for license details.

package firrtlTests

import java.io.StringWriter

import firrtl.{ChirrtlForm, CircuitState, Parser, VerilogCompiler}
import firrtl.blackboxes.VerilogFileParser
import org.scalatest.{FreeSpec, Matchers}

import scala.io.Source

class BlackBoxEmitSpec extends FreeSpec with Matchers {
  """firrtl circuits that reference external modules""" - {
    """when emitting verilog""" - {
      """need to be able to emit the black box verilog in-line""" in {
        val input =
          """
            |circuit BlackBoxFloatAdder :
            |  extmodule BBFAdd :
            |    output out : UInt<64>
            |    input in2 : UInt<64>
            |    input in1 : UInt<64>
            |
            |    defname = BBFAdd
            |
            |
            |  extmodule BBFAdd_1 :
            |    output out : UInt<64>
            |    input in2 : UInt<64>
            |    input in1 : UInt<64>
            |
            |    defname = BBFAdd
            |
            |
            |  extmodule BBFAdd_2 :
            |    output out : UInt<64>
            |    input in2 : UInt<64>
            |    input in1 : UInt<64>
            |
            |    defname = BBFAdd
            |
            |
            |  module BlackBoxFloatAdder :
            |    input clock : Clock
            |    input reset : UInt<1>
            |    output io : {flip a : {node : UInt<64>}, flip b : {node : UInt<64>}, c : {node : UInt<64>}, d : {node : UInt<64>}, e : {node : UInt<64>}}
            |
            |    io is invalid
            |    io is invalid
            |    wire _T_5 : {node : UInt<64>} @[DspReal.scala 167:19]
            |    _T_5 is invalid @[DspReal.scala 167:19]
            |    _T_5.node <= UInt<64>("h03ff0000000000000") @[DspReal.scala 168:14]
            |    wire _T_15 : {node : UInt<64>} @[DspReal.scala 167:19]
            |    _T_15 is invalid @[DspReal.scala 167:19]
            |    _T_15.node <= UInt<64>("h03ff0000000000000") @[DspReal.scala 168:14]
            |    wire _T_25 : {node : UInt<64>} @[DspReal.scala 167:19]
            |    _T_25 is invalid @[DspReal.scala 167:19]
            |    _T_25.node <= UInt<64>("h03ff0000000000000") @[DspReal.scala 168:14]
            |    wire _T_35 : {node : UInt<64>} @[DspReal.scala 167:19]
            |    _T_35 is invalid @[DspReal.scala 167:19]
            |    _T_35.node <= UInt<64>("h03ff0000000000000") @[DspReal.scala 168:14]
            |    wire _T_45 : {node : UInt<64>} @[DspReal.scala 167:19]
            |    _T_45 is invalid @[DspReal.scala 167:19]
            |    _T_45.node <= UInt<64>("h03ff0000000000000") @[DspReal.scala 168:14]
            |    inst BBFAdd_3 of BBFAdd @[DspReal.scala 98:36]
            |    BBFAdd_3.out is invalid
            |    BBFAdd_3.in2 is invalid
            |    BBFAdd_3.in1 is invalid
            |    BBFAdd_3.in1 <= io.a.node @[DspReal.scala 81:21]
            |    BBFAdd_3.in2 <= io.b.node @[DspReal.scala 82:21]
            |    wire _T_55 : {node : UInt<64>} @[DspReal.scala 83:19]
            |    _T_55 is invalid @[DspReal.scala 83:19]
            |    _T_55.node <= BBFAdd_3.out @[DspReal.scala 84:14]
            |    io.c.node <= _T_55.node @[BlackBoxFloat.scala 54:8]
            |    inst BBFAdd_1_1 of BBFAdd_1 @[DspReal.scala 98:36]
            |    BBFAdd_1_1.out is invalid
            |    BBFAdd_1_1.in2 is invalid
            |    BBFAdd_1_1.in1 is invalid
            |    BBFAdd_1_1.in1 <= io.a.node @[DspReal.scala 81:21]
            |    BBFAdd_1_1.in2 <= io.a.node @[DspReal.scala 82:21]
            |    wire _T_61 : {node : UInt<64>} @[DspReal.scala 83:19]
            |    _T_61 is invalid @[DspReal.scala 83:19]
            |    _T_61.node <= BBFAdd_1_1.out @[DspReal.scala 84:14]
            |    io.d.node <= _T_61.node @[BlackBoxFloat.scala 55:8]
            |    inst BBFAdd_2_1 of BBFAdd_2 @[DspReal.scala 98:36]
            |    BBFAdd_2_1.out is invalid
            |    BBFAdd_2_1.in2 is invalid
            |    BBFAdd_2_1.in1 is invalid
            |    BBFAdd_2_1.in1 <= io.b.node @[DspReal.scala 81:21]
            |    BBFAdd_2_1.in2 <= io.b.node @[DspReal.scala 82:21]
            |    wire _T_67 : {node : UInt<64>} @[DspReal.scala 83:19]
            |    _T_67 is invalid @[DspReal.scala 83:19]
            |    _T_67.node <= BBFAdd_2_1.out @[DspReal.scala 84:14]
            |    io.e.node <= _T_67.node @[BlackBoxFloat.scala 56:8]
          """.stripMargin

        val verilogBlackBoxes = VerilogFileParser.getModules("/Volumes/UCB-BAR/dsptools/src/main/resources/BlackBoxFloat.v")
        val compiler = new VerilogCompiler(verilogBlackBoxes)
        val writer = new StringWriter()
        val parsedInput = Parser.parse(input)
        val newState = compiler.compile(CircuitState(parsedInput, ChirrtlForm), writer)

        val lines = writer.toString().split("\n")

        """
          |module BBFAdd(
          |    input  [63:0] in1,
          |    input  [63:0] in2,
          |    output reg [63:0] out
          |);
          |  always @* begin
          |  out <= $realtobits($bitstoreal(in1) + $bitstoreal(in2));
          |  end
          |endmodule
        """.stripMargin.split("\n").foreach { blackBoxLine =>
            lines.exists { s =>
              s.trim == blackBoxLine.trim
            } should be (true)
        }

//        println(s"newstate\n${writer.toString}")
      }
    }
  }
}
