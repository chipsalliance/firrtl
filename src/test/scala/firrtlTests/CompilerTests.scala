/*
Copyright (c) 2014 - 2016 The Regents of the University of
California (Regents). All Rights Reserved.  Redistribution and use in
source and binary forms, with or without modification, are permitted
provided that the following conditions are met:
   * Redistributions of source code must retain the above
     copyright notice, this list of conditions and the following
     two paragraphs of disclaimer.
   * Redistributions in binary form must reproduce the above
     copyright notice, this list of conditions and the following
     two paragraphs of disclaimer in the documentation and/or other materials
     provided with the distribution.
   * Neither the name of the Regents nor the names of its contributors
     may be used to endorse or promote products derived from this
     software without specific prior written permission.
IN NO EVENT SHALL REGENTS BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS,
ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
REGENTS HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE. THE SOFTWARE AND ACCOMPANYING DOCUMENTATION, IF
ANY, PROVIDED HEREUNDER IS PROVIDED "AS IS". REGENTS HAS NO OBLIGATION
TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
MODIFICATIONS.
*/

package firrtlTests

import java.io.StringWriter

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner

import firrtl.{Parser,Circuit}
import firrtl.{
   HighFirrtlCompiler,
   LowFirrtlCompiler,
   VerilogCompiler,
   Compiler
}
import firrtl.Annotations.AnnotationMap

/**
 * An example methodology for testing Firrtl compilers.
 *
 * Given an input Firrtl circuit (expressed as a string),
 * the compiler is executed. The output of the compiler
 * should be compared against the check string.
 */
abstract class CompilerSpec extends FlatSpec {
   def parse (s: String): Circuit = Parser.parse(s.split("\n").toIterator)
   val writer = new StringWriter()
   def compiler: Compiler
   def input: String
   def check: String
   def getOutput: String = {
      compiler.compile(parse(input), new AnnotationMap(Seq.empty), writer)
      writer.toString()
   }
}

/**
 * An example test for testing the HighFirrtlCompiler.
 *
 * Given an input Firrtl circuit (expressed as a string),
 * the compiler is executed. The output of the compiler
 * is parsed again and compared (in-memory) to the parsed
 * input.
 */
class HighFirrtlCompilerSpec extends CompilerSpec with Matchers {
   val compiler = new HighFirrtlCompiler()
   val input =
"""circuit Top :
  module Top :
    input a : UInt<1>[2]
    node x = a
"""
   val check = input
   "Any circuit" should "match exactly to its input" in {
      (parse(getOutput)) should be (parse(check))
   }
}

/**
 * An example test for testing the LoweringCompiler.
 *
 * Given an input Firrtl circuit (expressed as a string),
 * the compiler is executed. The output of the compiler is
 * a lowered version of the input circuit. The output is
 * string compared to the correct lowered circuit.
 */
class LowFirrtlCompilerSpec extends CompilerSpec with Matchers {
   val compiler = new LowFirrtlCompiler()
   val input =
"""
circuit Top :
  module Top :
    input a : UInt<1>[2]
    node x = a
"""
   val check = Seq(
      "circuit Top :",
      "  module Top :",
      "    input a_0 : UInt<1>",
      "    input a_1 : UInt<1>",
      "    node x_0 = a_0",
      "    node x_1 = a_1\n\n"
   ).reduce(_ + "\n" + _)
   "A circuit" should "match exactly to its lowered state" in {
      (parse(getOutput)) should be (parse(check))
   }
}

/**
 * An example test for testing the VerilogCompiler.
 *
 * Given an input Firrtl circuit (expressed as a string),
 * the compiler is executed. The output of the compiler is
 * the corresponding Verilog. The output is string compared
 * to the correct Verilog.
 */
class VerilogCompilerSpec extends CompilerSpec with Matchers {
   val compiler = new VerilogCompiler()
   val input =
"""
circuit Top :
  module Top :
    input a : UInt<1>[2]
    output b : UInt<1>[2]
    b <= a
"""
   val check = Seq(
      "module Top(",
      "  input   a_0,",
      "  input   a_1,",
      "  output  b_0,",
      "  output  b_1",
      ");",
      "  assign b_0 = a_0;",
      "  assign b_1 = a_1;",
      "endmodule\n"
   ).reduce(_ + "\n" + _)
   "A circuit's verilog output" should "match the given string" in {
      (getOutput) should be (check)
   }
}
