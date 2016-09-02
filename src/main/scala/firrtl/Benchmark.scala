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

package firrtl

import firrtl.ir.Circuit

object Benchmark extends App {

  val tests = args

  // Modified from Utils.time
  private def time[R](block: => R): Double = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    val timeMillis = (t1 - t0) / 1000000.0
    timeMillis
  }
  private def average(xs: Seq[Double]) = xs.sum / xs.length
  private def median(xs: Seq[Double]) = {
    require(xs.length % 2 == 1, "Must be odd!")
    xs(xs.length / 2)
  }

  private def getCircuit(path: String): Circuit = {
    val lines = io.Source.fromFile(path).getLines()
    firrtl.Parser.parse(lines)
  }
  private val compiler = new firrtl.VerilogCompiler
  private val annotations = new firrtl.Annotations.AnnotationMap(List.empty)
  private def compile(circuit: Circuit): Unit = {
    compiler.compile(circuit, annotations, new java.io.StringWriter)
  }

  for (test <- tests) {
    println(s"Running $test")
    val circuit = getCircuit(test)
    for (i <- 1 to 5) {
      val t = time { compile(circuit) }
      println(s"Warmup run $i took $t ms to compile")
    }
    val times = (1 to 5) map { i =>
      val t = time { compile(circuit) }
      println(s"Real run $i took $t ms to compile")
      t 
    }
    println(s"  average = ${average(times)}")
    println(s"  median = ${median(times)}")
  }
}
