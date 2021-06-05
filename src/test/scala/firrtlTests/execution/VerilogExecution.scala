// SPDX-License-Identifier: Apache-2.0

package firrtlTests.execution

import firrtl._
import firrtl.ir._

import firrtl.stage.{FirrtlCircuitAnnotation, FirrtlStage}
import firrtl.options.TargetDirAnnotation

/**
  * Mixing in this trait causes a SimpleExecutionTest to be run in Verilog simulation.
  */
trait VerilogExecution extends TestExecution {
  this: SimpleExecutionTest =>

  /** can be overwritten to mix-in custom annotations */
  val customAnnotations: AnnotationSeq = Seq()

  // @todo deprecate java.io.File
  def runEmittedDUT(c: Circuit, testDir: java.io.File): Unit = {
    // Run FIRRTL, emit Verilog file
    val cAnno = FirrtlCircuitAnnotation(c)
    val tdAnno = TargetDirAnnotation(testDir.getAbsolutePath)

    (new FirrtlStage).execute(Array.empty, AnnotationSeq(Seq(cAnno, tdAnno)) ++ customAnnotations)

    // Copy harness resource to test directory
    // @todo remove java.io.File
    val harness = new java.io.File(testDir, s"top.cpp")
    copyResourceToFile(cppHarnessResourceName, harness)

    // Make and run Verilog simulation
    verilogToCpp(c.main, testDir, Nil, harness) #&&
      cppToExe(c.main, testDir) ! loggingProcessLogger
    assert(executeExpectingSuccess(c.main, testDir))
  }
}
