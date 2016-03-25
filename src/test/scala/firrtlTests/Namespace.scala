
package firrtlTests

import firrtl._

import java.io._

class NameExpansionSpec extends FirrtlPropSpec {

  property("Bundles subfields should expand with $") {
    val prefix = "NameExpansion"
    val testDir = compileFirrtlTest(prefix, "/features")
    val harness = new File(testDir, s"top.cpp")
    copyResourceToFile(cppHarness.toString, harness)

    verilogToCpp(prefix, testDir, Seq(), harness).!
    cppToExe(prefix, testDir).!
    assert(executeExpectingSuccess(prefix, testDir))
  }
}
