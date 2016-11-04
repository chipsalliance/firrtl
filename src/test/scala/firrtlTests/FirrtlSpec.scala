// See LICENSE for license details.

package firrtlTests

import java.io._

import com.typesafe.scalalogging.LazyLogging
import scala.sys.process._
import org.scalatest._
import org.scalatest.prop._
import scala.io.Source

import firrtl._
import firrtl.Parser.IgnoreInfo
import firrtl.Annotations.AnnotationMap

// This trait is borrowed from Chisel3, ideally this code should only exist in one location
trait BackendCompilationUtilities {
  /** Create a temporary directory with the prefix name. Exists here because it doesn't in Java 6.
    */
  def createTempDirectory(prefix: String): File = {
    val temp = File.createTempFile(prefix, "")
    if (!temp.delete()) {
      throw new IOException(s"Unable to delete temp file '$temp'")
    }
    if (!temp.mkdir()) {
      throw new IOException(s"Unable to create temp directory '$temp'")
    }
    temp
  }

  /** Copy the contents of a resource to a destination file.
    */
  def copyResourceToFile(name: String, file: File) {
    val in = getClass().getResourceAsStream(name)
    if (in == null) {
      throw new FileNotFoundException(s"Resource '$name'")
    }
    val out = new FileOutputStream(file)
    Iterator.continually(in.read).takeWhile(-1 !=).foreach(out.write)
    out.close()
  }


  def makeHarness(template: String => String, post: String)(f: File): File = {
    val prefix = f.toString.split("/").last
    val vf = new File(f.toString + post)
    val w = new FileWriter(vf)
    w.write(template(prefix))
    w.close()
    vf
  }

  /** Generates a Verilator invocation to convert Verilog sources to C++
    * simulation sources.
    *
    * The Verilator prefix will be V$dutFile, and running this will generate
    * C++ sources and headers as well as a makefile to compile them.
    *
    * Verilator will automatically locate the top-level module as the one among
    * all the files which are not included elsewhere. If multiple ones exist,
    * the compilation will fail.
    *
    * @param dutFile name of the DUT .v without the .v extension
    * @param dir output directory
    * @param vSources list of additional Verilog sources to compile
    * @param cppHarness C++ testharness to compile/link against
    */
  def verilogToCpp(
      dutFile: String,
      dir: File,
      vSources: Seq[File],
      cppHarness: File): ProcessBuilder =

    Seq("verilator",
        "--cc", s"$dutFile.v") ++
        vSources.map(file => Seq("-v", file.toString)).flatten ++
        Seq("--assert",
            "--Wno-fatal",
            "--trace",
            "-O2",
            "--top-module", dutFile,
            "+define+TOP_TYPE=V" + dutFile,
            "-CFLAGS", s"""-Wno-undefined-bool-conversion -O2 -DTOP_TYPE=V$dutFile -include V$dutFile.h""",
            "-Mdir", dir.toString,
            "--exe", cppHarness.toString)

  def cppToExe(prefix: String, dir: File): ProcessBuilder =
    Seq("make", "-C", dir.toString, "-j", "-f", s"V${prefix}.mk", s"V${prefix}")

  def executeExpectingFailure(
      prefix: String,
      dir: File,
      assertionMsg: String = "Assertion failed"): Boolean = {
    var triggered = false
    val e = Process(s"./V${prefix}", dir) !
      ProcessLogger(line => {
        triggered = triggered || line.contains(assertionMsg)
        System.out.println(line)
      })
    triggered
  }

  def executeExpectingSuccess(prefix: String, dir: File): Boolean = {
    !executeExpectingFailure(prefix, dir)
  }
}

trait FirrtlRunners extends BackendCompilationUtilities {
  def parse(str: String) = Parser.parse(str.split("\n").toIterator, IgnoreInfo)
  lazy val cppHarness = new File(s"/top.cpp")
  /** Compile a Firrtl file
    *
    * @param prefix is the name of the Firrtl file without path or file extension
    * @param srcDir directory where all Resources for this test are located
    * @param annotations Optional Firrtl annotations
    */
  def compileFirrtlTest(
      prefix: String,
      srcDir: String,
      customTransforms: Seq[Transform] = Seq.empty,
      annotations: AnnotationMap = new AnnotationMap(Seq.empty)): File = {
    val testDir = createTempDirectory(prefix)
    copyResourceToFile(s"${srcDir}/${prefix}.fir", new File(testDir, s"${prefix}.fir"))

    Driver.compile(
      s"$testDir/$prefix.fir",
      s"$testDir/$prefix.v",
      new VerilogCompiler(),
      Parser.IgnoreInfo,
      customTransforms,
      annotations)
    testDir
  }
  /** Execute a Firrtl Test
    *
    * @param prefix is the name of the Firrtl file without path or file extension
    * @param srcDir directory where all Resources for this test are located
    * @param verilogPrefixes names of option Verilog resources without path or file extension
    * @param annotations Optional Firrtl annotations
    */
  def runFirrtlTest(
      prefix: String,
      srcDir: String,
      verilogPrefixes: Seq[String] = Seq.empty,
      customTransforms: Seq[Transform] = Seq.empty,
      annotations: AnnotationMap = new AnnotationMap(Seq.empty)) = {
    val testDir = compileFirrtlTest(prefix, srcDir, customTransforms, annotations)
    val harness = new File(testDir, s"top.cpp")
    copyResourceToFile(cppHarness.toString, harness)

    // Note file copying side effect
    val verilogFiles = verilogPrefixes map { vprefix =>
      val file = new File(testDir, s"$vprefix.v")
      copyResourceToFile(s"$srcDir/$vprefix.v", file)
      file
    }

    verilogToCpp(prefix, testDir, verilogFiles, harness).!
    cppToExe(prefix, testDir).!
    assert(executeExpectingSuccess(prefix, testDir))
  }
}

trait FirrtlMatchers {
  // Replace all whitespace with a single space and remove leading and
  //   trailing whitespace
  // Note this is intended for single-line strings, no newlines
  def normalized(s: String): String = {
    require(!s.contains("\n"))
    s.replaceAll("\\s+", " ").trim
  }
}

abstract class FirrtlPropSpec extends PropSpec with PropertyChecks with FirrtlRunners with LazyLogging

abstract class FirrtlFlatSpec extends FlatSpec with Matchers with FirrtlRunners with FirrtlMatchers with LazyLogging

/** Super class for execution driven Firrtl tests */
abstract class ExecutionTest(name: String, dir: String, vFiles: Seq[String] = Seq.empty) extends FirrtlPropSpec {
  property(s"$name should execute correctly") {
    runFirrtlTest(name, dir, vFiles)
  }
}
/** Super class for compilation driven Firrtl tests */
abstract class CompilationTest(name: String, dir: String) extends FirrtlPropSpec {
  property(s"$name should compile correctly") {
    compileFirrtlTest(name, dir)
  }
}


