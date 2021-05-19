// SPDX-License-Identifier: Apache-2.0

package firrtl.util

import java.text.SimpleDateFormat
import java.util.Calendar
import logger.LazyLogging
import firrtl.FileUtils

import scala.sys.process.{ProcessBuilder, ProcessLogger, _}

object BackendCompilationUtilities extends LazyLogging {

  /** Parent directory for tests */
  // @todo remove java.io.File
  lazy val TestDirectory = new java.io.File("test_run_dir")

  def timeStamp: String = {
    val format = new SimpleDateFormat("yyyyMMddHHmmss")
    val now = Calendar.getInstance.getTime
    format.format(now)
  }

  def loggingProcessLogger: ProcessLogger =
    ProcessLogger(logger.info(_), logger.warn(_))

  /**
    * Copy the contents of a resource to a destination file.
    * @param name the name of the resource
    * @param file the file to write it into
    * @todo deprecate java.io.File
    */
  def copyResourceToFile(name: String, file: java.io.File): Unit = {
    val in = getClass.getResourceAsStream(name)
    if (in == null) {
      throw new java.io.FileNotFoundException(s"Resource '$name'")
    }
    // @todo remove java.io
    val out = new java.io.FileOutputStream(file)
    Iterator.continually(in.read).takeWhile(-1 != _).foreach(out.write)
    out.close()
  }

  /** Create a test directory
    *
    * Will create outer directory called testName then inner directory based on
    * the current time
    * @todo deprecate java.io.File
    */
  def createTestDirectory(testName: String): java.io.File = {
    val outer = new java.io.File(TestDirectory, testName)
    outer.mkdirs()
    // @todo remove java.io.File
    java.nio.file.Files.createTempDirectory(outer.toPath, timeStamp).toFile
  }

  // @todo deprecate java.io.File
  def makeHarness(template: String => String, post: String)(f: java.io.File): java.io.File = {
    val prefix = f.toString.split("/").last
    // @todo remove java.io.File
    val vf = new java.io.File(f.toString + post)
    // @todo remove java.io
    val w = new java.io.FileWriter(vf)
    w.write(template(prefix))
    w.close()
    vf
  }

  /**
    * compule chirrtl to verilog by using a separate process
    *
    * @param prefix basename of the file
    * @param dir    directory where file lives
    * @return       true if compiler completed successfully
    * @todo deprecate java.io.File
    */
  def firrtlToVerilog(prefix: String, dir: java.io.File): ProcessBuilder = {
    Process(Seq("firrtl", "-i", s"$prefix.fir", "-o", s"$prefix.v", "-X", "verilog"), dir)
  }

  /** Generates a Verilator invocation to convert Verilog sources to C++
    * simulation sources.
    *
    * The Verilator prefix will be V\$dutFile, and running this will generate
    * C++ sources and headers as well as a makefile to compile them.
    *
    * Verilator will automatically locate the top-level module as the one among
    * all the files which are not included elsewhere. If multiple ones exist,
    * the compilation will fail.
    *
    * If the file BlackBoxSourceHelper.fileListName (or an overridden .f resource filename that is
    * specified with the optional resourceFileName parameter) exists in the output directory,
    * it contains a list of source files to be included. Filter out any files in the vSources
    * sequence that are in this file so we don't include the same file multiple times.
    * This complication is an attempt to work-around the fact that clients used to have to
    * explicitly include additional Verilog sources. Now, more of that is automatic.
    *
    * @param dutFile name of the DUT .v without the .v extension
    * @param dir output directory
    * @param vSources list of additional Verilog sources to compile
    * @param cppHarness C++ testharness to compile/link against
    * @param suppressVcd specifies if VCD tracing should be suppressed
    * @param resourceFileName specifies what filename to look for to find a .f file
    * @param extraCmdLineArgs list of additional command line arguments
    */
  def verilogToCpp(
    dutFile: String,
    // @todo deprecate java.io.File
    dir: java.io.File,
    // @todo deprecate java.io.File
    vSources: Seq[java.io.File],
    // @todo deprecate java.io.File
    cppHarness:       java.io.File,
    suppressVcd:      Boolean = false,
    resourceFileName: String = firrtl.transforms.BlackBoxSourceHelper.defaultFileListName,
    extraCmdLineArgs: Seq[String] = Seq.empty
  ): ProcessBuilder = {

    val topModule = dutFile

    // @todo remove java.io.File
    val list_file = new java.io.File(dir, resourceFileName)
    val blackBoxVerilogList = {
      if (list_file.exists()) {
        Seq("-f", list_file.getAbsolutePath)
      } else {
        Seq.empty[String]
      }
    }

    // Don't include the same file multiple times.
    // If it's in the main .f resource file, don't explicitly include it on the command line.
    // Build a set of canonical file paths to use as a filter to exclude already included additional Verilog sources.
    val blackBoxHelperFiles: Set[String] = {
      if (list_file.exists()) {
        FileUtils.getLines(list_file).toSet
      } else {
        Set.empty
      }
    }
    val vSourcesFiltered = vSources.filterNot(f => blackBoxHelperFiles.contains(f.getCanonicalPath))
    val command = Seq(
      "verilator",
      "--cc",
      s"${dir.getAbsolutePath}/$dutFile.v"
    ) ++
      extraCmdLineArgs ++
      blackBoxVerilogList ++
      vSourcesFiltered.flatMap(file => Seq("-v", file.getCanonicalPath)) ++
      Seq("--assert", "-Wno-fatal", "-Wno-WIDTH", "-Wno-STMTDLY") ++ {
      if (suppressVcd) { Seq.empty }
      else { Seq("--trace") }
    } ++
      Seq(
        "-O1",
        "--top-module",
        topModule,
        "+define+TOP_TYPE=V" + dutFile,
        s"+define+PRINTF_COND=!$topModule.reset",
        s"+define+STOP_COND=!$topModule.reset",
        "-CFLAGS",
        s"""-Wno-undefined-bool-conversion -O1 -DTOP_TYPE=V$dutFile -DVL_USER_FINISH -include V$dutFile.h""",
        "-Mdir",
        dir.getAbsolutePath,
        "--exe",
        cppHarness.getAbsolutePath
      )
    logger.info(s"${command.mkString(" ")}")
    command
  }

  // @todo deprecate java.io.File
  def cppToExe(prefix: String, dir: java.io.File): ProcessBuilder =
    Seq("make", "-C", dir.toString, "-j", "-f", s"V$prefix.mk", s"V$prefix")

  def executeExpectingFailure(
    prefix: String,
    // @todo deprecate java.io.File
    dir:          java.io.File,
    assertionMsg: String = ""
  ): Boolean = {
    var triggered = false
    val assertionMessageSupplied = assertionMsg != ""
    val e = Process(s"./V$prefix", dir) !
      ProcessLogger(
        line => {
          triggered = triggered || (assertionMessageSupplied && line.contains(assertionMsg))
          logger.info(line)
        },
        logger.warn(_)
      )
    // Fail if a line contained an assertion or if we get a non-zero exit code
    //  or, we get a SIGABRT (assertion failure) and we didn't provide a specific assertion message
    triggered || (e != 0 && (e != 134 || !assertionMessageSupplied))
  }

  // @todo deprecate java.io.File
  def executeExpectingSuccess(prefix: String, dir: java.io.File): Boolean = {
    !executeExpectingFailure(prefix, dir)
  }

  /** Creates and runs a Yosys script that creates and runs SAT on a miter
    * circuit. Returns true if SAT succeeds, false otherwise
    *
    * The custom and reference Verilog files must not contain any modules with
    * the same name otherwise Yosys will not be able to create a miter circuit
    *
    * @param customTop    name of the DUT with custom transforms without the .v
    *                     extension
    * @param referenceTop name of the DUT without custom transforms without the
    *                     .v extension
    * @param testDir      directory containing verilog files
    * @param timesteps    the maximum number of timesteps for Yosys equivalence
    *                     checking to consider
    * @todo deprecate java.io.File
    */
  def yosysExpectSuccess(
    customTop:    String,
    referenceTop: String,
    testDir:      java.io.File,
    timesteps:    Int = 1
  ): Boolean = {
    !yosysExpectFailure(customTop, referenceTop, testDir, timesteps)
  }

  /** Creates and runs a Yosys script that creates and runs SAT on a miter
    * circuit. Returns false if SAT succeeds, true otherwise
    *
    * The custom and reference Verilog files must not contain any modules with
    * the same name otherwise Yosys will not be able to create a miter circuit
    *
    * @param customTop    name of the DUT with custom transforms without the .v
    *                     extension
    * @param referenceTop name of the DUT without custom transforms without the
    *                     .v extension
    * @param testDir      directory containing verilog files
    * @param timesteps    the maximum number of timesteps for Yosys equivalence
    *                     checking to consider
    * @todo deprecate java.io.File
    */
  def yosysExpectFailure(
    customTop:    String,
    referenceTop: String,
    testDir:      java.io.File,
    timesteps:    Int = 1
  ): Boolean = {

    val scriptFileName = s"${testDir.getAbsolutePath}/yosys_script"
    // @todo remove java.io
    val yosysScriptWriter = new java.io.PrintWriter(scriptFileName)
    yosysScriptWriter.write(s"""read_verilog ${testDir.getAbsolutePath}/$customTop.v
                               |prep -flatten -top $customTop; proc; opt; memory
                               |design -stash custom
                               |read_verilog ${testDir.getAbsolutePath}/$referenceTop.v
                               |prep -flatten -top $referenceTop; proc; opt; memory
                               |design -stash reference
                               |design -copy-from custom -as custom $customTop
                               |design -copy-from reference -as reference $referenceTop
                               |equiv_make custom reference equiv
                               |hierarchy -top equiv
                               |prep -flatten -top equiv
                               |clean -purge
                               |equiv_simple -seq $timesteps
                               |equiv_induct -seq $timesteps
                               |equiv_status -assert
         """.stripMargin)
    yosysScriptWriter.close()

    val resultFileName = testDir.getAbsolutePath + "/yosys_results"
    // @todo remove java.io.File
    val command = s"yosys -s $scriptFileName" #> new java.io.File(resultFileName)
    command.! != 0
  }
}

@deprecated("use object BackendCompilationUtilities", "FIRRTL 1.3")
trait BackendCompilationUtilities extends LazyLogging {
  lazy val TestDirectory = BackendCompilationUtilities.TestDirectory
  def timeStamp:            String = BackendCompilationUtilities.timeStamp
  def loggingProcessLogger: ProcessLogger = BackendCompilationUtilities.loggingProcessLogger
  // @todo deprecate java.io.File
  def copyResourceToFile(name: String, file: java.io.File): Unit =
    BackendCompilationUtilities.copyResourceToFile(name, file)
  // @todo deprecate java.io.File
  def createTestDirectory(testName: String): java.io.File = BackendCompilationUtilities.createTestDirectory(testName)
  // @todo deprecate java.io.File
  def makeHarness(template: String => String, post: String)(f: java.io.File): java.io.File =
    BackendCompilationUtilities.makeHarness(template, post)(f)
  // @todo deprecate java.io.File
  def firrtlToVerilog(prefix: String, dir: java.io.File): ProcessBuilder =
    BackendCompilationUtilities.firrtlToVerilog(prefix, dir)
  def verilogToCpp(
    dutFile: String,
    // @todo deprecate java.io.File
    dir: java.io.File,
    // @todo deprecate java.io.File
    vSources: Seq[java.io.File],
    // @todo deprecate java.io.File
    cppHarness:       java.io.File,
    suppressVcd:      Boolean = false,
    resourceFileName: String = firrtl.transforms.BlackBoxSourceHelper.defaultFileListName
  ): ProcessBuilder = {
    BackendCompilationUtilities.verilogToCpp(dutFile, dir, vSources, cppHarness, suppressVcd, resourceFileName)
  }
  // @todo deprecate java.io.File
  def cppToExe(prefix: String, dir: java.io.File): ProcessBuilder = BackendCompilationUtilities.cppToExe(prefix, dir)
  def executeExpectingFailure(
    prefix: String,
    // @todo deprecate java.io.File
    dir:          java.io.File,
    assertionMsg: String = ""
  ): Boolean = {
    BackendCompilationUtilities.executeExpectingFailure(prefix, dir, assertionMsg)
  }
  // @todo deprecate java.io.File
  def executeExpectingSuccess(prefix: String, dir: java.io.File): Boolean =
    BackendCompilationUtilities.executeExpectingSuccess(prefix, dir)
}
