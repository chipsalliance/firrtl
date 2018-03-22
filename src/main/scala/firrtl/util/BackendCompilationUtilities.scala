// See LICENSE for license details.

package firrtl.util

import java.io._
import java.nio.file.Files
import java.text.SimpleDateFormat
import java.util.{Calendar, Locale}

import scala.sys.process.{ProcessBuilder, ProcessLogger, _}
import scala.io.Source._
import BackendCompilationUtilities._

object BackendCompilationUtilities {

  object OSVersion extends Enumeration {
    type OSVersion = Value
    val Unrecognized, Linux, MacOS, Unix, Windows = Value
  }
  import OSVersion._
  // The following is borrowed from com.sun.javafx.PlatformUtil to avoid having to install javafx on non-Oracle JVMs.
  //  import com.sun.javafx.PlatformUtil
  object PlatformUtil {
    val os: String = System.getProperty("os.name").toLowerCase(Locale.ENGLISH)
    def isWindows = os.startsWith("windows")
    def isLinux = os.startsWith("linux")
    def isMac = os.startsWith("mac")
    def isUnix = os.startsWith("unix") || os.startsWith("sunos")
  }
  val osVersion: OSVersion = {
    if (PlatformUtil.isWindows)
      Windows
    else if (PlatformUtil.isLinux)
      Linux
    else if (PlatformUtil.isMac)
      MacOS
    else if (PlatformUtil.isUnix)
      Unix
    else
      Unrecognized
  }
  val sharedLibraryExtension = osVersion match {
    case MacOS => "dylib"
    case Windows => "dll"
    case _ => ".so"
  }
}

trait BackendCompilationUtilities {
  /** Parent directory for tests */
  lazy val TestDirectory = new File("test_run_dir")

  def timeStamp: String = {
    val format = new SimpleDateFormat("yyyyMMddHHmmss")
    val now = Calendar.getInstance.getTime
    format.format(now)
  }

  /** Copy the contents of a resource to a destination file.
    */
  def copyResourceToFile(name: String, file: File) {
    val in = getClass.getResourceAsStream(name)
    if (in == null) {
      throw new FileNotFoundException(s"Resource '$name'")
    }
    val out = new FileOutputStream(file)
    Iterator.continually(in.read).takeWhile(-1 != _).foreach(out.write)
    out.close()
  }

  /** Create a test directory
    *
    * Will create outer directory called testName then inner directory based on
    * the current time
    */
  def createTestDirectory(testName: String): File = {
    val outer = new File(TestDirectory, testName)
    outer.mkdirs()
    Files.createTempDirectory(outer.toPath, timeStamp).toFile
  }

  def makeHarness(template: String => String, post: String)(f: File): File = {
    val prefix = f.toString.split("/").last
    val vf = new File(f.toString + post)
    val w = new FileWriter(vf)
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
    */
  def firrtlToVerilog(prefix: String, dir: File): ProcessBuilder = {
    Process(
      Seq("firrtl",
        "-i", s"$prefix.fir",
        "-o", s"$prefix.v",
        "-X", "verilog"),
      dir)
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
    * @param dutFile name of the DUT .v without the .v extension
    * @param dir output directory
    * @param vSources list of additional Verilog sources to compile
    * @param cppHarness C++ testharness to compile/link against
    */
  def verilogToCpp(
                    dutFile: String,
                    dir: File,
                    vSources: Seq[File],
                    cppHarness: File
                  ): ProcessBuilder = {
    val topModule = dutFile

    val blackBoxVerilogList = {
      val list_file = new File(dir, firrtl.transforms.BlackBoxSourceHelper.FileListName)
      if(list_file.exists()) {
        Seq("-f", list_file.getAbsolutePath)
      }
      else {
        Seq.empty[String]
      }
    }

    val command = Seq(
      "verilator",
      "--cc", s"${dir.getAbsolutePath}/$dutFile.v"
    ) ++
      blackBoxVerilogList ++
      vSources.flatMap(file => Seq("-v", file.getAbsolutePath)) ++
      Seq("--assert",
        "-Wno-fatal",
        "-Wno-WIDTH",
        "-Wno-STMTDLY",
        "--trace",
        "-O1",
        "--top-module", topModule,
        "+define+TOP_TYPE=V" + dutFile,
        s"+define+PRINTF_COND=!$topModule.reset",
        s"+define+STOP_COND=!$topModule.reset",
        "-CFLAGS",
        s"""-Wno-undefined-bool-conversion -O1 -DTOP_TYPE=V$dutFile -DVL_USER_FINISH -include V$dutFile.h""",
        "-Mdir", dir.getAbsolutePath,
        "--exe", cppHarness.getAbsolutePath)
    System.out.println(s"${command.mkString(" ")}") // scalastyle:ignore regex
    command
  }

  def cppToExe(prefix: String, dir: File): ProcessBuilder =
    Seq("make", "-C", dir.toString, "-j", "-f", s"V$prefix.mk", s"V$prefix")

  def cppToLib(prefix: String, dir: File, shimPieces: Seq[String], extraObjects: Seq[String] = Seq.empty): ProcessBuilder = {
    // Add a shared library rule to the make file.
    val inputMakefile = new File(dir, s"V$prefix.mk")
    val outputMakefile = new File(dir, s"V$prefix.so.mk")
    val out = new java.io.BufferedWriter( new java.io.FileWriter(outputMakefile))
    val makeBuffer = new collection.mutable.ArrayBuffer[String](2)
    val lineMatch = """^### Link rules\.\.\..*"""
    val exeLine = """^(\w+):(.*)""".r
    var collectLines = -1
    // Add ".o" to the shim pieces.
    val shimObjects = shimPieces.map(s => s + ".o")
    val shimDependencies = shimObjects.mkString(" ")
    val extraDependencies = extraObjects.mkString(" ")
    implicit class Regex(sc: StringContext) {
      def r = new scala.util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
    }
    for (line <- fromFile(inputMakefile).getLines()) {
      // Grab the first two lines following the '### Link rules... (from --exe)' line
      if (collectLines >= 0 && collectLines < 2 && !line.isEmpty) {
        makeBuffer += line
        collectLines += 1
      }
      if (line.matches(lineMatch)) {
        collectLines = 0
      }
      out.write(line + "\n")
    }
    out.write("")
    // Add lines to cover generic cpp compilation
    out.write("LIBS += -lc++\n")
    out.write(extraDependencies + ": %.o : %.cpp\n\techo $(PATH)\n\t$(CXX) -I$(JAVA_HOME)/include/darwin -I$(JAVA_HOME)/include $(CXXFLAGS) $(CPPFLAGS) $(OPT_FAST) -c -o $@ $<\n")
    makeBuffer(0) match { case r"""^(\w+)${target}:(.*)${dependencies}""" =>
        out.write(s"${target}.${sharedLibraryExtension}: ${dependencies} ${extraDependencies}\n")
    }
    makeBuffer(1) match { case r"""^(.+)${preLDFLAGS} \$$\(LDFLAGS\) (.*)${postLDFLAGS}""" =>
      out.write(s"${preLDFLAGS} -shared $$(LDFLAGS) ${postLDFLAGS}\n")
      out.write(s"${shimPieces.head}.${sharedLibraryExtension}: ${shimDependencies}\n")
      out.write(s"${preLDFLAGS} -shared $$(LDFLAGS) ${postLDFLAGS}\n")
    }
    out.close()
    Seq("make", "-C", dir.toString, "-j", "-f", outputMakefile.getName, s"${shimPieces.head}.${sharedLibraryExtension}", s"V${prefix}.${sharedLibraryExtension}")
  }

  def executeExpectingFailure(
                               prefix: String,
                               dir: File,
                               assertionMsg: String = ""): Boolean = {
    var triggered = false
    val assertionMessageSupplied = assertionMsg != ""
    val e = Process(s"./V$prefix", dir) !
      ProcessLogger(line => {
        triggered = triggered || (assertionMessageSupplied && line.contains(assertionMsg))
        System.out.println(line) // scalastyle:ignore regex
      })
    // Fail if a line contained an assertion or if we get a non-zero exit code
    //  or, we get a SIGABRT (assertion failure) and we didn't provide a specific assertion message
    triggered || (e != 0 && (e != 134 || !assertionMessageSupplied))
  }

  def executeExpectingSuccess(prefix: String, dir: File): Boolean = {
    !executeExpectingFailure(prefix, dir)
  }
}
