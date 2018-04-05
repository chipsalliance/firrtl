// See LICENSE for license details.

package firrtl.util

import java.io._
import java.nio.file.{Files, Path, Paths}
import java.text.SimpleDateFormat
import java.util.{Calendar, Locale}

import scala.sys.process.{ProcessBuilder, ProcessLogger, _}
import scala.io.Source._
import BackendCompilationUtilities._

object BackendCompilationUtilities {

  object OSVersion extends Enumeration {
    type OSVersion = Value
    val Unrecognized, Linux, MacOS, Unix, Windows, SunOS = Value
  }
  import OSVersion._
  // The following is borrowed from com.sun.javafx.PlatformUtil to avoid having to install javafx on non-Oracle JVMs.
  //  import com.sun.javafx.PlatformUtil
  object PlatformUtil {
    val os: String = System.getProperty("os.name").toLowerCase(Locale.ENGLISH)
    def isWindows: Boolean = os.startsWith("windows")
    def isLinux: Boolean = os.startsWith("linux")
    def isMac: Boolean = os.startsWith("mac")
    def isSun: Boolean = os.startsWith("sunos")
    def isUnix: Boolean = os.startsWith("unix")
  }
  val osVersion: OSVersion = {
    if (PlatformUtil.isWindows)
      Windows
    else if (PlatformUtil.isLinux)
      Linux
    else if (PlatformUtil.isMac)
      MacOS
    else if (PlatformUtil.isSun)
      SunOS
    else if (PlatformUtil.isUnix)
      Unix
    else
      Unrecognized
  }
  /** The file extension for shared libraries.
    *
    */
  val sharedLibraryExtension: String = osVersion match {
    case MacOS => "dylib"
    case Windows => "dll"
    case _ => "so"
  }
  /** Extra CFLAGS required to generate a shared library.
    *
    */
  val sharedLibraryFlags: String = osVersion match {
    case MacOS => "-shared"
    case Linux => "-fPIC -shared"
      // We don't know what the rest require at the moment.
    case _ => "-shared"
  }

  /** Extra libraries to be loaded with a shared library.
    *
    */
  val sharedLibraryLibraries: String = osVersion match {
    case _ => ""
  }
  /**
    * The include paths required to compile JNI C code.
    * This can be turned into C/C++ flags via something like:
    * includePathsJNI.map("-I" + _).mkString(" ")
    * @return Seq[String]
    */
  lazy val includePathsJNI: Seq[String] = {
    val javaHome = System.getProperty("java.home")
    // Find the OS-specific sub-directory (and any others)
    val includeDir = new File(new File(javaHome).getParentFile, "include")
    val subDirs = if (includeDir.exists() && includeDir.isDirectory) {
      includeDir.listFiles.filter(_.isDirectory).map(f => getPosixCompatibleAbsolutePath(f.getAbsolutePath)).toSeq
    } else {
      Seq[String]()
    }
    // Sort any sub-directories so the OS-specific one comes first.
    val subDirOS = osVersion match {
      case Windows => "windows"
      case Linux => "linux"
      case MacOS => "darwin"
      case SunOS => "sunos"
      case _ => ""
    }
    def sortByOSSubDir(a: String, b: String): Boolean = {
      if (a.endsWith(subDirOS))
        true
      else if (b.endsWith(subDirOS))
        false
      else
        a.toString <= b.toString
    }
    Seq(getPosixCompatibleAbsolutePath(includeDir.getAbsolutePath)) ++ (subDirs sortWith sortByOSSubDir)
  }

  /** A function to generate Posix compatible paths.
    * @param path - a string possibly containing Windows path delimiters,
    * @return - a string with '/' delimiters.
    */
  def getPosixCompatibleAbsolutePath(path: String): String = {
    if (osVersion == Windows)
      path.replace('\\', '/')
    else
      path
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
        Seq("-f", getPosixCompatibleAbsolutePath(list_file.getAbsolutePath))
      }
      else {
        Seq.empty[String]
      }
    }

    // On Windows, we need to quote multi-word arguments. This is handled by exec (and isn't required) on UNIX-like systems.
    val quoteDelimiter = if (osVersion == OSVersion.Windows) "'" else ""
    val command = Seq(
      "verilator",
      "--cc", s"${getPosixCompatibleAbsolutePath(dir.getAbsolutePath)}/$dutFile.v"
    ) ++
      blackBoxVerilogList ++
      vSources.flatMap(file => Seq("-v", getPosixCompatibleAbsolutePath(file.getAbsolutePath))) ++
      Seq("--assert",
        "-Wno-fatal",
        "-Wno-WIDTH",
        "-Wno-STMTDLY",
        "--trace",
        "-O1",
        "--top-module", topModule,
        "+define+TOP_TYPE=V" + dutFile,
        s"+define+PRINTF_COND='!$topModule.reset'",
        s"+define+STOP_COND='!$topModule.reset'",
        "-CFLAGS",
        s"""${quoteDelimiter}-Wno-undefined-bool-conversion -O1 -DTOP_TYPE=V$dutFile -DVL_USER_FINISH -include V$dutFile.h${quoteDelimiter}""",
        "-Mdir", getPosixCompatibleAbsolutePath(dir.getAbsolutePath),
        "--exe", getPosixCompatibleAbsolutePath(cppHarness.getAbsolutePath))

    val commandString = command.mkString(" ")
    System.out.println(s"${commandString}") // scalastyle:ignore regex
    // If we're on Windows, put this in a script and return the command required to invoke it.
    // In principle, we should be able to invoke bash directly with the rest of the command as arguments,
    //  but we seem to lose most of our environment variables (notably PATH) if we don't go through this
    //  two-step dance.
    if (osVersion == OSVersion.Windows) {
      val bashFile = new File(dir, s"${dutFile}.sh")
      val bashWriter = new FileWriter(bashFile)
    	bashWriter.write(commandString)
    	bashWriter.close()
    	Seq("cmd", "/c", "\"", "bash", getPosixCompatibleAbsolutePath(bashFile.getPath), "\"")
    } else {
      command
    }
  }

  def cppToExe(prefix: String, dir: File): ProcessBuilder = {
    val commandSeq = Seq("make", "-C", dir.toString, "-j", "-f", s"V$prefix.mk", s"V$prefix")
    // If we're on Windows, put this in a script and return the command required to invoke it.
    // In principle, we should be able to invoke bash directly with the rest of the command as arguments,
    //  but we seem to lose most of our environment variables (notably PATH) if we don't go through this
    //  two-step dance.
    if (osVersion == OSVersion.Windows) {
      val commandString = commandSeq.mkString(" ")
      val bashFile = new File(dir, s"M${prefix}.mk.sh")
      val bashWriter = new FileWriter(bashFile)
	    bashWriter.write(commandString)
	    bashWriter.close()
	    Seq("cmd", "/c", "\"", "bash", getPosixCompatibleAbsolutePath(bashFile.getPath), "\"")
    } else {
      commandSeq
    }
  }

  /** system/shell commands to build a shared library suitable for run-time loading.
    * We assume verilator has already run and generated a <prefix>.mk file.
    * We'll copy and edit it to add rules for building a shared library.
    * We'll use JNI to access the code in the library.
    * Since JAVA has no mechanism to unload JNI code, we'll actually do this in two parts:
    *  - load the shim JNI/C++ code which is dut-independent and will persist for this Java session,
    *  - load the dut-specific C++ code using the system level dlopen()/dlclose() API (with suitable wrappers on Windows)
    * This allows us to unload the dut-specific code once we've finished testing it.
    * @param prefix - this is typically the dut name and will be used to prefix generated files
    * @param dir - the directory in which to generate files and run make
    * @param shimPieces - source files (without a .c or .cpp or .c++ extension) required for the JNI/C++/dlopen shim
    * @param extraObjects - any extra object files required for the shared library.
    * @return - a ProcessBuilder object suitable for constructing the shared library.
    */
  def cppToLib(prefix: String, dir: File, shimPieces: Seq[String], extraObjects: Seq[String] = Seq.empty): ProcessBuilder = {
    // Add a shared library rule to a copy of the Makefile.
    val inputMakefile = new File(dir, s"V$prefix.mk")
    val outputMakefile = new File(dir, s"V$prefix.so.mk")
    val out = new java.io.BufferedWriter( new java.io.FileWriter(outputMakefile))
    val makeBuffer = new collection.mutable.ArrayBuffer[String](2)
    // The following depends on the format of Verilator .mk files.
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
    val JNI_CPPFLAGS = includePathsJNI.map("-I\"" + _ + "\"").mkString(" ")
    out.write(s"LIBS += ${sharedLibraryLibraries}\n")
    out.write(s"CXXFLAGS += ${JNI_CPPFLAGS} ${sharedLibraryFlags}\n")
    out.write(extraDependencies + ": %.o : %.cpp\n\techo $(PATH)\n\t$(CXX) $(CXXFLAGS) $(CPPFLAGS) $(OPT_FAST) -c -o $@ $<\n")

    // Add lines to build the shared libary and the JNI/C++/dlopen shim.
    makeBuffer(0) match { case r"""^(\w+)${target}:(.*)${dependencies}""" =>
        out.write(s"${target}.${sharedLibraryExtension}: ${dependencies} ${extraDependencies}\n")
    }
    makeBuffer(1) match { case r"""^(.+)${preLDFLAGS} \$$\(LDFLAGS\) (.*)${postLDFLAGS}""" =>
      out.write(s"${preLDFLAGS} ${sharedLibraryFlags} $$(LDFLAGS) ${postLDFLAGS}\n")
      out.write(s"${shimPieces.head}.${sharedLibraryExtension}: ${shimDependencies}\n")
      out.write(s"${preLDFLAGS} ${sharedLibraryFlags} $$(LDFLAGS) ${postLDFLAGS}\n")
    }
    out.close()
    // Ensure .o files are recompiled with the correct flags by deleting any existing ones.
    val objFiles = new File(dir.toString).listFiles.filter(_.getName.endsWith(".o"))
    for (file <- objFiles) {
      file.delete()
    }
    val commandSeq = Seq("make", "-C", dir.toString, "-j", "-f", outputMakefile.getName, s"${shimPieces.head}.${sharedLibraryExtension}", s"V${prefix}.${sharedLibraryExtension}")
    if (osVersion == OSVersion.Windows) {
      // If we're on Windows, put this in a script and return the command required to invoke it.
      // In principle, we should be able to invoke bash directly with the rest of the command as arguments,
      //  but we seem to lose most of our environment variables (notably PATH) if we don't go through this
      //  two-step dance.
      val commandString = commandSeq.mkString(" ")
      val bashFile = new File(dir, s"${outputMakefile.getName}.make.sh")
      val bashWriter = new FileWriter(bashFile)
	    bashWriter.write(commandString)
	    bashWriter.close()
	    Seq("cmd", "/c", "\"", "bash", getPosixCompatibleAbsolutePath(bashFile.getPath), "\"")
    } else {
      commandSeq
    }
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
