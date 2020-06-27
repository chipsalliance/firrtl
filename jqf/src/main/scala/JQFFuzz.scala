package firrtl.fuzzer

import java.io.{File, FileNotFoundException, IOException, PrintStream}
import java.net.{MalformedURLException, URLClassLoader}
import java.time.Duration
import java.time.format.DateTimeParseException

import edu.berkeley.cs.jqf.fuzz.ei.ExecutionIndexingGuidance
import edu.berkeley.cs.jqf.fuzz.ei.ZestGuidance
import edu.berkeley.cs.jqf.fuzz.junit.GuidedFuzzing
import edu.berkeley.cs.jqf.instrument.InstrumentingClassLoader


case class JQFException(message: String) extends Exception(message)

sealed trait JQFEngine
case object Zeal extends JQFEngine
case object Zest extends JQFEngine

case class JQFFuzzOptions(
  // required
  classpathElements: Seq[String] = null,
  outputDirectory: File = null,
  testClassName: String = null,
  testMethod: String = null,

  excludes: Seq[String] = Seq.empty,
  includes: Seq[String] = Seq.empty,
  time: Option[String] = None,
  blind: Boolean = false,
  engine: JQFEngine = Zest,
  disableCoverage: Boolean = false,
  inputDirectory: Option[File] = None,
  saveAll: Boolean = false,
  libFuzzerCompatOutput: Boolean = false,
  quiet: Boolean = false,
  exitOnCrash: Boolean = false,
  runTimeout: Option[Int] = None
)

object JQFFuzz {
  final def main(args: Array[String]): Unit = {
    val parser = new scopt.OptionParser[JQFFuzzOptions]("JQF-plugin") {
        opt[String]("classpathElements")
          .required()
          .unbounded()
          .action((x, c) => c.copy(classpathElements = x.split(":")))
        opt[File]("outputDirectory")
          .required()
          .unbounded()
          .action((x, c) => c.copy(outputDirectory = x))
        opt[String]("testClassName")
          .required()
          .unbounded()
          .action((x, c) => c.copy(testClassName = x))
        opt[String]("testMethod")
          .required()
          .unbounded()
          .action((x, c) => c.copy(testMethod = x))

        opt[Seq[String]]("excludes")
          .unbounded()
          .action((x, c) => c.copy(excludes = x))
        opt[Seq[String]]("includes")
          .unbounded()
          .action((x, c) => c.copy(includes = x))
        opt[String]("time")
          .unbounded()
          .action((x, c) => c.copy(time = Some(x)))
        opt[Boolean]("blind")
          .unbounded()
          .action((x, c) => c.copy(blind = x))
        opt[String]("engine")
          .unbounded()
          .action((x, c) => x match {
            case "zest" => c.copy(engine = Zest)
            case "zeal" => c.copy(engine = Zeal)
            case _ =>
              throw new JQFException(s"bad a value '$x' for --engine, must be zest|zeal")
          })
        opt[Boolean]("disableCoverage")
          .unbounded()
          .action((x, c) => c.copy(disableCoverage = x))
        opt[File]("inputDirectory")
          .unbounded()
          .action((x, c) => c.copy(inputDirectory = Some(x)))
        opt[Boolean]("saveAll")
          .unbounded()
          .action((x, c) => c.copy(saveAll = x))
        opt[Boolean]("libFuzzerCompatOutput")
          .unbounded()
          .action((x, c) => c.copy(libFuzzerCompatOutput = x))
        opt[Boolean]("quiet")
          .unbounded()
          .action((x, c) => c.copy(quiet = x))
        opt[Boolean]("exitOnCrash")
          .unbounded()
          .action((x, c) => c.copy(exitOnCrash = x))
        opt[Int]("runTimeout")
          .unbounded()
          .action((x, c) => c.copy(runTimeout = Some(x)))
    }

    parser.parse(args, JQFFuzzOptions()) match {
      case Some(opts) => execute(opts)
      case _ => System.exit(1)
    }
  }

  def execute(opts: JQFFuzzOptions): Unit = {
    // Configure classes to instrument
    if (opts.excludes.nonEmpty) {
      System.setProperty("janala.excludes", opts.excludes.mkString(","))
    }
    if (opts.includes.nonEmpty) {
      System.setProperty("janala.includes", opts.includes.mkString(","))
    }

    // Configure Zest Guidance
    if (opts.saveAll) {
      System.setProperty("jqf.ei.SAVE_ALL_INPUTS", "true")
    }
    if (opts.libFuzzerCompatOutput) {
      System.setProperty("jqf.ei.LIBFUZZER_COMPAT_OUTPUT", "true")
    }
    if (opts.quiet) {
      System.setProperty("jqf.ei.QUIET_MODE", "true")
    }
    if (opts.exitOnCrash) {
      System.setProperty("jqf.ei.EXIT_ON_CRASH", "true")
    }
    if (opts.runTimeout.isDefined) {
      System.setProperty("jqf.ei.TIMEOUT", opts.runTimeout.get.toString)
    }

    val duration = opts.time.map { time =>
      try {
        Duration.parse("PT" + time);
      } catch {
        case e: DateTimeParseException =>
          throw new JQFException("Invalid time duration: " + time)
      }
    }.getOrElse(null)

    val loader = try {
      val classpathElements = opts.classpathElements.toArray
      if (opts.disableCoverage) {
        new URLClassLoader(
          classpathElements.map(cpe => new File(cpe).toURI().toURL()),
          getClass().getClassLoader())
      } else {
        new InstrumentingClassLoader(
          classpathElements,
          getClass().getClassLoader())
      }
    } catch {
      case e: MalformedURLException =>
        throw new JQFException("Could not get project classpath")
    }

    val guidance = try {
      val resultsDir = opts.outputDirectory
      val targetName = opts.testClassName + "#" + opts.testMethod
      val seedsDirOpt = opts.inputDirectory
      val guidance = (opts.engine, seedsDirOpt) match {
        case (Zest, Some(seedsDir)) =>
          new ZestGuidance(targetName, duration, resultsDir, seedsDir)
        case (Zest, None) =>
          new ZestGuidance(targetName, duration, resultsDir)
        case (Zeal, Some(seedsDir)) =>
          new ExecutionIndexingGuidance(targetName, duration, resultsDir, seedsDir)
        case (Zeal, None) =>
          throw new JQFException("--inputDirectory required when using zeal engine")
      }
      guidance.setBlind(opts.blind)
      guidance
    } catch {
      case e: FileNotFoundException =>
        throw new JQFException("File not found")
      case e: IOException =>
        throw new JQFException("I/O error")
    }

    val result = try {
      GuidedFuzzing.run(opts.testClassName, opts.testMethod, loader, guidance, System.out)
    } catch {
      case e: ClassNotFoundException =>
        throw new JQFException("could not load test class")
      case e: IllegalArgumentException =>
        throw new JQFException("Bad request")
      case e: RuntimeException =>
        throw new JQFException("Internal error")
    }

    if (!result.wasSuccessful()) {
      throw new JQFException(
        "Fuzzing revealed errors. Use mvn jqf:repro to reproduce failing test case.")
    }
  }
}
