// See LICENSE for license details.

package firrtlTests

import java.io.{File, FileInputStream, FileWriter}

import org.scalatest.{FreeSpec, Matchers}
import firrtl.passes.{InlineAnnotation, InlineInstances}
import firrtl.passes.memlib.{InferReadWrite, InferReadWriteAnnotation, ReplSeqMem, ReplSeqMemAnnotation}
import firrtl.transforms.BlackBoxTargetDirAnno
import firrtl._
import firrtl.annotations._
import firrtl.util.BackendCompilationUtilities
import firrtl.options.ExecutionOptionsManager
import firrtl.options.Viewer._
import firrtl.FirrtlViewer._

import scala.io.Source

class ExceptingTransform extends Transform {
  def inputForm = HighForm
  def outputForm = HighForm
  def execute(state: CircuitState): CircuitState = {
    throw new ExceptingTransform.CustomException("I ran!")
  }
}
object ExceptingTransform {
  case class CustomException(msg: String) extends Exception
}

//noinspection ScalaStyle
class DriverSpec extends FreeSpec with Matchers with BackendCompilationUtilities {
  /* Default annotations that appear in any circuit */
  val defaultAnnotations: Set[Annotation] = Set(
    TargetDirAnnotation("."),
    BlackBoxTargetDirAnno("."),
    LogLevelAnnotation(logger.LogLevel.None),
    CompilerNameAnnotation("verilog"),
    EmitterAnnotation(classOf[VerilogEmitter]) )

  def argsToOptions(args: Array[String]): FirrtlExecutionOptions = {
    val optionsManager = new ExecutionOptionsManager("test") with HasFirrtlExecutionOptions
    view[FirrtlExecutionOptions](optionsManager.parse(args)).get
  }

  "CommonOptions are some simple options available across the chisel3 ecosystem" - {
    "CommonOption provide an scopt implementation of an OptionParser" - {
      "Options can be set from an Array[String] as is passed into a main" - {
        "With no arguments default values come out" in {
          val firrtlOptions = argsToOptions(Array("--top-name", "null"))

          firrtlOptions.topName should be(Some("null"))
          firrtlOptions.targetDirName should be(".")
        }
        "top name and target can be set" in {
          val firrtlOptions = argsToOptions(Array("--top-name", "dog", "--target-dir", "a/b/c"))

          firrtlOptions.topName should be(Some("dog"))
          firrtlOptions.targetDirName should be("a/b/c")

          firrtlOptions.getBuildFileName(".fir") should be("a/b/c/dog.fir")
          firrtlOptions.getBuildFileName("fir") should be("a/b/c/dog.fir")
        }
      }
      "CommonOptions can create a directory" in {
        var dir = new java.io.File("a/b/c")
        if(dir.exists()) {
          dir.delete()
        }
        val firrtlOptions = argsToOptions(Array("--top-name", "dog", "--target-dir", "a/b/c"))

        firrtlOptions.topName should be (Some("dog"))
        firrtlOptions.targetDirName should be ("a/b/c")

        firrtlOptions.makeTargetDir() should be (true)
        dir = new java.io.File("a/b/c")
        dir.exists() should be (true)
        FileUtils.deleteDirectoryHierarchy("a") should be (true)
      }
    }
    "options include by default a list of strings that are returned in firrtlOptions.programArgs" in {
      val firrtlOptions = Seq(Array("--top-name", "dog", "fox", "tardigrade", "stomatopod"),
                              Array("dog", "stomatopod", "--top-name", "null"),
                              Array("fox", "--top-name", "dog", "tardigrade", "stomatopod"))
        .map(argsToOptions(_))

      firrtlOptions(0).programArgs.length should be (3)
      firrtlOptions(0).programArgs should be ("fox" :: "tardigrade" :: "stomatopod" :: Nil)

      firrtlOptions(1).programArgs.length should be (2)
      firrtlOptions(1).programArgs should be ("dog" :: "stomatopod" :: Nil)

      firrtlOptions(2).programArgs.length should be (3)
      firrtlOptions(2).programArgs should be ("fox" :: "tardigrade" :: "stomatopod" :: Nil)
    }
  }

  "FirrtlOptions holds option information for the firrtl compiler" - {
    "It includes a CommonOptions" in {
      argsToOptions(Array("--top-name", "null")).targetDirName should be (".")
    }
    "It provides input and output file names based on target" in {
      val firrtlOptions = argsToOptions(Array("--top-name", "cat"))

      val inputFileName = firrtlOptions.getBuildFileName("fir", firrtlOptions.inputFileNameOverride)
      inputFileName should be ("./cat.fir")
      val outputFileName = firrtlOptions.getTargetFile
      outputFileName should be ("./cat.v")
    }
    "input and output file names can be overridden, overrides do not use targetDir" in {
      val firrtlOptions = argsToOptions(Array("--top-name", "cat", "-i", "./bob.fir", "-o", "carol.v"))

      val inputFileName = firrtlOptions.getBuildFileName("fir", firrtlOptions.inputFileNameOverride)
      inputFileName should be ("./bob.fir")
      val outputFileName = firrtlOptions.getTargetFile
      outputFileName should be ("carol.v")
    }
    "various annotations can be created from command line, currently:" - {
      "inline annotation" in {
        val inlines = List("module", "module.submodule", "module.submodule.instance")
        val args = Array(Array("--top-name", "null"), Array("--inline", inlines.mkString(",")))
        val firrtlOptions = argsToOptions(args.flatten)

        val addedAnnotations = inlines.map(i => InlineAnnotation(AnnotationUtils.toNamed(i))).toSet
        val expectedAnnotations = defaultAnnotations ++ addedAnnotations ++
          Set(TopNameAnnotation("null"), RunFirrtlTransformAnnotation(classOf[InlineInstances]))

        // The `+1` comes from the run transform annotation
        firrtlOptions.annotations.length should be (defaultAnnotations.size + inlines.size + args.dropRight(1).size + 1)
        firrtlOptions.annotations.toSet should be (expectedAnnotations)
      }
      "infer-rw annotation" in {
        val args = Array(Array("--top-name", "null"), Array("--infer-rw"))
        val firrtlOptions = argsToOptions(args.flatten)
        // The `+` comes from the run firrtl transform annotation
        firrtlOptions.annotations.length should be (defaultAnnotations.size + args.size + 1)

        val expectedAnnotations = defaultAnnotations ++
          Set(InferReadWriteAnnotation, TopNameAnnotation("null"),
              RunFirrtlTransformAnnotation(classOf[InferReadWrite]))
        firrtlOptions.annotations.toSet should be (expectedAnnotations)
      }
      "repl-seq-mem annotation" in {
        val args = Array(Array("--top-name", "null"),
                         Array("--repl-seq-mem", "-c:circuit1:-i:infile1:-o:outfile1"))
        val firrtlOptions = argsToOptions(args.flatten)
        // The `+1 comes from the run firrtl transform annotation
        firrtlOptions.annotations.length should be (defaultAnnotations.size + args.size + 1)

        val expectedAnnotations = defaultAnnotations ++
          Set(ReplSeqMemAnnotation("infile1", "outfile1"),TopNameAnnotation("null"),
              RunFirrtlTransformAnnotation(classOf[ReplSeqMem]))
        firrtlOptions.annotations.toSet should be (expectedAnnotations)
      }
    }
  }

  // Deprecated
  "Annotations can be read implicitly from the name of the circuit" in {
    val top = "foo"
    val annoFile =  new File(top + ".anno")
    copyResourceToFile("/annotations/SampleAnnotations.anno", annoFile)
    val args = Array(Array("--top-name", top))
    val firrtlOptions = argsToOptions(args.flatten)

    import net.jcazevedo.moultingyaml._
    val text = io.Source.fromFile(annoFile).mkString
    val annosInFile = text.parseYaml match { case YamlArray(xs) => xs }

    // The `+1` comes from the fact that we found an implicit annotation file
    val annos = firrtlOptions.annotations
    annos.length should be (annosInFile.size + args.size + defaultAnnotations.size + 1)
    annos.count(_.isInstanceOf[InlineAnnotation]) should be (9)
    annoFile.delete()
  }

  // Deprecated
  "Supported LegacyAnnotations will be converted automagically" in {
    val testDir = createTestDirectory("test")
    val annoFilename = "LegacyAnnotations.anno"
    val annotationsTestFile =  new File(testDir, annoFilename)
    copyResourceToFile(s"/annotations/$annoFilename", annotationsTestFile)

    val args = Array(Array("--top-name", "test"),
                     Array("--target-dir", testDir.toString),
                     Array("--annotation-file", annotationsTestFile.toString))
    val firrtlOptions = argsToOptions(args.flatten)

    val cname = CircuitName("foo")
    val mname = ModuleName("bar", cname)
    val compname = ComponentName("x", mname)
    import firrtl.passes.clocklist._
    import firrtl.passes.memlib._
    import firrtl.passes.wiring._
    import firrtl.transforms._
    val expected = List( InlineAnnotation(cname),
                         ClockListAnnotation(mname, "output"),
                         InferReadWriteAnnotation,
                         ReplSeqMemAnnotation("input", "output"),
                         NoDedupMemAnnotation(compname),
                         NoDedupAnnotation(mname),
                         SourceAnnotation(compname, "pin"),
                         SinkAnnotation(compname, "pin"),
                         BlackBoxResourceAnno(mname, "resource"),
                         BlackBoxInlineAnno(mname, "name", "text"),
                         BlackBoxTargetDirAnno("targetdir"),
                         NoDCEAnnotation,
                         DontTouchAnnotation(compname),
                         OptimizableExtModuleAnnotation(mname) )
    for (e <- expected) {
      firrtlOptions.annotations should contain (e)
    }
  }

  // Deprecated
  "Unsupported LegacyAnnotations should throw errors" in {
    val testDir = createTestDirectory("test")
    val annoFilename = "InvalidLegacyAnnotations.anno"
    val annotationsTestFile =  new File(testDir, annoFilename)
    copyResourceToFile(s"/annotations/$annoFilename", annotationsTestFile)

    import net.jcazevedo.moultingyaml._
    val text = io.Source.fromFile(annotationsTestFile).mkString
    val yamlAnnos = text.parseYaml match { case YamlArray(xs) => xs }

    // Since each one should error, emit each one to an anno file and try to read it
    for ((anno, i) <- yamlAnnos.zipWithIndex) {
      val annoFile = new File(testDir, s"anno_$i.anno")
      val fw = new FileWriter(annoFile)
      fw.write(YamlArray(anno).prettyPrint)
      fw.close()

      (the [Exception] thrownBy {argsToOptions(Array("-faf", annoFile.toString))})
        .getMessage should include ("Old-style annotations")
    }
  }

  "Annotations can be read from multiple files" in {
    val filenames = Seq("SampleAnnotations1.anno.json", "SampleAnnotations2.anno.json")
    filenames.foreach(f => copyResourceToFile(s"/annotations/SampleAnnotations.anno.json", new File(f)))
    val args = Array(Array("--top-name", "a.fir")) ++ filenames.map(f => Array("--annotation-file", f))
    val firrtlOptions = argsToOptions(args.flatten)

    import net.jcazevedo.moultingyaml._
    val annosInFile = io.Source.fromFile(filenames(0)).mkString
      .parseYaml match { case YamlArray(xs) => xs }

    val annos = firrtlOptions.annotations
    annos.length should be (defaultAnnotations.size + args.size + annosInFile.size * 2)
    annos.count(_.isInstanceOf[InlineAnnotation]) should be (annosInFile.size * 2)

    filenames.foreach(f => new File(f).delete())
  }

  "Annotations won't be read from the same file twice" in {
    val filename = "SampleAnnotations.anno.json"
    copyResourceToFile(s"/annotations/$filename", new File(filename))
    val args = Array(Array("--top-name", "a.fir")) ++
      List.fill(2)(filename).map(f => Array("--annotation-file", f))

    import net.jcazevedo.moultingyaml._
    val annosInFile = io.Source.fromFile(filename).mkString
      .parseYaml match { case YamlArray(xs) => xs }

    val annos = argsToOptions(args.flatten).annotations
    annos.length should be (defaultAnnotations.size + args.size + annosInFile.size)
    annos.count(_.isInstanceOf[InlineAnnotation]) should be (annosInFile.size)

    new File(filename).delete()
  }

  "Annotations can be created from the command line and read from a file at the same time" in {
    val annoFile = new File("annotations.anno")
    copyResourceToFile("/annotations/SampleAnnotations.anno.json", annoFile)
    val args = Array( Array("--top-name", "null"),
                      Array("--infer-rw"),
                      Array("-faf", annoFile.toString) )

    val annosInFile = JsonProtocol.deserialize(annoFile)
    val annotations = argsToOptions(args.flatten).annotations
    // The `+1` comes from the run firrtl transform annotation
    annotations.length should be (defaultAnnotations.size + annosInFile.size + args.size + 1)

    annotations should contain (BlackBoxTargetDirAnno(".")) // built in to getAnnotations
    annotations should contain (InferReadWriteAnnotation)  // --infer-rw
    annotations.collect { case a: InlineAnnotation => a }.length should be (9)  // annotations file

    annoFile.delete()
  }
  "An annotation file is equivalent to command line options" in {
    val annoFile = new File("annotations.anno")
    val importedAnnoFile = new File("annotations-imported.anno")
    copyResourceToFile("/annotations/AnnotationsAsOptions.anno.json", annoFile)
    copyResourceToFile("/annotations/SampleAnnotations.anno.json", importedAnnoFile)
    val args = Array("--annotation-file", annoFile.toString)

    val annosInFile = JsonProtocol.deserialize(annoFile) ++ JsonProtocol.deserialize(importedAnnoFile)
    argsToOptions(args).annotations.length should be (defaultAnnotations.size + annosInFile.size + 1)

    annoFile.delete()
    importedAnnoFile.delete()
  }

  "Circuits are emitted on properly" - {
    val input =
      """|circuit Top :
         |  module Top :
         |    output foo : UInt<32>
         |    inst c of Child
         |    inst e of External
         |    foo <= tail(add(c.foo, e.foo), 1)
         |  module Child :
         |    output foo : UInt<32>
         |    inst e of External
         |    foo <= e.foo
         |  extmodule External :
         |    output foo : UInt<32>
      """.stripMargin

    "To a single file with file extension depending on the compiler by default" in {
      Seq(
        "low" -> "./Top.lo.fir",
        "middle" -> "./Top.mid.fir",
        "high" -> "./Top.hi.fir",
        "verilog" -> "./Top.v"
      ).foreach { case (compilerName, expectedOutputFileName) =>
          val args = Array("--firrtl-source", input, "--compiler", compilerName)
          firrtl.Driver.execute(args)

          val file = new File(expectedOutputFileName)
          info(s"for $compilerName compiler")
          file.exists() should be (true)
      }
    }
    "To a single file per module if OneFilePerModule is specified" in {
      Seq(
        "low" -> Seq("./Top.lo.fir", "./Child.lo.fir"),
        "middle" -> Seq("./Top.mid.fir", "./Child.mid.fir"),
        "high" -> Seq("./Top.hi.fir", "./Child.hi.fir"),
        "verilog" -> Seq("./Top.v", "./Child.v")
      ).foreach { case (compilerName, expectedOutputFileNames) =>
          val args = Array("--firrtl-source", input,
                           "--compiler", compilerName,
                           "--split-modules")

          info(s"for $compilerName compiler")
          firrtl.Driver.execute(args) match {
            case success: FirrtlExecutionSuccess =>
              success.circuitState.annotations.length should be > (0)
            case _ =>
        }

        for (name <- expectedOutputFileNames) {
          val file = new File(name)
          file.exists() should be (true)
          file.delete()
        }
      }
    }
  }

  "The Driver is sensitive to the file extension of input files" - {
    val design = "GCDTester"
    val outputDir = createTestDirectory("DriverFileExtensionSensitivity")
    val verilogFromFir = new File(outputDir, s"$design.fromfir.v")
    val verilogFromPb = new File(outputDir, s"$design.frompb.v")
    val commonArgs = Array("-X", "verilog", "--info-mode", "use")
    ".fir means FIRRTL file" in {
      val inFile = new File(getClass.getResource(s"/integration/$design.fir").getFile)
      val args = Array("-i", inFile.getAbsolutePath, "-o", verilogFromFir.getAbsolutePath) ++ commonArgs
      Driver.execute(args)
    }
    ".pb means ProtoBuf file" in {
      val inFile = new File(getClass.getResource(s"/integration/$design.pb").getFile)
      val args = Array("-i", inFile.getAbsolutePath, "-o", verilogFromPb.getAbsolutePath) ++ commonArgs
      Driver.execute(args)
    }
    "Both paths do the same thing" in {
      val s1 = Source.fromFile(verilogFromFir).mkString
      val s2 = Source.fromFile(verilogFromPb).mkString
      s1 should equal (s2)
    }
  }

  "Directory deleter is handy for cleaning up after tests" - {
    "for example making a directory tree, and deleting it looks like" in {
      FileUtils.makeDirectory("dog/fox/wolf")
      val dir = new File("dog/fox/wolf")
      dir.exists() should be (true)
      dir.isDirectory should be (true)

      FileUtils.deleteDirectoryHierarchy("wolf") should be (false)
      FileUtils.deleteDirectoryHierarchy("dog") should be (true)
      dir.exists() should be (false)
    }
  }
}
