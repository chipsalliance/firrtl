// See LICENSE for license details.

package firrtlTests

import java.io.{File, FileWriter}
import org.scalatest.{FreeSpec, Matchers}

import firrtl.passes.{InlineAnnotation, InlineInstances}
import firrtl.passes.memlib.{
  InferReadWrite,
  InferReadWriteAnnotation,
  ReplSeqMem,
  ReplSeqMemAnnotation
}
import firrtl.transforms.BlackBoxTargetDirAnno
import firrtl._
import firrtl.annotations._
import firrtl.util.BackendCompilationUtilities

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

  "CommonOptions are some simple options available across the chisel3 ecosystem" - {
    "CommonOption provide an scopt implementation of an OptionParser" - {
      "Options can be set from an Array[String] as is passed into a main" - {
        "With no arguments default values come out" in {
          val optionsManager = new ExecutionOptionsManager("test", Array("--top-name", "null"))

          val firrtlOptions = optionsManager.firrtlOptions
          firrtlOptions.topName should be(Some("null"))
          firrtlOptions.targetDirName should be(".")
        }
        "top name and target can be set" in {
          val optionsManager = new ExecutionOptionsManager(
            "test",
            Array("--top-name", "dog", "--target-dir", "a/b/c") )
          val firrtlOptions = optionsManager.firrtlOptions

          firrtlOptions.topName should be(Some("dog"))
          firrtlOptions.targetDirName should be("a/b/c")

          optionsManager.getBuildFileName(".fir") should be("a/b/c/dog.fir")
          optionsManager.getBuildFileName("fir") should be("a/b/c/dog.fir")
        }
      }
      "CommonOptions can create a directory" in {
        var dir = new java.io.File("a/b/c")
        if(dir.exists()) {
          dir.delete()
        }
        val optionsManager = new ExecutionOptionsManager(
          "test",
          Array("--top-name", "dog", "--target-dir", "a/b/c") )
        val firrtlOptions = optionsManager.firrtlOptions

        firrtlOptions.topName should be (Some("dog"))
        firrtlOptions.targetDirName should be ("a/b/c")

        optionsManager.makeTargetDir() should be (true)
        dir = new java.io.File("a/b/c")
        dir.exists() should be (true)
        FileUtils.deleteDirectoryHierarchy("a") should be (true)
      }
    }
    "options include by default a list of strings that are returned in firrtlOptions.programArgs" in {
      val optionsManager = new ExecutionOptionsManager(
        "test",
        Array("--top-name", "dog", "fox", "tardigrade", "stomatopod") )

      println(s"programArgs ${optionsManager.firrtlOptions.programArgs}")
      optionsManager.firrtlOptions.programArgs.length should be (3)
      optionsManager.firrtlOptions.programArgs should be ("fox" :: "tardigrade" :: "stomatopod" :: Nil)

      val optionsManager2 = new ExecutionOptionsManager(
        "test2",
        Array("dog", "stomatopod", "--top-name", "null") )
      println(s"programArgs ${optionsManager2.firrtlOptions.programArgs}")
      optionsManager2.firrtlOptions.programArgs.length should be (2)
      optionsManager2.firrtlOptions.programArgs should be ("dog" :: "stomatopod" :: Nil)

      val optionsManager3 = new ExecutionOptionsManager(
        "test3",
        Array("fox", "--top-name", "dog", "tardigrade", "stomatopod") )

      println(s"programArgs ${optionsManager3.firrtlOptions.programArgs}")
      optionsManager3.firrtlOptions.programArgs.length should be (3)
      optionsManager3.firrtlOptions.programArgs should be ("fox" :: "tardigrade" :: "stomatopod" :: Nil)

    }
  }

  "FirrtlOptions holds option information for the firrtl compiler" - {
    "It includes a CommonOptions" in {
      val optionsManager = new ExecutionOptionsManager("test", Array("--top-name", "null"))
      optionsManager.firrtlOptions.targetDirName should be (".")
    }
    "It provides input and output file names based on target" in {
      val optionsManager = new ExecutionOptionsManager(
        "test",
        Array("--top-name", "cat") ) with HasFirrtlOptions

      val firrtlOptions = optionsManager.firrtlOptions
      val inputFileName = optionsManager.getBuildFileName("fir", firrtlOptions.inputFileNameOverride)
      inputFileName should be ("./cat.fir")
      val outputFileName = firrtlOptions.getTargetFile(optionsManager)
      outputFileName should be ("./cat.v")
    }
    "input and output file names can be overridden, overrides do not use targetDir" in {
      val optionsManager = new ExecutionOptionsManager(
        "test",
        Array("--top-name", "cat", "-i", "./bob.fir", "-o", "carol.v") ) with HasFirrtlOptions

      val firrtlOptions = optionsManager.firrtlOptions
      val inputFileName = optionsManager.getBuildFileName("fir", firrtlOptions.inputFileNameOverride)
      inputFileName should be ("./bob.fir")
      val outputFileName = firrtlOptions.getTargetFile(optionsManager)
      outputFileName should be ("carol.v")
    }
    "various annotations can be created from command line, currently:" - {
      "inline annotation" in {
        val inlines = List("module", "module.submodule", "module.submodule.instance")
        val optionsManager = new ExecutionOptionsManager(
          "test",
          Array("--top-name", "null", "--inline") :+ inlines.mkString(",") ) with HasFirrtlOptions

        val firrtlOptions = optionsManager.firrtlOptions
        val addedAnnotations = inlines.map(i => InlineAnnotation(AnnotationUtils.toNamed(i))).toSet
        val expectedAnnotations = defaultAnnotations ++ addedAnnotations ++ Set(TopNameAnnotation("null"))

        // The `+1` comes from the `--top-name`
        firrtlOptions.annotations.length should be (defaultAnnotations.size + inlines.size + 1)
        firrtlOptions.annotations.toSet should be (expectedAnnotations)
      }
      "infer-rw annotation" in {
        val args = Array(Array("--top-name", "null"), Array("--infer-rw", "circuit"))
        val optionsManager = new ExecutionOptionsManager("test", args.flatMap(x => x) ) with HasFirrtlOptions

        val firrtlOptions = optionsManager.firrtlOptions
        firrtlOptions.annotations.length should be (defaultAnnotations.size + args.size)

        val expectedAnnotations = defaultAnnotations ++ Set(InferReadWriteAnnotation, TopNameAnnotation("null"))
        firrtlOptions.annotations.toSet should be (expectedAnnotations)
      }
      "repl-seq-mem annotation" in {
        val args = Array(Array("--top-name", "null"), Array("--repl-seq-mem", "-c:circuit1:-i:infile1:-o:outfile1"))
        val optionsManager = new ExecutionOptionsManager("test", args.flatMap(x => x)) with HasFirrtlOptions

        val firrtlOptions = optionsManager.firrtlOptions
        firrtlOptions.annotations.length should be (defaultAnnotations.size + args.size)

        val expectedAnnotations = defaultAnnotations ++ Set(ReplSeqMemAnnotation("infile1", "outfile1"),
                                                            TopNameAnnotation("null"))
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
    val optionsManager = new ExecutionOptionsManager("test", args.flatMap(x=>x)) with HasFirrtlOptions

    import net.jcazevedo.moultingyaml._
    val text = io.Source.fromFile(annoFile).mkString
    val annosInFile = text.parseYaml match { case YamlArray(xs) => xs }

    // The `+1` comes from the fact that we found an implicit annotation file
    val annos = optionsManager.firrtlOptions.annotations
    annos.length should be (annosInFile.size + args.size + defaultAnnotations.size + 1)
    annos.count(_.isInstanceOf[InlineAnnotation]) should be (9)
    annoFile.delete()
  }

  // Deprecated
  "Supported LegacyAnnotations will be converted automagically" in {
    val testDir = createTestDirectory("test")
    println(testDir)
    val annoFilename = "LegacyAnnotations.anno"
    val annotationsTestFile =  new File(testDir, annoFilename)
    val optionsManager = new ExecutionOptionsManager(
      "test",
      Array("--top-name", "test",
            "--target-dir", testDir.toString,
            "--annotation-file", annotationsTestFile.toString)) with HasFirrtlOptions {
    }
    copyResourceToFile(s"/annotations/$annoFilename", annotationsTestFile)
    val annos = Driver.getAnnotations(optionsManager)

    val cname = CircuitName("foo")
    val mname = ModuleName("bar", cname)
    val compname = ComponentName("x", mname)
    import firrtl.passes.clocklist._
    import firrtl.passes.memlib._
    import firrtl.passes.wiring._
    import firrtl.transforms._
    val expected = List(
      InlineAnnotation(cname),
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
      OptimizableExtModuleAnnotation(mname)
    )
    for (e <- expected) {
      annos should contain (e)
    }
  }

  // Deprecated
  "UNsupported LegacyAnnotations should throw errors" in {
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
      val optionsManager = new ExecutionOptionsManager(
        "test",
        Array("--top-name", "test",
              "--target-dir", testDir.toString,
              "--annotation-file", annoFile.toString) ) with HasFirrtlOptions
      (the [Exception] thrownBy {
        Driver.getAnnotations(optionsManager)
      }).getMessage should include ("Old-style annotations")
    }
  }

  "Annotations can be read from multiple files" in {
    val filenames = Seq("SampleAnnotations1.anno.json", "SampleAnnotations2.anno.json")
    filenames.foreach(f => copyResourceToFile(s"/annotations/SampleAnnotations.anno.json", new File(f)))
    val args = Array(Array("--top-name", "a.fir")) ++ filenames.map(f => Array("--annotation-file", f))
    val optionsManager = new ExecutionOptionsManager("test", args.flatMap(x=>x)) with HasFirrtlOptions

    import net.jcazevedo.moultingyaml._
    val annosInFile = io.Source.fromFile(filenames(0)).mkString
      .parseYaml match { case YamlArray(xs) => xs }

    val annos = optionsManager.firrtlOptions.annotations
    annos.length should be (defaultAnnotations.size + args.size + annosInFile.size * 2)
    annos.count(_.isInstanceOf[InlineAnnotation]) should be (annosInFile.size * 2)

    filenames.foreach(f => new File(f).delete())
  }

  "Annotations won't be read from the same file twice" in {
    val filename = "SampleAnnotations.anno.json"
    copyResourceToFile(s"/annotations/$filename", new File(filename))
    val args = Array(Array("--top-name", "a.fir")) ++
      List.fill(2)(filename).map(f => Array("--annotation-file", f))
    val optionsManager = new ExecutionOptionsManager("test", args.flatMap(x=>x)) with HasFirrtlOptions

    import net.jcazevedo.moultingyaml._
    val annosInFile = io.Source.fromFile(filename).mkString
      .parseYaml match { case YamlArray(xs) => xs }

    val annos = optionsManager.firrtlOptions.annotations
    annos.length should be (defaultAnnotations.size + args.size + annosInFile.size)
    annos.count(_.isInstanceOf[InlineAnnotation]) should be (annosInFile.size)

    new File(filename).delete()
  }

  "Annotations can be created from the command line and read from a file at the same time" in {
    // [todo] You shouldn't have to specify the explicit type here...

    val annoFile = new File("annotations.anno")
    copyResourceToFile("/annotations/SampleAnnotations.anno.json", annoFile)
    val args = Array( Array("--top-name", "null"),
                      Array("--infer-rw", "circuit"),
                      Array("-faf", annoFile.toString) )

    val optionsManager = new ExecutionOptionsManager(
      "test",
      args.flatMap(x => x) ) with HasFirrtlOptions

    val annosInFile = JsonProtocol.deserialize(annoFile)

    val firrtlOptions = optionsManager.firrtlOptions
    firrtlOptions.annotations.length should be (defaultAnnotations.size + annosInFile.size + args.size) // infer-rw

    val anns = Driver.getAnnotations(optionsManager)
    anns should contain (BlackBoxTargetDirAnno(".")) // built in to getAnnotations
    anns should contain (InferReadWriteAnnotation)  // --infer-rw
    anns.collect { case a: InlineAnnotation => a }.length should be (9)  // annotations file

    annoFile.delete()
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
        "high" -> "./Top.hi.fir",
        "middle" -> "./Top.mid.fir",
        "verilog" -> "./Top.v"
      ).foreach { case (compilerName, expectedOutputFileName) =>
          val manager = new ExecutionOptionsManager(
            "test",
            Array("--firrtl-source", input,
                  "--compiler", compilerName) ) with HasFirrtlOptions

        firrtl.Driver.execute(manager)

        val file = new File(expectedOutputFileName)
        file.exists() should be (true)
        file.delete()
      }
    }
    "To a single file per module if OneFilePerModule is specified" in {
      Seq(
        "low" -> Seq("./Top.lo.fir", "./Child.lo.fir"),
        "high" -> Seq("./Top.hi.fir", "./Child.hi.fir"),
        "middle" -> Seq("./Top.mid.fir", "./Child.mid.fir"),
        "verilog" -> Seq("./Top.v", "./Child.v")
      ).foreach { case (compilerName, expectedOutputFileNames) =>
        println(s"$compilerName -> $expectedOutputFileNames")
          val manager = new ExecutionOptionsManager(
            "test",
            Array("--firrtl-source", input,
                  "--compiler", compilerName,
                  "--split-modules") ) with HasFirrtlOptions

        firrtl.Driver.execute(manager) match {
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
