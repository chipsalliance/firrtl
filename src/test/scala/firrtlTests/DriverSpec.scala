<<<<<<< HEAD
// See LICENSE for license details.
=======
//// See LICENSE for license details.
>>>>>>> d9572b3... annotations can be read from a file.  are added to the optionsManager if the file exists

package firrtlTests

import java.io.File

<<<<<<< HEAD
import firrtl.Annotations.Annotation
import org.scalatest.{Matchers, FreeSpec}

import firrtl._
import firrtl.passes.InlineInstances
import firrtl.passes.memlib.{ReplSeqMem, InferReadWrite}
=======
//import firrtl.passes.memlib.ReplSeqMemAnnotation
import org.scalatest.{Matchers, FreeSpec}

import firrtl._
>>>>>>> d9572b3... annotations can be read from a file.  are added to the optionsManager if the file exists

class DriverSpec extends FreeSpec with Matchers {
  "CommonOptions are some simple options available across the chisel3 ecosystem" - {
    "CommonOption provide an scopt implementation of an OptionParser" - {
      "Options can be set from an Array[String] as is passed into a main" - {
        "With no arguments default values come out" in {
          val optionsManager = new ExecutionOptionsManager("test")
          optionsManager.parse(Array.empty[String]) should be(true)

          val commonOptions = optionsManager.commonOptions
          commonOptions.topName should be("")
<<<<<<< HEAD
          commonOptions.targetDirName should be(".")
=======
          commonOptions.targetDirName should be("test_run_dir")
>>>>>>> d9572b3... annotations can be read from a file.  are added to the optionsManager if the file exists
        }
        "top name and target can be set" in {
          val optionsManager = new ExecutionOptionsManager("test")
          optionsManager.parse(Array("--top-name", "dog", "--target-dir", "a/b/c")) should be(true)
          val commonOptions = optionsManager.commonOptions

          commonOptions.topName should be("dog")
          commonOptions.targetDirName should be("a/b/c")

          optionsManager.getBuildFileName(".fir") should be("a/b/c/dog.fir")
          optionsManager.getBuildFileName("fir") should be("a/b/c/dog.fir")
        }
      }
      "CommonOptions can create a directory" in {
        var dir = new java.io.File("a/b/c")
        if(dir.exists()) {
          dir.delete()
        }
        val optionsManager = new ExecutionOptionsManager("test")
        optionsManager.parse(Array("--top-name", "dog", "--target-dir", "a/b/c")) should be (true)
        val commonOptions = optionsManager.commonOptions

        commonOptions.topName should be ("dog")
        commonOptions.targetDirName should be ("a/b/c")

        optionsManager.makeTargetDir() should be (true)
        dir = new java.io.File("a/b/c")
        dir.exists() should be (true)
        FileUtils.deleteDirectoryHierarchy(commonOptions.targetDirName)
      }
    }
  }
  "FirrtlOptions holds option information for the firrtl compiler" - {
    "It includes a CommonOptions" in {
      val optionsManager = new ExecutionOptionsManager("test")
<<<<<<< HEAD
      optionsManager.commonOptions.targetDirName should be (".")
=======
      optionsManager.commonOptions.targetDirName should be ("test_run_dir")
>>>>>>> d9572b3... annotations can be read from a file.  are added to the optionsManager if the file exists
    }
    "It provides input and output file names based on target" in {
      val optionsManager = new ExecutionOptionsManager("test") with HasFirrtlOptions

      optionsManager.parse(Array("--top-name", "cat")) should be (true)

      val firrtlOptions = optionsManager.firrtlOptions
      val inputFileName = optionsManager.getBuildFileName("fir", firrtlOptions.inputFileNameOverride)
<<<<<<< HEAD
      inputFileName should be ("./cat.fir")
      val outputFileName = optionsManager.getBuildFileName("v", firrtlOptions.outputFileNameOverride)
      outputFileName should be ("./cat.v")
=======
      inputFileName should be ("test_run_dir/cat.fir")
      val outputFileName = optionsManager.getBuildFileName("v", firrtlOptions.outputFileNameOverride)
      outputFileName should be ("test_run_dir/cat.v")
>>>>>>> d9572b3... annotations can be read from a file.  are added to the optionsManager if the file exists
    }
    "input and output file names can be overridden, overrides do not use targetDir" in {
      val optionsManager = new ExecutionOptionsManager("test") with HasFirrtlOptions

      optionsManager.parse(
        Array("--top-name", "cat", "-i", "./bob.fir", "-o", "carol.v")
      ) should be (true)

      val firrtlOptions = optionsManager.firrtlOptions
      val inputFileName = optionsManager.getBuildFileName("fir", firrtlOptions.inputFileNameOverride)
      inputFileName should be ("./bob.fir")
      val outputFileName = optionsManager.getBuildFileName("v", firrtlOptions.outputFileNameOverride)
      outputFileName should be ("carol.v")
    }
<<<<<<< HEAD
    "various annotations can be created from command line, currently:" - {
      "inline annotation" in {
        val optionsManager = new ExecutionOptionsManager("test") with HasFirrtlOptions

        optionsManager.parse(
          Array("--inline", "module,module.submodule,module.submodule.instance")
        ) should be (true)

        val firrtlOptions = optionsManager.firrtlOptions
        firrtlOptions.annotations.length should be (3)
        val x = classOf[InlineInstances]
        firrtlOptions.annotations.foreach { annotation =>
          annotation.transform shouldBe classOf[InlineInstances]
        }
      }
      "infer-rw annotation" in {
        val optionsManager = new ExecutionOptionsManager("test") with HasFirrtlOptions

        optionsManager.parse(
          Array("--infer-rw", "circuit")
        ) should be (true)

        val firrtlOptions = optionsManager.firrtlOptions
        firrtlOptions.annotations.length should be (1)
        firrtlOptions.annotations.foreach { annotation =>
          annotation.transform shouldBe classOf[InferReadWrite]
        }
      }
      "repl-seq-mem annotation" in {
        val optionsManager = new ExecutionOptionsManager("test") with HasFirrtlOptions

        optionsManager.parse(
          Array("--repl-seq-mem", "-c:circuit1:-i:infile1:-o:outfile1")
        ) should be (true)

        val firrtlOptions = optionsManager.firrtlOptions

        firrtlOptions.annotations.length should be (1)
        firrtlOptions.annotations.foreach { annotation =>
          annotation.transform shouldBe classOf[ReplSeqMem]
        }
      }
    }
=======
//    "various annotations can be created from command line, currently:" - {
//      "inline annotation" in {
//        val optionsManager = new ExecutionOptionsManager("test") with HasFirrtlOptions
//
//        optionsManager.parse(
//          Array("--inline", "module,module.submodule,module.submodule.instance")
//        ) should be (true)
//
//        val firrtlOptions = optionsManager.firrtlOptions
//        firrtlOptions.annotations.length should be (3)
//        firrtlOptions.annotations.foreach { annotation =>
//          annotation shouldBe a [passes.I]
//        }
//      }
//      "infer-rw annotation" in {
//        val optionsManager = new ExecutionOptionsManager("test") with HasFirrtlOptions
//
//        optionsManager.parse(
//          Array("--infer-rw", "circuit")
//        ) should be (true)
//
//        val firrtlOptions = optionsManager.firrtlOptions
//        firrtlOptions.annotations.length should be (1)
//        firrtlOptions.annotations.foreach { annotation =>
//          annotation shouldBe a [passes.InferReadWriteAnnotation]
//        }
//      }
//      "repl-seq-mem annotation" in {
//        val optionsManager = new ExecutionOptionsManager("test") with HasFirrtlOptions
//
//        optionsManager.parse(
//          Array("--repl-seq-mem", "-c:circuit1:-i:infile1:-o:outfile1")
//        ) should be (true)
//
//        val firrtlOptions = optionsManager.firrtlOptions
//
//        firrtlOptions.annotations.length should be (1)
//        firrtlOptions.annotations.foreach { annotation =>
//          annotation shouldBe a [ReplSeqMemAnnotation]
//        }
//      }
//    }
  }

  "Annotations can be read from a file" in {
    val optionsManager = new ExecutionOptionsManager("test") with HasFirrtlOptions {
      commonOptions = commonOptions.copy(topName = "a.fir")
      firrtlOptions = firrtlOptions.copy(
        annotationFileNameOverride = "src/test/resources/annotations/SampleAnnotations"
      )
    }
    optionsManager.firrtlOptions.annotations.length should be (0)
    Driver.loadAnnotations(optionsManager)
    optionsManager.firrtlOptions.annotations.length should be (9)

    optionsManager.firrtlOptions.annotations.head.transformClass should be ("chick.chisel.DiamondPass")
>>>>>>> d9572b3... annotations can be read from a file.  are added to the optionsManager if the file exists
  }

  val input =
    """
      |circuit Dummy :
      |  module Dummy :
      |    input x : UInt<1>
      |    output y : UInt<1>
      |    y <= x
    """.stripMargin

  "Driver produces files with different names depending on the compiler" - {
    "compiler changes the default name of the output file" in {

      Seq(
<<<<<<< HEAD
        "low" -> "./Dummy.lo.fir",
        "high" -> "./Dummy.hi.fir",
        "verilog" -> "./Dummy.v"
=======
        "low" -> "test_run_dir/Dummy.lo.fir",
        "high" -> "test_run_dir/Dummy.hi.fir",
        "verilog" -> "test_run_dir/Dummy.v"
>>>>>>> d9572b3... annotations can be read from a file.  are added to the optionsManager if the file exists
      ).foreach { case (compilerName, expectedOutputFileName) =>
        val manager = new ExecutionOptionsManager("test") with HasFirrtlOptions {
          commonOptions = CommonOptions(topName = "Dummy")
          firrtlOptions = FirrtlExecutionOptions(firrtlSource = Some(input), compilerName = compilerName)
        }

        firrtl.Driver.execute(manager)

        val file = new File(expectedOutputFileName)
        file.exists() should be (true)
        file.delete()
      }
    }
  }
}
