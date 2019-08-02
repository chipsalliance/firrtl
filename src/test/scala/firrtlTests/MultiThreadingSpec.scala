// See LICENSE for license details.

package firrtlTests

import java.io.{File, PrintWriter}

import firrtl.FileUtils
import firrtl.{ChirrtlForm, CircuitState}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

class MultiThreadingSpec extends FirrtlPropSpec {

  // TODO Test with annotations and source locator
  property("The FIRRTL compiler should be thread safe") {
    // Run the compiler we're testing
    def runCompiler(input: Seq[String], compiler: firrtl.Compiler): String = {
      val parsedInput = firrtl.Parser.parse(input)
      val res = compiler.compileAndEmit(CircuitState(parsedInput, ChirrtlForm))
      res.getEmittedCircuit.value
    }
    // The parameters we're testing with
    val compilers = Seq(
      new firrtl.HighFirrtlCompiler,
      new firrtl.MiddleFirrtlCompiler,
      new firrtl.LowFirrtlCompiler,
      new firrtl.VerilogCompiler)
    val inputFilePath = s"/integration/GCDTester.fir" // arbitrary
    val numThreads = 64 // arbitrary

    // Begin the actual test

    val inputStrings = FileUtils.getLinesResource(inputFilePath)

    import ExecutionContext.Implicits.global
    try { // Use try-catch because error can manifest in many ways
      // Execute for each compiler
      val compilerResults = compilers map { compiler =>
        // Run compiler serially once
        val serialResult = runCompiler(inputStrings, compiler)
        Future {
          val threadFutures = (0 until numThreads) map { i =>
              Future {
                runCompiler(inputStrings, compiler) == serialResult
              }
            }
          Await.result(Future.sequence(threadFutures), Duration.Inf)
        }
      }
      val results = Await.result(Future.sequence(compilerResults), Duration.Inf)
      assert(results.flatten reduce (_ && _)) // check all true (ie. success)
    } catch {
      case _: Throwable => fail("The Compiler is not thread safe")
    }
  }

  property("getLines should work when multi-threaded") {
    val targetDir = "test_run_dir/multi_thread_getLines"
    val fileName = targetDir + "/input_file"
    FileUtils.makeDirectory(targetDir)
    val f = new PrintWriter(new File(fileName))
    f.write((0 to 1000).map { i => f"line $i%08d" }.mkString("\n") )
    f.close()

    val numberOfLines = FileUtils.getLines(fileName).length

    val inputStrings = FileUtils.getLines(fileName)

    val numThreads = 64 // arbitrary

    import ExecutionContext.Implicits.global
    try { // Use try-catch because error can manifest in many ways

      val runResults = (0 to 10).map { _ =>
        Future {
          val threadFutures = (0 until numThreads) map { i =>
            Future {
              inputStrings.length == numberOfLines
            }
          }
          Await.result(Future.sequence(threadFutures), Duration.Inf)
        }
      }

      val results = Await.result(Future.sequence(runResults), Duration.Inf)
      assert(results.flatten reduce (_ && _)) // check all true (ie. success)
    } catch {
      case _: Throwable => fail("The getLines is not thread safe")
    }
  }
}
