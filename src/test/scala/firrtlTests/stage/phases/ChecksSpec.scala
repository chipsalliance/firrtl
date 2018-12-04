// See LICENSE for license details.

package firrtlTests.stage.phases

import org.scalatest.{FlatSpec, Matchers}

import firrtl.stage._

import firrtl.{AnnotationSeq, ChirrtlEmitter, EmitAllModulesAnnotation, NoneCompiler}
import firrtl.options.{OptionsException, OutputAnnotationFileAnnotation, Phase}
import firrtl.stage.phases.{AddImplicitOutputFile, Checks}

class ChecksSpec extends FlatSpec with Matchers {

  val inputFile = FirrtlFileAnnotation("foo")
  val outputFile = OutputFileAnnotation("bar")
  val emitAllModules = EmitAllModulesAnnotation(classOf[ChirrtlEmitter])
  val outputAnnotationFile = OutputAnnotationFileAnnotation("baz")
  val goodCompiler = CompilerAnnotation(new NoneCompiler())
  val infoMode = InfoModeAnnotation("ignore")

  val min = Seq(inputFile, goodCompiler, infoMode)

  def checkExceptionMessage(phase: Phase, annotations: AnnotationSeq, messageStart: String): Unit =
    intercept[OptionsException]{ phase.transform(annotations) }.getMessage should startWith(messageStart)

  behavior of Checks.getClass.getName

  it should "require exactly one input source" in {
    info("0 input source causes an exception")
    checkExceptionMessage(Checks, Seq.empty, "Unable to determine FIRRTL source to read")

    info("2 input sources causes an exception")
    val in = min :+ FirrtlSourceAnnotation("circuit Foo:")
    checkExceptionMessage(Checks, in, "Multiply defined input FIRRTL sources")
  }

  it should "require mutual exclusivity of OutputFileAnnotation and EmitAllModulesAnnotation" in {
    info("OutputFileAnnotation alone works")
    Checks.transform(min :+ outputFile)

    info("OneFilePerModuleAnnotation alone works")
    Checks.transform(min :+ emitAllModules)

    info("Together, they throw an exception")
    val in = min ++ Seq(outputFile, emitAllModules)
    checkExceptionMessage(Checks, in, "Output file is incompatible with emit all modules annotation")
  }

  it should "enforce zero or one output files" in {
    val in = min ++ Seq(outputFile, outputFile)
    checkExceptionMessage(Checks, in, "No more than one output file can be specified")
  }

  it should "enforce exactly one compiler" in {
    info("0 compilers should throw an exception")
    val inZero = Seq(inputFile, infoMode)
    checkExceptionMessage(Checks, inZero, "Exactly one compiler must be specified, but none found")

    info("2 compilers should throw an exception")
    val c = goodCompiler.compiler
    val inTwo = min :+ goodCompiler
    checkExceptionMessage(Checks, inTwo, s"Exactly one compiler must be specified, but found '$c, $c'")
  }

  it should "validate info mode names" in {
    info("Good info mode names should work")
    Seq("ignore", "use", "gen", "append")
      .map(info => Checks.transform(Seq(inputFile, goodCompiler, InfoModeAnnotation(info))))
  }

  it should "enforce exactly one info mode" in {
    info("0 info modes should throw an exception")
    checkExceptionMessage(Checks, Seq(inputFile, goodCompiler),
                          "Exactly one info mode must be specified, but none found")

    info("2 info modes should throw an exception")
    val i = infoMode.modeName
    checkExceptionMessage(Checks, min :+ infoMode, s"Exactly one info mode must be specified, but found '$i, $i'")
  }

  it should "pass if the minimum annotations are specified" in {
    info(s"""Minimum required: ${min.map(_.getClass.getSimpleName).mkString(", ")}""")
    Checks.transform(min)
  }

}
