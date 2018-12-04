// See LICENSE for license details.

package firrtlTests.stage.phases

import org.scalatest.{FlatSpec, Matchers}

import firrtl.{AnnotationSeq, Parser}
import firrtl.annotations.{NoTargetAnnotation, DeletedAnnotation}
import firrtl.options.{OptionsException, PhasePrerequisiteException}
import firrtl.stage.{FirrtlCircuitAnnotation, FirrtlSourceAnnotation, InfoModeAnnotation, FirrtlFileAnnotation}
import firrtl.stage.phases.AddCircuit

import java.io.{File, FileWriter}

class AddCircuitSpec extends FlatSpec with Matchers {

  case class FooAnnotation(x: Int) extends NoTargetAnnotation
  case class BarAnnotation(x: String) extends NoTargetAnnotation

  behavior of AddCircuit.getClass.getName

  def firrtlSource(name: String): String =
    s"""|circuit $name:
        |  module $name:
        |    input a: UInt<1>
        |    output b: UInt<1>
        |    b <= not(a)
        |""".stripMargin

  it should "throw a PhasePrerequisiteException if a CircuitOption exists without an InfoModeAnnotation" in {
    {the [PhasePrerequisiteException] thrownBy AddCircuit.transform(Seq(FirrtlSourceAnnotation("foo")))}
      .message should startWith ("An InfoModeAnnotation must be present")
  }

  it should "do nothing if no CircuitOption annotations are present" in {
    val annotations = (1 to 10).map(FooAnnotation) ++
      ('a' to 'm').map(_.toString).map(BarAnnotation) :+ InfoModeAnnotation("ignore")
    AddCircuit.transform(annotations).toSeq should be (annotations.toSeq)
  }

  val (file, fileCircuit) = {
    val source = firrtlSource("foo")
    val fileName = "test_run_dir/AddCircuitSpec.fir"
    val fw = new FileWriter(new File(fileName))
    fw.write(source)
    fw.close()
    (fileName, Parser.parse(source))
  }

  val (source, sourceCircuit) = {
    val source = firrtlSource("bar")
    (source, Parser.parse(source))
  }

  it should "transform and delete CircuitOption annotations" in {
    val circuit = Parser.parse(firrtlSource("baz"))

    val annotations = Seq(
      FirrtlFileAnnotation(file),
      FirrtlSourceAnnotation(source),
      FirrtlCircuitAnnotation(circuit),
      InfoModeAnnotation("ignore") )

    val annotationsExpected = Set(
      FirrtlCircuitAnnotation(fileCircuit),
      FirrtlCircuitAnnotation(sourceCircuit),
      FirrtlCircuitAnnotation(circuit) )

    val deletionsExpected = Set(
      // AddCircuit is an object, so it gets a trailing '$'
      DeletedAnnotation(AddCircuit.name, FirrtlFileAnnotation(file)),
      DeletedAnnotation(AddCircuit.name, FirrtlSourceAnnotation(source))
    )

    val out = AddCircuit.transform(annotations).toSeq

    info("generated expected FirrtlCircuitAnnotations")
    out.collect{ case a: FirrtlCircuitAnnotation => a}.toSet should be (annotationsExpected)

    info("generated expected DeletecAnnotations")
    out.collect{ case a: DeletedAnnotation => a }.toSet should be (deletionsExpected)

    info("no FirrtlFileAnnotations or FirrtlSourceAnnotations present")
    out.collect{ case a @ (_: FirrtlFileAnnotation | _: FirrtlSourceAnnotation) => a }.toSeq should be (empty)
  }

  it should """add info for a FirrtlFileAnnotation with a "gen" info mode""" in {
    AddCircuit.transform(Seq(InfoModeAnnotation("gen"), FirrtlFileAnnotation(file)))
      .collectFirst{ case a: FirrtlCircuitAnnotation => a.circuit.serialize }
      .get should include ("AddCircuitSpec")
  }

  it should """add info for a FirrtlSourceAnnotation with an "append" info mode""" in {
    AddCircuit.transform(Seq(InfoModeAnnotation("append"), FirrtlSourceAnnotation(source)))
      .collectFirst{ case a: FirrtlCircuitAnnotation => a.circuit.serialize }
      .get should include ("anonymous source")
  }

  it should "throw an OptionsException if the specified file doesn't exist" in {
    val a = Seq(InfoModeAnnotation("ignore"), FirrtlFileAnnotation("test_run_dir/I-DO-NOT-EXIST"))

    {the [OptionsException] thrownBy AddCircuit.transform(a)}
      .message should startWith (s"Input file 'test_run_dir/I-DO-NOT-EXIST' not found")
  }

}
