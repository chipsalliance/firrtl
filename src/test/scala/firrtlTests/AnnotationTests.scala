// See LICENSE for license details.

package firrtlTests

import java.io.{File, FileWriter, Writer}

import firrtl.annotations.AnnotationYamlProtocol._
import firrtl.annotations._
import firrtl._
import firrtl.passes.InlineAnnotation
import firrtl.passes.memlib.PinAnnotation
import net.jcazevedo.moultingyaml._
import org.scalatest.Matchers
import logger._

/**
 * An example methodology for testing Firrtl annotations.
 */
trait AnnotationSpec extends LowTransformSpec {
  // Dummy transform
  def transform = new ResolveAndCheck

  // Check if Annotation Exception is thrown
  override def failingexecute(annotations: AnnotationMap, input: String): Exception = {
    intercept[AnnotationException] {
      compile(CircuitState(parse(input), ChirrtlForm, Some(annotations)), Seq.empty)
    }
  }
  def execute(aMap: Option[AnnotationMap], input: String, check: Annotation): Unit = {
    val cr = compile(CircuitState(parse(input), ChirrtlForm, aMap), Seq.empty)
    cr.annotations.get.annotations should contain (check)
  }
}


/**
 * Tests for Annotation Permissibility and Tenacity
 *
 * WARNING(izraelevitz): Not a complete suite of tests, requires the LowerTypes
 * pass and ConstProp pass to correctly populate its RenameMap before Strict, Rigid, Firm,
 * Unstable, Fickle, and Insistent can be tested.
 */
class AnnotationTests extends AnnotationSpec with Matchers {
  def getAMap(a: Annotation): Option[AnnotationMap] = Some(AnnotationMap(Seq(a)))
  def getAMap(as: Seq[Annotation]): Option[AnnotationMap] = Some(AnnotationMap(as))
  def anno(s: String, value: String ="this is a value"): Annotation =
    Annotation(ComponentName(s, ModuleName("Top", CircuitName("Top"))), classOf[Transform], value)

  "Loose and Sticky annotation on a node" should "pass through" in {
    val input: String =
      """circuit Top :
         |  module Top :
         |    input a : UInt<1>[2]
         |    input b : UInt<1>
         |    node c = b""".stripMargin
    val ta = anno("c", "")
    execute(getAMap(ta), input, ta)
  }

  "Annotations" should "be readable from file" in {
    val annotationStream = getClass.getResourceAsStream("/annotations/SampleAnnotations.anno")
    val annotationsYaml = scala.io.Source.fromInputStream(annotationStream).getLines().mkString("\n").parseYaml
    val annotationArray = annotationsYaml.convertTo[Array[Annotation]]
    annotationArray.length should be (9)
    annotationArray(0).targetString should be ("ModC")
    annotationArray(7).transformClass should be ("firrtl.passes.InlineInstances")
    val expectedValue = "TopOfDiamond\nWith\nSome new lines"
    annotationArray(7).value should be (expectedValue)
  }

  "Badly formatted serializations" should "return reasonable error messages" in {
    var badYaml =
      """
        |- transformClass: firrtl.passes.InlineInstances
        |  targetString: circuit.module..
        |  value: ModC.this params 16 32
      """.stripMargin.parseYaml

    var thrown = intercept[Exception] {
      badYaml.convertTo[Array[Annotation]]
    }
    thrown.getMessage should include ("Illegal component name")

    badYaml =
      """
        |- transformClass: firrtl.passes.InlineInstances
        |  targetString: .circuit.module.component
        |  value: ModC.this params 16 32
      """.stripMargin.parseYaml

    thrown = intercept[Exception] {
      badYaml.convertTo[Array[Annotation]]
    }
    thrown.getMessage should include ("Illegal circuit name")
  }

  "Round tripping annotations through text file" should "preserve annotations" in {
    val annos: Array[Annotation] = Seq(
      InlineAnnotation(CircuitName("fox")),
      InlineAnnotation(ModuleName("dog", CircuitName("bear"))),
      InlineAnnotation(ComponentName("chocolate", ModuleName("like", CircuitName("i")))),
      PinAnnotation(CircuitName("Pinniped"), Seq("sea-lion", "monk-seal"))
    ).toArray

    val annoFile = new File("temp-anno")
    val writer = new FileWriter(annoFile)
    writer.write(annos.toYaml.prettyPrint)
    writer.close()

    val yaml = io.Source.fromFile(annoFile).getLines().mkString("\n").parseYaml
    annoFile.delete()

    val readAnnos = yaml.convertTo[Array[Annotation]]

    annos.zip(readAnnos).foreach { case (beforeAnno, afterAnno) =>
      beforeAnno.targetString should be (afterAnno.targetString)
      beforeAnno.target should be (afterAnno.target)
      beforeAnno.transformClass should be (afterAnno.transformClass)
      beforeAnno.transform should be (afterAnno.transform)

      beforeAnno should be (afterAnno)
    }
  }

  "Deleting annotations" should "create a DeletedAnnotation" in {
    val compiler = new VerilogCompiler
    val input =
     """circuit Top :
        |  module Top :
        |    input in: UInt<3>
        |""".stripMargin
    class DeletingTransform extends Transform {
      val inputForm = LowForm
      val outputForm = LowForm
      def execute(state: CircuitState) = state.copy(annotations = None)
    }
    val inlineAnn = InlineAnnotation(CircuitName("Top"))
    val result = compiler.compile(CircuitState(parse(input), ChirrtlForm, getAMap(inlineAnn)), Seq(new DeletingTransform))
    result.annotations.get.annotations.head should matchPattern {
      case DeletedAnnotation(x, inlineAnn) =>
    }
    val exception = (intercept[FIRRTLException] {
      result.getEmittedCircuit
    })
    val deleted = result.deletedAnnotations
    exception.str should be (s"No EmittedCircuit found! Did you delete any annotations?\n$deleted")
  }

  "Renaming" should "propagate in Lowering of memories" in {
    val compiler = new VerilogCompiler
    // Uncomment to help debugging failing tests
    // Logger.setClassLogLevels(Map(compiler.getClass.getName -> LogLevel.Debug))
    val input =
     """circuit Top :
        |  module Top :
        |    input clk: Clock
        |    input in: UInt<3>
        |    mem m: 
        |      data-type => {a: UInt<4>, b: UInt<4>[2]}
        |      depth => 8
        |      write-latency => 1
        |      read-latency => 0
        |      reader => r
        |    m.r.clk <= clk
        |    m.r.en <= UInt(1)
        |    m.r.addr <= in
        |""".stripMargin
    val annos = Seq(anno("m.r.data.b", "sub"), anno("m.r.data", "all"), anno("m", "mem"))
    val result = compiler.compile(CircuitState(parse(input), ChirrtlForm, getAMap(annos)), Nil)
    val resultAnno = result.annotations.get.annotations
    resultAnno should contain (anno("m_a", "mem"))
    resultAnno should contain (anno("m_b_0", "mem"))
    resultAnno should contain (anno("m_b_1", "mem"))
    resultAnno should contain (anno("m_a.r.data", "all"))
    resultAnno should contain (anno("m_b_0.r.data", "all"))
    resultAnno should contain (anno("m_b_1.r.data", "all"))
    resultAnno should contain (anno("m_b_0.r.data", "sub"))
    resultAnno should contain (anno("m_b_1.r.data", "sub"))
    resultAnno should not contain (anno("m"))
    resultAnno should not contain (anno("r"))
  }

  "Renaming" should "propagate in RemoveChirrtl and Lowering of memories" in {
    val compiler = new VerilogCompiler
    Logger.setClassLogLevels(Map(compiler.getClass.getName -> LogLevel.Debug))
    val input =
     """circuit Top :
        |  module Top :
        |    input clk: Clock
        |    input in: UInt<3>
        |    cmem m: {a: UInt<4>, b: UInt<4>[2]}[8]
        |    read mport r = m[in], clk
        |""".stripMargin
    val annos = Seq(anno("r.b", "sub"), anno("r", "all"), anno("m", "mem"))
    val result = compiler.compile(CircuitState(parse(input), ChirrtlForm, getAMap(annos)), Nil)
    val resultAnno = result.annotations.get.annotations
    resultAnno should contain (anno("m_a", "mem"))
    resultAnno should contain (anno("m_b_0", "mem"))
    resultAnno should contain (anno("m_b_1", "mem"))
    resultAnno should contain (anno("m_a.r.data", "all"))
    resultAnno should contain (anno("m_b_0.r.data", "all"))
    resultAnno should contain (anno("m_b_1.r.data", "all"))
    resultAnno should contain (anno("m_b_0.r.data", "sub"))
    resultAnno should contain (anno("m_b_1.r.data", "sub"))
    resultAnno should not contain (anno("m"))
    resultAnno should not contain (anno("r"))
  }

  "Renaming" should "propagate in ZeroWidth" in {
    val compiler = new VerilogCompiler
    val input =
     """circuit Top :
        |  module Top :
        |    input zero: UInt<0>
        |    wire x: {a: UInt<3>, b: UInt<0>}
        |    wire y: UInt<0>[3]
        |    y[0] <= zero
        |    y[1] <= zero
        |    y[2] <= zero
        |    x.a <= zero
        |    x.b <= zero
        |""".stripMargin
    val annos = Seq(anno("zero"), anno("x.a"), anno("x.b"), anno("y[0]"), anno("y[1]"), anno("y[2]"))
    val result = compiler.compile(CircuitState(parse(input), ChirrtlForm, getAMap(annos)), Nil)
    val resultAnno = result.annotations.get.annotations
    resultAnno should contain (anno("x_a"))
    resultAnno should not contain (anno("zero"))
    resultAnno should not contain (anno("x.a"))
    resultAnno should not contain (anno("x.b"))
    resultAnno should not contain (anno("x_b"))
    resultAnno should not contain (anno("y[0]"))
    resultAnno should not contain (anno("y[1]"))
    resultAnno should not contain (anno("y[2]"))
    resultAnno should not contain (anno("y_0"))
    resultAnno should not contain (anno("y_1"))
    resultAnno should not contain (anno("y_2"))
  }

  "Renaming subcomponents" should "propagate in Lowering" in {
    val compiler = new VerilogCompiler
    val input =
     """circuit Top :
        |  module Top :
        |    input clk: Clock
        |    input pred: UInt<1>
        |    input in: {a: UInt<3>, b: UInt<3>[2]}
        |    output out: {a: UInt<3>, b: UInt<3>[2]}
        |    wire w: {a: UInt<3>, b: UInt<3>[2]}
        |    w is invalid
        |    node n = mux(pred, in, w)
        |    out <= n
        |    reg r: {a: UInt<3>, b: UInt<3>[2]}, clk
        |    cmem mem: {a: UInt<3>, b: UInt<3>[2]}[8]
        |    write mport write = mem[pred], clk
        |    write <= in
        |""".stripMargin
    val annos = Seq(
      anno("in.a"), anno("in.b[0]"), anno("in.b[1]"),
      anno("out.a"), anno("out.b[0]"), anno("out.b[1]"),
      anno("w.a"), anno("w.b[0]"), anno("w.b[1]"),
      anno("n.a"), anno("n.b[0]"), anno("n.b[1]"),
      anno("r.a"), anno("r.b[0]"), anno("r.b[1]"),
      anno("write.a"), anno("write.b[0]"), anno("write.b[1]")
    )
    val result = compiler.compile(CircuitState(parse(input), ChirrtlForm, getAMap(annos)), Nil)
    val resultAnno = result.annotations.get.annotations
    resultAnno should not contain (anno("in.a"))
    resultAnno should not contain (anno("in.b[0]"))
    resultAnno should not contain (anno("in.b[1]"))
    resultAnno should not contain (anno("out.a"))
    resultAnno should not contain (anno("out.b[0]"))
    resultAnno should not contain (anno("out.b[1]"))
    resultAnno should not contain (anno("w.a"))
    resultAnno should not contain (anno("w.b[0]"))
    resultAnno should not contain (anno("w.b[1]"))
    resultAnno should not contain (anno("n.a"))
    resultAnno should not contain (anno("n.b[0]"))
    resultAnno should not contain (anno("n.b[1]"))
    resultAnno should not contain (anno("r.a"))
    resultAnno should not contain (anno("r.b[0]"))
    resultAnno should not contain (anno("r.b[1]"))
    resultAnno should contain (anno("in_a"))
    resultAnno should contain (anno("in_b_0"))
    resultAnno should contain (anno("in_b_1"))
    resultAnno should contain (anno("out_a"))
    resultAnno should contain (anno("out_b_0"))
    resultAnno should contain (anno("out_b_1"))
    resultAnno should contain (anno("w_a"))
    resultAnno should contain (anno("w_b_0"))
    resultAnno should contain (anno("w_b_1"))
    resultAnno should contain (anno("n_a"))
    resultAnno should contain (anno("n_b_0"))
    resultAnno should contain (anno("n_b_1"))
    resultAnno should contain (anno("r_a"))
    resultAnno should contain (anno("r_b_0"))
    resultAnno should contain (anno("r_b_1"))
    resultAnno should contain (anno("mem_a.write.data"))
    resultAnno should contain (anno("mem_b_0.write.data"))
    resultAnno should contain (anno("mem_b_1.write.data"))
  }

  "Renaming components" should "expand in Lowering" in {
    val compiler = new VerilogCompiler
    val input =
     """circuit Top :
        |  module Top :
        |    input clk: Clock
        |    input pred: UInt<1>
        |    input in: {a: UInt<3>, b: UInt<3>[2]}
        |    output out: {a: UInt<3>, b: UInt<3>[2]}
        |    wire w: {a: UInt<3>, b: UInt<3>[2]}
        |    w is invalid
        |    node n = mux(pred, in, w)
        |    out <= n
        |    reg r: {a: UInt<3>, b: UInt<3>[2]}, clk
        |""".stripMargin
    val annos = Seq(anno("in"), anno("out"), anno("w"), anno("n"), anno("r"))
    val result = compiler.compile(CircuitState(parse(input), ChirrtlForm, getAMap(annos)), Nil)
    val resultAnno = result.annotations.get.annotations
    resultAnno should contain (anno("in_a"))
    resultAnno should contain (anno("in_b_0"))
    resultAnno should contain (anno("in_b_1"))
    resultAnno should contain (anno("out_a"))
    resultAnno should contain (anno("out_b_0"))
    resultAnno should contain (anno("out_b_1"))
    resultAnno should contain (anno("w_a"))
    resultAnno should contain (anno("w_b_0"))
    resultAnno should contain (anno("w_b_1"))
    resultAnno should contain (anno("n_a"))
    resultAnno should contain (anno("n_b_0"))
    resultAnno should contain (anno("n_b_1"))
    resultAnno should contain (anno("r_a"))
    resultAnno should contain (anno("r_b_0"))
    resultAnno should contain (anno("r_b_1"))
  }

  "Renaming subcomponents that aren't leaves" should "expand in Lowering" in {
    val compiler = new VerilogCompiler
    val input =
     """circuit Top :
        |  module Top :
        |    input clk: Clock
        |    input pred: UInt<1>
        |    input in: {a: UInt<3>, b: UInt<3>[2]}
        |    output out: {a: UInt<3>, b: UInt<3>[2]}
        |    wire w: {a: UInt<3>, b: UInt<3>[2]}
        |    w is invalid
        |    node n = mux(pred, in, w)
        |    out <= n
        |    reg r: {a: UInt<3>, b: UInt<3>[2]}, clk
        |""".stripMargin
    val annos = Seq(anno("in.b"), anno("out.b"), anno("w.b"), anno("n.b"), anno("r.b"))
    val result = compiler.compile(CircuitState(parse(input), ChirrtlForm, getAMap(annos)), Nil)
    val resultAnno = result.annotations.get.annotations
    resultAnno should contain (anno("in_b_0"))
    resultAnno should contain (anno("in_b_1"))
    resultAnno should contain (anno("out_b_0"))
    resultAnno should contain (anno("out_b_1"))
    resultAnno should contain (anno("w_b_0"))
    resultAnno should contain (anno("w_b_1"))
    resultAnno should contain (anno("n_b_0"))
    resultAnno should contain (anno("n_b_1"))
    resultAnno should contain (anno("r_b_0"))
    resultAnno should contain (anno("r_b_1"))
  }


  "Renaming" should "track dce" in {
    val compiler = new VerilogCompiler
    val input =
     """circuit Top :
        |  module Top :
        |    input clk: Clock
        |    input pred: UInt<1>
        |    input in: {a: UInt<3>, b: UInt<3>[2]}
        |    output out: {a: UInt<3>, b: UInt<3>[2]}
        |    node n = in
        |    out <= n
        |""".stripMargin
    val annos = Seq(
      anno("in.a"), anno("in.b[0]"), anno("in.b[1]"),
      anno("out.a"), anno("out.b[0]"), anno("out.b[1]"),
      anno("n.a"), anno("n.b[0]"), anno("n.b[1]")
    )
    val result = compiler.compile(CircuitState(parse(input), ChirrtlForm, getAMap(annos)), Nil)
    val resultAnno = result.annotations.get.annotations
    resultAnno should not contain (anno("in.a"))
    resultAnno should not contain (anno("in.b[0]"))
    resultAnno should not contain (anno("in.b[1]"))
    resultAnno should not contain (anno("out.a"))
    resultAnno should not contain (anno("out.b[0]"))
    resultAnno should not contain (anno("out.b[1]"))
    resultAnno should not contain (anno("n.a"))
    resultAnno should not contain (anno("n.b[0]"))
    resultAnno should not contain (anno("n.b[1]"))
    resultAnno should not contain (anno("n_a"))
    resultAnno should not contain (anno("n_b_0"))
    resultAnno should not contain (anno("n_b_1"))
    resultAnno should contain (anno("in_a"))
    resultAnno should contain (anno("in_b_0"))
    resultAnno should contain (anno("in_b_1"))
    resultAnno should contain (anno("out_a"))
    resultAnno should contain (anno("out_b_0"))
    resultAnno should contain (anno("out_b_1"))
  }
}
