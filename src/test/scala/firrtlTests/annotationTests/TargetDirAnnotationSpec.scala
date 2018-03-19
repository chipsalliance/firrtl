// See LICENSE for license details.

package firrtlTests
package annotationTests

import firrtlTests._
import firrtl._
import firrtl.annotations.{Annotation, NoTargetAnnotation, SingleStringAnnotation}

/** Annotation emitted if [[FindTargetDirTransform]] runs */
case class FlockOfSeagullsAnnotation() extends NoTargetAnnotation
/** Annotation emitted with the target dir [[FindTargetDirTransform]] finds */
case class FoundTargetDirAnnotation(value: String) extends SingleStringAnnotation

/** Looks for [[TargetDirAnnotation]] */
class FindTargetDirTransform extends Transform {
  def inputForm = HighForm
  def outputForm = HighForm
  def execute(state: CircuitState): CircuitState = {
    var annosx: List[Annotation] = List(FlockOfSeagullsAnnotation())
    state.annotations.collectFirst {
      case TargetDirAnnotation(e) => annosx :+= new FoundTargetDirAnnotation(e) }
    state.copy(annotations = state.annotations ++ annosx)
  }
}

class TargetDirAnnotationSpec extends FirrtlFlatSpec {
  behavior of "The target directory"

  val input =
    """circuit Top :
      |  module Top :
      |    input foo : UInt<32>
      |    output bar : UInt<32>
      |    bar <= foo
      """.stripMargin
  val targetDir = "a/b/c"

  /** Given some annotations, check to see if [[FindTargetDirTRansform]] ran */
  def itRan(implicit annos: AnnotationSeq): Boolean = annos
    .collectFirst{ case a: FlockOfSeagullsAnnotation => a }
    .nonEmpty

  /** Extract the target directory from a [[FoundTargetDirAnnotation]] if one exists */
  def foundTargetDir(implicit annos: AnnotationSeq): Option[String] = annos
    .collectFirst{ case FoundTargetDirAnnotation(a) => a }

  it should "be available as an annotation when using execution options" in {
    val optionsManager = new ExecutionOptionsManager(
      "TargetDir",
      Array("--target-dir", targetDir,
            "--top-name", "Top",
            "--compiler", "high",
            "--firrtl-source", input,
            "--custom-transforms", "firrtlTests.annotationTests.FindTargetDirTransform") ) with HasFirrtlOptions
    implicit val annos = Driver.execute(optionsManager) match {
      case success: FirrtlExecutionSuccess => success.circuitState.annotations
      case _ => throw new Exception("Driver failed to run to completion") }

    // Check that FindTargetDirTransform transform is run and finds the annotation
    itRan should be (true)
    foundTargetDir should be (Some(targetDir))

    // Delete created directory
    val dir = new java.io.File(targetDir)
    dir.exists should be (true)
    FileUtils.deleteDirectoryHierarchy("a") should be (true)
  }

  it should "NOT be available as an annotation when using a raw compiler" in {
    val findTargetDir = new FindTargetDirTransform // looks for the annotation
    val compiler = new VerilogCompiler
    val circuit = Parser.parse(input split "\n")
    implicit val annos = compiler
      .compileAndEmit(CircuitState(circuit, HighForm), Seq(findTargetDir))
      .annotations

    // Check that FindTargetDirTransform does not find the annotation
    itRan should be (true)
    foundTargetDir should be (None)
  }
}
