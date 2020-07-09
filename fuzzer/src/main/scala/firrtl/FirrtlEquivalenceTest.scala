package firrtl.fuzzer

import firrtl._
import firrtl.annotations.{
  Annotation,
  CircuitTarget,
  ModuleTarget,
  Target
}
import firrtl.ir.Circuit
import firrtl.stage.{FirrtlCircuitAnnotation, InfoModeAnnotation, OutputFileAnnotation, TransformManager}
import firrtl.stage.phases.WriteEmitted
import firrtl.transforms.ManipulateNames
import firrtl.util.BackendCompilationUtilities

import java.io.{File, FileWriter}

object FirrtlEquivalenceTestUtils {

  private class AddSuffixToTop(suffix: String) extends ManipulateNames[AddSuffixToTop] {
    override def manipulate = (a: String, b: Namespace) => Some(b.newName(a + suffix))

    /** Return a circuit state with all sensitive names manipulated */
    override def execute(state: CircuitState): CircuitState = {

      val block = (_: Target) => false

      val allow: Target => Boolean = {
        case _: ModuleTarget => true
        case _: CircuitTarget => true
        case _: Target => false
      }

      val renames = RenameMap()
      val circuitx = run(state.circuit, renames, block, allow)


      state.copy(circuit = circuitx, renames = Some(renames))
    }
  }

  private def writeEmitted(state: CircuitState, outputFile: String): Unit = {
    (new WriteEmitted).transform(state.annotations :+ OutputFileAnnotation(outputFile))
  }

  def firrtlEquivalenceTestPass(circuit: Circuit,
                            referenceCompiler: TransformManager,
                            referenceAnnos: Seq[Annotation],
                            customCompiler: TransformManager,
                            customAnnos: Seq[Annotation],
                            testDir: File,
                            timesteps: Int = 1): Boolean = {
    val baseAnnos = Seq(
      InfoModeAnnotation("ignore"),
      FirrtlCircuitAnnotation(circuit)
    )

    testDir.mkdirs()

    val customTransforms = Seq(
      customCompiler,
      new AddSuffixToTop("_custom"),
      new VerilogEmitter
    )
    val customResult = customTransforms.foldLeft(CircuitState(
      circuit,
      ChirrtlForm,
      baseAnnos ++:
      EmitCircuitAnnotation(classOf[VerilogEmitter]) +:
      customAnnos
    )) { case (state, transform) => transform.transform(state) }
    val customName = customResult.circuit.main
    val customOutputFile = new File(testDir, s"$customName.v")
    writeEmitted(customResult, customOutputFile.toString)

    val referenceTransforms = Seq(
      referenceCompiler,
      new AddSuffixToTop("_reference"),
      new MinimumVerilogEmitter
    )
    val referenceResult = referenceTransforms.foldLeft(CircuitState(
      circuit,
      ChirrtlForm,
      baseAnnos ++:
      EmitCircuitAnnotation(classOf[MinimumVerilogEmitter]) +:
      referenceAnnos
    )) { case (state, transform) => transform.transform(state) }
    val referenceName = referenceResult.circuit.main
    val referenceOutputFile = new File(testDir, s"$referenceName.v")
    writeEmitted(referenceResult, referenceOutputFile.toString)

    BackendCompilationUtilities.yosysExpectSuccess(customName, referenceName, testDir, timesteps)
  }
}
