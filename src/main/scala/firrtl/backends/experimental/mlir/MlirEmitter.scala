package firrtl

import firrtl.Utils.error
import firrtl.annotations.NoTargetAnnotation
import firrtl.ir._
import firrtl.options.CustomFileEmission
import firrtl.options.Viewer.view
import firrtl.stage.FirrtlOptions
import firrtl.stage.TransformManager.TransformDependency

import java.io.{StringWriter, Writer}

case class EmittedMlirCircuitAnnotation(name: String, value: String)
    extends NoTargetAnnotation
    with CustomFileEmission {
  def baseFileName(annotations: AnnotationSeq): String =
    view[FirrtlOptions](annotations).outputFileName.getOrElse(name)
  def suffix:   Option[String] = Some("mlir")
  def getBytes: Iterable[Byte] = value.getBytes
}

sealed abstract class MlirEmitter(form: Seq[TransformDependency])
    extends Transform
    with Emitter
    with DependencyAPIMigration {
  override def prerequisites = form
  override def optionalPrerequisites = Seq.empty
  override def optionalPrerequisiteOf = Seq.empty
  override def invalidates(a: Transform) = false
  override def emit(state:    CircuitState, writer: Writer): Unit = error("Deprecated since firrtl 1.0!")
  private val writer: Writer = new StringWriter

  private def emitCircuit(circuit: Circuit) = {
    writer.write(s"firrtl.circuit ${circuit.main} {\n")
    // write body
    circuit.modules.foreach(emitDefModule)
    writer.write("}")
  }
  private def emitDefModule(module: DefModule) = module match {
    case m: Module    => emitModule(m)
    case m: ExtModule => emitExtModule(m)
  }
  // Module
  private def emitModule(module: Module) = {
    // name
    writer.write(s"firrtl.module @${module.name}(")
    // ports
    module.ports.foreach(emitPort)
    writer.write(") {\n")
    // body
    // TODO: emit body
    writer.write("}")
  }
  private def emitExtModule(module: ExtModule) = {
    // name
    writer.write(s"firrtl.extmodule @${module.name}(")
    // ports
    module.ports.foreach(emitPort)
    writer.write(")")
    // TODO: implement attr
  }
  private def emitPort(port: Port) = {
    writer.write((port.direction match {
      case Input  => "in"
      case Output => "out"
    }) + s" : ${emitType(port.tpe)}")
  }
  // Type
  private def emitType(tpe: Type) = {
    tpe match {
      case ClockType      => emitClockType()
      case ClockType      => emitClockType
      case ResetType      => emitResetType
      case AsyncResetType => emitAsyncResetType
      case t: SIntType   => emitSIntType(t)
      case t: UIntType   => emitUIntType(t)
      case t: AnalogType => emitAnalogType(t)
      case t: BundleType => emitBundleType(t)
      case t: VectorType => emitVectorType(t)
    }
  }
  private def emitClockType() = writer.write("!firrtl.clock")
  private def emitResetType() = writer.write("!firrtl.reset")
  private def emitAsyncResetType() = writer.write("!firrtl.asyncreset")
  private def emitSIntType(tpe: SIntType) = {
    writer.write("!firrtl.sint")
    emitWidth(tpe.width)
  }
  private def emitUIntType(tpe: UIntType) = {
    writer.write("!firrtl.uint")
    emitWidth(tpe.width)
  }
  private def emitWidth(width: Width) = {
    writer.write(width match {
      case IntWidth(w)  => s"<$w>"
      case UnknownWidth => ""
      case _            => error("only support IntWidth and UnknownWidth")
    })
  }
  private def emitAnalogType(tpe: AnalogType) = {
    writer.write("!firrtl.analog")
    emitWidth(tpe.width)
  }
  private def emitBundleType(tpe: BundleType) = {
    writer.write("!firrtl.bundle<")
    tpe.fields.foreach { f =>
      emitField(f)
      writer.write(",")
    }
    writer.write(">")
  }
  private def emitVectorType(tpe: VectorType) = {
    writer.write("!firrtl.vector<")
    emitType(tpe.tpe)
    writer.write(s", ${tpe.size}>")
  }
  private def emitField(field: Field) = {
    writer.write(field.name)
    writer.write(" : ")
    emitOrientation(field.flip)
    emitType(field.tpe)
  }
  private def emitOrientation(flip: Orientation) =
    flip match {
      case Flip    => writer.write("flip ")
      case Default =>
    }

  override protected def execute(state: CircuitState): CircuitState = {
    state.annotations.map {
      case EmitCircuitAnnotation(a) if this.getClass == a =>
        EmittedMlirCircuitAnnotation(state.circuit.main, emitCircuit(state.circuit))
      case EmitAllModulesAnnotation(a) if this.getClass == a => error("EmitAllModulesAnnotation not supported!")
      case _                                                 =>
    }

    state.copy(annotations = state.annotations :+)
  }
}
