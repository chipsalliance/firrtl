package firrtl

import com.typesafe.scalalogging.LazyLogging
import java.nio.file.{Paths, Files}
import java.io.{Reader, Writer}

import scala.collection.mutable
import scala.sys.process._
import scala.io.Source

import firrtl.ir._
import firrtl.passes._
import firrtl.annotations._
import firrtl.Mappers._
import firrtl.PrimOps._
import firrtl.WrappedExpression._
import Utils._
import MemPortUtils.{memPortField, memType}
// Datastructures
import scala.collection.mutable.{ArrayBuffer, LinkedHashMap, HashSet}


class RemoveComponents extends Pass {
  def makeModule(modNS: Namespace)(info: Info, name: String, moduleName: String, tpe: Type, nInputs: Int): (DefModule, Statement, Seq[Expression]) = {
    def stripType(t: Type): Type = t match {
      case g: GroundType => UIntType(UnknownWidth)
      case other => other map stripType
    }
    val inPorts = nInputs match {
      case 0 => Nil
      case 1 => Seq(Port(info, "in", Input, tpe))
      case x if x > 1 => Seq(Port(info, "in", Input, VectorType(tpe, x)))
    }
    val outPorts = Seq(Port(info, "out", Output, tpe))
    val instName = name
    val thisModName = moduleName + "_" + instName
    val mod = ExtModule(info, thisModName, inPorts ++ outPorts, moduleName, Seq(RawStringParam("type", tpe.serialize)))
    val inst = WDefInstance(info, instName, thisModName, tpe)
    val exps = create_exps(WRef(instName, Utils.module_type(mod), InstanceKind, MALE))
    (mod, inst, exps)
  }
  private val newModules = mutable.HashSet[DefModule]()
  // TODO need to first add all default module names? e.g. mux?
  //  if so, then need to rename any modules that conflict (and instances of the modules)
  private val namespace = Namespace()
  def run(c: Circuit): Circuit = {
    c.modules.foreach { m =>
      namespace.tryName(m.name)
    }
    val cx = c map removeModuleComponents map fixModuleComponents
    Circuit(cx.info, cx.modules ++ newModules, cx.main)
  }
  private def removeModuleComponents(d: DefModule): DefModule = d match {
    case e: ExtModule => e
    case m@Module(info, name, ports, body) =>
      val modNS = Namespace(m)
      val stmts = removeStatementDecs(modNS)(body)
      Module(info, name, ports, Utils.squashEmpty(stmts))
  }
  private def removeStatementDecs(modNS: Namespace)(s: Statement): Statement = s match {
    case DefWire(info, name, tpe) =>
      val (mx, sx, exps) = makeModule(modNS)(info, name, "wire", tpe, 1)
      newModules += mx
      sx
    case DefNode(info, name, value) =>
      val (sx, ex) = removeExpressionComponents(modNS)(value)
      val (nodeMod, nodeDec, exps) = makeModule(modNS)(info, name, "wire", value.tpe, 1)
      newModules += nodeMod
      Block(Seq(sx, nodeDec, Connect(info, WRef(name, value.tpe, NodeKind, FEMALE), ex)))
    case Connect(info, loc, expr) =>
      val (sx, ex) = removeExpressionComponents(modNS)(expr)
      Block(Seq(sx, Connect(info, loc, ex)))
    case i: WDefInstance => i
    case EmptyStmt => EmptyStmt
    case m: DefMemory => sys.error("DefMemory not implemented yet!")
    case r: DefRegister => r
    case i: IsInvalid => sys.error("IsInvalid not implemented yet!")
    case a: Attach => sys.error("Attach not implemented yet!")
    case a: Stop => sys.error("Stop not implemented yet!")
    case a: Print => sys.error("Print not implemented yet!")
    case b: Block => b map removeStatementDecs(modNS)
  }
  private def removeExpressionComponents(modNS: Namespace)(e: Expression): (Statement, Expression) = {
    val stmts = mutable.ArrayBuffer[Statement]()
    def onExp(e: Expression): Expression = e map onExp match {
      case w: WRef => w
      case w: WSubField => w
      case w: WSubIndex => w
      case Mux(p, tVal, fVal, tpe) =>
        val (mx, sx, exps) = makeModule(modNS)(NoInfo, modNS.newName("mux"), "mux", tpe, 3)
        newModules += mx
        stmts += Block(Seq(sx,
          Connect(NoInfo, exps(0), p),
          Connect(NoInfo, exps(1), tVal),
          Connect(NoInfo, exps(2), fVal)))
        exps(3)
      //case DoPrim(op, args, const, tpe) =>
      //  val (mx, sx, ex) = makeModule(modNS)(NoInfo, op.toString, op.toString, tpe, 3)
      //  newModules += mx
      //  stmts += sx
      //  ex
      case other => other
    }
    val newExp = onExp(e)
    (Block(stmts), newExp)
  }
  private def fixModuleComponents(m: DefModule): DefModule = m map fixStatementComponents
  private def fixStatementComponents(s: Statement): Statement = s map fixStatementComponents map fixExpressionComponents
  private def fixExpressionComponents(e: Expression): Expression = e map fixExpressionComponents match {
    case WRef(name, tpe, WireKind|NodeKind, FEMALE) =>
      WSubField(WRef(name, tpe, InstanceKind, MALE), "in", UnknownType, MALE)
    case WRef(name, tpe, WireKind|NodeKind, MALE)   =>
      WSubField(WRef(name, tpe, InstanceKind, MALE), "out", UnknownType, FEMALE)
    case w: WRef => println(w); w
    case other => other
  }
}

class CoreIREmitter extends SeqTransform with Emitter {
  def inputForm = MidForm
  def outputForm = MidForm

  def transforms = Seq(
    //Add pass to fix modules whose names conflict with default coreir modules
    new RemoveComponents()
  )

  override def execute(state: CircuitState): CircuitState = {
    val newAnnos = getMyAnnotations(state).flatMap {
      case EmitCircuitAnnotation() =>
        val writer = new java.io.StringWriter
        emit(state, writer)
        Seq(EmittedVerilogCircuitAnnotation(EmittedVerilogCircuit(state.circuit.main, writer.toString)))
      case EmitAllModulesAnnotation() =>
        sys.error("Not supported: emitting multiple modules for CoreIR")
      case _ => Seq()
    }
    val annos = newAnnos ++ (state.annotations match {
      case None => Seq.empty
      case Some(a) => a.annotations
    })
    state.copy(annotations = Some(AnnotationMap(annos)))
  }

  // Old style, deprecated
  def emit(state: CircuitState, writer: Writer): Unit = {
    val circuit = runTransforms(state).circuit
    writer.write(circuit.serialize)
    //val moduleMap = circuit.modules.map(m => m.name -> m).toMap
    //circuit.modules.foreach {
    //  case m: Module => emit_coreir(m, moduleMap)(writer)
    //  case _: ExtModule => // do nothing
    //}
  }
}
