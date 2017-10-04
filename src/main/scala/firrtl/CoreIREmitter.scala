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

  /* Contains the module namespace of this circuit
   * Gets populated when calling run
   *
   * TODO(azidar) need to first add all default module names? e.g. mux?
   * if so, then need to rename any modules that conflict (and instances of the modules)
   */
  private val namespace = Namespace()

  /* Contains all new Modules that we create during the pass */
  private val newModules = mutable.HashSet[DefModule]()

  /* Returns a circuit where all circuit components are ripped out into external modules
   *
   * Currently implemented: Wire, DefNode, Mux
   * TODO: Register, Memory, DoPrim, Stop, Print, IsInvalid, Attach
   */
  def run(c: Circuit): Circuit = {
    c.modules.foreach { m =>
      namespace.tryName(m.name)
    }
    val cx = c map removeModuleComponents
    Circuit(cx.info, cx.modules ++ newModules, cx.main)
  }

  /* Returns an external module for a given component, with a name, intended module name, type, and number of inputs
   * Also returns a declaration of an instance of the module.
   * Finally, returns a sequence of expressions that reference io port.
   *
   * Looks into and updates namespace.
   */
  def makeModule(info: Info, name: String, moduleName: String, tpe: Type, nInputs: Int): (DefModule, Statement, Seq[Expression]) = {
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
    val thisModName = namespace.newName(moduleName)
    val mod = ExtModule(info, thisModName, inPorts ++ outPorts, moduleName, Seq(RawStringParam("type", tpe.serialize)))
    val inst = WDefInstance(info, instName, thisModName, tpe)
    val exps = create_exps(WRef(instName, Utils.module_type(mod), InstanceKind, MALE))
    (mod, inst, exps)
  }

  /* Returns a new module with components ripped out.
   * Populates newModules.
   */
  private def removeModuleComponents(d: DefModule): DefModule = d match {
    case e: ExtModule => e
    case m@Module(info, name, ports, body) =>
      val modNS = Namespace(m)
      val stmts = removeStatementDecs(modNS)(body)
      Module(info, name, ports, Utils.squashEmpty(stmts))
  }

  /* Returns a new statement with components ripped out.
   * Requires a module's internal namespace modNS, in case we need to generate a temporary node.
   * Populates newModules and namespace.
   *
   * Note that wires and nodes are replaced with isntances, but their references are not fixed until
   */
  private def removeStatementDecs(modNS: Namespace)(s: Statement): Statement = s match {
    case DefWire(info, name, tpe) =>
      val (mx, sx, exps) = makeModule(info, name, "WIRE", tpe, 1)
      newModules += mx
      sx
    case DefNode(info, name, value) =>
      val (sx, ex) = removeExpressionComponents(modNS)(value)
      val (nodeMod, nodeDec, exps) = makeModule(info, name, "WIRE", value.tpe, 1)
      newModules += nodeMod
      Block(Seq(sx, nodeDec, Connect(info, WSubField(WRef(name, ex.tpe, InstanceKind, FEMALE), "in", UnknownType, MALE), ex)))
    case Connect(info, loc, expr) =>
      val (dc, locx) = removeExpressionComponents(modNS)(loc)
      val (sx, ex) = removeExpressionComponents(modNS)(expr)
      Block(Seq(sx, Connect(info, locx, ex)))
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

  /* Takes an expression and rips out components into instance declarations.
   * Returns a reference to the output of the expreesion, as well as a sequence of statements
   *   containing all nested expression's new declarations and connections
   * Requires a module's internal namespace modNS, in case we need to generate a temporary node.
   * Populates newModules and namespace.
   */
  private def removeExpressionComponents(modNS: Namespace)(e: Expression): (Statement, Expression) = {
    val stmts = mutable.ArrayBuffer[Statement]()
    /* Recursive walk on Expression
     * Fixes references to wires and nodes to use .in and .out
     */
    def onExp(e: Expression): Expression = e map onExp match {
      case WRef(name, tpe, WireKind|RegKind, FEMALE) =>
        WSubField(WRef(name, tpe, InstanceKind, FEMALE), "in", UnknownType, MALE)
      case WRef(name, tpe, WireKind|NodeKind|RegKind, MALE)   =>
        WSubField(WRef(name, tpe, InstanceKind, MALE), "out", UnknownType, FEMALE)
      case w: WRef => w
      case w: WSubField => w
      case w: WSubIndex => w
      case Mux(p, tVal, fVal, tpe) =>
        val (mx, sx, exps) = makeModule(NoInfo, modNS.newName("mux"), "MUX", tpe, 3)
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
}


/* Translates a MidForm Firrtl into CoreIR
 *
 * First, we need to remove all FIRRTL components and represent them as
 *  an external module + declaration and connections to IO
 *
 * Then, we will emit the CoreIR.
 *
 * Currently, implementation just emits FIRRTL because the CoreIR syntax
 *  is unknown or in flux.
 */
class CoreIREmitter extends SeqTransform with Emitter {
  def inputForm = MidForm
  def outputForm = MidForm

  def transforms = Seq(
    // TODO(azidar): Add pass to fix modules whose names conflict with default coreir modules
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
