// See LICENSE for license details.

package firrtl

import java.io.Writer

import firrtl.Mappers._
import firrtl.PrimOps._
import firrtl.Utils.{create_exps, error}
import firrtl.ir._
import firrtl.passes._

import play.api.libs.json.Json._
import play.api.libs.json._
import coreir.json.TypeWrites._
import scala.collection.mutable
// Datastructures


//noinspection ScalaStyle
class RemoveComponents extends Pass {
  override val inputForm = MidForm
  override val outputForm = MidForm
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
  def makeModule(
                  info: Info,
                  name: String,
                  moduleName: String,
                  ref: String,
                  tpe: Type,
                  nInputs: Int
                ): (DefModule, Statement, Seq[Expression]) = {
    /*
    def stripType(t: Type): Type = t match {
      case _: GroundType => UIntType(UnknownWidth)
      case other => other map stripType
    }
    */
    val inPorts = nInputs match {
      case 0 => Nil
      case 1 => Seq(Port(info, "in", Input, tpe))
      case 3 if (moduleName == "MUX") =>
        Seq(
          Port(info, "sel", Input, UIntType(IntWidth(1))),
          Port(info, "in0", Input, tpe),
          Port(info, "in1", Input, tpe)
        )
      case x if x > 1 => Range(0, x).map(n => Port(info, s"in$n", Input, tpe))
    }
    val outPorts = Seq(Port(info, "out", Output, tpe))
    val instName = name
    val thisModName = namespace.newName(moduleName)
    val width = tpe match {
      case GroundType(IntWidth(n)) => n
      case GroundType(_) => error("Don't support non-inferred types")
      case _ => error("Don't support bundled types")
    }
    val mod = ExtModule(info, thisModName, inPorts ++ outPorts, moduleName, Seq(IntParam("width", width), RawStringParam("genref", ref)))
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
   * Note that wires and nodes are replaced with instances, but their references are not fixed until
   */
  private def removeStatementDecs(modNS: Namespace)(s: Statement): Statement = s match {
    case DefWire(info, name, tpe) =>
      val (mx, sx, exps) = makeModule(info, name, "WIRE", "coreir.wire", tpe, 1)
      newModules += mx
      sx
    case DefNode(info, name, value) =>
      val (sx, ex) = removeExpressionComponents(modNS)(value)
      val (nodeMod, nodeDec, exps) = makeModule(info, name, "WIRE", "coreir.wire", value.tpe, 1)
      newModules += nodeMod
      Block(Seq(sx, nodeDec, Connect(info, exps.head, ex)))
    case Connect(info, loc, expr) =>
      val (dc, locx) = removeExpressionComponents(modNS)(loc)
      val (sx, ex) = removeExpressionComponents(modNS)(expr)
      Block(Seq(sx, Connect(info, locx, ex)))
    case i: WDefInstance => i
    case EmptyStmt => EmptyStmt
    case m: DefMemory => sys.error("DefMemory not implemented yet!")
    case r: DefRegister =>
      val (rx, sx, ports) = makeModule(NoInfo, r.name, "REG", "mantle.reg", r.tpe, 4)
      val input :: clock :: reset :: initValue :: tail = ports
      newModules += rx
      Block(Seq(sx,
        Connect(NoInfo, clock, r.clock),
        Connect(NoInfo, reset, r.reset),
        Connect(NoInfo, initValue, r.init)
      ))
    case i: IsInvalid => sys.error("IsInvalid not implemented yet!")
    case a: Attach => sys.error("Attach not implemented yet!")
    case a: Stop => sys.error("Stop not implemented yet!")
    case a: Print => sys.error("Print not implemented yet!")
    case b: Block => b map removeStatementDecs(modNS)
  }

  /* Takes an expression and rips out components into instance declarations.
   * Returns a reference to the output of the expression, as well as a sequence of statements
   *   containing all nested expression's new declarations and connections
   * Requires a module's internal namespace modNS, in case we need to generate a temporary node.
   * Populates newModules and namespace.
   */
  private def removeExpressionComponents(modNS: Namespace)(e: Expression): (Statement, Expression) = {
    val stmts = mutable.ArrayBuffer[Statement]()
    //noinspection ScalaStyle
    /* Recursive walk on Expression
         * Fixes references to wires and nodes to use .in and .out
         */
    def onExp(e: Expression): Expression = e map onExp match {
      case WRef(name, tpe, WireKind, FEMALE) =>
        WSubField(WRef(name, tpe, InstanceKind, FEMALE), "in", UnknownType, MALE)
      case WRef(name, tpe, RegKind, FEMALE) =>
        val regRef = WRef(name, tpe, InstanceKind, FEMALE)
        val in = WSubField(regRef, "in", UnknownType, MALE)
        WSubIndex(in, 3, UnknownType, MALE)
      case WRef(name, tpe, WireKind|NodeKind|RegKind, MALE)   =>
        WSubField(WRef(name, tpe, InstanceKind, MALE), "out", UnknownType, FEMALE)
      case w: WRef => w
      case w: WSubField => w
      case w: WSubIndex => w
      case Mux(p, tVal, fVal, tpe) =>
        val (mx, sx, exps) = makeModule(NoInfo, modNS.newName("mux"), "MUX", "coreir.mux", tpe, 3)
        newModules += mx
        stmts += Block(Seq(sx,
          Connect(NoInfo, exps.head, p),
          Connect(NoInfo, exps(1), tVal),
          Connect(NoInfo, exps(2), fVal)))
        exps(3)
      case d@DoPrim(op, args, const, tpe) =>
        val nInputs = op match {
          case Add | Sub | Mul | Div | Rem | Lt | Leq | Gt | Geq |
               Eq | Neq | Dshl | Dshr | And | Or | Xor | Cat => 2
          case AsUInt | AsSInt | AsClock | Cvt | Neq | Not => 1
          case AsFixedPoint | Pad | Shl | Shr | Head | Tail | BPShl | BPShr | BPSet => 2
          case Bits => 3
          case Andr | Orr | Xorr | Neg => 1
        }
        val nConsts = const.length
        assert(nConsts == 0)
        val (mx, sx, exps) = makeModule(NoInfo, modNS.newName(op.toString), op.toString, s"coreir.${op.toString().toLowerCase()}", tpe, nInputs)
        stmts += sx
        newModules += mx
        (args ++ (const.map(SIntLiteral(_, UnknownWidth)))).zipWithIndex.foreach { case ((input, index)) =>
          stmts += Connect(NoInfo, exps(index), input)
        }
        exps.last
      case other => other
    }
    val newExp = onExp(e)
    (Block(stmts), newExp)
  }
}


/* Translates a MidForm Firrtl to CoreIR
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
  def inputForm: CircuitForm = MidForm
  def outputForm: CircuitForm = MidForm

  def transforms: Seq[Transform] = Seq(
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
    println(circuit.serialize)
    /* Map module name to ir.DefModule */
    val modules = circuit.modules.foldLeft(Map.empty[String, DefModule]) {
        case (map, module) => map + (module.name -> module)
    }
    /* Per module instances */
    val instances = mutable.HashMap[String, Set[WDefInstance]]()
    /* Per module connections */
    val connections = mutable.HashMap[String, Set[Connect]]()

    /* Populate instances and connections */
    circuit.modules.foreach {
      _ match {
        case _: ExtModule =>
        case Module(info, thisModule, ports, body) =>
          def onStmt(s: Statement): Statement = s match {
            case Block(_) => s map onStmt
            case i: WDefInstance =>
              instances(thisModule) = instances.getOrElse(thisModule, Set.empty[WDefInstance]) + i
              i
            case c: Connect =>
              connections(thisModule) = connections.getOrElse(thisModule, Set.empty[Connect]) + c
              c
          }

          onStmt(body)
      }
    }

    /* Emit json */
    val newModuleMap = circuit.modules.flatMap(m => convertModule(m, instances.toMap, connections.toMap, modules).map(Seq(_)).getOrElse(Nil)).toMap
    val namespace = new coreir.ir.Namespace(None, None, Some(newModuleMap), None)

    val tpe = coreir.ir.Record(Map("a" -> coreir.ir.Array(16, coreir.ir.Bit()), "b" -> coreir.ir.Array(16, coreir.ir.Bit())))
    val x = Json.toJson(tpe)
    println(x)
    val out: JsValue = Json.toJson(namespace)
    writer.write(Json.prettyPrint(out))
  }

  def convert(c: Connect): coreir.ir.Connection = {
    def getSeq(s: String): Seq[String] = s.split("""\]\.""").flatMap(_.split("""\[""")).flatMap(_.split("""\."""))
    val left  = new coreir.ir.Wireable(getSeq(c.loc.serialize))
    val right = new coreir.ir.Wireable(getSeq(c.expr.serialize))
    new coreir.ir.Connection(left, right)
  }

  def convert(i: WDefInstance, modules: Map[String, DefModule]): (String, coreir.ir.Instance) = {
    modules(i.module) match {
      case e@ExtModule(_, name, ports, defname, params) =>
        val (genRef, genArgs) = getGens(e)
        val optGenArgs = if(genArgs.isEmpty) None else Some(genArgs)
        (i.name, new coreir.ir.Instance(genRef, optGenArgs, None, None))
      case Module(_, name, ports, body) =>
        (i.name, new coreir.ir.Instance(None, None, Some(coreir.ir.NamedRef("global", name)), None))
    }
  }

  def convertModule(m: DefModule,
                    instances: Map[String, Set[WDefInstance]],
                    connections: Map[String, Set[Connect]],
                    modules: Map[String, DefModule]
                   ): Option[(String, coreir.ir.Module)] = m match {
    case e@ExtModule(_, name, ports, defname, params) =>
      val (genRef, genArgs) = getGens(e)
      if(genRef.isDefined && genRef.get.namespaceName != "coreir") {
        error(s"Don't support blackboxes yet: $name")
      }
      None
    case m@Module(_, name, ports, body) =>
      val insts = instances(name).map(i => convert(i, modules)).toMap
      val cons  = connections(name).map(convert(_)).toSeq
      Some((name, new coreir.ir.Module(convertType(Utils.module_type(m)), None, None, Some(insts), Some(cons))))
  }

  def getBits(in: Boolean)(t: GroundType): coreir.ir.Type = t match {
    case ClockType if in => coreir.ir.BitIn()
    case ClockType if !in => coreir.ir.Bit()
    case AnalogType(_) => error("Analog types not supported")
    case GroundType(IntWidth(w)) if in => coreir.ir.Array(w.toInt, coreir.ir.BitIn())
    case GroundType(IntWidth(w)) if !in => coreir.ir.Array(w.toInt, coreir.ir.Bit())
    case GroundType(_) => error("All ground types must have a known width")
  }

  def sinkFlips(in: Boolean)(t: Type): coreir.ir.Type = t match {
    case BundleType(fields) =>
      val sunkFields = fields.map { f =>
        val newIn = (in, f.flip) match {
          case (true, Default) => true
          case (false, Default) => false
          case (true, Flip) => false
          case (false, Flip) => true
        }
        (f.tpe) match {
          case g: GroundType => (f.name, getBits(in)(g))
          case other => (f.name, sinkFlips(newIn)(other))
        }
      }.toMap
      new coreir.ir.Record(sunkFields)
    case VectorType(tpe, size) =>
      tpe match {
        case g: GroundType => new coreir.ir.Array(size, getBits(in)(g))
        case other => new coreir.ir.Array(size, sinkFlips(in)(other))
      }
    case other => error("Shouldn't be here")
  }

  def convertType(tpe: Type): coreir.ir.Type = {
    sinkFlips(false)(tpe)
  }

  def getGens(m: ExtModule): (Option[coreir.ir.NamedRef], Seq[coreir.ir.Arg]) = {
    m.params.foldLeft((None: Option[coreir.ir.NamedRef], Seq.empty[coreir.ir.Arg])) {
      case ((ref, args), param) => param match {
        case RawStringParam("genref", value) =>
          println(param)
          val splitted = value.split('.')
          println(splitted)
          val h = splitted.head
          val l = splitted.last
          (Some(coreir.ir.NamedRef(h, l)), args)
        case RawStringParam(argName, value) => (ref, args ++ Seq(coreir.ir.Arg(coreir.ir.ValueString(), value)))
        case IntParam(argName, value) => (ref, args ++ Seq(coreir.ir.Arg(coreir.ir.ValueInt(), value.toString())))
        case _ => (ref, args)
      }
    }
  }
}
