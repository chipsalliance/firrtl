// See LICENSE for license details.

package firrtl

import java.io.Writer

import firrtl.Mappers._
import firrtl.PrimOps._
import firrtl.Utils.{create_exps, error}
import firrtl.ir._
import firrtl.passes._

import play.api.libs.json._
import coreir.json.CoreIRWrites._
import scala.collection.mutable
// Datastructures


class RewriteComponents extends Pass {
  override val inputForm = MidForm
  override val outputForm = MidForm

  def run(c: Circuit): Circuit = {
    c map rewriteModule
  }

  /* Rewrite expressions to use CoreIR primops */
  def rewriteModule(m: DefModule): DefModule = m map rewriteStmt
  def rewriteStmt(s: Statement): Statement = s map rewriteStmt map rewriteExpressions
  def rewriteExpressions(e: Expression): Expression = {
    val rewritten = e match {
      case DoPrim(Tail, Seq(x), Seq(const), tpe@UIntType(IntWidth(width))) =>
        DoPrim(Bits, Seq(x), Seq(width - 1, 0), tpe)
      case DoPrim(Head, Seq(x), Seq(const), tpe) => x.tpe match {
        case GroundType(IntWidth(width)) => DoPrim(Bits, Seq(x), Seq(width - 1, width - 1 - const), tpe)
        case _ => error("Shouldn't be here")
      }
      case DoPrim(Pad, Seq(x), Seq(const), tpe: UIntType) =>
        val xWidth = bitWidth(x.tpe)
        if(const > xWidth) {
          DoPrim(Cat, Seq(UIntLiteral(0, IntWidth(const - xWidth)), x), Nil, tpe)
        } else if(const > xWidth) {
          DoPrim(Bits, Seq(x), Seq(const - 1, 0), tpe)
        } else {
          x
        }
      case DoPrim(Pad, Seq(x), Seq(const), tpe: SIntType) =>
        val xWidth = bitWidth(x.tpe)
        val newExp = if(const > xWidth) {
          DoPrim(Cat, Seq(UIntLiteral(0, IntWidth(const - xWidth)), x), Nil, tpe)
          val msb = DoPrim(Bits, Seq(x), Seq(xWidth, xWidth), UnknownType)
          Range(0, (const - xWidth).toInt).foldLeft(x) { case (exp, _) =>
            DoPrim(Cat, Seq(msb, exp), Nil, UnknownType)
          }
        } else if(const > xWidth) {
          DoPrim(Bits, Seq(x), Seq(const - 1, 0), tpe)
        } else {
          x
        }
        DoPrim(AsSInt, Seq(newExp), Nil, UnknownType)
      case other => other
    }
    rewritten map rewriteExpressions
  }

}

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
                  tpe: Type,
                  nInputs: Int,
                  args: Map[String, Any]
                ): (DefModule, Statement, Seq[Expression]) = {
    val inPorts = nInputs match {
      case 0 => Nil
      case 1 => Seq(Port(info, "in", Input, tpe))
      case 3 if (moduleName == "MUX") =>
        Seq(
          Port(info, "sel", Input, UIntType(IntWidth(1))),
          Port(info, "in0", Input, tpe),
          Port(info, "in1", Input, tpe)
        )
      case 2 if (moduleName == "REG") =>
        Seq(
          Port(info, "in", Input, tpe),
          Port(info, "clk", Input, ClockType)
        )
      case 3 if (moduleName == "REG") =>
        Seq(
          Port(info, "in", Input, tpe),
          Port(info, "clk", Input, ClockType),
          Port(info, "clr", Input, UIntType(IntWidth(1)))
        )
      case x if x > 1 => Range(0, x).map(n => Port(info, s"in$n", Input, tpe))
    }
    val outPorts = Seq(Port(info, "out", Output, tpe))
    val instName = name
    val thisModName = namespace.newName(moduleName)
    val allArgs = args.map { case (name, value) =>
      value match {
        case v: String => RawStringParam(name, v)
        case i: Int => IntParam(name, i)
        case i: Boolean => BooleanParam(name, i)
        case (size: Int, num: Int) => BitVectorParam(name, size, num)
      }
    }.toSeq
    val mod = ExtModule(info,
      thisModName,
      inPorts ++ outPorts,
      moduleName,
      allArgs
    )
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
      val (mx, sx, exps) =
        makeModule(info, name, "WIRE", tpe, 1,
          Map("width" -> bitWidth(tpe).toInt, "genref" -> "coreir.wire")
        )
      newModules += mx
      sx
    case DefNode(info, name, value) =>
      val (sx, ex) = removeExpressionComponents(modNS)(value)
      val (nodeMod, nodeDec, exps) =
        makeModule(info, name, "WIRE", value.tpe, 1,
          Map("width" -> bitWidth(value.tpe).toInt, "genref" -> "coreir.wire")
        )
      //Seq(IntParam("width", width), RawStringParam("genref", ref))
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
      val nInputs = r.reset match {
        case x: Literal => 2
        case _ => 3
      }
      val (initValue, hasReset) = r.init match {
        case x: Literal => (x.value.toInt, true)
        case other if nInputs == 2 => (0, false)
        case other => error(s"Non-literal reg init is not supported: $other")
      }
      val (rx, sx, exps) =
        makeModule(NoInfo, r.name, "REG", r.tpe, nInputs,
          Map(
            "width" -> bitWidth(r.tpe).toInt,
            "genref" -> "mantle.reg",
            "has_clr" -> hasReset,
            "has_en" -> false,
            "has_rst" -> false,
            "width" -> bitWidth(r.tpe).toInt,
            "modArg init" -> (bitWidth(r.tpe).toInt, initValue)
          )
        )
      newModules += rx
      val (clkstmts, clkexp) = removeExpressionComponents(modNS)(r.clock)
      if (nInputs == 3) {
        val in :: clock :: reset :: out :: Nil = exps
        val (resetstmts, resetexp) = removeExpressionComponents(modNS)(r.reset)
        Block(Seq(sx, clkstmts, resetstmts,
          Connect(NoInfo, clock, clkexp),
          Connect(NoInfo, reset, resetexp)
        ))
      } else {
        val in :: clock :: out :: Nil = exps
        val (clkstmts, clkexp) = removeExpressionComponents(modNS)(r.clock)
        Block(Seq(sx, clkstmts,
          Connect(NoInfo, clock, clkexp)
        ))
      }
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
        WSubField(WRef(name, tpe, InstanceKind, FEMALE), "in", tpe, MALE)
      case WRef(name, tpe, RegKind, FEMALE) =>
        val regRef = WRef(name, tpe, InstanceKind, FEMALE)
        WSubField(regRef, "in", UnknownType, MALE)
      case WRef(name, tpe, WireKind|NodeKind|RegKind, MALE)   =>
        WSubField(WRef(name, tpe, InstanceKind, MALE), "out", tpe, FEMALE)
      case WRef(name, tpe, PortKind, _) =>
        WSubField(WRef("self", UnknownType, PortKind, FEMALE), name, tpe, MALE)
      case w: WRef => w
      case w: WSubField => w
      case w: WSubIndex => w
      case Mux(p, tVal, fVal, tpe) =>
        val (mx, sx, exps) =
          makeModule(NoInfo, modNS.newName("mux"), "MUX", tpe, 3,
            Map("width" -> bitWidth(tpe).toInt, "genref" -> "coreir.mux")
          )
        newModules += mx
        stmts += Block(Seq(sx,
          Connect(NoInfo, exps.head, WSubIndex(p, 0, UnknownType, MALE)),
          Connect(NoInfo, exps(1), tVal),
          Connect(NoInfo, exps(2), fVal)))
        exps(3)
      case d@DoPrim(op, args, const, tpe) =>
        val nInputs = args.length
        val nConsts = const.length

        val argMap = op match {
          case Bits =>
            Map("width" -> bitWidth(args(0).tpe).toInt, "genref" -> s"coreir.slice",
              "lo" -> const(1).toInt,
              "hi" -> (const(0).toInt + 1)
            )
          case Cat =>
            Map(
              "width0" -> bitWidth(args(0).tpe).toInt,
              "width1" -> bitWidth(args(1).tpe).toInt,
              "genref" -> s"coreir.concat"
            )
          case _ =>
            val opName = op match {
              case Addw => "add"
              case Subw => "sub"
              case And | Or | Xor | Not | Eq => op.toString()
              case Gt | Lt =>
                (args(0).tpe, args(1).tpe) match {
                  case (_: UIntType, _: UIntType) => "u" + op.toString()
                  case (_: SIntType, _: SIntType) => "s" + op.toString()
                }
              case Geq | Leq =>
                (args(0).tpe, args(1).tpe) match {
                  case (_: UIntType, _: UIntType) => "u" + op.toString().slice(0, 2)
                  case (_: SIntType, _: SIntType) => "s" + op.toString().slice(0, 2)
                }
              case Dshl => "shl"
              case Dshr => args.head.tpe match {
                case u: UIntType => "lshr"
                case s: SIntType => "ashr"
                case other => error(s"Unsupported input type for Dshr: $other")
              }
              case other => error(s"Unsupported op: $op")
            }
            Map("width" -> bitWidth(args(0).tpe).toInt, "genref" -> s"coreir.$opName")
        }
        val name = if(modNS.tryName(op.toString + "_0")) {
          op.toString + "_0"
        } else {
          modNS.newName(op.toString)
        }
        val (mx, sx, exps) =
          makeModule(NoInfo, name, op.toString, tpe, nInputs, argMap)

        stmts += sx
        newModules += mx
        args.zipWithIndex.foreach { case ((input, index)) =>
          stmts += Connect(NoInfo, exps(index), input)
        }
        exps.last
      case u@UIntLiteral(value, IntWidth(width)) =>
        val (mx, sx, exps) =
          makeModule(NoInfo, modNS.newName(s"uint$value"), s"uint$value", u.tpe, 0,
            Map("width" -> bitWidth(u.tpe).toInt, "genref" -> "coreir.const", "modArg value" -> (width.toInt, value.toInt))
          )
        stmts += sx
        newModules += mx
        exps.last
      case other => other
    }
    val newExp = onExp(rewriteExpressions(e))
    (Block(stmts), newExp)
  }

  def rewriteExpressions(e: Expression): Expression = {
    val rewritten = e match {
      case DoPrim(Bits, Seq(DoPrim(Add, Seq(in0, in1), Nil, xtpe)), Seq(hi, lo), tpe) if hi == bitWidth(xtpe) - 2 && lo == 0 =>
        DoPrim(Addw, Seq(in0, in1), Nil, tpe)
      case DoPrim(Bits, Seq(DoPrim(Sub, Seq(in0, in1), Nil, xtpe)), Seq(hi, lo), tpe) if hi == bitWidth(xtpe) - 2 && lo == 0 =>
        DoPrim(Subw, Seq(in0, in1), Nil, tpe)
      case DoPrim(Shl, Seq(x), Seq(const), tpe) if const == 0 => x
      case DoPrim(Shl, Seq(x), Seq(const), tpe@GroundType(IntWidth(width))) if const > 0 =>
        val uint = UIntLiteral(0, IntWidth(const))
        DoPrim(Dshl, Seq(DoPrim(Cat, Seq(uint, x), Nil, tpe), UIntLiteral(const, IntWidth(width))), Nil, tpe)
      case DoPrim(Shr, Seq(x), Seq(const), tpe@GroundType(IntWidth(width))) =>
        DoPrim(Bits, Seq(DoPrim(Dshr, Seq(x, UIntLiteral(const, IntWidth(width))), Nil, x.tpe)), Seq(width - 1, 0), tpe)
      case other => other
    }
    rewritten map rewriteExpressions
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
    passes.VerilogRename,
    NormalPadWidths,
    new RewriteComponents(),
    InferTypes,
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
    val instances = mutable.HashMap[String, Seq[WDefInstance]]()
    /* Per module connections */
    val connections = mutable.HashMap[String, Seq[Connect]]()

    /* Populate instances and connections */
    circuit.modules.foreach {
      _ match {
        case _: ExtModule =>
        case Module(info, thisModule, ports, body) =>
          def onStmt(s: Statement): Statement = s match {
            case Block(_) => s map onStmt
            case i: WDefInstance =>
              instances(thisModule) = instances.getOrElse(thisModule, Seq.empty[WDefInstance]) ++ Seq(i)
              i
            case c: Connect =>
              connections(thisModule) = connections.getOrElse(thisModule, Seq.empty[Connect]) ++ Seq(c)
              c
          }

          onStmt(body)
      }
    }

    /* Emit json */
    val newModuleMap = circuit.modules.flatMap(m => convertModule(m, instances.toMap, connections.toMap, modules).map(Seq(_)).getOrElse(Nil)).toMap
    val namespace = new coreir.ir.Namespace(None, None, Some(newModuleMap), None)
    val top = new coreir.ir.Top(Map(("global" -> namespace)), new coreir.ir.NamedRef("global", circuit.main))
    val out: JsValue = Json.toJson(top)
    val pretty = Json.prettyPrint(out)
    println(pretty)
    writer.write(pretty)
  }

  def convert(c: Connect): coreir.ir.Connection = {
    def getSeq(s: String): Seq[String] = s.split("""\]\.""").flatMap(_.split("""\[""")).flatMap(_.split("""\.""")).flatMap(_.split("""\]"""))
    val left  = new coreir.ir.Wireable(getSeq(c.loc.serialize))
    val right = new coreir.ir.Wireable(getSeq(c.expr.serialize))
    new coreir.ir.Connection(right, left)
  }

  def convert(i: WDefInstance, modules: Map[String, DefModule]): (String, coreir.ir.Instance) = {
    modules(i.module) match {
      case e@ExtModule(_, name, ports, defname, params) =>
        val (genRef, genArgs, modRef, modArgs) = getGens(e)
        val optGenArgs = if(genArgs.values.isEmpty) None else Some(genArgs)
        val optModArgs = if(modArgs.values.isEmpty) None else Some(modArgs)
        (i.name, new coreir.ir.Instance(genRef, optGenArgs, modRef, optModArgs))
      case Module(_, name, ports, body) =>
        (i.name, new coreir.ir.Instance(None, None, Some(coreir.ir.NamedRef("global", name)), None))
    }
  }

  def convertModule(m: DefModule,
                    instances: Map[String, Seq[WDefInstance]],
                    connections: Map[String, Seq[Connect]],
                    modules: Map[String, DefModule]
                   ): Option[(String, coreir.ir.Module)] = m match {
    case e@ExtModule(_, name, ports, defname, params) =>
      val (genRef, genArgs, modRef, modArgs) = getGens(e)
      val defaults = Seq("coreir", "corebit", "mantle")
      if(genRef.isDefined && (!defaults.contains(genRef.get.namespaceName))) {
        error(s"Don't support blackboxes yet: $name in ${genRef.get.namespaceName}")
      }
      None
    case m@Module(_, name, ports, body) =>
      val instsOpt = instances.get(name) match {
        case None => None
        case Some(_) => Some(instances(name).map(i => convert(i, modules)).toMap)
      }
      val cons  = connections(name).map(convert(_)).toSeq
      Some((name, new coreir.ir.Module(convertType(Utils.module_type(m)), None, None, instsOpt, Some(cons))))
  }

  def getBits(in: Boolean)(t: GroundType): coreir.ir.Type = t match {
    case ClockType if in => coreir.ir.Named(coreir.ir.NamedRef("coreir", "clkIn"), None)
    case ClockType if !in => coreir.ir.Named(coreir.ir.NamedRef("coreir", "clk"), None)
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
          case g: GroundType => (f.name, getBits(newIn)(g))
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

  def getGens(m: ExtModule): (Option[coreir.ir.NamedRef], coreir.ir.Values, Option[coreir.ir.NamedRef], coreir.ir.Values) = {
    def param2value(param: Param): coreir.ir.Value = param match {
      case RawStringParam(argName, value) if argName.split(" ").head == "modArg" =>
        coreir.ir.Const(coreir.ir.ValueString(), value)
      case IntParam(argName, value) =>
        coreir.ir.Const(coreir.ir.ValueInt(), value.toString())
      case BooleanParam(argName, value) =>
        coreir.ir.Const(coreir.ir.ValueBool(), value.toString())
      case BitVectorParam(argName, size, value) =>
        coreir.ir.Const(coreir.ir.ValueBitVector(size), value.toString())
    }
    val (genref, genargmap, modref, modargmap) =
      m.params.foldLeft(
        (None: Option[coreir.ir.NamedRef], Map.empty[String, coreir.ir.Value], None: Option[coreir.ir.NamedRef], Map.empty[String, coreir.ir.Value])
      ) {
        case ((ref, gargs, mref, margs), param) => param.name.split(" ").toSeq match {
          case Seq("modArg", name: String) =>
            (ref, gargs, mref, margs + (name -> param2value(param)))
          case Seq("genref") =>
            val splitted = param.asInstanceOf[RawStringParam].value.split('.')
            (Some(coreir.ir.NamedRef(splitted.head, splitted.last)), gargs, mref, margs)
          case Seq("modref") =>
            val splitted = param.asInstanceOf[RawStringParam].value.split('.')
            (ref, gargs, Some(coreir.ir.NamedRef(splitted.head, splitted.last)), margs)
          case Seq(name: String) =>
            (ref, gargs + (name -> param2value(param)), mref, margs)
        }
    }
    (genref, new coreir.ir.Values(genargmap), modref, new coreir.ir.Values(modargmap))
  }
}

object NormalPadWidths extends Pass {
  private def width(t: Type): Int = bitWidth(t).toInt
  private def width(e: Expression): Int = width(e.tpe)
  // Returns an expression with the correct integer width
  private def fixup(i: Int)(e: Expression) = {
    def tx = e.tpe match {
      case t: UIntType => UIntType(IntWidth(i))
      case t: SIntType => SIntType(IntWidth(i))
      // default case should never be reached
    }
    width(e) match {
      case j if i > j => DoPrim(Pad, Seq(e), Seq(i), tx)
      case j if i < j =>
        val e2 = DoPrim(Bits, Seq(e), Seq(i - 1, 0), UIntType(IntWidth(i)))
        // Bit Select always returns UInt, cast if selecting from SInt
        e.tpe match {
          case UIntType(_) => e2
          case SIntType(_) => DoPrim(AsSInt, Seq(e2), Seq.empty, SIntType(IntWidth(i)))
        }
      case _ => e
    }
  }

  // Recursive, updates expression so children exp's have correct widths
  private def onExp(e: Expression): Expression = e map onExp match {
    case Mux(cond, tval, fval, tpe) =>
      Mux(cond, fixup(width(tpe))(tval), fixup(width(tpe))(fval), tpe)
    case ex: ValidIf => ex copy (value = fixup(width(ex.tpe))(ex.value))
    case ex: DoPrim => ex.op match {
      case Lt | Leq | Gt | Geq | Eq | Neq | Not | And | Or | Xor |
           Add | Sub | Mul | Div | Rem | Shr =>
        // sensitive ops
        ex map fixup((ex.args map width foldLeft 0)(math.max))
      //case Dshl =>
      //  // special case as args aren't all same width
      //  ex copy (op = Dshlw, args = Seq(fixup(width(ex.tpe))(ex.args.head), ex.args(1)))
      //case Shl =>
      //  // special case as arg should be same width as result
      //  ex copy (op = Shlw, args = Seq(fixup(width(ex.tpe))(ex.args.head)))
      case _ => ex
    }
    case ex => ex
  }

  // Recursive. Fixes assignments and register initialization widths
  private def onStmt(s: Statement): Statement = s map onExp match {
    case sx: Connect =>
      sx copy (expr = fixup(width(sx.loc))(sx.expr))
    case sx: DefRegister =>
      sx copy (init = fixup(width(sx.tpe))(sx.init))
    case sx => sx map onStmt
  }

  def run(c: Circuit): Circuit = c copy (modules = c.modules map (_ map onStmt))
}
