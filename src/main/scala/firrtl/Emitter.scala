// See LICENSE for license details.

package firrtl

import com.typesafe.scalalogging.LazyLogging
import java.nio.file.{Paths, Files}
import java.io.{Reader, Writer}

import scala.collection.mutable
import scala.sys.process._
import scala.io.Source

import firrtl.ir._
import firrtl.passes._
import firrtl.Mappers._
import firrtl.PrimOps._
import firrtl.WrappedExpression._
import Utils._
import MemPortUtils.{memPortField, memType}
// Datastructures
import scala.collection.mutable.{ArrayBuffer, LinkedHashMap, HashSet}

case class EmitterException(message: String) extends PassException(message)

class FirrtlEmitter extends Emitter {
  def emit(state: CircuitState, writer: Writer): Unit = writer.write(state.circuit.serialize)
}

case class VRandom(width: BigInt) extends Expression {
  def tpe = UIntType(IntWidth(width))
  def nWords = (width + 31) / 32
  def realWidth = nWords * 32
  def serialize: String = "RANDOM"
  def mapExpr(f: Expression => Expression): Expression = this
  def mapType(f: Type => Type): Expression = this
  def mapWidth(f: Width => Width): Expression = this
}

class VerilogEmitter extends Emitter with PassBased {
  val tab = "  "
  def AND(e1: WrappedExpression, e2: WrappedExpression): Expression = {
    if (e1 == e2) e1.e1
    else if ((e1 == we(zero)) | (e2 == we(zero))) zero
    else if (e1 == we(one)) e2.e1
    else if (e2 == we(one)) e1.e1
    else DoPrim(And, Seq(e1.e1, e2.e1), Nil, UIntType(IntWidth(1)))
  }
  def wref(n: String, t: Type) = WRef(n, t, ExpKind, UNKNOWNGENDER)
  def remove_root(ex: Expression): Expression = ex match {
    case ex: WSubField => ex.exp match {
      case (e: WSubField) => remove_root(e)
      case (_: WRef) => WRef(ex.name, ex.tpe, InstanceKind, UNKNOWNGENDER)
    }
    case _ => error("Shouldn't be here")
  }
  /** Turn Params into Verilog Strings */
  def stringify(param: Param): String = param match {
    case IntParam(name, value) => s".$name($value)"
    case DoubleParam(name, value) => s".$name($value)"
    case StringParam(name, value) =>
      val strx = "\"" + VerilogStringLitHandler.escape(value) + "\""
      s".${name}($strx)"
    case RawStringParam(name, value) => s".$name($value)"
  }
  def emit(x: Any)(implicit w: Writer) { emit(x, 0) }
  def emit(x: Any, top: Int)(implicit w: Writer) {
    def cast(e: Expression): Any = e.tpe match {
      case (t: UIntType) => e
      case (t: SIntType) => Seq("$signed(",e,")")
      case ClockType => e
      case AnalogType(_) => e
    }
    x match {
      case (e: DoPrim) => emit(op_stream(e), top + 1)
      case (e: Mux) => emit(Seq(e.cond," ? ",cast(e.tval)," : ",cast(e.fval)),top + 1)
      case (e: ValidIf) => emit(Seq(cast(e.value)),top + 1)
      case (e: WRef) => w write e.serialize
      case (e: WSubField) => w write LowerTypes.loweredName(e)
      case (e: WSubAccess) => w write s"${LowerTypes.loweredName(e.exp)}[${LowerTypes.loweredName(e.index)}]"
      case (e: WSubIndex) => w write e.serialize
      case (e: Literal) => v_print(e)
      case (e: VRandom) => w write s"{${e.nWords}{$$random}}"
      case (t: UIntType) => 
        val wx = bitWidth(t) - 1
        if (wx > 0) w write s"[$wx:0]"
      case (t: SIntType) => 
        val wx = bitWidth(t) - 1
        if (wx > 0) w write s"[$wx:0]"
      case ClockType =>
      case t: AnalogType =>
        val wx = bitWidth(t) - 1
        if (wx > 0) w write s"[$wx:0]"
      case (t: VectorType) => 
        emit(t.tpe, top + 1)
        w write s"[${t.size - 1}:0]"
      case Input => w write "input"
      case Output => w write "output"
      case (s: String) => w write s
      case (i: Int) => w write i.toString
      case (i: Long) => w write i.toString
      case (i: BigInt) => w write i.toString
      case (s: Seq[Any]) =>
        s foreach (emit(_, top + 1))
        if (top == 0) w write "\n"
      case _ => error("Shouldn't be here")
    }
  }

   //;------------- PASS -----------------
   def v_print(e: Expression)(implicit w: Writer) = e match {
     case UIntLiteral(value, IntWidth(width)) =>
       w write s"$width'h${value.toString(16)}"
     case SIntLiteral(value, IntWidth(width)) =>
       val unsignedValue = value + (if (value < 0) BigInt(1) << width.toInt else 0)
       w write s"$width'sh${unsignedValue.toString(16)}"
   }

   def op_stream(doprim: DoPrim): Seq[Any] = {
     def cast_if(e: Expression): Any = {
       doprim.args find (_.tpe match {
         case (_: SIntType) => true
         case (_) => false
       }) match {
         case None => e
         case Some(_) => e.tpe match {
           case (_: SIntType) => Seq("$signed(", e, ")")
           case (_: UIntType) => Seq("$signed({1'b0,", e, "})")
         }
       }
     }
     def cast(e: Expression): Any = doprim.tpe match {
       case (t: UIntType) => e
       case (t: SIntType) => Seq("$signed(",e,")")
     }
     def cast_as(e: Expression): Any = e.tpe match {
       case (t: UIntType) => e
       case (t: SIntType) => Seq("$signed(",e,")")
     }
     def a0: Expression = doprim.args.head
     def a1: Expression = doprim.args(1)
     def c0: Int = doprim.consts.head.toInt
     def c1: Int = doprim.consts(1).toInt

     def checkArgumentLegality(e: Expression) = e match {
       case _: UIntLiteral | _: SIntLiteral | _: WRef | _: WSubField =>
       case _ => throw EmitterException(s"Can't emit ${e.getClass.getName} as PrimOp argument")
     }
     doprim.args foreach checkArgumentLegality
     doprim.op match {
       case Add => Seq(cast_if(a0), " + ", cast_if(a1))
       case Addw => Seq(cast_if(a0), " + ", cast_if(a1))
       case Sub => Seq(cast_if(a0), " - ", cast_if(a1))
       case Subw => Seq(cast_if(a0), " - ", cast_if(a1))
       case Mul => Seq(cast_if(a0), " * ", cast_if(a1))
       case Div => Seq(cast_if(a0), " / ", cast_if(a1))
       case Rem => Seq(cast_if(a0), " % ", cast_if(a1))
       case Lt => Seq(cast_if(a0), " < ", cast_if(a1))
       case Leq => Seq(cast_if(a0), " <= ", cast_if(a1))
       case Gt => Seq(cast_if(a0), " > ", cast_if(a1))
       case Geq => Seq(cast_if(a0), " >= ", cast_if(a1))
       case Eq => Seq(cast_if(a0), " == ", cast_if(a1))
       case Neq => Seq(cast_if(a0), " != ", cast_if(a1))
       case Pad =>
         val w = bitWidth(a0.tpe)
         val diff = c0 - w
         if (w == BigInt(0)) Seq(a0)
         else doprim.tpe match {
           // Either sign extend or zero extend.
           // If width == BigInt(1), don't extract bit
           case (_: SIntType) if w == BigInt(1) => Seq("{", c0, "{", a0, "}}")
           case (_: SIntType) => Seq("{{", diff, "{", a0, "[", w - 1, "]}},", a0, "}")
           case (_) => Seq("{{", diff, "'d0}, ", a0, "}")
         }
       case AsUInt => Seq("$unsigned(", a0, ")")
       case AsSInt => Seq("$signed(", a0, ")")
       case AsClock => Seq("$unsigned(", a0, ")")
       case Dshlw => Seq(cast(a0), " << ", a1)
       case Dshl => Seq(cast(a0), " << ", a1)
       case Dshr => doprim.tpe match {
         case (_: SIntType) => Seq(cast(a0)," >>> ", a1)
         case (_) => Seq(cast(a0), " >> ", a1)
       }
       case Shlw => Seq(cast(a0), " << ", c0)
       case Shl => Seq(cast(a0), " << ", c0)
       case Shr if c0 >= bitWidth(a0.tpe) =>
         error("Verilog emitter does not support SHIFT_RIGHT >= arg width")
       case Shr => Seq(a0,"[", bitWidth(a0.tpe) - 1, ":", c0, "]")
       case Neg => Seq("-{", cast(a0), "}")
       case Cvt => a0.tpe match {
         case (_: UIntType) => Seq("{1'b0,", cast(a0), "}")
         case (_: SIntType) => Seq(cast(a0))
       }
       case Not => Seq("~ ", a0)
       case And => Seq(cast_as(a0), " & ", cast_as(a1))
       case Or => Seq(cast_as(a0), " | ", cast_as(a1))
       case Xor => Seq(cast_as(a0), " ^ ", cast_as(a1))
       case Andr => Seq("&", cast(a0))
       case Orr => Seq("|", cast(a0))
       case Xorr => Seq("^", cast(a0))
       case Cat => Seq("{", cast(a0), ",", cast(a1), "}")
       // If selecting zeroth bit and single-bit wire, just emit the wire
       case Bits if c0 == 0 && c1 == 0 && bitWidth(a0.tpe) == BigInt(1) => Seq(a0)
       case Bits if c0 == c1 => Seq(a0, "[", c0, "]")
       case Bits => Seq(a0, "[", c0, ":", c1, "]")
       case Head =>
         val w = bitWidth(a0.tpe)
         val high = w - 1
         val low = w - c0
         Seq(a0, "[", high, ":", low, "]")
       case Tail =>
         val w = bitWidth(a0.tpe)
         val low = w - c0 - 1
         Seq(a0, "[", low, ":", 0, "]")
     }
   }
   
    def emit_verilog(m: Module, moduleMap: Map[String, DefModule])(implicit w: Writer): DefModule = {
      val netlist = mutable.LinkedHashMap[WrappedExpression, Expression]()
      val addrRegs = mutable.HashSet[WrappedExpression]()
      val namespace = Namespace(m)
      def build_netlist(s: Statement): Statement = s map build_netlist match {
        case sx: Connect =>
          netlist(sx.loc) = sx.expr
          (kind(sx.loc), kind(sx.expr)) match {
            case (MemKind, RegKind) => addrRegs += sx.expr
            case _ =>
          }
          sx
        case sx: IsInvalid =>
          netlist(sx.expr) = wref(namespace.newTemp, sx.expr.tpe)
          sx
        case sx: DefNode =>
          val e = WRef(sx.name, sx.value.tpe, NodeKind, MALE)
          netlist(e) = sx.value
          sx
        case sx => sx
      }
   
      val portdefs = ArrayBuffer[Seq[Any]]()
      val declares = ArrayBuffer[Seq[Any]]()
      val instdeclares = ArrayBuffer[Seq[Any]]()
      val assigns = ArrayBuffer[Seq[Any]]()
      val at_clock = mutable.LinkedHashMap[Expression,ArrayBuffer[Seq[Any]]]()
      val initials = ArrayBuffer[Seq[Any]]()
      val simulates = ArrayBuffer[Seq[Any]]()
      def declare(b: String, n: String, t: Type) = t match {
        case tx: VectorType =>
          declares += Seq(b, " ", tx.tpe, " ", n, " [0:", tx.size - 1, "];")
        case tx =>
          declares += Seq(b, " ", tx, " ", n,";")
      }
      def assign(e: Expression, value: Expression) {
        assigns += Seq("assign ", e, " = ", value, ";")
      }

      // In simulation, assign garbage under a predicate
      def garbageAssign(e: Expression, syn: Expression, garbageCond: Expression) = {
        assigns += Seq("`ifndef RANDOMIZE_GARBAGE_ASSIGN")
        assigns += Seq("assign ", e, " = ", syn, ";")
        assigns += Seq("`else")
        assigns += Seq("assign ", e, " = ", garbageCond, " ? ", rand_string(syn.tpe), " : ", syn, ";")
        assigns += Seq("`endif")
      }
      def invalidAssign(e: Expression) = {
        assigns += Seq("`ifdef RANDOMIZE_INVALID_ASSIGN")
        assigns += Seq("assign ", e, " = ", rand_string(e.tpe), ";")
        assigns += Seq("`endif")
      }
      def update_and_reset(r: Expression, clk: Expression, reset: Expression, init: Expression) = {
        // We want to flatten Mux trees for reg updates into if-trees for
        // improved QoR for conditional updates.  However, unbounded recursion
        // would take exponential time, so don't redundantly flatten the same
        // Mux more than a bounded number of times, preserving linear runtime.
        // The threshold is empirical but ample.
        val flattenThreshold = 4
        val numTimesFlattened = collection.mutable.HashMap[Mux, Int]()
        def canFlatten(m: Mux) = {
          val n = numTimesFlattened.getOrElse(m, 0)
          numTimesFlattened(m) = n + 1
          n < flattenThreshold
        }

        def addUpdate(e: Expression, tabs: String): Seq[Seq[Any]] = {
          if (weq(e, r)) Nil else netlist getOrElse (e, e) match {
            case m: Mux if canFlatten(m) =>
              val ifStatement = Seq(tabs, "if (", m.cond, ") begin")
              val trueCase =
                // Don't generate mux trees for mem addr pipes
                if (addrRegs(r)) Seq(Seq(tabs + tab, r, " <= ", m.tval, ";"))
                else addUpdate(m.tval, tabs + tab)
              val elseStatement = Seq(tabs, "end else begin")
              val ifNotStatement = Seq(tabs, "if (!(", m.cond, ")) begin")
              val falseCase = addUpdate(m.fval, tabs + tab)
              val endStatement = Seq(tabs, "end")

              ((trueCase.nonEmpty, falseCase.nonEmpty): @ unchecked) match {
                case (true, true) =>
                  ifStatement +: trueCase ++: elseStatement +: falseCase :+ endStatement
                case (true, false) =>
                  ifStatement +: trueCase :+ endStatement
                case (false, true) =>
                  ifNotStatement +: falseCase :+ endStatement
              }
            case _ => Seq(Seq(tabs, r, " <= ", e, ";"))
          }
        }

        at_clock.getOrElseUpdate(clk, ArrayBuffer[Seq[Any]]()) ++= {
          val tv = init
          val fv = netlist(r)
          if (weq(tv, r))
            addUpdate(fv, "")
          else
            addUpdate(Mux(reset, tv, fv, mux_type_and_widths(tv, fv)), "")
        }
      }

      def update(e: Expression, value: Expression, clk: Expression, en: Expression) {
         if (!at_clock.contains(clk)) at_clock(clk) = ArrayBuffer[Seq[Any]]()
         if (weq(en,one)) at_clock(clk) += Seq(e," <= ",value,";")
         else {
            at_clock(clk) += Seq("if(",en,") begin")
            at_clock(clk) += Seq(tab,e," <= ",value,";")
            at_clock(clk) += Seq("end")
         }
      }

      // Declares an intermediate wire to hold a large enough random number.
      // Then, return the correct number of bits selected from the random value
      def rand_string(t: Type) : Seq[Any] = {
         val nx = namespace.newTemp
         val rand = VRandom(bitWidth(t))
         val tx = SIntType(IntWidth(rand.realWidth))
         declare("reg",nx, tx)
         initials += Seq(wref(nx, tx), " = ", VRandom(bitWidth(t)), ";")
         Seq(nx, "[", bitWidth(t) - 1, ":0]")
      }

      def initialize(e: Expression) = {
        initials += Seq("`ifdef RANDOMIZE_REG_INIT")
        initials += Seq(e, " = ", rand_string(e.tpe), ";")
        initials += Seq("`endif")
      }

      def initialize_mem(s: DefMemory) {
        val index = wref("initvar", s.dataType)
        val rstring = rand_string(s.dataType)
        initials += Seq("`ifdef RANDOMIZE_MEM_INIT")
        initials += Seq("for (initvar = 0; initvar < ", s.depth, "; initvar = initvar+1)")
        initials += Seq(tab, WSubAccess(wref(s.name, s.dataType), index, s.dataType, FEMALE),
                             " = ", rstring,";")
        initials += Seq("`endif")
      }

      def simulate(clk: Expression, en: Expression, s: Seq[Any], cond: Option[String]) {
        if (!at_clock.contains(clk)) at_clock(clk) = ArrayBuffer[Seq[Any]]()
        at_clock(clk) += Seq("`ifndef SYNTHESIS")
        if (cond.nonEmpty) {
          at_clock(clk) += Seq(s"`ifdef ${cond.get}")
          at_clock(clk) += Seq(tab, s"if (`${cond.get}) begin")
          at_clock(clk) += Seq("`endif")
        }
        at_clock(clk) += Seq(tab,tab,"if (",en,") begin")
        at_clock(clk) += Seq(tab,tab,tab,s)
        at_clock(clk) += Seq(tab,tab,"end")
        if (cond.nonEmpty) {
          at_clock(clk) += Seq(s"`ifdef ${cond.get}")
          at_clock(clk) += Seq(tab,"end")
          at_clock(clk) += Seq("`endif")
        }
        at_clock(clk) += Seq("`endif")
      }

      def stop(ret: Int): Seq[Any] = Seq(if (ret == 0) "$finish;" else "$fatal;")

      def printf(str: StringLit, args: Seq[Expression]): Seq[Any] = {
        val q = '"'.toString
	val strx = s"""$q${VerilogStringLitHandler escape str}$q""" +:
                  (args flatMap (Seq("," , _)))
        Seq("$fwrite(32'h80000002,", strx, ");")
      }

      def build_ports(): Unit = portdefs ++= m.ports.zipWithIndex map {
        case (p, i) => (p.tpe, p.direction) match {
          case (AnalogType(_), _) =>
            Seq("inout", "  ", p.tpe, " ", p.name)
          case (_, Input) =>
            Seq(p.direction, "  ", p.tpe, " ", p.name)
          case (_, Output) =>
            val ex = WRef(p.name, p.tpe, PortKind, FEMALE)
            assign(ex, netlist(ex))
            Seq(p.direction, " ", p.tpe, " ", p.name)
        }
      }

      def build_streams(s: Statement): Statement = s map build_streams match {
        case sx: DefWire =>
          declare("wire",sx.name,sx.tpe)
          val e = wref(sx.name,sx.tpe)
          netlist get e match {
            case Some(n) => assign(e,n)
            case None =>
          }
          sx
        case sx: DefRegister =>
          declare("reg", sx.name, sx.tpe)
          val e = wref(sx.name, sx.tpe)
          update_and_reset(e, sx.clock, sx.reset, sx.init)
          initialize(e)
          sx
        case sx: IsInvalid =>
          val wref = netlist(sx.expr) match { case e: WRef => e }
          declare("reg", wref.name, sx.expr.tpe)
          initialize(wref)
          sx
        case sx: DefNode =>
          declare("wire", sx.name, sx.value.tpe)
          assign(WRef(sx.name, sx.value.tpe, NodeKind, MALE), sx.value)
          sx
        case sx: Stop =>
          val errorString = StringLit(s"${sx.ret}\n".getBytes)
          simulate(sx.clk, sx.en, stop(sx.ret), Some("STOP_COND"))
          sx
        case sx: Print =>
          simulate(sx.clk, sx.en, printf(sx.string, sx.args), Some("PRINTF_COND"))
          sx
        case sx: WDefInstanceConnector =>
          val (module, params) = moduleMap(sx.module) match {
            case ExtModule(_, _, _, extname, params) => (extname, params)
            case Module(_, name, _, _) => (name, Seq.empty)
          }
          val es = create_exps(WRef(sx.name, sx.tpe, InstanceKind, MALE))
          val ps = if (params.nonEmpty) params map stringify mkString ("#(", ", ", ") ") else ""
          instdeclares += Seq(module, " ", ps, sx.name ," (")
          (es zip sx.exprs).zipWithIndex foreach {case ((l, r), i) =>
            val s = Seq(tab, ".", remove_root(l), "(", r, ")")
            if (i != es.size - 1) instdeclares += Seq(s, ",")
            else instdeclares += s
          }
          instdeclares += Seq(");")
          sx
        case sx: DefMemory =>
          declare("reg", sx.name, VectorType(sx.dataType, sx.depth))
          initialize_mem(sx)
          if (sx.readLatency != 0 || sx.writeLatency != 1)
            throw EmitterException("All memories should be transformed into " +
              "blackboxes or combinational by previous passses")
          for (r <- sx.readers) {
            val data = memPortField(sx, r, "data")
            val addr = memPortField(sx, r, "addr")
            val en = memPortField(sx, r, "en")
            // Ports should share an always@posedge, so can't have intermediary wire
            val clk = netlist(memPortField(sx, r, "clk"))

            declare("wire", LowerTypes.loweredName(data), data.tpe)
            declare("wire", LowerTypes.loweredName(addr), addr.tpe)
            // declare("wire", LowerTypes.loweredName(en), en.tpe)

            //; Read port
            assign(addr, netlist(addr)) //;Connects value to m.r.addr
            // assign(en, netlist(en))     //;Connects value to m.r.en
            val mem = WRef(sx.name, memType(sx), MemKind, UNKNOWNGENDER)
            val memPort = WSubAccess(mem, addr, sx.dataType, UNKNOWNGENDER)
            val depthValue = UIntLiteral(sx.depth, IntWidth(BigInt(sx.depth).bitLength))
            val garbageGuard = DoPrim(Geq, Seq(addr, depthValue), Seq(), UnknownType)

            if ((sx.depth & (sx.depth - 1)) == 0)
              assign(data, memPort)
            else
              garbageAssign(data, memPort, garbageGuard)
          }
 
          for (w <- sx.writers) {
            val data = memPortField(sx, w, "data")
            val addr = memPortField(sx, w, "addr")
            val mask = memPortField(sx, w, "mask")
            val en = memPortField(sx, w, "en")
            //Ports should share an always@posedge, so can't have intermediary wire
            val clk = netlist(memPortField(sx, w, "clk"))

            declare("wire", LowerTypes.loweredName(data), data.tpe)
            declare("wire", LowerTypes.loweredName(addr), addr.tpe)
            declare("wire", LowerTypes.loweredName(mask), mask.tpe)
            declare("wire", LowerTypes.loweredName(en), en.tpe)

            //; Write port
            assign(data, netlist(data))
            assign(addr, netlist(addr))
            assign(mask, netlist(mask))
            assign(en, netlist(en))

            val mem = WRef(sx.name, memType(sx), MemKind, UNKNOWNGENDER)
            val memPort = WSubAccess(mem, addr, sx.dataType, UNKNOWNGENDER)
            update(memPort, data, clk, AND(en, mask))
          }

          if (sx.readwriters.nonEmpty)
            throw EmitterException("All readwrite ports should be transformed into " +
              "read & write ports by previous passes")
          sx
        case sx => sx
      }
   
      def emit_streams() {
        emit(Seq("module ", m.name, "("))
        for ((x, i) <- portdefs.zipWithIndex) {
          if (i != portdefs.size - 1)
            emit(Seq(tab, x, ","))
          else
            emit(Seq(tab, x))
        }
        emit(Seq(");"))

        if (declares.isEmpty && assigns.isEmpty) emit(Seq(tab, "always @(*) begin end"))
        for (x <- declares) emit(Seq(tab, x))
        for (x <- instdeclares) emit(Seq(tab, x))
        for (x <- assigns) emit(Seq(tab, x))
        if (initials.nonEmpty) {
          emit(Seq("`ifdef RANDOMIZE"))
          emit(Seq("  integer initvar;"))
          emit(Seq("  initial begin"))
          // This enables test benches to set the random values at time 0.001,
          //  then start the simulation later
          // Verilator does not support delay statements, so they are omitted.
          emit(Seq("    `ifndef verilator"))
          emit(Seq("      #0.002 begin end"))
          emit(Seq("    `endif"))
          for (x <- initials) emit(Seq(tab, x))
          emit(Seq("  end"))
          emit(Seq("`endif"))
        }
 
        for (clk_stream <- at_clock if clk_stream._2.nonEmpty) {
          emit(Seq(tab, "always @(posedge ", clk_stream._1, ") begin"))
          for (x <- clk_stream._2) emit(Seq(tab, tab, x))
          emit(Seq(tab, "end"))
        }
        emit(Seq("endmodule"))
      }

      build_netlist(m.body)
      build_ports()
      build_streams(m.body)
      emit_streams()
      m
   }

   def emit_preamble(implicit w: Writer) {
    emit(Seq(
        "`ifdef RANDOMIZE_GARBAGE_ASSIGN\n",
        "`define RANDOMIZE\n",
        "`endif\n",
        "`ifdef RANDOMIZE_INVALID_ASSIGN\n",
        "`define RANDOMIZE\n",
        "`endif\n",
        "`ifdef RANDOMIZE_REG_INIT\n",
        "`define RANDOMIZE\n",
        "`endif\n",
        "`ifdef RANDOMIZE_MEM_INIT\n",
        "`define RANDOMIZE\n",
        "`endif\n"))
   }

  def passSeq = Seq(
    passes.VerilogWrap,
    passes.VerilogRename,
    passes.VerilogPrep)

  def emit(state: CircuitState, writer: Writer): Unit = {
    val circuit = runPasses(state.circuit)
    emit_preamble(writer)
    val moduleMap = (circuit.modules map (m => m.name -> m)).toMap
    circuit.modules foreach {
      case (m: Module) => emit_verilog(m, moduleMap)(writer)
      case (m: ExtModule) =>
    }
  }
}
