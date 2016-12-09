package firrtl
//import java.nio.file.{Paths, Files}
import java.io.{Reader, Writer, StringWriter}
import logger._

//import scala.sys.process._
//import scala.io.Source
import ir._
import passes._
import Mappers._
import PrimOps._
//import firrtl.WrappedExpression._
import Utils._
import MemPortUtils.{memPortField, memType}
import MemPortUtils.{memPortField, memType}
import memlib.AnalysisUtils.{getOrigin, getConnects, Connects}
//// Datastructures
//import scala.collection.mutable.{ArrayBuffer, LinkedHashMap, HashSet}

/* x Add register assignments and simulation stuff to clock-specific Blocks
 * x Rewrite reg reset as mux
 * x Declare invalids as reg's
 * o Initialize mem's randomly
 * x Assign values with instances, add defname/params for extmodules
 */

case class EmitterException(message: String) extends PassException(message)
case class DefMemoryConnector(
    info: Info,
    name: String,
    dataType: Type,
    depth: Int,
    writeLatency: Int,
    readLatency: Int,
    readers: Seq[String],
    writers: Seq[String],
    readwriters: Seq[String],
    // TODO: handle read-under-write
    readUnderWrite: Option[String] = None,
    init: String) extends Statement with IsDeclaration {
  def serialize: String =
    s"mem $name :" + info.serialize +
    indent(
      (Seq("\ndata-type => " + dataType.serialize,
          "depth => " + depth,
          "read-latency => " + readLatency,
          "write-latency => " + writeLatency) ++
          (readers map ("reader => " + _)) ++
          (writers map ("writer => " + _)) ++
          (readwriters map ("readwriter => " + _)) ++
       Seq("read-under-write => undefined")) mkString "\n")
  def mapStmt(f: Statement => Statement): Statement = this
  def mapExpr(f: Expression => Expression): Statement = this
  def mapType(f: Type => Type): Statement = this.copy(dataType = f(dataType))
  def mapString(f: String => String): Statement = this.copy(name = f(name))
}

case class GarbageConnect(i: Info, loc: Expression, exp: Expression) extends Statement {
  def serialize: String = s"${loc.serialize} <==garbage== ${exp.serialize}"
  def mapStmt(f: Statement => Statement): Statement = this
  def mapExpr(f: Expression => Expression): Statement = this.copy(loc = f(loc), exp = f(exp))
  def mapType(f: Type => Type): Statement = this
  def mapString(f: String => String): Statement = this
}

case class Garbage(tpe: Type, start: Option[BigInt]) extends Expression {
  def serialize: String = s"GARBAGE(start)"
  def mapExpr(f: Expression => Expression): Expression = this
  def mapType(f: Type => Type): Expression = this.copy(tpe = f(tpe))
  def mapWidth(f: Width => Width): Expression = this
  def mapString(f: String => String): Expression = this
}

class AddMemoryConnector extends Pass {
  def name = this.getClass.getSimpleName
  private def AND(e1: Expression, e2: Expression) = DoPrim(And, Seq(e1, e2), Nil, BoolType)
  private def onModule(m: DefModule): DefModule = {
    val connects = getConnects(m)
    val namespace = Namespace(m)
    val readPortAssignments = collection.mutable.ArrayBuffer[Statement]()
    def addReadPortAssignments(s: Statement): Statement = Block(Seq(s) ++ readPortAssignments)
    m map onStmt(connects, namespace, readPortAssignments) map addReadPortAssignments
  }
  private def onExp(e: Expression): Expression = e match {
    case WSubField(exp, "data", t, MALE) if kind(exp) == MemKind =>
      WRef(LowerTypes.loweredName(e), t, WireKind, MALE)
    case ex => ex map onExp
  }
  private def onStmt(connects: Connects, namespace: Namespace, readPortAssignments: collection.mutable.ArrayBuffer[Statement])(s: Statement): Statement = s map onExp match {
    case sx: DefMemory =>
      val stmts = collection.mutable.ArrayBuffer[Statement]()
      stmts += sx
      for (r <- sx.readers) {
        //Read port data could be read from twice, so must keep intermediate wire
        //val clk = getOrigin(connects, memPortField(sx, r, "clk"))
        //val en = getOrigin(connects, memPortField(sx, r, "en"))
        val lowName = LowerTypes.loweredName(memPortField(sx, r, "data"))
        val lowExpr = WRef(lowName, UnknownType, WireKind, FEMALE)
        val addr = getOrigin(connects)(memPortField(sx, r, "addr"))
        val mem = WRef(sx.name, memType(sx), MemKind, UNKNOWNGENDER)
        val memPort = WSubAccess(mem, addr, sx.dataType, UNKNOWNGENDER)
        val depthValue = UIntLiteral(sx.depth, IntWidth(BigInt(sx.depth).bitLength))
        val garbageGuard = DoPrim(Geq, Seq(addr, depthValue), Seq(), UnknownType)
        stmts += DefWire(sx.info, lowName, sx.dataType)
        if ((sx.depth & (sx.depth - 1)) == 0)
          readPortAssignments += Connect(sx.info, lowExpr, memPort)
        else {
          val newName = namespace.newName("INVALID")
          //readPortAssignments += DefWire(sx.info, newName, memPort.tpe)
          //readPortAssignments += IsInvalid(sx.info, WRef(newName, memPort.tpe, WireKind, MALE))
          val tval = Garbage(memPort.tpe, None)
          val muxTpe = mux_type_and_widths(tval, memPort)
          readPortAssignments += Connect(sx.info, lowExpr, ConstProp.constProp(connects)(Mux(garbageGuard, tval, memPort, muxTpe)))
        }
      }

      for (w <- sx.writers) {
        val data = getOrigin(connects)(memPortField(sx, w, "data"))
        val addr = getOrigin(connects)(memPortField(sx, w, "addr"))
        val clk = getOrigin(connects)(memPortField(sx, w, "clk"))
        val en = getOrigin(connects)(memPortField(sx, w, "en"))
        val mask = getOrigin(connects)(memPortField(sx, w, "mask"))
        //println(data.serialize)
        stmts += CDefMPort(sx.info, "", UnknownType, sx.name, Seq(data, addr, clk, AND(en, mask)), MWrite)
      }

      if (sx.readwriters.nonEmpty)
        throw EmitterException("All readwrite ports should be transformed into " +
          "read & write ports by previous passes")
      Block(stmts.toSeq)
    case Connect(i, WSubField(WSubField(WRef(mem, _, MemKind, _), port, _, _), field, _, _), r) =>
      EmptyStmt
    case sx => sx map onStmt(connects, namespace, readPortAssignments)
  }
  def run(c: Circuit): Circuit = c.copy(modules = c.modules map onModule)
}

case class DefInvalid(info: Info, name: String, tpe: Type) extends Statement with IsDeclaration {
  def serialize: String = s"invalid $name : ${tpe.serialize}" + info.serialize
  def mapStmt(f: Statement => Statement): Statement = this
  def mapExpr(f: Expression => Expression): Statement = this
  def mapType(f: Type => Type): Statement = DefInvalid(info, name, f(tpe))
  def mapString(f: String => String): Statement = DefInvalid(info, f(name), tpe)
}

case class InvalidBlock(info: Info, name: String, width: BigInt, exps: Seq[(Expression, BigInt)]) extends Statement {
  def serialize: String = "InvalidBlock"
  def mapStmt(f: Statement => Statement): Statement = this
  def mapExpr(f: Expression => Expression): Statement = this
  def mapType(f: Type => Type): Statement = this
  def mapString(f: String => String): Statement = this.copy(name = f(name))
}

class RewireInvalidWires extends Pass {
  def name = this.getClass.getSimpleName
  private def onModule(m: DefModule): DefModule = {
    val invalidExps = collection.mutable.ArrayBuffer[(Expression, BigInt)]()
    var randomWidth = BigInt(0)
    val namespace = Namespace(m)
    val randomName = namespace.newName("_RANDOM")
    def buildRand(t: Type): Expression = {
      val width = bitWidth(t)
      val e = DoPrim(Bits, Seq(WRef(randomName, UIntType(UnknownWidth), ExpKind, MALE)), Seq(randomWidth + width - 1, randomWidth), UIntType(IntWidth(width)))
      randomWidth += width
      e
    }
    def buildExp(e: Expression): Expression = e match {
      case Garbage(tpe, None) => buildRand(tpe)
      case e => e map buildExp
    }
    def buildStmt(s: Statement): Statement = s map buildExp match {
      case IsInvalid(i, e) => Connect(i, e, buildRand(e.tpe))
      case DefMemory(i, n, dt, depth, wL, rL, rds, wrs, rws, ruw) =>
        invalidExps += ((WRef(n, VectorType(dt, depth), MemKind, MALE), randomWidth))
        randomWidth += bitWidth(dt) * depth
        s
      case DefRegister(i, n, t, clk, reset, init) =>
        invalidExps += ((WRef(n, t, RegKind, MALE), randomWidth))
        randomWidth += bitWidth(t)
        s
      case sx => sx map buildStmt
    }
    def onStmt(s: Statement): Statement =
      Block(Seq(DefInvalid(NoInfo, randomName, UIntType(IntWidth(randomWidth))), s, InvalidBlock(NoInfo, randomName, randomWidth, invalidExps)))
    m map buildStmt map onStmt
  }
  def run(c: Circuit): Circuit = c.copy(modules = c.modules map onModule)
}

class InlineRegisterReset extends Pass {
  def name = this.getClass.getSimpleName
  private def onModule(m: DefModule): DefModule = {
    val registerResets = collection.mutable.HashMap[String, (Expression, Expression)]()
    def onStmt(s: Statement): Statement = s match {
      case DefRegister(i, n, t, c, reset, init) =>
        registerResets(n) = (reset, init)
        DefRegister(i, n, t, c, zero, zero)
      case Connect(i, WRef(n, t, RegKind, g), r) =>
        val (reset, init) = registerResets(n)
        Connect(i, WRef(n, t, RegKind, g), Mux(reset, init, r, mux_type_and_widths(init, r)))
      case sx => sx map onStmt
    }
    m map onStmt
  }
  def run(c: Circuit): Circuit = c.copy(modules = c.modules map onModule)
}


private case class ClockBlock(clock: Expression, stmts: Seq[Statement]) extends Statement {
  def serialize: String =
    s"under ${clock.serialize} :" +
    indent("\n" + (stmts map (_.serialize) mkString "\n"))
  def mapStmt(f: Statement => Statement): Statement = ClockBlock(clock, stmts map f)
  def mapExpr(f: Expression => Expression): Statement = ClockBlock(f(clock), stmts)
  def mapType(f: Type => Type): Statement = this
  def mapString(f: String => String): Statement = this
}

class BuildClockBlocks extends Pass {
  def name = this.getClass.getSimpleName
  def onModule(m: DefModule): DefModule = {
    val clockBlocks = collection.mutable.HashMap[Expression, Seq[Statement]]()
    val registerClocks = collection.mutable.HashMap[String, Expression]()
    def onStmt(s: Statement): Statement = s match {
      case DefRegister(_, n, _, clk, _, _) =>
        registerClocks(n) = clk
        s
      case CDefMPort(i, _, t, memName, Seq(data, addr, clk, en), MWrite) =>
        clockBlocks(clk) = clockBlocks.getOrElse(clk, Seq[Statement]()).+:( s)
        EmptyStmt
      case Connect(_, WRef(n, _, RegKind, _), r) => //loc only WRef in loform
        val clk = registerClocks(n)
        clockBlocks(clk) = clockBlocks.getOrElse(clk, Seq[Statement]()).+:(s)
        EmptyStmt
      case Print(_, _, _, clk, _) =>
        clockBlocks(clk) = clockBlocks.getOrElse(clk, Seq[Statement]()).+:(s)
        EmptyStmt
      case Stop(_, _, clk, _) =>
        clockBlocks(clk) = clockBlocks.getOrElse(clk, Seq[Statement]()).+:(s)
        EmptyStmt
      case sx => sx map onStmt
    }
    m map onStmt match {
      case e: ExtModule => m
      case Module(i, n, ps, b) => 
        val bodyx = clockBlocks.foldLeft(Seq[Statement](b)) { case (seq, (clk, block)) =>
          seq :+ ClockBlock(clk, block)
        }
        Module(i, n, ps, Block(bodyx))
    }
  }
  def run(c: Circuit): Circuit = c.copy(modules = c.modules map onModule)
}

/** A 
  * one source component
  */

class VerilogWriter(writer: Writer) extends Transform with PassBased {
  def passSeq = Seq(
    passes.VerilogWrap,
    passes.VerilogRename,
    passes.VerilogPrep,
    new AddMemoryConnector(),
    new RewireInvalidWires(),
    new InlineRegisterReset(),
    new BuildClockBlocks()
  )
  def execute(cs: CircuitState): CircuitState = {
    //Logger.globalLevel = LogLevel.Debug
    //val w = new StringWriter()
    val circuit = runPasses(cs.circuit)
    run(circuit, writer)
    cs
  }
  def run(c: Circuit, w: Writer) = {
    c.modules map onModule(w)
  }
  private def vwidth(w: Width): String = w match {
    case IntWidth(w) =>
      val msb = w - 1
      if (msb > 0) s"[${msb.toString(10)}:0]"
      else ""
    case w => throwInternalError(w)
  }
  private def vwidth(t: Type): String = t match {
    case UIntType(w) => vwidth(w)
    case SIntType(w) => vwidth(w)
    case ClockType => ""
    case AnalogType(w) => vwidth(w)
    case t => throwInternalError(t)
  }
  def stringify(param: Param): String = param match {
    case IntParam(name, value) => s".$name($value)"
    case DoubleParam(name, value) => s".$name($value)"
    case StringParam(name, value) =>
      val strx = "\"" + VerilogStringLitHandler.escape(value) + "\""
      s".${name}($strx)"
    case RawStringParam(name, value) => s".$name($value)"
  }
  private def onRegisterMuxes(tabs: String, r: String, e: Expression, connects: Connects): Seq[String] = 
    ConstProp.constProp(connects)(e) match {
      case Mux(cond, tval, fval, t) =>
        val w = new StringWriter()
        val ifStatement = s"${tabs}if (${vString(cond)}) begin\n"
        val trueCase = onRegisterMuxes(s"$tabs  ", r, tval, connects)
        val elseStatement = s"${tabs}end else begin\n"
        val ifNotStatement = s"${tabs}if (!${vString(cond)}) begin\n"
        val falseCase = onRegisterMuxes(s"$tabs  ", r, fval, connects)
        val endStatement = s"${tabs}end\n"
        ((trueCase.nonEmpty, falseCase.nonEmpty): @ unchecked) match {
          case (true, true) =>
            ifStatement +: trueCase ++: elseStatement +: falseCase :+ endStatement
          case (true, false) =>
            ifStatement +: trueCase :+ endStatement
          case (false, true) =>
            ifNotStatement +: falseCase :+ endStatement
          case (false, false) =>
            println(r)
            Nil
        }
      case ex if vString(ex) == r => Nil
      case ex => Seq(s"${tabs}$r <= ${vString(ex)};\n")
    }
  private def vBits(hi: BigInt, lo: BigInt): String = if(hi == lo) s"[$hi]" else s"[$hi:$lo]"
  private def vString(e: Expression): String = e match {
    case UIntLiteral(value, IntWidth(width)) => s"$width'h${value.toString(16)}"
    case SIntLiteral(value, IntWidth(width)) =>
      val unsignedValue = value + (if (value < 0) BigInt(1) << width.toInt else 0)
      s"$width'sh${unsignedValue.toString(16)}"
    case WRef(n, _, _, _) => n
    case WSubAccess(WRef(n, _, _, _), addr, _, _) => s"$n[${vString(addr)}]"
    case Mux(p, tval, fval, t) => 
      def cast(e: Expression): String = e.tpe match {
        case UIntType(_) => s"$$unsigned(${vString(e)})"
        case SIntType(_) => s"$$signed(${vString(e)})"
        case _ => throwInternalError(e)
      }
      s"${vString(p)} ? ${cast(tval)} : ${cast(fval)}"
    case DoPrim(op, args, consts, doprimTpe) => 
      val argsAsStrings: Seq[String] = args map vString
      val argsSignedIfAnySigned: Seq[String] = args.map(_.tpe).collectFirst { case t: SIntType => t } match {
        case Some(_) => argsAsStrings.map { x => s"$$signed($x)" }
        case None => argsAsStrings
      }
      val argsSignedIfOpSigned: Seq[String] = doprimTpe match {
        case SIntType(_) => argsAsStrings.map { x => s"$$signed($x)" }
        case _ => argsAsStrings
      }
      val argsCastToType: Seq[String] = args map {
        e => e.tpe match {
          case UIntType(_) => s"$$unsigned(${vString(e)})"
          case SIntType(_) => s"$$signed(${vString(e)})"
          case t => throwInternalError(t)
        }
      }

      def a0 = argsAsStrings(0)
      def a1 = argsAsStrings(1)

      def a0SignedIfAnySigned = argsSignedIfAnySigned(0)
      def a1SignedIfAnySigned = argsSignedIfAnySigned(1)

      def a0SignedIfOpSigned = argsSignedIfOpSigned(0)

      def a0CastToType = argsCastToType(0)
      def a1CastToType = argsCastToType(1)

      def c0 = consts(0)
      def c1 = consts(1)
        
      val x: String = op match {
        case Add => s"$a0SignedIfAnySigned + $a1SignedIfAnySigned"
        case Addw => s"$a0SignedIfAnySigned + $a1SignedIfAnySigned"
        case Sub => s"$a0SignedIfAnySigned - $a1SignedIfAnySigned"
        case Subw => s"$a0SignedIfAnySigned - $a1SignedIfAnySigned"
        case Mul => s"$a0SignedIfAnySigned * $a1SignedIfAnySigned"
        case Div => s"$a0SignedIfAnySigned / $a1SignedIfAnySigned"
        case Rem => s"$a0SignedIfAnySigned % $a1SignedIfAnySigned"
        case Lt => s"$a0SignedIfAnySigned < $a1SignedIfAnySigned"
        case Leq => s"$a0SignedIfAnySigned <= $a1SignedIfAnySigned"
        case Gt => s"$a0SignedIfAnySigned > $a1SignedIfAnySigned"
        case Geq => s"$a0SignedIfAnySigned >= $a1SignedIfAnySigned"
        case Eq => s"$a0SignedIfAnySigned == $a1SignedIfAnySigned"
        case Neq => s"$a0SignedIfAnySigned != $a1SignedIfAnySigned"
        case Pad => (bitWidth(args(0).tpe).toInt, doprimTpe) match {
          case (0, _) => a0
          case (1, SIntType(_)) => s"{$c0{$a0}}"
          case (w, SIntType(_)) => s"{{${consts(0) - w}{$a0[${w - 1}]}}, $a0}"
          case (w, UIntType(_)) => s"{{${consts(0) - w}'d0}, $a0}"
          case x => throwInternalError(x)
        }
        case AsUInt => s"$$unsigned($a0)"
        case AsSInt => s"$$signed($a0)"
        case AsClock => s"$$unsigned($a0)"
        case Dshlw => s"$a0SignedIfOpSigned << $a1"
        case Dshl => s"$a0SignedIfOpSigned << $a1"
        case Dshr => doprimTpe match {
          case SIntType(_) => s"$a0SignedIfOpSigned >>> $a1"
          case UIntType(_) => s"$a0SignedIfOpSigned >> $a1"
          case x => throwInternalError(x)
        }
        case Shlw => s"$a0SignedIfOpSigned << $c0"
        case Shl => s"$a0SignedIfOpSigned << $c0"
        case Shr if c0 >= bitWidth(args(0).tpe) =>
          error("Verilog emitter does not support SHIFT_RIGHT >= arg width")
        case Shr => s"$a0[${bitWidth(args(0).tpe) - 1}:$c0]"
        case Neg => s"-{$a0SignedIfOpSigned}"
        case Cvt => args(0).tpe match {
          case UIntType(_) => s"{1'b0, $a0SignedIfOpSigned}"
          case SIntType(_) => a0SignedIfOpSigned
          case x => throwInternalError(x)
        }
        case Not => s"~ $a0"
        case And => s"$a0CastToType & $a1CastToType"
        case Or => s"$a0CastToType | $a1CastToType"
        case Xor => s"$a0CastToType ^ $a1CastToType"
        case Andr => s"&$a0SignedIfOpSigned"
        case Orr => s"|$a0SignedIfOpSigned"
        case Xorr => s"^$a0SignedIfOpSigned"
        case Cat => s"{$a0SignedIfOpSigned, $a1CastToType}"
        // If selecting zeroth bit and single-bit wire, just emit the wire
        case Bits if c0 == 0 && c1 == 0 && args(0).tpe == UIntType(UnknownWidth) =>  s"$a0[$c0]"
        case Bits if c0 == 0 && c1 == 0 && bitWidth(args(0).tpe) == BigInt(1) => a0
        case Bits if c0 == c1 => s"$a0[$c0]"
        case Bits => s"$a0[$c0:$c1]"
        case Head =>
          val width = bitWidth(args(0).tpe)
          val high = width - 1
          val low = width - c0
          s"$a0[$high:$low]"
        case Tail =>
          val width = bitWidth(args(0).tpe)
          val low = width - c0 - 1
          s"$a0[$low:0]"
      }
      x
    case x => throwInternalError(x)
  }
  private def vInfoLine(i: Info) = i match {
    case NoInfo => ""
    case FileInfo(info) => s"  /*${i.serialize}*/\n"
  }
  private def vInfo(i: Info) = i match {
    case NoInfo => ""
    case FileInfo(info) => s"  /*${i.serialize}*/"
  }
  private def removeRoot(e: Expression): Expression = e match {
    case WSubField(WRef(_, _, k, _), name, tpe, g) => WRef(name, tpe, k, g)
    case _ => e map removeRoot
  }
  private def onStmt(w: Writer, namespace: Namespace, connects: Connects)(s: Statement): Statement = {
    s match {
      case ClockBlock(clock, stmts) =>
        w.write(s"  always @(posedge ${vString(clock)}) begin\n")
        stmts map onStmt(w, namespace, connects)
        w.write(s"  end\n")
      case Connect(i, WRef(r, t, RegKind, g), exp) =>
        w.write(vInfoLine(i))
        w.write(onRegisterMuxes("    ", r, exp, connects).mkString(""))
      case CDefMPort(i, _, t, memName, Seq(data, addr, clk, en), MWrite) =>
        val mem = WRef(memName, UnknownType, MemKind, UNKNOWNGENDER)
        val memPort = WSubAccess(mem, addr, UnknownType, UNKNOWNGENDER)
        w.write(vInfoLine(i))
        w.write(onRegisterMuxes("    ", vString(memPort), Mux(en, data, memPort, UnknownType), connects).mkString(""))
      case _: Print | _: Stop =>
        val (command, cond, en) = s match {
          case Print(i, string, args, clk, en) =>
            val q = '"'.toString
            val strx =
     s"""$q${VerilogStringLitHandler escape string}$q${(args map { a => "," + vString(a) }).mkString}"""
            (s"$$fwrite(32'h80000002, $strx); ${vInfo(i)}", "PRINTF_COND", vString(en))
          case Stop(i, ret, clk, en) =>
            val command =
              if (ret == 0) s"      $$finish; ${vInfo(i)}"
              else s"$$fatal; ${vInfo(i)}"
            (command, "STOP_COND", vString(en))
        }
        w.write(s"`ifndef SYNTHESIS\n")
        w.write(s"  `ifdef $cond\n")
        w.write(s"  if (`$cond) begin\n")
        w.write(s"  `endif\n")
        w.write(s"    if ($en) begin\n")
        w.write(s"      $command\n")
        w.write(s"    end\n")
        w.write(s"  `ifdef $cond\n")
        w.write(s"  end\n")
        w.write(s"  `endif\n")
        w.write(s"`endif")
      case Connect(i, WRef(n, t, k, g), exp) =>
        w.write(s"  assign $n = ${vString(exp)}; ${vInfo(i)}\n")
      case GarbageConnect(i, WRef(n, _, _, _), Mux(cond, tval, fval, t)) =>
        w.write("`ifndef RANDOMIZE_GARBAGE_ASSIGN\n")
        w.write(s"  assign $n = ${vString(Mux(cond, tval, fval, t))};\n")
        w.write("`else\n")
        w.write(s"  assign $n = ${vString(fval)}; ${vInfo(i)}\n")
        w.write("`endif\n")
      case DefNode(i, n, exp) =>
        w.write(s"  wire ${vwidth(exp.tpe)} $n; ${vInfo(i)}\n")
        w.write(s"  assign $n = ${vString(exp)}; ${vInfo(i)}\n")
      case DefInvalid(i, n, t) =>
        val nWords = (bitWidth(t) + 31) / 32
        val fullWidth = nWords * 32
        w.write(s"  reg [$fullWidth:0] $n;\n")
      case InvalidBlock(i, random, minimumWidth, exps) =>
        val memTemps = collection.mutable.HashMap[String, String]()
        val nWords = (minimumWidth + 31) / 32
        w.write(s"/*======================*\n")
        w.write(s" * INITIALIZATION BLOCK *\n")
        w.write(s" *----------------------*/\n")
        exps.collect {
          case (WRef(n, VectorType(dt, depth), MemKind, _), start) => (n, bitWidth(dt), start)
        } foreach { case (n, width, start) =>
          val tempName = namespace.newName(s"_TEMP_RANDOM_ELEMENT_$n")
          memTemps(n) = tempName
          w.write(s"  reg [${width - 1}:0] $tempName;\n")
        }
        w.write(s"`ifdef RANDOMIZE\n")
        w.write(s"   integer initvar;\n")
        w.write(s"   initial begin\n")
        w.write(s"     `ifndef verilator\n")
        w.write(s"      #0.002 begin end\n")
        w.write(s"     `endif\n")
        val rString = "{" + Seq.fill(nWords.toInt)("$$random").mkString(",") + "}"
        w.write(s"      $random = $rString;\n")
        w.write(s"`ifdef RANDOMIZE_REG_INIT\n")
        exps.collect {
          case (WRef(n, t, RegKind, _), start) => (n, bitWidth(t), start) 
        } foreach { case (n, width, start) =>
          w.write(s"    assign $n = $random${vBits(start + width - 1,start)};\n")
        }
        w.write(s"`endif /*RANDOMIZE_REG_INIT*/\n")
        w.write(s"`ifdef RANDOMIZE_MEM_INIT\n")
        exps.collect {
          case (WRef(n, VectorType(dt, depth), MemKind, _), start) => (n, bitWidth(dt), depth, start)
        } foreach { case (n, width, depth, start) =>
          val max = start + width * depth - 1
          val tempName = memTemps(n)
          w.write(s"  for (initvar = 0; initvar < $depth; initvar = initvar+1) begin\n")
          w.write(s"    $tempName = ($random${vBits(max,start)} << (($depth - initvar) * $width) >> (initvar * $width));\n")
          w.write(s"    $n[initvar] = $tempName${vBits({width - 1},0)};\n")
          w.write(s"  end\n")
        }
        w.write(s"`endif /*RANDOMIZE_MEM_INIT*/\n")
        w.write(s"  end /*initial block*/\n")
        w.write(s"`endif /*RANDOMIZE*/\n")
        w.write(s"/*======================*/\n")
      case DefWire(i, n, t) =>
        w.write(s"  wire ${vwidth(t)} $n; ${vInfo(i)}\n")
      case DefRegister(i, n, t, _, _, _) =>
        w.write(s"  reg ${vwidth(t)} $n; ${vInfo(i)}\n")
      case WDefInstanceConnector(i, n, _, t, exprs, module, params) =>
        val es = create_exps(WRef(n, t, InstanceKind, MALE))
        val ps = if (params.nonEmpty) params map stringify mkString ("#(", ", ", ") ") else ""
        w.write(s"  $module $ps $n (\n")
        (es zip exprs).zipWithIndex foreach {case ((l, r), i) =>
          w.write(s"    .${removeRoot(l).serialize}(${r.serialize})")
          if (i != es.size - 1) w.write(s",\n")
          else w.write(s"\n")
        }
        w.write(");\n")
      case DefMemory(i, n, dt, depth, wL, rL, rds, wrs, rws, ruw) =>
        // Declare Memory
        w.write(s"  reg ${vwidth(dt)} $n [${depth - 1}:0]; ${vInfo(i)}\n")
      case sx: Block => sx map onStmt(w, namespace, connects)
      case EmptyStmt =>
      case _ => throwInternalError(s)
    }
    s
  }
  private def onModule(w: Writer)(m: DefModule): DefModule = m match {
    case Module(i, n, ps, b) =>
      w.write(s"module $n(\n")
      ps.zipWithIndex.foreach { case (p, index) =>
        p match {
          case Port(i, n, d, AnalogType(width)) =>
            w.write(s"  inout  ${vwidth(width)} $n")
          case Port(i, n, Input, t) =>
            w.write(s"  input  ${vwidth(t)} $n")
          case Port(i, n, Output, t) =>
            w.write(s"  output ${vwidth(t)} $n")
        }
        if(index == ps.size - 1) w.write("\n")
        else w.write(",\n")
      }
      w.write(s");\n")
      val namespace = Namespace(m)
      val connects = getConnects(m)
      onStmt(w, namespace, connects)(b)
      w.write(s"endmodule\n")
      m
  }
  val inputForm = LowForm
  val outputForm = LowForm
}

class VerilogEmitter extends Emitter {
  def emit(state: CircuitState, writer: Writer): Unit = {
    new VerilogWriter(writer).execute(state)
  }
}
