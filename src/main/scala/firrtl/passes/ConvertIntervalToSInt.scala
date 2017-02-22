// See LICENSE for license details.

package firrtl.passes

import scala.collection.mutable
import firrtl.PrimOps._
import firrtl.ir._
import firrtl._
import firrtl.Mappers._
import firrtl.Utils._

/** Replaces FixedType with SIntType, and correctly aligns all binary points
  */
object ConvertIntervalToSInt extends Pass {
  def name = "Convert Interval Types to SInt Types"
  def toSIntType(t: Type): Type = t match {
    case it: IntervalType => it match {
      case IntervalType(IVal(a, b)) => SIntType(it.width)
      case _ => throwInternalError
    }
    case _ => t
  }
  def set(e: Expression): Expression = e match {
    case d: DoPrim => set_primop_type(d)
    case Mux(p, a, b, t) => Mux(p, a, b, mux_type_and_widths(a, b))
    case _ => throwInternalError
  }
  def SIntLit(v: BigInt): SIntLiteral = {
    val width = IntWidth(BigInt(v.bitLength + 1))
    if(v >= 0) SIntLiteral(v, width)
    else {
      val twoComp = (0 until v.bitLength).foldLeft(v) { case(value, i) => v.flipBit(i) } + 1
      //SIntLiteral(twoComp, width)
      SIntLiteral(v, width)
    }
  }
  def trim(t: Type, e: Expression): Expression = {
    if(e.tpe == t) e else (t, e.tpe) match {
      case (SIntType(IntWidth(desired)), SIntType(IntWidth(actual))) => 
        val select =
          if(desired < actual)
            set(DoPrim(Bits, Seq(e), Seq(desired - 1, BigInt(0)), UnknownType))
          else
            set(DoPrim(Pad, Seq(e), Seq(desired - 1), UnknownType))
        val sint = DoPrim(AsSInt, Seq(select), Nil, UnknownType)
        set(sint)
      case (it: IntervalType, _) => trim(SIntType(it.width), e)
      case _ => println(s"$t: $e"); throwInternalError
    }
  }
  def run(c: Circuit): Circuit = {
    val moduleTypes = mutable.HashMap[String,Type]()
    def onModule(m:DefModule) : DefModule = {
      val types = mutable.HashMap[String,Type]()
      def updateExpType(e: Expression): Expression = e match {
        case DoPrim(AsInterval, args, consts, tpe: IntervalType) =>
          set(DoPrim(Cvt, args, Nil, UnknownType))
        case DoPrim(Wrap, args, consts, tpe: IntervalType) => (consts(1), consts(0), args(0).tpe, args(0)) match {
          case (lo, hi, _, _) if (lo == BigInt(0)) && (hi == BigInt(0)) => // [0, 0]
            SIntLit(BigInt(0))
          case (lo, hi, _, _) if (lo == BigInt(0)) && ((hi > BigInt(0)) && ((hi + 1).bitCount == 1)) => // [0, 2^n]
            set(DoPrim(Shr, args, Seq(BigInt(hi.bitLength)), UnknownType))
          case (lo, hi, IntervalType(IVal(min, max)), x) if ((max - min) < (2 * (hi - lo))) && (max > lo) && (hi > min) => // [-(hi-lo), (hi-lo)], where at least part of the intervals overlap
            //val ltLo = hi - (lo - x)
            val ltLoVal = set(DoPrim(Add, Seq(x, SIntLit(hi - lo)), Nil, UnknownType))
            //val gtHi = lo + (x - hi)
            val gtHiVal = set(DoPrim(Add, Seq(x, SIntLit(lo - hi)), Nil, UnknownType))
            println(lo-hi)
            println(SIntLit(lo-hi))
            println(SIntLit(lo-hi).serialize)
            println((lo-hi).bitLength)
            println((lo-hi).byteValue)
            println((lo-hi).underlying)
            println(SIntLit(lo-hi).value.toString(2).substring(1))
            val ltLo = set(DoPrim(Lt, Seq(x, SIntLit(lo)), Nil, UnknownType))
            val gtHi = set(DoPrim(Gt, Seq(x, SIntLit(hi)), Nil, UnknownType))
            if(min >= lo && max <= hi) {
              x
            } else if(min >= lo) {
              set(Mux(gtHi, gtHiVal, x, UnknownType))
            } else if(max <= hi) {
              set(Mux(ltLo, ltLoVal, x, UnknownType))
            } else {
              set(Mux(ltLo, ltLoVal, set(Mux(gtHi, gtHiVal, x, UnknownType)), UnknownType))
            }
          case (lo, hi, IntervalType(i), x) =>
            val sub = set(DoPrim(Sub, Seq(x, SIntLit(lo)), Nil, UnknownType))
            val mod = set(DoPrim(Rem, Seq(sub, SIntLit(hi - lo + 1)), Nil, UnknownType))
            val add = set(DoPrim(Add, Seq(mod, SIntLit(lo)), Nil, UnknownType))
            add
        }
        case DoPrim(op, args, consts, tpe: IntervalType) =>
          set(DoPrim(op, args map updateExpType, consts, UnknownType))
        case _ => e map updateExpType match {
          case ValidIf(cond, value, tpe) => ValidIf(cond, value, value.tpe)
          case WRef(name, tpe, k, g) => WRef(name, types(name), k, g)
          case WSubField(exp, name, tpe, g) => WSubField(exp, name, field_type(exp.tpe, name), g)
          case WSubIndex(exp, value, tpe, g) => WSubIndex(exp, value, sub_type(exp.tpe), g)
          case WSubAccess(exp, index, tpe, g) => WSubAccess(exp, index, sub_type(exp.tpe), g)
          case x => x
        }
      }
      def updateStmtType(s: Statement): Statement = s match {
        case DefRegister(info, name, tpe, clock, reset, init) =>
          val newType = toSIntType(tpe)
          types(name) = newType
          DefRegister(info, name, newType, clock, reset, init) map updateExpType
        case DefWire(info, name, tpe) =>
          val newType = toSIntType(tpe)
          types(name) = newType
          DefWire(info, name, newType)
        case DefNode(info, name, DoPrim(op, args, consts, tpe: IntervalType)) =>
          val value = DoPrim(op, args, consts, tpe)
          val newArg = set(updateExpType(value).asInstanceOf[DoPrim])
          val newValue = trim(tpe, newArg)
          types(name) = newValue.tpe
          DefNode(info, name, newValue)
        case DefNode(info, name, value) =>
          types(name) = value.tpe
          DefNode(info, name, value)
        case DefMemory(info, name, dt, depth, wL, rL, rs, ws, rws, ruw) =>
          val newStmt = DefMemory(info, name, toSIntType(dt), depth, wL, rL, rs, ws, rws, ruw)
          val newType = MemPortUtils.memType(newStmt)
          types(name) = newType
          newStmt
        case WDefInstance(info, name, module, tpe) =>
          val newType = moduleTypes(module) 
          types(name) = newType
          WDefInstance(info, name, module, newType)
        case s => (s map updateStmtType) map updateExpType
      }
 
      m.ports.foreach(p => types(p.name) = p.tpe)
      m match {
        case Module(info, name, ports, body) => Module(info,name,ports,updateStmtType(body))
        case m:ExtModule => m
      }
    }
 
    val newModules = for(m <- c.modules) yield { 
      val newPorts = m.ports.map(p => Port(p.info,p.name,p.direction,toSIntType(p.tpe)))
      m match {
         case Module(info, name, ports, body) => Module(info,name,newPorts,body)
         case ext: ExtModule => ext.copy(ports = newPorts)
      }
    }
    newModules.foreach(m => moduleTypes(m.name) = module_type(m))
    println(c.serialize)
    InferTypes.run(Circuit(c.info, newModules.map(onModule(_)), c.main ))
  }
}
