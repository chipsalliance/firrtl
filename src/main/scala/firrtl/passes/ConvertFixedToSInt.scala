// See LICENSE for license details.

package firrtl.passes

import scala.collection.mutable
import firrtl.PrimOps._
import firrtl.ir._
import firrtl._
import firrtl.Mappers._
import firrtl.Utils.{sub_type, module_type, field_type, BoolType, max, min, pow_minus_one}

/** Replaces FixedType with SIntType, and correctly aligns all binary points
  */
object ConvertFixedToSInt extends Pass {
  def name = "Convert Fixed Types to SInt Types"
  def alignArg(e: Expression, point: BigInt): Expression = e.tpe match {
    case FixedType(IntWidth(w), IntWidth(p)) => // assert(point >= p)
      if((point - p) > 0) {
        DoPrim(Shl, Seq(e), Seq(point - p), UnknownType)
      } else if (point - p < 0) {
        DoPrim(Shr, Seq(e), Seq(p - point), UnknownType)
      } else e
    case FixedType(w, p) => error("Shouldn't be here")
    case _ => e
  }
  def calcPoint(es: Seq[Expression]): BigInt =
    es.map(_.tpe match {
      case FixedType(IntWidth(w), IntWidth(p)) => p
      case _ => BigInt(0)
    }).reduce(max(_, _))
  def toSIntType(t: Type): Type = t match {
    case FixedType(IntWidth(w), IntWidth(p)) => SIntType(IntWidth(w))
    case FixedType(w, p) => error("Shouldn't be here")
    case _ => t
  }
  def run(c: Circuit): Circuit = {
    val moduleTypes = mutable.HashMap[String,Type]()
    def onModule(m:DefModule) : DefModule = {
      val types = mutable.HashMap[String,Type]()
      def updateExpType(e:Expression): Expression = e match {
        case DoPrim(Mul, args, consts, tpe) => e map updateExpType
        case DoPrim(AsFixedPoint, args, consts, tpe) => DoPrim(AsSInt, args, Seq.empty, tpe)
        case DoPrim(BPShl, args, consts, tpe) => DoPrim(Shl, args, consts, tpe)
        case DoPrim(BPShr, args, consts, tpe) => DoPrim(Shr, args, consts, tpe)
        case DoPrim(BPSet, args, consts, FixedType(w, IntWidth(p))) => alignArg(args.head, p)
        case DoPrim(op, args, consts, tpe) =>
          val point = calcPoint(args)
          val newExp = DoPrim(op, args.map(x => alignArg(x, point)), consts, UnknownType)
          newExp map updateExpType match {
            case DoPrim(AsFixedPoint, args, consts, tpe) => DoPrim(AsSInt, args, Seq.empty, tpe)
            case e => e
          }
        case Mux(cond, tval, fval, tpe) =>
          val point = calcPoint(Seq(tval, fval))
          val newExp = Mux(cond, alignArg(tval, point), alignArg(fval, point), UnknownType)
          newExp map updateExpType
        case e: UIntLiteral => e
        case e: SIntLiteral => e
        case _ => e map updateExpType match {
          case ValidIf(cond, value, tpe) => ValidIf(cond, value, value.tpe)
          case WRef(name, tpe, k, g) => WRef(name, types(name), k, g)
          case WSubField(exp, name, tpe, g) => WSubField(exp, name, field_type(exp.tpe, name), g)
          case WSubIndex(exp, value, tpe, g) => WSubIndex(exp, value, sub_type(exp.tpe), g)
          case WSubAccess(exp, index, tpe, g) => WSubAccess(exp, index, sub_type(exp.tpe), g)
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
        case DefNode(info, name, value) =>
          val newValue = updateExpType(value)
          val newType = toSIntType(newValue.tpe)
          types(name) = newType
          DefNode(info, name, newValue)
        case DefMemory(info, name, dt, depth, wL, rL, rs, ws, rws, ruw) =>
          val newStmt = DefMemory(info, name, toSIntType(dt), depth, wL, rL, rs, ws, rws, ruw)
          val newType = MemPortUtils.memType(newStmt)
          types(name) = newType
          newStmt
        case WDefInstance(info, name, module, tpe) =>
          val newType = moduleTypes(module) 
          types(name) = newType
          WDefInstance(info, name, module, newType)
        case Connect(info, loc, exp) => 
          val point = calcPoint(Seq(loc))
          val newExp = alignArg(exp, point)
          Connect(info, loc, newExp) map updateExpType
        case PartialConnect(info, loc, exp) => 
          val point = calcPoint(Seq(loc))
          val newExp = alignArg(exp, point)
          PartialConnect(info, loc, newExp) map updateExpType
        // check Connect case, need to shl
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
    firrtl.passes.InferTypes.run(Circuit(c.info, newModules.map(onModule(_)), c.main ))
  }
}

// vim: set ts=4 sw=4 et:
