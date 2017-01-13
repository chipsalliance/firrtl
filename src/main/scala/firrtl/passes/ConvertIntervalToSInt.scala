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
  def trim(t: Type, e: Expression): Expression = {
    if(e.tpe == t) e else (t, e.tpe) match {
      case (SIntType(IntWidth(desired)), SIntType(IntWidth(actual))) => 
        val select =
          if(desired < actual)
            set_primop_type(DoPrim(Bits, Seq(e), Seq(desired - 1, BigInt(0)), UnknownType))
          else
            set_primop_type(DoPrim(Pad, Seq(e), Seq(desired - 1), UnknownType))
        val sint = DoPrim(AsSInt, Seq(select), Nil, UnknownType)
        set_primop_type(sint)
      case (it: IntervalType, _) => trim(SIntType(it.width), e)
      case _ => println(s"$t: $e"); throwInternalError
    }
  }
  def run(c: Circuit): Circuit = {
    val moduleTypes = mutable.HashMap[String,Type]()
    def onModule(m:DefModule) : DefModule = {
      val types = mutable.HashMap[String,Type]()
      def updateExpType(e:Expression): Expression = e match {
        case DoPrim(AsInterval, args, consts, tpe: IntervalType) =>
          set_primop_type(DoPrim(Cvt, args, Nil, UnknownType))
        case DoPrim(op, args, consts, tpe: IntervalType) =>
          set_primop_type(DoPrim(op, args map updateExpType, consts, UnknownType))
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
          val newArg = set_primop_type(updateExpType(value).asInstanceOf[DoPrim])
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
    InferTypes.run(Circuit(c.info, newModules.map(onModule(_)), c.main ))
  }
}
