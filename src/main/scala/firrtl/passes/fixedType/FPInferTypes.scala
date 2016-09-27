//package dspTools.firrtlPasses
//
//import scala.collection.mutable
//import firrtl.passes.Pass
//import firrtl.PrimOps._
//import firrtl.ir._
//import firrtl.Annotations.ComponentName
//import firrtl.Mappers._
//import firrtl._
//import firrtl.Utils.{get_type, sub_type, module_type, field_type, BoolType, max, min, pow_minus_one}
//
//// These should be distributed into separate files
//object FPInferTypes extends Pass {
//  def name = "Infer Fixed Point Types"
//  def PLUS (w1:Width,w2:Width) : Width = (w1, w2) match {
//    case (IntWidth(i), IntWidth(j)) => IntWidth(i + j)
//    case _ => PlusWidth(w1,w2)
//  }
//  def MAX (w1:Width,w2:Width) : Width = (w1, w2) match {
//    case (IntWidth(i), IntWidth(j)) => IntWidth(max(i,j))
//    case _ => MaxWidth(Seq(w1,w2))
//  }
//  def MINUS (w1:Width,w2:Width) : Width = (w1, w2) match {
//    case (IntWidth(i), IntWidth(j)) => IntWidth(i - j)
//    case _ => MinusWidth(w1,w2)
//  }
//  def POW (w1:Width) : Width = w1 match {
//    case IntWidth(i) => IntWidth(pow_minus_one(BigInt(2), i))
//    case _ => ExpWidth(w1)
//  }
//  def MIN (w1:Width,w2:Width) : Width = (w1, w2) match {
//    case (IntWidth(i), IntWidth(j)) => IntWidth(min(i,j))
//    case _ => MinWidth(Seq(w1,w2))
//  }
//  def ifNoFix(ts: Seq[Type], default: Type) = 
//    ts.filter(_ match {
//      case FixedType(_, _) => true
//      case _ => false
//    }) match {
//      case Seq() => default
//      case _ => UnknownType
//    }
//  def setPrimOpType (e:DoPrim) : DoPrim = {
//    def t1 = e.args(0).tpe
//    def t2 = e.args(1).tpe
//    def t3 = e.args(2).tpe
//    def ts = e.args.map(_.tpe)
//    def w1 = t1 match { case FixedType(w, p) => w } //Intentional
//    def w2 = t2 match { case FixedType(w, p) => w } //Intentional
//    def p1 = t1 match { case FixedType(w, p) => p } //Intentional
//    def p2 = t2 match { case FixedType(w, p) => p } //Intentional
//    def c1 = IntWidth(e.consts(0))
//    def c2 = IntWidth(e.consts(1))
//    val newType = e match {
//      case DoPrim(Add, _, _, t) => (t1, t2) match {
//        case (t1:FixedType, t2:FixedType) => FixedType(PLUS(PLUS(MAX(p1, p2),MAX(MINUS(w1, p1), MINUS(w2, p2))),Utils.ONE), MAX(p1, p2))
//        case _ => ifNoFix(ts, t)
//      }
//      case DoPrim(Sub, _, _, t) => (t1, t2) match {
//        case (t1:FixedType, t2:FixedType) => FixedType(PLUS(PLUS(MAX(p1, p2),MAX(MINUS(w1, p1), MINUS(w2, p2))),Utils.ONE), MAX(p1, p2))
//        case _ => ifNoFix(ts, t)
//      }
//      case DoPrim(Mul, _, _, t) => (t1, t2) match {
//        case (t1:FixedType, t2:FixedType) => FixedType(PLUS(w1, w2),PLUS(p1, p2))
//        case _ => ifNoFix(ts, t)
//      }
//      case DoPrim(Leq, _, _, t) => (t1, t2) match {
//        case (t1:FixedType, t2:FixedType) => BoolType
//        case _ => ifNoFix(ts, t)
//      }
//      case DoPrim(Lt, _, _, t) => (t1, t2) match {
//        case (t1:FixedType, t2:FixedType) => BoolType
//        case _ => ifNoFix(ts, t)
//      }
//      case DoPrim(Geq, _, _, t) => (t1, t2) match {
//        case (t1:FixedType, t2:FixedType) => BoolType
//        case _ => ifNoFix(ts, t)
//      }
//      case DoPrim(Gt, _, _, t) => (t1, t2) match {
//        case (t1:FixedType, t2:FixedType) => BoolType
//        case _ => ifNoFix(ts, t)
//      }
//      case DoPrim(Eq, _, _, t) => (t1, t2) match {
//        case (t1:FixedType, t2:FixedType) => BoolType
//        case _ => ifNoFix(ts, t)
//      }
//      case DoPrim(Neq, _, _, t) => (t1, t2) match {
//        case (t1:FixedType, t2:FixedType) => BoolType
//        case _ => ifNoFix(ts, t)
//      }
//      case DoPrim(Pad, _, _, t) => t1 match {
//        case (t1:FixedType) => FixedType(MAX(w1, c1), p1)
//        case _ => ifNoFix(ts, t)
//      }
//      case DoPrim(AsUInt, _, _, t) => t1 match {
//        case (t1:FixedType) => UIntType(w1)
//        case _ => ifNoFix(ts, t)
//      }
//      case DoPrim(AsSInt, _, _, t) => t1 match {
//        case (t1:FixedType) => SIntType(w1)
//        case _ => ifNoFix(ts, t)
//      }
//      case DoPrim(Bits, _, _, t) => t1 match {
//        case t1:FixedType =>  UIntType(PLUS(MINUS(c1,c2),Utils.ONE))
//        case _ => ifNoFix(ts, t)
//      }
//      case DoPrim(Cat, _, _, t) => (t1, t2) match {
//        case (t1:FixedType, (_:UIntType|_:SIntType)) => FixedType(PLUS(w1, w2), PLUS(p1, w2))
//        case ((_:UIntType|_:SIntType), t2:FixedType) => FixedType(PLUS(w1, w2), p1)
//        case _ => ifNoFix(ts, t)
//      }
//      case DoPrim(Shl, _, _, t) => ifNoFix(ts, t) //TODO(izraelevitz)
//      case DoPrim(Shr, _, _, t) => ifNoFix(ts, t) //TODO(izraelevitz)
//      case DoPrim(Dshl, _, _, t) => ifNoFix(ts, t)
//      case DoPrim(Dshr, _, _, t) => ifNoFix(ts, t)
//      case DoPrim(Div, _, _, t) => ifNoFix(ts, t) //TODO(izraelevitz)
//      case DoPrim(Rem, _, _, t) => ifNoFix(ts, t) //TODO(izraelevitz)
//      case DoPrim(Cvt, _, _, t) => ifNoFix(ts, t)
//      case DoPrim(Neg, _, _, t) => ifNoFix(ts, t)
//      case DoPrim(Not, _, _, t) => ifNoFix(ts, t)
//      case DoPrim(And, _, _, t) => ifNoFix(ts, t)
//      case DoPrim(Or, _, _, t) => ifNoFix(ts, t)
//      case DoPrim(Xor, _, _, t) => ifNoFix(ts, t)
//      case DoPrim(Andr, _, _, t) => ifNoFix(ts, t)
//      case DoPrim(Orr, _, _, t) => ifNoFix(ts, t)
//      case DoPrim(Xorr, _, _, t) => ifNoFix(ts, t)
//      case DoPrim(Head, _, _, t) => ifNoFix(ts, t) //TODO(izraelevitz)
//      case DoPrim(Tail, _, _, t) => ifNoFix(ts, t) //TODO(izraelevitz)
//    }
//    DoPrim(e.op, e.args, e.consts, newType)
//  }
//  def muxType(t1: Type, t2: Type, default: Type): Type = (t1,t2) match {
//    case (FixedType(w1, p1), FixedType(w2, p2)) =>
//      FixedType(PLUS(MAX(p1, p2),MAX(MINUS(w1, p1), MINUS(w2, p2))), MAX(p1, p2))
//    case _ => ifNoFix(Seq(t1, t2), default)
//  }
//  def addVarWidth(t: Type)(implicit n: Namespace): Type = t match {
//    case FixedType(UnknownWidth, UnknownWidth) =>
//      FixedType(VarWidth(n.newName("w")), VarWidth(n.newName("w")))
//    case _ => t
//  }
//  def run(c: Circuit): Circuit = {
//    val moduleTypes = mutable.HashMap[String,Type]()
//    implicit val wnamespace = Namespace()
//    def onModule(m:DefModule) : DefModule = {
//      val types = mutable.HashMap[String,Type]()
//      def updateExpType(e:Expression): Expression = e map (updateExpType) match {
//        case ValidIf(cond, value, tpe) => ValidIf(cond, value, value.tpe)
//        case WRef(name, tpe, k, g) => WRef(name, types(name), k, g)
//        case WSubField(exp, name, tpe, g) => WSubField(exp, name, field_type(exp.tpe, name), g)
//        case WSubIndex(exp, value, tpe, g) => WSubIndex(exp, value, sub_type(exp.tpe), g)
//        case WSubAccess(exp, index, tpe, g) => WSubAccess(exp, index, sub_type(exp.tpe), g)
//        case e: DoPrim => setPrimOpType(e)
//        case Mux(cond, tval, fval, tpe) => Mux(cond, tval, fval, muxType(tval.tpe, fval.tpe, tpe))
//        case e: UIntLiteral => e
//        case e: SIntLiteral => e
//      }
//      def updateStmtType(s: Statement): Statement = s match {
//        case DefRegister(info, name, tpe, clock, reset, init) =>
//          val newType = addVarWidth(tpe)
//          types(name) = newType
//          DefRegister(info, name, newType, clock, reset, init) map updateExpType
//        case DefWire(info, name, tpe) =>
//          val newType = addVarWidth(tpe)
//          types(name) = newType
//          DefWire(info, name, newType)
//        case DefNode(info, name, value) =>
//          val newValue = updateExpType(value)
//          val newType = addVarWidth(newValue.tpe)
//          types(name) = newType
//          DefNode(info, name, newValue)
//        case DefMemory(info, name, dt, depth, wL, rL, rs, ws, rws, ruw) =>
//          val newStmt = DefMemory(info, name, addVarWidth(dt), depth, wL, rL, rs, ws, rws, ruw)
//          val newType = get_type(newStmt)
//          types(name) = newType
//          newStmt
//        case WDefInstance(info, name, module, tpe) =>
//          val newType = moduleTypes(module) 
//          types(name) = newType
//          WDefInstance(info, name, module, newType)
//        case s => (s map updateStmtType) map updateExpType
//      }
// 
//      m.ports.foreach(p => types(p.name) = p.tpe)
//      m match {
//        case Module(info, name, ports, body) => Module(info,name,ports,updateStmtType(body))
//        case m:ExtModule => m
//      }
//    }
// 
//    val newModules = for(m <- c.modules) yield { 
//      val newPorts = m.ports.map(p => Port(p.info,p.name,p.direction,addVarWidth(p.tpe)))
//      m match {
//         case Module(info, name, ports, body) => Module(info,name,newPorts,body)
//         case ExtModule(info, name, ports) => ExtModule(info,name,newPorts)
//      }
//    }
//    newModules.foreach(m => moduleTypes(m.name) = module_type(m))
//    Circuit(c.info, newModules.map(onModule(_)), c.main )
//  }
//}
//
//
//// vim: set ts=4 sw=4 et:
