/*
Copyright (c) 2014 - 2016 The Regents of the University of
California (Regents). All Rights Reserved.  Redistribution and use in
source and binary forms, with or without modification, are permitted
provided that the following conditions are met:
   * Redistributions of source code must retain the above
     copyright notice, this list of conditions and the following
     two paragraphs of disclaimer.
   * Redistributions in binary form must reproduce the above
     copyright notice, this list of conditions and the following
     two paragraphs of disclaimer in the documentation and/or other materials
     provided with the distribution.
   * Neither the name of the Regents nor the names of its contributors
     may be used to endorse or promote products derived from this
     software without specific prior written permission.
IN NO EVENT SHALL REGENTS BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS,
ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
REGENTS HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE. THE SOFTWARE AND ACCOMPANYING DOCUMENTATION, IF
ANY, PROVIDED HEREUNDER IS PROVIDED "AS IS". REGENTS HAS NO OBLIGATION
TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
MODIFICATIONS.
*/

package firrtl.passes

import com.typesafe.scalalogging.LazyLogging
import java.nio.file.{Paths, Files}

// Datastructures
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayBuffer

import firrtl._
import firrtl.ir._
import firrtl.Utils._
import firrtl.Mappers._
import firrtl.PrimOps._
import firrtl.WrappedExpression._

trait Pass extends LazyLogging {
  def name: String
  def run(c: Circuit): Circuit
}

// Error handling
class PassException(message: String) extends Exception(message)
class PassExceptions(exceptions: Seq[PassException]) extends Exception("\n" + exceptions.mkString("\n"))
class Errors {
  val errors = ArrayBuffer[PassException]()
  def append(pe: PassException) = errors.append(pe)
  def trigger = errors.size match {
    case 0 =>
    case 1 => throw errors.head
    case _ =>
      append(new PassException(s"${errors.length} errors detected!"))
      throw new PassExceptions(errors)
  }
}

// These should be distributed into separate files
object ToWorkingIR extends Pass {
   private var mname = ""
   def name = "Working IR"
   def run (c:Circuit): Circuit = {
      def toExp (e:Expression) : Expression = {
         e map (toExp) match {
            case e:Reference => WRef(e.name, e.tpe, NodeKind(), UNKNOWNGENDER)
            case e:SubField => WSubField(e.expr, e.name, e.tpe, UNKNOWNGENDER)
            case e:SubIndex => WSubIndex(e.expr, e.value, e.tpe, UNKNOWNGENDER)
            case e:SubAccess => WSubAccess(e.expr, e.index, e.tpe, UNKNOWNGENDER)
            case e => e
         }
      }
      def toStmt (s:Statement) : Statement = {
         s map (toExp) match {
            case s:DefInstance => WDefInstance(s.info,s.name,s.module,UnknownType)
            case s => s map (toStmt)
         }
      }
      val modulesx = c.modules.map { m => 
         mname = m.name
         m match {
            case m:Module => Module(m.info,m.name, m.ports, toStmt(m.body))
            case m:ExtModule => m
         }
      }
      Circuit(c.info,modulesx,c.main)
   }
}

object ResolveKinds extends Pass {
   private var mname = ""
   def name = "Resolve Kinds"
   def run (c:Circuit): Circuit = {
      def resolve_kinds (m:DefModule, c:Circuit):DefModule = {
         val kinds = LinkedHashMap[String,Kind]()
         def resolve (body:Statement) = {
            def resolve_expr (e:Expression):Expression = {
               e match {
                  case e:WRef => WRef(e.name,e.tpe,kinds(e.name),e.gender)
                  case e => e map (resolve_expr)
               }
            }
            def resolve_stmt (s:Statement):Statement = s map (resolve_stmt) map (resolve_expr)
            resolve_stmt(body)
         }
   
         def find (m:DefModule) = {
            def find_stmt (s:Statement):Statement = {
               s match {
                  case s:DefWire => kinds(s.name) = WireKind()
                  case s:DefNode => kinds(s.name) = NodeKind()
                  case s:DefRegister => kinds(s.name) = RegKind()
                  case s:WDefInstance => kinds(s.name) = InstanceKind()
                  case s:DefMemory => kinds(s.name) = MemKind(s.readers ++ s.writers ++ s.readwriters)
                  case s => false
               }
               s map (find_stmt)
            }
            m.ports.foreach { p => kinds(p.name) = PortKind() }
            m match {
               case m:Module => find_stmt(m.body)
               case m:ExtModule => false
            }
         }
       
         mname = m.name
         find(m)   
         m match {
            case m:Module => {
               val bodyx = resolve(m.body)
               Module(m.info,m.name,m.ports,bodyx)
            }
            case m:ExtModule => ExtModule(m.info,m.name,m.ports)
         }
      }
      val modulesx = c.modules.map(m => resolve_kinds(m,c))
      Circuit(c.info,modulesx,c.main)
   }
}

object ResolveGenders extends Pass {
   private var mname = ""
   def name = "Resolve Genders"
   def run (c:Circuit): Circuit = {
      def resolve_e (g:Gender)(e:Expression) : Expression = {
         e match {
            case e:WRef => WRef(e.name,e.tpe,e.kind,g)
            case e:WSubField => {
               val expx = 
                  field_flip(e.exp.tpe,e.name) match {
                     case Default => resolve_e(g)(e.exp)
                     case Flip => resolve_e(swap(g))(e.exp)
                  }
               WSubField(expx,e.name,e.tpe,g)
            }
            case e:WSubIndex => {
               val expx = resolve_e(g)(e.exp)
               WSubIndex(expx,e.value,e.tpe,g)
            }
            case e:WSubAccess => {
               val expx = resolve_e(g)(e.exp)
               val indexx = resolve_e(MALE)(e.index)
               WSubAccess(expx,indexx,e.tpe,g)
            }
            case e => e map (resolve_e(g))
         }
      }
            
      def resolve_s (s:Statement) : Statement = {
         s match {
            case s:IsInvalid => {
               val expx = resolve_e(FEMALE)(s.expr)
               IsInvalid(s.info,expx)
            }
            case s:Connect => {
               val locx = resolve_e(FEMALE)(s.loc)
               val expx = resolve_e(MALE)(s.expr)
               Connect(s.info,locx,expx)
            }
            case s:PartialConnect => {
               val locx = resolve_e(FEMALE)(s.loc)
               val expx = resolve_e(MALE)(s.expr)
               PartialConnect(s.info,locx,expx)
            }
            case s => s map (resolve_e(MALE)) map (resolve_s)
         }
      }
      val modulesx = c.modules.map { 
         m => {
            mname = m.name
            m match {
               case m:Module => {
                  val bodyx = resolve_s(m.body)
                  Module(m.info,m.name,m.ports,bodyx)
               }
               case m:ExtModule => m
            }
         }
      }
      Circuit(c.info,modulesx,c.main)
   }
}

object PullMuxes extends Pass {
   def name = "Pull Muxes"
   def run(c: Circuit): Circuit = {
     def pull_muxes_e(e: Expression): Expression = {
       val ex = e map (pull_muxes_e) match {
         case (e: WSubField) => e.exp match {
           case (ex: Mux) => Mux(ex.cond, 
              WSubField(ex.tval, e.name, e.tpe, e.gender), 
              WSubField(ex.fval, e.name, e.tpe, e.gender), e.tpe)
           case (ex: ValidIf) => ValidIf(ex.cond, 
              WSubField(ex.value, e.name, e.tpe, e.gender), e.tpe)
           case (ex) => e
         }
         case (e: WSubIndex) => e.exp match {
           case (ex: Mux) => Mux(ex.cond, 
              WSubIndex(ex.tval, e.value, e.tpe, e.gender), 
              WSubIndex(ex.fval, e.value, e.tpe, e.gender), e.tpe)
           case (ex: ValidIf) => ValidIf(ex.cond, 
              WSubIndex(ex.value, e.value, e.tpe, e.gender), e.tpe)
           case (ex) => e
         }
         case (e: WSubAccess) => e.exp match {
           case (ex: Mux) => Mux(ex.cond, 
              WSubAccess(ex.tval, e.index, e.tpe, e.gender), 
              WSubAccess(ex.fval, e.index, e.tpe, e.gender), e.tpe)
           case (ex: ValidIf) => ValidIf(ex.cond, 
              WSubAccess(ex.value, e.index, e.tpe, e.gender), e.tpe)
           case (ex) => e
         }
         case (e) => e
       }
       ex map (pull_muxes_e)
     }
     def pull_muxes(s: Statement): Statement = s map (pull_muxes) map (pull_muxes_e)
     val modulesx = c.modules.map {
       case (m:Module) => Module(m.info, m.name, m.ports, pull_muxes(m.body))
       case (m:ExtModule) => m
     }
     Circuit(c.info, modulesx, c.main)
   }
}

object ExpandConnects extends Pass {
  def name = "Expand Connects"
  def run (c:Circuit): Circuit = {
    def expand_connects (m :Module): Module = {
      val genders = LinkedHashMap[String,Gender]()
      def expand_s(s: Statement): Statement = {
        def set_gender(e: Expression): Expression = e map (set_gender) match {
          case (e: WRef) => WRef(e.name, e.tpe, e.kind, genders(e.name))
          case (e: WSubField) =>
            val f = get_field(e.exp.tpe, e.name)
            val genderx = times(gender(e.exp), f.flip)
            WSubField(e.exp, e.name, e.tpe, genderx)
          case (e: WSubIndex) => WSubIndex(e.exp, e.value, e.tpe, gender(e.exp))
          case (e: WSubAccess) => WSubAccess(e.exp, e.index, e.tpe, gender(e.exp))
          case (e) => e
        }
        s match {
          case (s: DefWire) => genders(s.name) = BIGENDER; s
          case (s: DefRegister) => genders(s.name) = BIGENDER; s
          case (s: WDefInstance) => genders(s.name) = MALE; s
          case (s: DefMemory) => genders(s.name) = MALE; s
          case (s: DefNode) => genders(s.name) = MALE; s
          case (s: IsInvalid) =>
            val invalids = (create_exps(s.expr) foldLeft Seq[Statement]())(
               (invalids,  expx) => gender(set_gender(expx)) match {
                  case BIGENDER => invalids :+ IsInvalid(s.info, expx)
                  case FEMALE => invalids :+ IsInvalid(s.info, expx)
                  case _ => invalids
               }
            )
            invalids.size match {
               case 0 => EmptyStmt
               case 1 => invalids.head
               case _ => Block(invalids)
            }
          case (s: Connect) =>
            val locs = create_exps(s.loc)
            val exps = create_exps(s.expr)
            Block((locs zip exps).zipWithIndex map {case ((locx, expx), i) =>
               get_flip(s.loc.tpe, i, Default) match {
                  case Default => Connect(s.info, locx, expx)
                  case Flip => Connect(s.info, expx, locx)
               }
            })
          case (s: PartialConnect) =>
            val ls = get_valid_points(s.loc.tpe, s.expr.tpe, Default, Default)
            val locs = create_exps(s.loc)
            val exps = create_exps(s.expr)
            Block(ls map {case (x, y) =>
              get_flip(s.loc.tpe, x, Default) match {
                 case Default => Connect(s.info, locs(x), exps(y))
                 case Flip => Connect(s.info, exps(y), locs(x))
              }
            })
          case (s) => s map (expand_s)
        }
      }

      m.ports.foreach { p => genders(p.name) = to_gender(p.direction) }
      Module(m.info, m.name, m.ports, expand_s(m.body))
    }
 
    val modulesx = c.modules.map { 
       case (m: ExtModule) => m
       case (m: Module) => expand_connects(m)
    }
    Circuit(c.info, modulesx, c.main)
  }
}


// Replace shr by amount >= arg width with 0 for UInts and MSB for SInts
// TODO replace UInt with zero-width wire instead
object Legalize extends Pass {
  def name = "Legalize"
  def legalizeShiftRight (e: DoPrim): Expression = e.op match {
    case Shr => {
      val amount = e.consts(0).toInt
      val width = long_BANG(e.args.head.tpe)
      lazy val msb = width - 1
      if (amount >= width) {
        e.tpe match {
          case t: UIntType => UIntLiteral(0, IntWidth(1))
          case t: SIntType =>
            DoPrim(Bits, e.args, Seq(msb, msb), SIntType(IntWidth(1)))
          case t => error(s"Unsupported type ${t} for Primop Shift Right")
        }
      } else {
        e
      }
    }
    case _ => e
  }
  def legalizeConnect(c: Connect): Statement = {
    val t = c.loc.tpe
    val w = long_BANG(t)
    if (w >= long_BANG(c.expr.tpe)) c
    else {
      val newType = t match {
        case _: UIntType => UIntType(IntWidth(w))
        case _: SIntType => SIntType(IntWidth(w))
      }
      Connect(c.info, c.loc, DoPrim(Bits, Seq(c.expr), Seq(w-1, 0), newType))
    }
  }
  def run (c: Circuit): Circuit = {
    def legalizeE (e: Expression): Expression = {
      e map (legalizeE) match {
        case e: DoPrim => legalizeShiftRight(e)
        case e => e
      }
    }
    def legalizeS (s: Statement): Statement = {
      val legalizedStmt = s match {
        case c: Connect => legalizeConnect(c)
        case _ => s
      }
      legalizedStmt map legalizeS map legalizeE
    }
    def legalizeM (m: DefModule): DefModule = m map (legalizeS)
    Circuit(c.info, c.modules.map(legalizeM), c.main)
  }
}

object VerilogWrap extends Pass {
   def name = "Verilog Wrap"
   var mname = ""
   def v_wrap_e (e:Expression) : Expression = {
      e map (v_wrap_e) match {
         case (e:DoPrim) => {
            def a0 () = e.args(0)
            if (e.op == Tail) {
               (a0()) match {
                  case (e0:DoPrim) => {
                     if (e0.op == Add) DoPrim(Addw,e0.args,Seq(),e.tpe)
                     else if (e0.op == Sub) DoPrim(Subw,e0.args,Seq(),e.tpe)
                     else e
                  }
                  case (e0) => e
               }
            }
            else e
         }
         case (e) => e
      }
   }
   def v_wrap_s (s:Statement) : Statement = {
      s map (v_wrap_s) map (v_wrap_e) match {
        case s: Print =>
           Print(s.info, VerilogStringLitHandler.format(s.string), s.args, s.clk, s.en)
        case s => s
      }
   }
   def run (c:Circuit): Circuit = {
      val modulesx = c.modules.map{ m => {
         (m) match {
            case (m:Module) => {
               mname = m.name
               Module(m.info,m.name,m.ports,v_wrap_s(m.body))
            }
            case (m:ExtModule) => m
         }
      }}
      Circuit(c.info,modulesx,c.main)
   }
}

object VerilogRename extends Pass {
   def name = "Verilog Rename"
   def run (c:Circuit): Circuit = {
      def verilog_rename_n (n:String) : String = {
         if (v_keywords.contains(n)) (n + "$") else n
      }
      def verilog_rename_e (e:Expression) : Expression = {
         (e) match {
           case (e:WRef) => WRef(verilog_rename_n(e.name),e.tpe,kind(e),gender(e))
           case (e) => e map (verilog_rename_e)
         }
      }
      def verilog_rename_s (s:Statement) : Statement = {
        s map (verilog_rename_s) map (verilog_rename_e) map (verilog_rename_n)
      }
      val modulesx = c.modules.map{ m => {
         val portsx = m.ports.map{ p => {
            Port(p.info,verilog_rename_n(p.name),p.direction,p.tpe)
         }}
         m match {
            case (m:Module) => Module(m.info,m.name,portsx,verilog_rename_s(m.body))
            case (m:ExtModule) => m
         }
      }}
      Circuit(c.info,modulesx,c.main)
   }
}

object CInferMDir extends Pass {
   def name = "CInfer MDir"
   var mname = ""
   def run (c:Circuit) : Circuit = {
      def infer_mdir (m:DefModule) : DefModule = {
         val mports = LinkedHashMap[String,MPortDir]()
         def infer_mdir_e (dir:MPortDir)(e:Expression) : Expression = {
            (e map (infer_mdir_e(dir))) match { 
               case (e:Reference) => {
                  if (mports.contains(e.name)) {
                     val new_mport_dir = {
                        (mports(e.name),dir) match {
                           case (MInfer,MInfer) => error("Shouldn't be here")
                           case (MInfer,MWrite) => MWrite
                           case (MInfer,MRead) => MRead
                           case (MInfer,MReadWrite) => MReadWrite
                           case (MWrite,MInfer) => error("Shouldn't be here")
                           case (MWrite,MWrite) => MWrite
                           case (MWrite,MRead) => MReadWrite
                           case (MWrite,MReadWrite) => MReadWrite
                           case (MRead,MInfer) => error("Shouldn't be here")
                           case (MRead,MWrite) => MReadWrite
                           case (MRead,MRead) => MRead
                           case (MRead,MReadWrite) => MReadWrite
                           case (MReadWrite,MInfer) => error("Shouldn't be here")
                           case (MReadWrite,MWrite) => MReadWrite
                           case (MReadWrite,MRead) => MReadWrite
                           case (MReadWrite,MReadWrite) => MReadWrite
                        }
                     }
                     mports(e.name) = new_mport_dir
                  }
                  e
               }
               case (e) => e
            }
         }
         def infer_mdir_s (s:Statement) : Statement = {
            (s) match { 
               case (s:CDefMPort) => {
                  mports(s.name) = s.direction
                  s map (infer_mdir_e(MRead))
               }
               case (s:Connect) => {
                  infer_mdir_e(MRead)(s.expr)
                  infer_mdir_e(MWrite)(s.loc)
                  s
               }
               case (s:PartialConnect) => {
                  infer_mdir_e(MRead)(s.expr)
                  infer_mdir_e(MWrite)(s.loc)
                  s
               }
               case (s) => s map (infer_mdir_s) map (infer_mdir_e(MRead))
            }
         }
         def set_mdir_s (s:Statement) : Statement = {
            (s) match { 
               case (s:CDefMPort) => 
                  CDefMPort(s.info,s.name,s.tpe,s.mem,s.exps,mports(s.name))
               case (s) => s map (set_mdir_s)
            }
         }
         (m) match { 
            case (m:Module) => {
               infer_mdir_s(m.body)
               Module(m.info,m.name,m.ports,set_mdir_s(m.body))
            }
            case (m:ExtModule) => m
         }
      }
   
      //; MAIN
      Circuit(c.info, c.modules.map(m => infer_mdir(m)), c.main)
   }
}

case class MPort( val name : String, val clk : Expression)
case class MPorts( val readers : ArrayBuffer[MPort], val writers : ArrayBuffer[MPort], val readwriters : ArrayBuffer[MPort])
case class DataRef( val exp : Expression, val male : String, val female : String, val mask : String, val rdwrite : Boolean)

object RemoveCHIRRTL extends Pass {
   def name = "Remove CHIRRTL"
   var mname = ""
   def create_exps (e:Expression) : Seq[Expression] = e match {
      case (e:Mux) =>
         val e1s = create_exps(e.tval)
         val e2s = create_exps(e.fval)
         (e1s,e2s).zipped map ((e1,e2) => Mux(e.cond,e1,e2,mux_type(e1,e2)))
      case (e:ValidIf) =>
         create_exps(e.value) map (e1 => ValidIf(e.cond,e1,e1.tpe))
      case (e) => (e.tpe) match {
         case (_:GroundType) => Seq(e)
         case (t:BundleType) => (t.fields foldLeft Seq[Expression]())((exps, f) =>
            exps ++ create_exps(SubField(e,f.name,f.tpe)))
         case (t:VectorType) => ((0 until t.size) foldLeft Seq[Expression]())((exps, i) =>
            exps ++ create_exps(SubIndex(e,i,t.tpe)))
         case UnknownType => Seq(e)
      }
   }
   def run (c:Circuit) : Circuit = {
      def remove_chirrtl_m (m:Module) : Module = {
         val hash = LinkedHashMap[String,MPorts]()
         val repl = LinkedHashMap[String,DataRef]()
         val raddrs = HashMap[String, Expression]()
         val ut = UnknownType
         val mport_types = LinkedHashMap[String,Type]()
         val smems = HashSet[String]()
         def EMPs () : MPorts = MPorts(ArrayBuffer[MPort](),ArrayBuffer[MPort](),ArrayBuffer[MPort]())
         def collect_smems_and_mports (s:Statement) : Statement = {
            (s) match { 
               case (s:CDefMemory) if s.seq =>
                  smems += s.name
                  s
               case (s:CDefMPort) => {
                  val mports = hash.getOrElse(s.mem,EMPs())
                  s.direction match {
                     case MRead => mports.readers += MPort(s.name,s.exps(1))
                     case MWrite => mports.writers += MPort(s.name,s.exps(1))
                     case MReadWrite => mports.readwriters += MPort(s.name,s.exps(1))
                  }
                  hash(s.mem) = mports
                  s
               }
               case (s) => s map (collect_smems_and_mports)
            }
         }
         def collect_refs (s:Statement) : Statement = {
            (s) match { 
               case (s:CDefMemory) => {
                  mport_types(s.name) = s.tpe
                  val stmts = ArrayBuffer[Statement]()
                  val taddr = UIntType(IntWidth(scala.math.max(1,ceil_log2(s.size))))
                  val tdata = s.tpe
                  def set_poison (vec:Seq[MPort],addr:String) : Unit = {
                     for (r <- vec ) {
                        stmts += IsInvalid(s.info,SubField(SubField(Reference(s.name,ut),r.name,ut),addr,taddr))
                        stmts += IsInvalid(s.info,SubField(SubField(Reference(s.name,ut),r.name,ut),"clk",taddr))
                     }
                  }
                  def set_enable (vec:Seq[MPort],en:String) : Unit = {
                     for (r <- vec ) {
                        stmts += Connect(s.info,SubField(SubField(Reference(s.name,ut),r.name,ut),en,taddr),zero)
                     }}
                  def set_wmode (vec:Seq[MPort],wmode:String) : Unit = {
                     for (r <- vec) {
                        stmts += Connect(s.info,SubField(SubField(Reference(s.name,ut),r.name,ut),wmode,taddr),zero)
                     }}
                  def set_write (vec:Seq[MPort],data:String,mask:String) : Unit = {
                     val tmask = create_mask(s.tpe)
                     for (r <- vec ) {
                        stmts += IsInvalid(s.info,SubField(SubField(Reference(s.name,ut),r.name,ut),data,tdata))
                        for (x <- create_exps(SubField(SubField(Reference(s.name,ut),r.name,ut),mask,tmask)) ) {
                           stmts += Connect(s.info,x,zero)
                        }}}
                  val rds = (hash.getOrElse(s.name,EMPs())).readers
                  set_poison(rds,"addr")
                  set_enable(rds,"en")
                  val wrs = (hash.getOrElse(s.name,EMPs())).writers
                  set_poison(wrs,"addr")
                  set_enable(wrs,"en")
                  set_write(wrs,"data","mask")
                  val rws = (hash.getOrElse(s.name,EMPs())).readwriters
                  set_poison(rws,"addr")
                  set_wmode(rws,"wmode")
                  set_enable(rws,"en")
                  set_write(rws,"wdata","wmask")
                  val read_l = if (s.seq) 1 else 0
                  val mem = DefMemory(s.info,s.name,s.tpe,s.size,1,read_l,rds.map(_.name),wrs.map(_.name),rws.map(_.name))
                  Block(Seq(mem,Block(stmts)))
               }
               case (s:CDefMPort) => {
                  mport_types(s.name) = mport_types(s.mem)
                  val addrs = ArrayBuffer[String]()
                  val clks = ArrayBuffer[String]()
                  val ens = ArrayBuffer[String]()
                  val masks = ArrayBuffer[String]()
                  s.direction match {
                     case MReadWrite => {
                        repl(s.name) = DataRef(SubField(Reference(s.mem,ut),s.name,ut),"rdata","wdata","wmask",true)
                        addrs += "addr"
                        clks += "clk"
                        ens += "en"
                        masks += "wmask"
                     }
                     case MWrite => {
                        repl(s.name) = DataRef(SubField(Reference(s.mem,ut),s.name,ut),"data","data","mask",false)
                        addrs += "addr"
                        clks += "clk"
                        ens += "en"
                        masks += "mask"
                     }
                     case MRead => {
                        repl(s.name) = DataRef(SubField(Reference(s.mem,ut),s.name,ut),"data","data","blah",false)
                        addrs += "addr"
                        clks += "clk"
                        s.exps(0) match {
                           case e: Reference if smems(s.mem) =>
                              raddrs(e.name) = SubField(SubField(Reference(s.mem,ut),s.name,ut),"en",ut)
                           case _ => ens += "en"
                        }
                     }
                  }
                  val stmts = ArrayBuffer[Statement]()
                  for (x <- addrs ) {
                     stmts += Connect(s.info,SubField(SubField(Reference(s.mem,ut),s.name,ut),x,ut),s.exps(0))
                  }
                  for (x <- clks ) {
                     stmts += Connect(s.info,SubField(SubField(Reference(s.mem,ut),s.name,ut),x,ut),s.exps(1))
                  }
                  for (x <- ens ) {
                     stmts += Connect(s.info,SubField(SubField(Reference(s.mem,ut),s.name,ut),x,ut),one)
                  }
                  Block(stmts)
               }
               case (s) => s map (collect_refs)
            }
         }
         def remove_chirrtl_s (s:Statement) : Statement = {
            var has_write_mport = false
            var has_read_mport: Option[Expression] = None
            var has_readwrite_mport: Option[Expression] = None
            def remove_chirrtl_e (g:Gender)(e:Expression) : Expression = {
               (e) match {
                  case (e:Reference) if repl contains e.name =>
                     val vt = repl(e.name)
                     g match {
                        case MALE => SubField(vt.exp,vt.male,e.tpe)
                        case FEMALE => {
                           has_write_mport = true
                           if (vt.rdwrite) 
                              has_readwrite_mport = Some(SubField(vt.exp,"wmode",UIntType(IntWidth(1))))
                           SubField(vt.exp,vt.female,e.tpe)
                        }
                     }
                  case (e:Reference) if g == FEMALE && (raddrs contains e.name) =>
                     has_read_mport = Some(raddrs(e.name))
                     e
                  case (e:Reference) => e
                  case (e:SubAccess) => SubAccess(remove_chirrtl_e(g)(e.expr),remove_chirrtl_e(MALE)(e.index),e.tpe)
                  case (e) => e map (remove_chirrtl_e(g))
               }
            }
            def get_mask (e:Expression) : Expression = {
               (e map (get_mask)) match { 
                  case (e:Reference) => {
                     if (repl.contains(e.name)) {
                        val vt = repl(e.name)
                        val t = create_mask(e.tpe)
                        SubField(vt.exp,vt.mask,t)
                     } else e
                  }
                  case (e) => e
               }
            }
            (s) match {
               case (s:DefNode) => {
                  val stmts = ArrayBuffer[Statement]()
                  val valuex = remove_chirrtl_e(MALE)(s.value)
                  stmts += DefNode(s.info,s.name,valuex)
                  has_read_mport match {
                    case None =>
                    case Some(en) => stmts += Connect(s.info,en,one)
                  }
                  if (stmts.size > 1) Block(stmts)
                  else stmts(0)
               }
               case (s:Connect) => {
                  val stmts = ArrayBuffer[Statement]()
                  val rocx = remove_chirrtl_e(MALE)(s.expr)
                  val locx = remove_chirrtl_e(FEMALE)(s.loc)
                  stmts += Connect(s.info,locx,rocx)
                  has_read_mport match {
                    case None =>
                    case Some(en) => stmts += Connect(s.info,en,one)
                  }
                  if (has_write_mport) {
                     val e = get_mask(s.loc)
                     for (x <- create_exps(e) ) {
                        stmts += Connect(s.info,x,one)
                     }
                     has_readwrite_mport match {
                        case None =>
                        case Some(wmode) => stmts += Connect(s.info,wmode,one)
                     }
                  }
                  if (stmts.size > 1) Block(stmts)
                  else stmts(0)
               }
               case (s:PartialConnect) => {
                  val stmts = ArrayBuffer[Statement]()
                  val locx = remove_chirrtl_e(FEMALE)(s.loc)
                  val rocx = remove_chirrtl_e(MALE)(s.expr)
                  stmts += PartialConnect(s.info,locx,rocx)
                  has_read_mport match {
                    case None =>
                    case Some(en) => stmts += Connect(s.info,en,one)
                  }
                  if (has_write_mport) {
                     val ls = get_valid_points(s.loc.tpe,s.expr.tpe,Default,Default)
                     val locs = create_exps(get_mask(s.loc))
                     for (x <- ls ) {
                        val locx = locs(x._1)
                        stmts += Connect(s.info,locx,one)
                     }
                     has_readwrite_mport match {
                        case None =>
                        case Some(wmode) => stmts += Connect(s.info,wmode,one)
                     }
                  }
                  if (stmts.size > 1) Block(stmts)
                  else stmts(0)
               }
               case (s) => s map (remove_chirrtl_s) map (remove_chirrtl_e(MALE))
            }
         }
         collect_smems_and_mports(m.body)
         val sx = collect_refs(m.body)
         Module(m.info,m.name, m.ports, remove_chirrtl_s(sx))
      }
      val modulesx = c.modules.map{ m => {
            (m) match { 
               case (m:Module) => remove_chirrtl_m(m)
               case (m:ExtModule) => m
            }}}
      Circuit(c.info,modulesx, c.main)
   }
}
