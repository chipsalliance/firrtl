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
import scala.collection.mutable.ArrayBuffer

import firrtl._
import firrtl.Utils._
import firrtl.Mappers._
import firrtl.Serialize._
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
            case e:Ref => WRef(e.name, e.tpe, NodeKind(), UNKNOWNGENDER)
            case e:SubField => WSubField(e.exp, e.name, e.tpe, UNKNOWNGENDER)
            case e:SubIndex => WSubIndex(e.exp, e.value, e.tpe, UNKNOWNGENDER)
            case e:SubAccess => WSubAccess(e.exp, e.index, e.tpe, UNKNOWNGENDER)
            case e => e
         }
      }
      def toStmt (s:Stmt) : Stmt = {
         s map (toExp) match {
            case s:DefInstance => WDefInstance(s.info,s.name,s.module,UnknownType())
            case s => s map (toStmt)
         }
      }
      val modulesx = c.modules.map { m => 
         mname = m.name
         m match {
            case m:InModule => InModule(m.info,m.name, m.ports, toStmt(m.body))
            case m:ExModule => m
         }
      }
      Circuit(c.info,modulesx,c.main)
   }
}

object ResolveKinds extends Pass {
   private var mname = ""
   def name = "Resolve Kinds"
   def run (c:Circuit): Circuit = {
      def resolve_kinds (m:Module, c:Circuit):Module = {
         val kinds = LinkedHashMap[String,Kind]()
         def resolve (body:Stmt) = {
            def resolve_expr (e:Expression):Expression = {
               e match {
                  case e:WRef => WRef(e.name,tpe(e),kinds(e.name),e.gender)
                  case e => e map (resolve_expr)
               }
            }
            def resolve_stmt (s:Stmt):Stmt = s map (resolve_stmt) map (resolve_expr)
            resolve_stmt(body)
         }
   
         def find (m:Module) = {
            def find_stmt (s:Stmt):Stmt = {
               s match {
                  case s:DefWire => kinds(s.name) = WireKind()
                  case s:DefPoison => kinds(s.name) = PoisonKind()
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
               case m:InModule => find_stmt(m.body)
               case m:ExModule => false
            }
         }
       
         mname = m.name
         find(m)   
         m match {
            case m:InModule => {
               val bodyx = resolve(m.body)
               InModule(m.info,m.name,m.ports,bodyx)
            }
            case m:ExModule => ExModule(m.info,m.name,m.ports)
         }
      }
      val modulesx = c.modules.map(m => resolve_kinds(m,c))
      Circuit(c.info,modulesx,c.main)
   }
}

object InferTypes extends Pass {
   private var mname = ""
   def name = "Infer Types"
   def set_type (s:Stmt,t:Type) : Stmt = {
      s match {
         case s:DefWire => DefWire(s.info,s.name,t)
         case s:DefRegister => DefRegister(s.info,s.name,t,s.clock,s.reset,s.init)
         case s:DefMemory => DefMemory(s.info,s.name,t,s.depth,s.write_latency,s.read_latency,s.readers,s.writers,s.readwriters)
         case s:DefNode => s
         case s:DefPoison => DefPoison(s.info,s.name,t)
      }
   }
   def remove_unknowns_w (w:Width)(implicit namespace: Namespace):Width = {
      w match {
         case w:UnknownWidth => VarWidth(namespace.newName("w"))
         case w => w
      }
   }
   def remove_unknowns (t:Type)(implicit n: Namespace): Type = mapr(remove_unknowns_w _,t)
   def run (c:Circuit): Circuit = {
      val module_types = LinkedHashMap[String,Type]()
      implicit val wnamespace = Namespace()
      def infer_types (m:Module) : Module = {
         val types = LinkedHashMap[String,Type]()
         def infer_types_e (e:Expression) : Expression = {
            e map (infer_types_e) match {
               case e:ValidIf => ValidIf(e.cond,e.value,tpe(e.value))
               case e:WRef => WRef(e.name, types(e.name),e.kind,e.gender)
               case e:WSubField => WSubField(e.exp,e.name,field_type(tpe(e.exp),e.name),e.gender)
               case e:WSubIndex => WSubIndex(e.exp,e.value,sub_type(tpe(e.exp)),e.gender)
               case e:WSubAccess => WSubAccess(e.exp,e.index,sub_type(tpe(e.exp)),e.gender)
               case e:DoPrim => set_primop_type(e)
               case e:Mux => Mux(e.cond,e.tval,e.fval,mux_type_and_widths(e.tval,e.fval))
               case e:UIntValue => e
               case e:SIntValue => e
            }
         }
         def infer_types_s (s:Stmt) : Stmt = {
            s match {
               case s:DefRegister => {
                  val t = remove_unknowns(get_type(s))
                  types(s.name) = t
                  set_type(s,t) map (infer_types_e)
               }
               case s:DefWire => {
                  val sx = s map(infer_types_e)
                  val t = remove_unknowns(get_type(sx))
                  types(s.name) = t
                  set_type(sx,t)
               }
               case s:DefPoison => {
                  val sx = s map (infer_types_e)
                  val t = remove_unknowns(get_type(sx))
                  types(s.name) = t
                  set_type(sx,t)
               }
               case s:DefNode => {
                  val sx = s map (infer_types_e)
                  val t = remove_unknowns(get_type(sx))
                  types(s.name) = t
                  set_type(sx,t)
               }
               case s:DefMemory => {
                  val t = remove_unknowns(get_type(s))
                  types(s.name) = t
                  val dt = remove_unknowns(s.data_type)
                  set_type(s,dt)
               }
               case s:WDefInstance => {
                  types(s.name) = module_types(s.module)
                  WDefInstance(s.info,s.name,s.module,module_types(s.module))
               }
               case s => s map (infer_types_s) map (infer_types_e)
            }
         }
 
         mname = m.name
         m.ports.foreach(p => types(p.name) = p.tpe)
         m match {
            case m:InModule => InModule(m.info,m.name,m.ports,infer_types_s(m.body))
            case m:ExModule => m
         }
       }
 
      val modulesx = c.modules.map { 
         m => {
            mname = m.name
            val portsx = m.ports.map(p => Port(p.info,p.name,p.direction,remove_unknowns(p.tpe)))
            m match {
               case m:InModule => InModule(m.info,m.name,portsx,m.body)
               case m:ExModule => ExModule(m.info,m.name,portsx)
            }
         }
      }
      modulesx.foreach(m => module_types(m.name) = module_type(m))
      Circuit(c.info,modulesx.map({m => mname = m.name; infer_types(m)}) , c.main )
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
                  field_flip(tpe(e.exp),e.name) match {
                     case DEFAULT => resolve_e(g)(e.exp)
                     case REVERSE => resolve_e(swap(g))(e.exp)
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
            
      def resolve_s (s:Stmt) : Stmt = {
         s match {
            case s:IsInvalid => {
               val expx = resolve_e(FEMALE)(s.exp)
               IsInvalid(s.info,expx)
            }
            case s:Connect => {
               val locx = resolve_e(FEMALE)(s.loc)
               val expx = resolve_e(MALE)(s.exp)
               Connect(s.info,locx,expx)
            }
            case s:BulkConnect => {
               val locx = resolve_e(FEMALE)(s.loc)
               val expx = resolve_e(MALE)(s.exp)
               BulkConnect(s.info,locx,expx)
            }
            case s => s map (resolve_e(MALE)) map (resolve_s)
         }
      }
      val modulesx = c.modules.map { 
         m => {
            mname = m.name
            m match {
               case m:InModule => {
                  val bodyx = resolve_s(m.body)
                  InModule(m.info,m.name,m.ports,bodyx)
               }
               case m:ExModule => m
            }
         }
      }
      Circuit(c.info,modulesx,c.main)
   }
}

object InferWidths extends Pass {
   def name = "Infer Widths"
   var mname = ""
   def solve_constraints (l:Seq[WGeq]) : LinkedHashMap[String,Width] = {
      def unique (ls:Seq[Width]) : Seq[Width] = ls.map(w => new WrappedWidth(w)).distinct.map(_.w)
      def make_unique (ls:Seq[WGeq]) : LinkedHashMap[String,Width] = {
         val h = LinkedHashMap[String,Width]()
         for (g <- ls) {
            (g.loc) match {
               case (w:VarWidth) => {
                  val n = w.name
                  if (h.contains(n)) h(n) = MaxWidth(Seq(g.exp,h(n))) else h(n) = g.exp
               }
               case (w) => w 
            }
         }
         h 
      }
      def simplify (w:Width) : Width = {
         (w map (simplify)) match {
            case (w:MinWidth) => {
               val v = ArrayBuffer[Width]()
               for (wx <- w.args) {
                  (wx) match {
                     case (wx:MinWidth) => for (x <- wx.args) { v += x }
                     case (wx) => v += wx } }
               MinWidth(unique(v)) }
            case (w:MaxWidth) => {
               val v = ArrayBuffer[Width]()
               for (wx <- w.args) {
                  (wx) match {
                     case (wx:MaxWidth) => for (x <- wx.args) { v += x }
                     case (wx) => v += wx } }
               MaxWidth(unique(v)) }
            case (w:PlusWidth) => {
               (w.arg1,w.arg2) match {
                  case (w1:IntWidth,w2:IntWidth) => IntWidth(w1.width + w2.width)
                  case (w1,w2) => w }}
            case (w:MinusWidth) => {
               (w.arg1,w.arg2) match {
                  case (w1:IntWidth,w2:IntWidth) => IntWidth(w1.width - w2.width)
                  case (w1,w2) => w }}
            case (w:ExpWidth) => {
               (w.arg1) match {
                  case (w1:IntWidth) => IntWidth(BigInt((scala.math.pow(2,w1.width.toDouble) - 1).toLong))
                  case (w1) => w }}
            case (w) => w } }
      def substitute (h:LinkedHashMap[String,Width])(w:Width) : Width = {
         //;println-all-debug(["Substituting for [" w "]"])
         val wx = simplify(w)
         //;println-all-debug(["After Simplify: [" wx "]"])
         (simplify(w) map (substitute(h))) match {
            case (w:VarWidth) => {
               //;("matched  println-debugvarwidth!")
               if (h.contains(w.name)) {
                  //;println-debug("Contained!")
                  //;println-all-debug(["Width: " w])
                  //;println-all-debug(["Accessed: " h[name(w)]])
                  val t = simplify(substitute(h)(h(w.name)))
                  //;val t = h[name(w)]
                  //;println-all-debug(["Width after sub: " t])
                  h(w.name) = t
                  t
               } else w
            }
            case (w) => w
               //;println-all-debug(["not varwidth!" w])
         }
      }
      def b_sub (h:LinkedHashMap[String,Width])(w:Width) : Width = {
         (w map (b_sub(h))) match {
            case (w:VarWidth) => if (h.contains(w.name)) h(w.name) else w
            case (w) => w
         }
      }
      def remove_cycle (n:String)(w:Width) : Width = {
         //;println-all-debug(["Removing cycle for " n " inside " w])
         val wx = (w map (remove_cycle(n))) match {
            case (w:MaxWidth) => MaxWidth(w.args.filter{ w => {
               w match {
                  case (w:VarWidth) => !(n equals w.name)
                  case (w) => true
               }}})
            case (w:MinusWidth) => {
               w.arg1 match {
                  case (v:VarWidth) => if (n == v.name) v else w
                  case (v) => w }}
            case (w) => w
         }
         //;println-all-debug(["After removing cycle for " n ", returning " wx])
         wx
      }
      def self_rec (n:String,w:Width) : Boolean = {
         var has = false
         def look (w:Width) : Width = {
            (w map (look)) match {
               case (w:VarWidth) => if (w.name == n) has = true
               case (w) => w }
            w }
         look(w)
         has }
          
      //; Forward solve
      //; Returns a solved list where each constraint undergoes:
      //;  1) Continuous Solving (using triangular solving)
      //;  2) Remove Cycles
      //;  3) Move to solved if not self-recursive
      val u = make_unique(l)
      
      //println("======== UNIQUE CONSTRAINTS ========")
      //for (x <- u) { println(x) }
      //println("====================================")
      
   
      val f = LinkedHashMap[String,Width]()
      val o = ArrayBuffer[String]()
      for (x <- u) {
         //println("==== SOLUTIONS TABLE ====")
         //for (x <- f) println(x)
         //println("=========================")
   
         val (n, e) = (x._1, x._2)
         val e_sub = substitute(f)(e)

         //println("Solving " + n + " => " + e)
         //println("After Substitute: " + n + " => " + e_sub)
         //println("==== SOLUTIONS TABLE (Post Substitute) ====")
         //for (x <- f) println(x)
         //println("=========================")

         val ex = remove_cycle(n)(e_sub)

         //println("After Remove Cycle: " + n + " => " + ex)
         if (!self_rec(n,ex)) {
            //println("Not rec!: " + n + " => " + ex)
            //println("Adding [" + n + "=>" + ex + "] to Solutions Table")
            o += n
            f(n) = ex
         }
      }
   
      //println("Forward Solved Constraints")
      //for (x <- f) println(x)
   
      //; Backwards Solve
      val b = LinkedHashMap[String,Width]()
      for (i <- 0 until o.size) {
         val n = o(o.size - 1 - i)
         /*
         println("SOLVE BACK: [" + n + " => " + f(n) + "]")
         println("==== SOLUTIONS TABLE ====")
         for (x <- b) println(x)
         println("=========================")
         */
         val ex = simplify(b_sub(b)(f(n)))
         /*
         println("BACK RETURN: [" + n + " => " + ex + "]")
         */
         b(n) = ex
         /*
         println("==== SOLUTIONS TABLE (Post backsolve) ====")
         for (x <- b) println(x)
         println("=========================")
         */
      }
      b
   }
      
   def width_BANG (t:Type) : Width = {
      (t) match {
         case (t:UIntType) => t.width
         case (t:SIntType) => t.width
         case (t:ClockType) => IntWidth(1)
         case (t) => error("No width!"); IntWidth(-1) } }
   def width_BANG (e:Expression) : Width = width_BANG(tpe(e))

   def reduce_var_widths(c: Circuit, h: LinkedHashMap[String,Width]): Circuit = {
      def evaluate(w: Width): Width = {
         def map2(a: Option[BigInt], b: Option[BigInt], f: (BigInt,BigInt) => BigInt): Option[BigInt] =
            for (a_num <- a; b_num <- b) yield f(a_num, b_num)
         def reduceOptions(l: Seq[Option[BigInt]], f: (BigInt,BigInt) => BigInt): Option[BigInt] =
            l.reduce(map2(_, _, f))

         // This function shouldn't be necessary
         // Added as protection in case a constraint accidentally uses MinWidth/MaxWidth
         // without any actual Widths. This should be elevated to an earlier error
         def forceNonEmpty(in: Seq[Option[BigInt]], default: Option[BigInt]): Seq[Option[BigInt]] =
            if(in.isEmpty) Seq(default)
            else in

         def max(a: BigInt, b: BigInt): BigInt = if (a >= b) a else b
         def min(a: BigInt, b: BigInt): BigInt = if (a >= b) b else a
         def pow_minus_one(a: BigInt, b: BigInt): BigInt = a.pow(b.toInt) - 1

         def solve(w: Width): Option[BigInt] = w match {
            case (w: VarWidth) =>
               for{
                  v <- h.get(w.name) if !v.isInstanceOf[VarWidth]
                  result <- solve(v)
               } yield result
            case (w: MaxWidth) => reduceOptions(forceNonEmpty(w.args.map(solve _), Some(BigInt(0))), max)
            case (w: MinWidth) => reduceOptions(forceNonEmpty(w.args.map(solve _), None), min)
            case (w: PlusWidth) => map2(solve(w.arg1), solve(w.arg2), {_ + _})
            case (w: MinusWidth) => map2(solve(w.arg1), solve(w.arg2), {_ - _})
            case (w: ExpWidth) => map2(Some(BigInt(2)), solve(w.arg1), pow_minus_one)
            case (w: IntWidth) => Some(w.width)
            case (w) => println(w); error("Shouldn't be here"); None;
         }

         val s = solve(w)
         (s) match {
            case Some(s) => IntWidth(s)
            case (s) => w
         }
      }

      def reduce_var_widths_w (w:Width) : Width = {
         //println-all-debug(["REPLACE: " w])
         val wx = evaluate(w)
         //println-all-debug(["WITH: " wx])
         wx
      }
   
      val modulesx = c.modules.map{ m => {
         val portsx = m.ports.map{ p => {
            Port(p.info,p.name,p.direction,mapr(reduce_var_widths_w _,p.tpe)) }}
         (m) match {
            case (m:ExModule) => ExModule(m.info,m.name,portsx)
            case (m:InModule) => mname = m.name; InModule(m.info,m.name,portsx,mapr(reduce_var_widths_w _,m.body)) }}}
      Circuit(c.info,modulesx,c.main)
   }
   
   def run (c:Circuit): Circuit = {
      val v = ArrayBuffer[WGeq]()
      def constrain (w1:Width,w2:Width) : Unit = v += WGeq(w1,w2)
      def get_constraints_t (t1:Type,t2:Type,f:Flip) : Unit = {
         (t1,t2) match {
            case (t1:UIntType,t2:UIntType) => constrain(t1.width,t2.width)
            case (t1:SIntType,t2:SIntType) => constrain(t1.width,t2.width)
            case (t1:BundleType,t2:BundleType) => {
               (t1.fields,t2.fields).zipped.foreach{ (f1,f2) => {
                  get_constraints_t(f1.tpe,f2.tpe,times(f1.flip,f)) }}}
            case (t1:VectorType,t2:VectorType) => get_constraints_t(t1.tpe,t2.tpe,f) }}
      def get_constraints_e (e:Expression) : Expression = {
         (e map (get_constraints_e)) match {
            case (e:Mux) => {
               constrain(width_BANG(e.cond),ONE)
               constrain(ONE,width_BANG(e.cond))
               e }
            case (e) => e }}
      def get_constraints (s:Stmt) : Stmt = {
         (s map (get_constraints_e)) match {
            case (s:Connect) => {
               val n = get_size(tpe(s.loc))
               val ce_loc = create_exps(s.loc)
               val ce_exp = create_exps(s.exp)
               for (i <- 0 until n) {
                  val locx = ce_loc(i)
                  val expx = ce_exp(i)
                  get_flip(tpe(s.loc),i,DEFAULT) match {
                     case DEFAULT => constrain(width_BANG(locx),width_BANG(expx))
                     case REVERSE => constrain(width_BANG(expx),width_BANG(locx)) }}
               s }
            case (s:BulkConnect) => {
               val ls = get_valid_points(tpe(s.loc),tpe(s.exp),DEFAULT,DEFAULT)
               for (x <- ls) {
                  val locx = create_exps(s.loc)(x._1)
                  val expx = create_exps(s.exp)(x._2)
                  get_flip(tpe(s.loc),x._1,DEFAULT) match {
                     case DEFAULT => constrain(width_BANG(locx),width_BANG(expx))
                     case REVERSE => constrain(width_BANG(expx),width_BANG(locx)) }}
               s }
            case (s:DefRegister) => {
               constrain(width_BANG(s.reset),ONE)
               constrain(ONE,width_BANG(s.reset))
               get_constraints_t(s.tpe,tpe(s.init),DEFAULT)
               s }
            case (s:Conditionally) => {
               v += WGeq(width_BANG(s.pred),ONE)
               v += WGeq(ONE,width_BANG(s.pred))
               s map (get_constraints) }
            case (s) => s map (get_constraints) }}

      for (m <- c.modules) {
         (m) match {
            case (m:InModule) => mname = m.name; get_constraints(m.body)
            case (m) => false }}
      //println-debug("======== ALL CONSTRAINTS ========")
      //for x in v do : println-debug(x)
      //println-debug("=================================")
      val h = solve_constraints(v)
      //println-debug("======== SOLVED CONSTRAINTS ========")
      //for x in h do : println-debug(x)
      //println-debug("====================================")
      reduce_var_widths(Circuit(c.info,c.modules,c.main),h)
   }
}

object PullMuxes extends Pass {
   private var mname = ""
   def name = "Pull Muxes"
   def run (c:Circuit): Circuit = {
      def pull_muxes_e (e:Expression) : Expression = {
         val ex = e map (pull_muxes_e) match {
            case (e:WRef) => e
            case (e:WSubField) => {
               e.exp match {
                  case (ex:Mux) => Mux(ex.cond,WSubField(ex.tval,e.name,e.tpe,e.gender),WSubField(ex.fval,e.name,e.tpe,e.gender),e.tpe)
                  case (ex:ValidIf) => ValidIf(ex.cond,WSubField(ex.value,e.name,e.tpe,e.gender),e.tpe)
                  case (ex) => e
               }
            }
            case (e:WSubIndex) => {
               e.exp match {
                  case (ex:Mux) => Mux(ex.cond,WSubIndex(ex.tval,e.value,e.tpe,e.gender),WSubIndex(ex.fval,e.value,e.tpe,e.gender),e.tpe)
                  case (ex:ValidIf) => ValidIf(ex.cond,WSubIndex(ex.value,e.value,e.tpe,e.gender),e.tpe)
                  case (ex) => e
               }
            }
            case (e:WSubAccess) => {
               e.exp match {
                  case (ex:Mux) => Mux(ex.cond,WSubAccess(ex.tval,e.index,e.tpe,e.gender),WSubAccess(ex.fval,e.index,e.tpe,e.gender),e.tpe)
                  case (ex:ValidIf) => ValidIf(ex.cond,WSubAccess(ex.value,e.index,e.tpe,e.gender),e.tpe)
                  case (ex) => e
               }
            }
            case (e:Mux) => e
            case (e:ValidIf) => e
            case (e) => e
         }
         ex map (pull_muxes_e)
      }
      def pull_muxes (s:Stmt) : Stmt = s map (pull_muxes) map (pull_muxes_e)
      val modulesx = c.modules.map {
         m => {
            mname = m.name
            m match {
               case (m:InModule) => InModule(m.info,m.name,m.ports,pull_muxes(m.body))
               case (m:ExModule) => m
            }
         }
      }
      Circuit(c.info,modulesx,c.main)
   }
}

object ExpandConnects extends Pass {
   private var mname = ""
   def name = "Expand Connects"
   def run (c:Circuit): Circuit = {
      def expand_connects (m:InModule) : InModule = { 
         mname = m.name
         val genders = LinkedHashMap[String,Gender]()
         def expand_s (s:Stmt) : Stmt = {
            def set_gender (e:Expression) : Expression = {
               e map (set_gender) match {
                  case (e:WRef) => WRef(e.name,e.tpe,e.kind,genders(e.name))
                  case (e:WSubField) => {
                     val f = get_field(tpe(e.exp),e.name)
                     val genderx = times(gender(e.exp),f.flip)
                     WSubField(e.exp,e.name,e.tpe,genderx)
                  }
                  case (e:WSubIndex) => WSubIndex(e.exp,e.value,e.tpe,gender(e.exp))
                  case (e:WSubAccess) => WSubAccess(e.exp,e.index,e.tpe,gender(e.exp))
                  case (e) => e
               }
            }
            s match {
               case (s:DefWire) => { genders(s.name) = BIGENDER; s }
               case (s:DefRegister) => { genders(s.name) = BIGENDER; s }
               case (s:WDefInstance) => { genders(s.name) = MALE; s }
               case (s:DefMemory) => { genders(s.name) = MALE; s }
               case (s:DefPoison) => { genders(s.name) = MALE; s }
               case (s:DefNode) => { genders(s.name) = MALE; s }
               case (s:IsInvalid) => {
                  val n = get_size(tpe(s.exp))
                  val invalids = ArrayBuffer[Stmt]()
                  val exps = create_exps(s.exp)
                  for (i <- 0 until n) {
                     val expx = exps(i)
                     val gexpx = set_gender(expx)
                     gender(gexpx) match {
                        case BIGENDER => invalids += IsInvalid(s.info,expx)
                        case FEMALE => invalids += IsInvalid(s.info,expx)
                        case _ => {}
                     }
                  }
                  if (invalids.length == 0) {
                     Empty()
                  } else if (invalids.length == 1) {
                     invalids(0)
                  } else Begin(invalids)
               }
               case (s:Connect) => {
                  val n = get_size(tpe(s.loc))
                  val connects = ArrayBuffer[Stmt]()
                  val locs = create_exps(s.loc)
                  val exps = create_exps(s.exp)
                  for (i <- 0 until n) {
                     val locx = locs(i)
                     val expx = exps(i)
                     val sx = get_flip(tpe(s.loc),i,DEFAULT) match {
                        case DEFAULT => Connect(s.info,locx,expx)
                        case REVERSE => Connect(s.info,expx,locx)
                     }
                     connects += sx
                  }
                  Begin(connects)
               }
               case (s:BulkConnect) => {
                  val ls = get_valid_points(tpe(s.loc),tpe(s.exp),DEFAULT,DEFAULT)
                  val connects = ArrayBuffer[Stmt]()
                  val locs = create_exps(s.loc)
                  val exps = create_exps(s.exp)
                  ls.foreach { x => {
                     val locx = locs(x._1)
                     val expx = exps(x._2)
                     val sx = get_flip(tpe(s.loc),x._1,DEFAULT) match {
                        case DEFAULT => Connect(s.info,locx,expx)
                        case REVERSE => Connect(s.info,expx,locx)
                     }
                     connects += sx
                  }}
                  Begin(connects)
               }
               case (s) => s map (expand_s)
            }
         }
   
         m.ports.foreach { p => genders(p.name) = to_gender(p.direction) }
         InModule(m.info,m.name,m.ports,expand_s(m.body))
      }
   
      val modulesx = c.modules.map { 
         m => {
            m match {
               case (m:ExModule) => m
               case (m:InModule) => expand_connects(m)
            }
         }
      }
      Circuit(c.info,modulesx,c.main)
   }
}

case class Location(base:Expression,guard:Expression)
object RemoveAccesses extends Pass {
   private var mname = ""
   def name = "Remove Accesses"
   def get_locations (e:Expression) : Seq[Location] = {
       e match {
         case (e:WRef) => create_exps(e).map(Location(_,one))
         case (e:WSubIndex) => {
            val ls = get_locations(e.exp)
            val start = get_point(e)
            val end = start + get_size(tpe(e))
            val stride = get_size(tpe(e.exp))
            val lsx = ArrayBuffer[Location]()
            var c = 0
            for (i <- 0 until ls.size) {
               if (((i % stride) >= start) & ((i % stride) < end)) {
                  lsx += ls(i)
               }
            }
            lsx
         }
         case (e:WSubField) => {
            val ls = get_locations(e.exp)
            val start = get_point(e)
            val end = start + get_size(tpe(e))
            val stride = get_size(tpe(e.exp))
            val lsx = ArrayBuffer[Location]()
            var c = 0
            for (i <- 0 until ls.size) {
               if (((i % stride) >= start) & ((i % stride) < end)) { lsx += ls(i) }
            }
            lsx
         }
         case (e:WSubAccess) => {
            val ls = get_locations(e.exp)
            val stride = get_size(tpe(e))
            val wrap = tpe(e.exp).asInstanceOf[VectorType].size
            val lsx = ArrayBuffer[Location]()
            var c = 0
            for (i <- 0 until ls.size) {
               if ((c % wrap) == 0) { c = 0 }
               val basex = ls(i).base
               val guardx = AND(ls(i).guard,EQV(uint(c),e.index))
               lsx += Location(basex,guardx)
               if ((i + 1) % stride == 0) {
                  c = c + 1
               }
            }
            lsx
         }
      }
   }
   def has_access (e:Expression) : Boolean = {
      var ret:Boolean = false
      def rec_has_access (e:Expression) : Expression = {
         e match {
            case (e:WSubAccess) => { ret = true; e }
            case (e) => e map (rec_has_access)
         }
      }
      rec_has_access(e)
      ret
   }
   def run (c:Circuit): Circuit = {
      def remove_m (m:InModule) : InModule = {
         val namespace = Namespace(m)
         mname = m.name
         def remove_s (s:Stmt) : Stmt = {
            val stmts = ArrayBuffer[Stmt]()
            def create_temp (e:Expression) : Expression = {
               val n = namespace.newTemp
               stmts += DefWire(info(s),n,tpe(e))
               WRef(n,tpe(e),kind(e),gender(e))
            }
            def remove_e (e:Expression) : Expression = { //NOT RECURSIVE (except primops) INTENTIONALLY!
               e match {
                  case (e:DoPrim) => e map (remove_e)
                  case (e:Mux) => e map (remove_e)
                  case (e:ValidIf) => e map (remove_e)
                  case (e:SIntValue) => e
                  case (e:UIntValue) => e
                  case x => {
                     val e = x match {
                        case (w:WSubAccess) => WSubAccess(w.exp,remove_e(w.index),w.tpe,w.gender)
                        case _ => x
                     }
                     if (has_access(e)) {
                        val rs = get_locations(e)
                        val foo = rs.find(x => {x.guard != one})
                        foo match {
                           case None => error("Shouldn't be here")
                           case foo:Some[Location] => {
                              val temp = create_temp(e)
                              val temps = create_exps(temp)
                              def get_temp (i:Int) = temps(i % temps.size)
                              (rs,0 until rs.size).zipped.foreach {
                                 (x,i) => {
                                    if (i < temps.size) {
                                       stmts += Connect(info(s),get_temp(i),x.base)
                                    } else {
                                       stmts += Conditionally(info(s),x.guard,Connect(info(s),get_temp(i),x.base),Empty())
                                    }
                                 }
                              }
                              temp
                           }
                        }
                     } else { e}
                  }
               }
            }

            val sx = s match {
               case (s:Connect) => {
                  if (has_access(s.loc)) {
                     val ls = get_locations(s.loc)
                     val locx = 
                        if (ls.size == 1 & weq(ls(0).guard,one)) s.loc
                        else {
                           val temp = create_temp(s.loc)
                           for (x <- ls) { stmts += Conditionally(s.info,x.guard,Connect(s.info,x.base,temp),Empty()) }
                           temp
                        }
                     Connect(s.info,locx,remove_e(s.exp))
                  } else { Connect(s.info,s.loc,remove_e(s.exp)) }
               }
               case (s) => s map (remove_e) map (remove_s)
            }
            stmts += sx
            if (stmts.size != 1) Begin(stmts) else stmts(0)
         }
         InModule(m.info,m.name,m.ports,remove_s(m.body))
      }
   
      val modulesx = c.modules.map{
         m => {
            m match {
               case (m:ExModule) => m
               case (m:InModule) => remove_m(m)
            }
         }
      }
      Circuit(c.info,modulesx,c.main)
   }
}

// Replace shr by amount >= arg width with 0 for UInts and MSB for SInts
// TODO replace UInt with zero-width wire instead
object Legalize extends Pass {
  def name = "Legalize"
  private def legalizeShiftRight(e: DoPrim): Expression = {
    val amount = e.consts(0).toInt
    val width = long_BANG(e.args(0).tpe)
    lazy val msb = width - 1
    if (amount >= width) {
      e.tpe match {
        case UIntType(_) => UIntValue(0, IntWidth(1))
        case SIntType(_) =>
          val e2 = DoPrim(BITS_SELECT_OP, e.args, Seq(msb, msb), UIntType(IntWidth(1)))
          DoPrim(AS_SINT_OP, Seq(e2), Seq(), SIntType(IntWidth(1)))
        case t => error(s"Unsupported type ${t} for Primop Shift Right")
      }
    } else {
      e
    }
  }
  private def legalizeBits(expr: DoPrim): Expression = {
    lazy val (hi, low) = (expr.consts(0), expr.consts(1))
    lazy val mask = (BigInt(1) << (hi - low + 1).toInt) - 1
    lazy val width = IntWidth(hi - low + 1)
    expr.args.head match {
      case UIntValue(value, _) => UIntValue((value >> low.toInt) & mask, width)
      case SIntValue(value, _) => SIntValue((value >> low.toInt) & mask, width)
      case _ => expr
    }
  }
  private def legalizePad(expr: DoPrim): Expression = expr.args.head match {
    case UIntValue(value, IntWidth(width)) if (width < expr.consts.head) =>
      UIntValue(value, IntWidth(expr.consts.head))
    case SIntValue(value, IntWidth(width)) if (width < expr.consts.head) =>
      SIntValue(value, IntWidth(expr.consts.head))
    case _ => expr
  }
  private def legalizeConnect(c: Connect): Stmt = {
    val t = c.loc.tpe
    val w = long_BANG(t)
    if (w >= long_BANG(c.exp.tpe)) {
      c
    } else {
      val e1 = DoPrim(BITS_SELECT_OP, Seq(c.exp), Seq(w-1, 0), UIntType(IntWidth(w)))
      val e2 = t match {
        case UIntType(_) => e1
        case SIntType(_) => DoPrim(AS_SINT_OP, Seq(e1), Seq(), SIntType(IntWidth(w)))
      }
      Connect(c.info, c.loc, e2)
    }
  }
  def run (c: Circuit): Circuit = {
    def legalizeE (expr: Expression): Expression = expr map legalizeE match {
      case prim: DoPrim => prim.op match {
        case SHIFT_RIGHT_OP => legalizeShiftRight(prim)
        case PAD_OP => legalizePad(prim)
        case BITS_SELECT_OP => legalizeBits(prim)
        case _ => prim
      }
      case e => e // respect pre-order traversal
    }
    def legalizeS (s: Stmt): Stmt = {
      val legalizedStmt = s match {
        case c: Connect => legalizeConnect(c)
        case _ => s
      }
      legalizedStmt map legalizeS map legalizeE
    }
    def legalizeM (m: Module): Module = m map (legalizeS)
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
            if (e.op == TAIL_OP) {
               (a0()) match {
                  case (e0:DoPrim) => {
                     if (e0.op == ADD_OP) DoPrim(ADDW_OP,e0.args,Seq(),tpe(e))
                     else if (e0.op == SUB_OP) DoPrim(SUBW_OP,e0.args,Seq(),tpe(e))
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
   def v_wrap_s (s:Stmt) : Stmt = {
      s map (v_wrap_s) map (v_wrap_e) match {
        case s: Print =>
           Print(s.info, VerilogStringLitHandler.format(s.string), s.args, s.clk, s.en)
        case s => s
      }
   }
   def run (c:Circuit): Circuit = {
      val modulesx = c.modules.map{ m => {
         (m) match {
            case (m:InModule) => {
               mname = m.name
               InModule(m.info,m.name,m.ports,v_wrap_s(m.body))
            }
            case (m:ExModule) => m
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
      def verilog_rename_s (s:Stmt) : Stmt = {
        s map (verilog_rename_s) map (verilog_rename_e) map (verilog_rename_n)
      }
      val modulesx = c.modules.map{ m => {
         val portsx = m.ports.map{ p => {
            Port(p.info,verilog_rename_n(p.name),p.direction,p.tpe)
         }}
         m match {
            case (m:InModule) => InModule(m.info,m.name,portsx,verilog_rename_s(m.body))
            case (m:ExModule) => m
         }
      }}
      Circuit(c.info,modulesx,c.main)
   }
}

object CInferTypes extends Pass {
   def name = "CInfer Types"
   var mname = ""
   def set_type (s:Stmt,t:Type) : Stmt = {
      (s) match { 
         case (s:DefWire) => DefWire(s.info,s.name,t)
         case (s:DefRegister) => DefRegister(s.info,s.name,t,s.clock,s.reset,s.init)
         case (s:CDefMemory) => CDefMemory(s.info,s.name,t,s.size,s.seq)
         case (s:CDefMPort) => CDefMPort(s.info,s.name,t,s.mem,s.exps,s.direction)
         case (s:DefNode) => s
         case (s:DefPoison) => DefPoison(s.info,s.name,t)
      }
   }
   
   def to_field (p:Port) : Field = {
      if (p.direction == OUTPUT) Field(p.name,DEFAULT,p.tpe)
      else if (p.direction == INPUT) Field(p.name,REVERSE,p.tpe)
      else error("Shouldn't be here"); Field(p.name,REVERSE,p.tpe)
   }
   def module_type (m:Module) : Type = BundleType(m.ports.map(p => to_field(p)))
   def field_type (v:Type,s:String) : Type = {
      (v) match { 
         case (v:BundleType) => {
            val ft = v.fields.find(p => p.name == s)
            if (ft != None) ft.get.tpe
            else  UnknownType()
         }
         case (v) => UnknownType()
      }
   }
   def sub_type (v:Type) : Type =
      (v) match { 
         case (v:VectorType) => v.tpe
         case (v) => UnknownType()
      }
   def run (c:Circuit) : Circuit = {
      val module_types = LinkedHashMap[String,Type]()
      def infer_types (m:Module) : Module = {
         val types = LinkedHashMap[String,Type]()
         def infer_types_e (e:Expression) : Expression = {
            (e map (infer_types_e)) match { 
               case (e:Ref) => Ref(e.name, types.getOrElse(e.name,UnknownType()))
               case (e:SubField) => SubField(e.exp,e.name,field_type(tpe(e.exp),e.name))
               case (e:SubIndex) => SubIndex(e.exp,e.value,sub_type(tpe(e.exp)))
               case (e:SubAccess) => SubAccess(e.exp,e.index,sub_type(tpe(e.exp)))
               case (e:DoPrim) => set_primop_type(e)
               case (e:Mux) => Mux(e.cond,e.tval,e.fval,mux_type(e.tval,e.tval))
               case (e:ValidIf) => ValidIf(e.cond,e.value,tpe(e.value))
               case (_:UIntValue|_:SIntValue) => e
            }
         }
         def infer_types_s (s:Stmt) : Stmt = {
            (s) match { 
               case (s:DefRegister) => {
                  types(s.name) = s.tpe
                  s map (infer_types_e)
                  s
               }
               case (s:DefWire) => {
                  types(s.name) = s.tpe
                  s
               }
               case (s:DefPoison) => {
                  types(s.name) = s.tpe
                  s
               }
               case (s:DefNode) => {
                  val sx = s map (infer_types_e)
                  val t = get_type(sx)
                  types(s.name) = t
                  sx
               }
               case (s:DefMemory) => {
                  types(s.name) = get_type(s)
                  s
               }
               case (s:CDefMPort) => {
                  val t = types.getOrElse(s.mem,UnknownType())
                  types(s.name) = t
                  CDefMPort(s.info,s.name,t,s.mem,s.exps,s.direction)
               }
               case (s:CDefMemory) => {
                  types(s.name) = s.tpe
                  s
               }
               case (s:DefInstance) => {
                  types(s.name) = module_types.getOrElse(s.module,UnknownType())
                  s
               }
               case (s) => s map(infer_types_s) map (infer_types_e)
            }
         }
         for (p <- m.ports) {
            types(p.name) = p.tpe
         }
         (m) match { 
            case (m:InModule) => InModule(m.info,m.name,m.ports,infer_types_s(m.body))
            case (m:ExModule) => m
         }
      }
   
      //; MAIN
      for (m <- c.modules) {
         module_types(m.name) = module_type(m)
      }
      val modulesx = c.modules.map(m => infer_types(m))
      Circuit(c.info, modulesx, c.main)
   }
}

object CInferMDir extends Pass {
   def name = "CInfer MDir"
   var mname = ""
   def run (c:Circuit) : Circuit = {
      def infer_mdir (m:Module) : Module = {
         val mports = LinkedHashMap[String,MPortDir]()
         def infer_mdir_e (dir:MPortDir)(e:Expression) : Expression = {
            (e map (infer_mdir_e(dir))) match { 
               case (e:Ref) => {
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
         def infer_mdir_s (s:Stmt) : Stmt = {
            (s) match { 
               case (s:CDefMPort) => {
                  mports(s.name) = s.direction
                  s map (infer_mdir_e(MRead))
               }
               case (s:Connect) => {
                  infer_mdir_e(MRead)(s.exp)
                  infer_mdir_e(MWrite)(s.loc)
                  s
               }
               case (s:BulkConnect) => {
                  infer_mdir_e(MRead)(s.exp)
                  infer_mdir_e(MWrite)(s.loc)
                  s
               }
               case (s) => s map (infer_mdir_s) map (infer_mdir_e(MRead))
            }
         }
         def set_mdir_s (s:Stmt) : Stmt = {
            (s) match { 
               case (s:CDefMPort) => 
                  CDefMPort(s.info,s.name,s.tpe,s.mem,s.exps,mports(s.name))
               case (s) => s map (set_mdir_s)
            }
         }
         (m) match { 
            case (m:InModule) => {
               infer_mdir_s(m.body)
               InModule(m.info,m.name,m.ports,set_mdir_s(m.body))
            }
            case (m:ExModule) => m
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
   def create_exps (e:Expression) : Seq[Expression] = {
      (e) match { 
         case (e:Mux)=> 
            (create_exps(e.tval),create_exps(e.fval)).zipped.map((e1,e2) => {
               Mux(e.cond,e1,e2,mux_type(e1,e2))
            })
         case (e:ValidIf) => 
            create_exps(e.value).map(e1 => {
               ValidIf(e.cond,e1,tpe(e1))
            })
         case (e) => (tpe(e)) match  { 
            case (_:UIntType|_:SIntType|_:ClockType) => Seq(e)
            case (t:BundleType) => 
               t.fields.flatMap(f => create_exps(SubField(e,f.name,f.tpe)))
            case (t:VectorType)=> 
               (0 until t.size).flatMap(i => create_exps(SubIndex(e,i,t.tpe)))
            case (t:UnknownType) => Seq(e)
         }
      }
   }
   def run (c:Circuit) : Circuit = {
      def remove_chirrtl_m (m:InModule) : InModule = {
         val hash = LinkedHashMap[String,MPorts]()
         val repl = LinkedHashMap[String,DataRef]()
         val ut = UnknownType()
         val mport_types = LinkedHashMap[String,Type]()
         def EMPs () : MPorts = MPorts(ArrayBuffer[MPort](),ArrayBuffer[MPort](),ArrayBuffer[MPort]())
         def collect_mports (s:Stmt) : Stmt = {
            (s) match { 
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
               case (s) => s map (collect_mports)
            }
         }
         def collect_refs (s:Stmt) : Stmt = {
            (s) match { 
               case (s:CDefMemory) => {
                  mport_types(s.name) = s.tpe
                  val stmts = ArrayBuffer[Stmt]()
                  val taddr = UIntType(IntWidth(scala.math.max(1,ceil_log2(s.size))))
                  val tdata = s.tpe
                  def set_poison (vec:Seq[MPort],addr:String) : Unit = {
                     for (r <- vec ) {
                        stmts += IsInvalid(s.info,SubField(SubField(Ref(s.name,ut),r.name,ut),addr,taddr))
                        stmts += IsInvalid(s.info,SubField(SubField(Ref(s.name,ut),r.name,ut),"clk",taddr))
                     }
                  }
                  def set_enable (vec:Seq[MPort],en:String) : Unit = {
                     for (r <- vec ) {
                        stmts += Connect(s.info,SubField(SubField(Ref(s.name,ut),r.name,ut),en,taddr),zero)
                     }}
                  def set_wmode (vec:Seq[MPort],wmode:String) : Unit = {
                     for (r <- vec) {
                        stmts += Connect(s.info,SubField(SubField(Ref(s.name,ut),r.name,ut),wmode,taddr),zero)
                     }}
                  def set_write (vec:Seq[MPort],data:String,mask:String) : Unit = {
                     val tmask = create_mask(s.tpe)
                     for (r <- vec ) {
                        stmts += IsInvalid(s.info,SubField(SubField(Ref(s.name,ut),r.name,ut),data,tdata))
                        for (x <- create_exps(SubField(SubField(Ref(s.name,ut),r.name,ut),mask,tmask)) ) {
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
                  set_write(rws,"data","mask")
                  val read_l = if (s.seq) 1 else 0
                  val mem = DefMemory(s.info,s.name,s.tpe,s.size,1,read_l,rds.map(_.name),wrs.map(_.name),rws.map(_.name))
                  Begin(Seq(mem,Begin(stmts)))
               }
               case (s:CDefMPort) => {
                  mport_types(s.name) = mport_types(s.mem)
                  val addrs = ArrayBuffer[String]()
                  val clks = ArrayBuffer[String]()
                  val ens = ArrayBuffer[String]()
                  val masks = ArrayBuffer[String]()
                  s.direction match {
                     case MReadWrite => {
                        repl(s.name) = DataRef(SubField(Ref(s.mem,ut),s.name,ut),"rdata","data","mask",true)
                        addrs += "addr"
                        clks += "clk"
                        ens += "en"
                        masks += "mask"
                     }
                     case MWrite => {
                        repl(s.name) = DataRef(SubField(Ref(s.mem,ut),s.name,ut),"data","data","mask",false)
                        addrs += "addr"
                        clks += "clk"
                        ens += "en"
                        masks += "mask"
                     }
                     case _ => {
                        repl(s.name) = DataRef(SubField(Ref(s.mem,ut),s.name,ut),"data","data","blah",false)
                        addrs += "addr"
                        clks += "clk"
                        ens += "en"
                     }
                  }
                  val stmts = ArrayBuffer[Stmt]()
                  for (x <- addrs ) {
                     stmts += Connect(s.info,SubField(SubField(Ref(s.mem,ut),s.name,ut),x,ut),s.exps(0))
                  }
                  for (x <- clks ) {
                     stmts += Connect(s.info,SubField(SubField(Ref(s.mem,ut),s.name,ut),x,ut),s.exps(1))
                  }
                  for (x <- ens ) {
                     stmts += Connect(s.info,SubField(SubField(Ref(s.mem,ut),s.name,ut),x,ut),one)
                  }
                  Begin(stmts)
               }
               case (s) => s map (collect_refs)
            }
         }
         def remove_chirrtl_s (s:Stmt) : Stmt = {
            var has_write_mport = false
            var has_readwrite_mport:Option[Expression] = None
            def remove_chirrtl_e (g:Gender)(e:Expression) : Expression = {
               (e) match { 
                  case (e:Ref) => {
                     if (repl.contains(e.name)) {
                        val vt = repl(e.name)
                        g match {
                           case MALE => SubField(vt.exp,vt.male,e.tpe)
                           case FEMALE => {
                              has_write_mport = true
                              if (vt.rdwrite == true) 
                                 has_readwrite_mport = Some(SubField(vt.exp,"wmode",UIntType(IntWidth(1))))
                              SubField(vt.exp,vt.female,e.tpe)
                           }
                        }
                     } else e
                  }
                  case (e:SubAccess) => SubAccess(remove_chirrtl_e(g)(e.exp),remove_chirrtl_e(MALE)(e.index),e.tpe)
                  case (e) => e map (remove_chirrtl_e(g))
               }
            }
            def get_mask (e:Expression) : Expression = {
               (e map (get_mask)) match { 
                  case (e:Ref) => {
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
               case (s:Connect) => {
                  val stmts = ArrayBuffer[Stmt]()
                  val rocx = remove_chirrtl_e(MALE)(s.exp)
                  val locx = remove_chirrtl_e(FEMALE)(s.loc)
                  stmts += Connect(s.info,locx,rocx)
                  if (has_write_mport) {
                     val e = get_mask(s.loc)
                     for (x <- create_exps(e) ) {
                        stmts += Connect(s.info,x,one)
                     }
                     if (has_readwrite_mport != None) {
                        val wmode = has_readwrite_mport.get
                        stmts += Connect(s.info,wmode,one)
                     }
                  }
                  if (stmts.size > 1) Begin(stmts)
                  else stmts(0)
               }
               case (s:BulkConnect) => {
                  val stmts = ArrayBuffer[Stmt]()
                  val locx = remove_chirrtl_e(FEMALE)(s.loc)
                  val rocx = remove_chirrtl_e(MALE)(s.exp)
                  stmts += BulkConnect(s.info,locx,rocx)
                  if (has_write_mport != false) {
                     val ls = get_valid_points(tpe(s.loc),tpe(s.exp),DEFAULT,DEFAULT)
                     val locs = create_exps(get_mask(s.loc))
                     for (x <- ls ) {
                        val locx = locs(x._1)
                        stmts += Connect(s.info,locx,one)
                     }
                     if (has_readwrite_mport != None) {
                        val wmode = has_readwrite_mport.get
                        stmts += Connect(s.info,wmode,one)
                     }
                  }
                  if (stmts.size > 1) Begin(stmts)
                  else stmts(0)
               }
               case (s) => s map (remove_chirrtl_s) map (remove_chirrtl_e(MALE))
            }
         }
         collect_mports(m.body)
         val sx = collect_refs(m.body)
         InModule(m.info,m.name, m.ports, remove_chirrtl_s(sx))
      }
      val modulesx = c.modules.map{ m => {
            (m) match { 
               case (m:InModule) => remove_chirrtl_m(m)
               case (m:ExModule) => m
            }}}
      Circuit(c.info,modulesx, c.main)
   }
}
