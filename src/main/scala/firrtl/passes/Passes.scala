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

// For calling Stanza 
import scala.sys.process._
import scala.io.Source

// Datastructures
import scala.collection.mutable.LinkedHashMap
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

// Trait for migration, trap to Stanza implementation for passes not yet implemented in Scala
trait StanzaPass extends LazyLogging {
  def stanzaPass(c: Circuit, n: String): Circuit = {
    // For migration from Stanza, handle unimplemented Passes
    logger.debug(s"Pass ${n} is not yet implemented in Scala")
    val stanzaPasses = Seq("resolve", n) 
    val toStanza = Files.createTempFile(Paths.get(""), n, ".fir")
    val fromStanza = Files.createTempFile(Paths.get(""), n, ".fir")
    Files.write(toStanza, c.serialize.getBytes)

    val cmd = Seq("firrtl-stanza", "-i", toStanza.toString, "-o", fromStanza.toString, "-b", "firrtl", "-p", "c") ++ 
              stanzaPasses.flatMap(x=>Seq("-x", x))
    logger.debug(cmd.mkString(" "))
    val ret = cmd.!
    //println(ret)
    val newC = Parser.parse(fromStanza.toString, Source.fromFile(fromStanza.toString).getLines)
    Files.delete(toStanza)
    Files.delete(fromStanza)
    newC
  }
}

object PassUtils extends LazyLogging {
  val listOfPasses: Seq[Pass] = Seq(ToWorkingIR,ResolveKinds,InferTypes,ResolveGenders,InferWidths,PullMuxes,ExpandConnects,RemoveAccesses,ExpandWhens,LowerTypes)
  lazy val mapNameToPass: Map[String, Pass] = listOfPasses.map(p => p.name -> p).toMap

  def executePasses(c: Circuit, passes: Seq[Pass]): Circuit = { 
    if (passes.isEmpty) {logger.debug(s"Done!"); c}
    else {
       val p = passes.head
       val name = p.name
       logger.debug(s"Starting ${name}")
       println(s"Starting ${name}")
       val x = p.run(c)
       logger.debug(x.serialize)
       logger.debug(s"Finished ${name}")
       println(x.serialize)
       println(s"Finished ${name}")
       executePasses(x, passes.tail)
    }
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

object Resolve extends Pass with StanzaPass {
  def name = "Resolve"
  def run (c:Circuit): Circuit = stanzaPass(c, "resolve")
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
   val width_name_hash = LinkedHashMap[String,Int]()
   def set_type (s:Stmt,t:Type) : Stmt = {
      s match {
         case s:DefWire => DefWire(s.info,s.name,t)
         case s:DefRegister => DefRegister(s.info,s.name,t,s.clock,s.reset,s.init)
         case s:DefMemory => DefMemory(s.info,s.name,t,s.depth,s.write_latency,s.read_latency,s.readers,s.writers,s.readwriters)
         case s:DefNode => s
         case s:DefPoison => DefPoison(s.info,s.name,t)
      }
   }
   def remove_unknowns_w (w:Width):Width = {
      w match {
         case w:UnknownWidth => VarWidth(firrtl_gensym("w",width_name_hash))
         case w => w
      }
   }
   def remove_unknowns (t:Type): Type = mapr(remove_unknowns_w _,t)
   def run (c:Circuit): Circuit = {
      val module_types = LinkedHashMap[String,Type]()
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
   def reduce_var_widths (c:Circuit,h:LinkedHashMap[String,Width]) : Circuit = {
      def evaluate (w:Width) : Width = {
         def apply_2 (a:Option[BigInt],b:Option[BigInt], f: (BigInt,BigInt) => BigInt) : Option[BigInt] = {
            (a,b) match {
               case (a:Some[BigInt],b:Some[BigInt]) => Some(f(a.get,b.get))
               case (a,b) => None } }
         def apply_1 (a:Option[BigInt], f: (BigInt) => BigInt) : Option[BigInt] = {
            (a) match {
               case (a:Some[BigInt]) => Some(f(a.get))
               case (a) => None } }
         def apply_l (l:Seq[Option[BigInt]],f:(BigInt,BigInt) => BigInt) : Option[BigInt] = {
            if (l.size == 0) Some(BigInt(0)) else apply_2(l.head,apply_l(l.tail,f),f) 
         }
         def max (a:BigInt,b:BigInt) : BigInt = if (a >= b) a else b
         def min (a:BigInt,b:BigInt) : BigInt = if (a >= b) b else a
         def pow (a:BigInt,b:BigInt) : BigInt = BigInt((scala.math.pow(a.toDouble,b.toDouble) - 1).toLong)
         def solve (w:Width) : Option[BigInt] = {
            (w) match {
               case (w:VarWidth) => {
                  val wx = h.get(w.name)
                  (wx) match {
                     case (wx:Some[Width]) => {
                        wx.get match {
                           case (v:VarWidth) => None
                           case (v) => solve(v) }}
                     case (None) => None }}
               case (w:MaxWidth) => apply_l(w.args.map(solve _),max)
               case (w:MinWidth) => apply_l(w.args.map(solve _),min)
               case (w:PlusWidth) => apply_2(solve(w.arg1),solve(w.arg2),{_ + _})
               case (w:MinusWidth) => apply_2(solve(w.arg1),solve(w.arg2),{_ - _})
               case (w:ExpWidth) => apply_2(Some(BigInt(2)),solve(w.arg1),pow)
               case (w:IntWidth) => Some(w.width)
               case (w) => println(w); error("Shouldn't be here"); None;
            }
         }
         val s = solve(w)
         (s) match {
            case (s:Some[BigInt]) => IntWidth(s.get)
            case (s) => w }
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
         val sh = sym_hash
         mname = m.name
         def remove_s (s:Stmt) : Stmt = {
            val stmts = ArrayBuffer[Stmt]()
            def create_temp (e:Expression) : Expression = {
               val n = firrtl_gensym_module(mname)
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
                  case e => {
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

object ExpandWhens extends Pass {
   def name = "Expand Whens"
   var mname = ""
// ; ========== Expand When Utilz ==========
   def add (hash:LinkedHashMap[WrappedExpression,Expression],key:WrappedExpression,value:Expression) = {
      hash += (key -> value)
   }

   def get_entries (hash:LinkedHashMap[WrappedExpression,Expression],exps:Seq[Expression]) : LinkedHashMap[WrappedExpression,Expression] = {
      val hashx = LinkedHashMap[WrappedExpression,Expression]()
      exps.foreach { e => {
         val value = hash.get(e)
         value match {
            case (value:Some[Expression]) => add(hashx,e,value.get)
            case (None) => {}
         }
      }}
      hashx
   }
   def get_female_refs (n:String,t:Type,g:Gender) : Seq[Expression] = {
      val exps = create_exps(WRef(n,t,ExpKind(),g))
      val expsx = ArrayBuffer[Expression]()
      def get_gender (t:Type, i:Int, g:Gender) : Gender = {
         val f = get_flip(t,i,DEFAULT)
         times(g, f)
      }
      for (i <- 0 until exps.size) {
         get_gender(t,i,g) match {
            case BIGENDER => expsx += exps(i)
            case FEMALE => expsx += exps(i)
            case _ => false
         }
      }
      expsx
   }
   
   // ------------ Pass -------------------
   def run (c:Circuit): Circuit = {
      def void_all (m:InModule) : InModule = {
         mname = m.name
         def void_all_s (s:Stmt) : Stmt = {
            (s) match {
               case (_:DefWire|_:DefRegister|_:WDefInstance|_:DefMemory) => {
                  val voids = ArrayBuffer[Stmt]()
                  for (e <- get_female_refs(get_name(s),get_type(s),get_gender(s))) {
                     voids += Connect(get_info(s),e,WVoid())
                  }
                  Begin(Seq(s,Begin(voids)))
               }
               case (s) => s map (void_all_s)
            }
         }
         val voids = ArrayBuffer[Stmt]()
         for (p <- m.ports) {
            for (e <- get_female_refs(p.name,p.tpe,get_gender(p))) {
               voids += Connect(p.info,e,WVoid())
            }
         }
         val bodyx = void_all_s(m.body)
         InModule(m.info,m.name,m.ports,Begin(Seq(Begin(voids),bodyx)))
      }
      def expand_whens (m:InModule) : Tuple2[LinkedHashMap[WrappedExpression,Expression],ArrayBuffer[Stmt]] = {
         val simlist = ArrayBuffer[Stmt]()
         mname = m.name
         def expand_whens (netlist:LinkedHashMap[WrappedExpression,Expression],p:Expression)(s:Stmt) : Stmt = {
            (s) match {
               case (s:Connect) => netlist(s.loc) = s.exp
               case (s:IsInvalid) => netlist(s.exp) = WInvalid()
               case (s:Conditionally) => {
                  val exps = ArrayBuffer[Expression]()
                  def prefetch (s:Stmt) : Stmt = {
                     (s) match {
                        case (s:Connect) => exps += s.loc; s
                        case (s) => s map(prefetch)
                     }
                  }
                  prefetch(s.conseq)
                  val c_netlist = get_entries(netlist,exps)
                  expand_whens(c_netlist,AND(p,s.pred))(s.conseq)
                  expand_whens(netlist,AND(p,NOT(s.pred)))(s.alt)
                  for (lvalue <- c_netlist.keys) {
                     val value = netlist.get(lvalue)
                     (value) match {
                        case (value:Some[Expression]) => {
                           val tv = c_netlist(lvalue)
                           val fv = value.get
                           val res = (tv,fv) match {
                              case (tv:WInvalid,fv:WInvalid) => WInvalid()
                              case (tv:WInvalid,fv) => ValidIf(NOT(s.pred),fv,tpe(fv))
                              case (tv,fv:WInvalid) => ValidIf(s.pred,tv,tpe(tv))
                              case (tv,fv) => Mux(s.pred,tv,fv,mux_type_and_widths(tv,fv))
                           }
                           netlist(lvalue) = res
                        }
                        case (None) => add(netlist,lvalue,c_netlist(lvalue))
                     }
                  }
               }
               case (s:Print) => {
                  if (weq(p,one)) {
                     simlist += s
                  } else {
                     simlist += Print(s.info,s.string,s.args,s.clk,AND(p,s.en))
                  }
               }
               case (s:Stop) => {
                  if (weq(p,one)) {
                     simlist += s
                  } else {
                     simlist += Stop(s.info,s.ret,s.clk,AND(p,s.en))
                  }
               }
               case (s) => s map(expand_whens(netlist,p))
            }
            s
         }
         val netlist = LinkedHashMap[WrappedExpression,Expression]()
         expand_whens(netlist,one)(m.body)
   
         //println("Netlist:")
         //println(netlist)
         //println("Simlist:")
         //println(simlist)
         ( netlist, simlist )
      }
   
      def create_module (netlist:LinkedHashMap[WrappedExpression,Expression],simlist:ArrayBuffer[Stmt],m:InModule) : InModule = {
         mname = m.name
         val stmts = ArrayBuffer[Stmt]()
         val connections = ArrayBuffer[Stmt]()
         def replace_void (e:Expression)(rvalue:Expression) : Expression = {
            (rvalue) match {
               case (rv:WVoid) => e
               case (rv) => rv map (replace_void(e))
            }
         }
         def create (s:Stmt) : Stmt = {
            (s) match {
               case (_:DefWire|_:WDefInstance|_:DefMemory) => {
                  stmts += s
                  for (e <- get_female_refs(get_name(s),get_type(s),get_gender(s))) {
                     val rvalue = netlist(e)
                     val con = (rvalue) match {
                        case (rvalue:WInvalid) => IsInvalid(get_info(s),e)
                        case (rvalue) => Connect(get_info(s),e,rvalue)
                     }
                     connections += con
                  }
               }
               case (s:DefRegister) => {
                  stmts += s
                  for (e <- get_female_refs(get_name(s),get_type(s),get_gender(s))) {
                     val rvalue = replace_void(e)(netlist(e))
                     val con = (rvalue) match {
                        case (rvalue:WInvalid) => IsInvalid(get_info(s),e)
                        case (rvalue) => Connect(get_info(s),e,rvalue)
                     }
                     connections += con
                  }
               }
               case (_:DefPoison|_:DefNode) => stmts += s
               case (s) => s map(create)
            }
            s
         }
         create(m.body)
         for (p <- m.ports) {
            for (e <- get_female_refs(p.name,p.tpe,get_gender(p))) {
               val rvalue = netlist(e)
               val con = (rvalue) match {
                  case (rvalue:WInvalid) => IsInvalid(p.info,e)
                  case (rvalue) => Connect(p.info,e,rvalue)
               }
               connections += con
            }
         }
         for (x <- simlist) { stmts += x }
         InModule(m.info,m.name,m.ports,Begin(Seq(Begin(stmts),Begin(connections))))
      }
   
      val voided_modules = c.modules.map{ m => {
            (m) match {
               case (m:ExModule) => m
               case (m:InModule) => void_all(m)
            } } }
      val modulesx = voided_modules.map{ m => {
            (m) match {
               case (m:ExModule) => m
               case (m:InModule) => {
                  val (netlist, simlist) = expand_whens(m)
                  create_module(netlist,simlist,m)
               }
            }}}
      Circuit(c.info,modulesx,c.main)
   }
}

object ConstProp extends Pass {
   def name = "Constant Propogation"
   var mname = ""
   def const_prop_e (e:Expression) : Expression = {
      e map (const_prop_e) match {
         case (e:DoPrim) => {
            e.op match {
               case SHIFT_RIGHT_OP => {
                  (e.args(0)) match {
                     case (x:UIntValue) => {

                        val b =
                           if (e.consts(0) > long_BANG(tpe(x))) // shift amount is larger than input width
                              BigInt(0)
                           else x.value >> e.consts(0).toInt
                        UIntValue(b,tpe(e).as[UIntType].get.width)
                     }
                     case (x:SIntValue) => {
                        val b = x.value >> (long_BANG(tpe(x)) - long_BANG(tpe(e))).toInt // take sign bit if shift amount is larger than input width
                        SIntValue(b,tpe(e).as[SIntType].get.width)
                     }
                     case (x) => e
                  }
               }
               case BITS_SELECT_OP => {
                  e.args(0) match {
                     case (x:UIntValue) => {
                        val hi = e.consts(0).toInt
                        val lo = e.consts(1).toInt
                        require(hi >= lo)
                        val b = (x.value >> lo) & ((BigInt(1) << (hi - lo + 1)) - 1)
                        UIntValue(b,tpe(e).as[UIntType].get.width)
                     }
                     case (x) => {
                        if (long_BANG(tpe(e)) == long_BANG(tpe(x))) {
                           tpe(x) match {
                              case (t:UIntType) => x
                              case _ => DoPrim(AS_UINT_OP,Seq(x),Seq(),tpe(e)) 
                           }
                        }
                        else e
                     }
                  }
               }
               case (_) => e
            }
         }
         case (e) => e
      }
   }
   def const_prop_s (s:Stmt) : Stmt = s map (const_prop_s) map (const_prop_e)
   def run (c:Circuit): Circuit = {
      val modulesx = c.modules.map{ m => {
         m match {
            case (m:ExModule) => m
            case (m:InModule) => {
               mname = m.name
               InModule(m.info,m.name,m.ports,const_prop_s(m.body))
            }
         }
      }}
      Circuit(c.info,modulesx,c.main)
   }
}

object LoToVerilog extends Pass with StanzaPass {
   def name = "Lo To Verilog"
   def run (c:Circuit): Circuit = stanzaPass(c, "lo-to-verilog")
}

object FromCHIRRTL extends Pass with StanzaPass {
   def name = "From CHIRRTL"
   def run (c:Circuit): Circuit = stanzaPass(c, "from-chirrtl")
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
   def v_wrap_s (s:Stmt) : Stmt = s map (v_wrap_s) map (v_wrap_e)
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

object SplitExp extends Pass {
   def name = "Split Expressions"
   var mname = ""
   def split_exp (m:InModule) : InModule = {
      mname = m.name
      val v = ArrayBuffer[Stmt]()
      def split_exp_s (s:Stmt) : Stmt = {
         def split (e:Expression) : Expression = {
            val n = firrtl_gensym_module(mname)
            v += DefNode(info(s),n,e)
            WRef(n,tpe(e),kind(e),gender(e))
         }
         def split_exp_e (i:Int)(e:Expression) : Expression = {
            e map (split_exp_e(i + 1)) match {
               case (e:DoPrim) => if (i > 0) split(e) else e
               case (e) => e
            }
         }
	 s match {
            case (s:Begin) => s map (split_exp_s)
	    case (s:Print) => {
		val sx = s map (split_exp_e(1))
                v += sx; sx
	    }
            case (s) => {
		val sx = s map (split_exp_e(0))
                v += sx; sx
	    }
         }
      }
      split_exp_s(m.body)
      InModule(m.info,m.name,m.ports,Begin(v))
   }
   
   def run (c:Circuit): Circuit = {
      val modulesx = c.modules.map{ m => {
         (m) match {
            case (m:InModule) => split_exp(m)
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

object LowerTypes extends Pass {
   def name = "Lower Types"
   var mname = ""
   def is_ground (t:Type) : Boolean = {
      (t) match {
         case (_:UIntType|_:SIntType) => true
         case (t) => false
      }
   }
   def data (ex:Expression) : Boolean = {
      (kind(ex)) match {
         case (k:MemKind) => (ex) match {
            case (_:WRef|_:WSubIndex) => false
            case (ex:WSubField) => {
               var yes = ex.name match {
                  case "rdata" => true
                  case "data" => true
                  case "mask" => true
                  case _ => false
               }
               yes && ((ex.exp) match {
                  case (e:WSubField) => kind(e).as[MemKind].get.ports.contains(e.name) && (e.exp.typeof[WRef])
                  case (e) => false
               })
            }
            case (ex) => false
         }
         case (k) => false
      }
   }
   def expand_name (e:Expression) : Seq[String] = {
      val names = ArrayBuffer[String]()
      def expand_name_e (e:Expression) : Expression = {
         (e map (expand_name_e)) match {
            case (e:WRef) => names += e.name
            case (e:WSubField) => names += e.name
            case (e:WSubIndex) => names += e.value.toString
         }
         e
      }
      expand_name_e(e)
      names
   }
   def lower_other_mem (e:Expression, dt:Type) : Seq[Expression] = {
      val names = expand_name(e)
      if (names.size < 3) error("Shouldn't be here")
      create_exps(names(0),dt).map{ x => {
         var base = lowered_name(x)
         for (i <- 0 until names.size) {
            if (i >= 3) base = base + "_" + names(i)
         }
         val m = WRef(base, UnknownType(), kind(e), UNKNOWNGENDER)
         val p = WSubField(m,names(1),UnknownType(),UNKNOWNGENDER)
         WSubField(p,names(2),UnknownType(),UNKNOWNGENDER)
      }}
   }
   def lower_data_mem (e:Expression) : Expression = {
      val names = expand_name(e)
      if (names.size < 3) error("Shouldn't be here")
      else {
         var base = names(0)
         for (i <- 0 until names.size) {
            if (i >= 3) base = base + "_" + names(i)
         }
         val m = WRef(base, UnknownType(), kind(e), UNKNOWNGENDER)
         val p = WSubField(m,names(1),UnknownType(),UNKNOWNGENDER)
         WSubField(p,names(2),UnknownType(),UNKNOWNGENDER)
      }
   }
   def merge (a:String,b:String,x:String) : String = a + x + b
   def root_ref (e:Expression) : WRef = {
      (e) match {
         case (e:WRef) => e
         case (e:WSubField) => root_ref(e.exp)
         case (e:WSubIndex) => root_ref(e.exp)
         case (e:WSubAccess) => root_ref(e.exp)
      }
   }
   
   //;------------- Pass ------------------
   
   def lower_types (m:Module) : Module = {
      val mdt = LinkedHashMap[String,Type]()
      mname = m.name
      def lower_types (s:Stmt) : Stmt = {
         def lower_mem (e:Expression) : Seq[Expression] = {
            val names = expand_name(e)
            if (Seq("data","mask","rdata").contains(names(2))) Seq(lower_data_mem(e))
            else lower_other_mem(e,mdt(root_ref(e).name))
         }
         def lower_types_e (e:Expression) : Expression = {
            e match {
               case (_:WRef|_:UIntValue|_:SIntValue) => e
               case (_:WSubField|_:WSubIndex) => {
                  (kind(e)) match {
                     case (k:InstanceKind) => {
                        val names = expand_name(e)
                        var n = names(1)
                        for (i <- 0 until names.size) {
                           if (i > 1) n = n + "_" + names(i)
                        }
                        WSubField(root_ref(e),n,tpe(e),gender(e))
                     }
                     case (k:MemKind) => {
                        if (gender(e) != FEMALE) lower_mem(e)(0)
                        else e
                     }
                     case (k) => WRef(lowered_name(e),tpe(e),kind(e),gender(e))
                  }
               }
               case (e:DoPrim) => e map (lower_types_e)
               case (e:Mux) => e map (lower_types_e)
               case (e:ValidIf) => e map (lower_types_e)
            }
         }
         (s) match {
            case (s:DefWire) => {
               if (is_ground(s.tpe)) s else {
                  val es = create_exps(s.name,s.tpe)
                  val stmts = (es, 0 until es.size).zipped.map{ (e,i) => {
                     DefWire(s.info,lowered_name(e),tpe(e))
                  }}
                  Begin(stmts)
               }
            }
            case (s:DefPoison) => {
               if (is_ground(s.tpe)) s else {
                  val es = create_exps(s.name,s.tpe)
                  val stmts = (es, 0 until es.size).zipped.map{ (e,i) => {
                     DefPoison(s.info,lowered_name(e),tpe(e))
                  }}
                  Begin(stmts)
               }
            }
            case (s:DefRegister) => {
               if (is_ground(s.tpe)) s else {
                  val es = create_exps(s.name,s.tpe)
                  val inits = create_exps(s.init) 
                  val stmts = (es, 0 until es.size).zipped.map{ (e,i) => {
                     val init = lower_types_e(inits(i))
                     DefRegister(s.info,lowered_name(e),tpe(e),s.clock,s.reset,init)
                  }}
                  Begin(stmts)
               }
            }
            case (s:WDefInstance) => {
               val fieldsx = s.tpe.as[BundleType].get.fields.flatMap{ f => {
                  val es = create_exps(WRef(f.name,f.tpe,ExpKind(),times(f.flip,MALE)))
                  es.map{ e => {
                     gender(e) match {
                        case MALE => Field(lowered_name(e),DEFAULT,f.tpe)
                        case FEMALE => Field(lowered_name(e),REVERSE,f.tpe)
                     }
                  }}
               }}
               WDefInstance(s.info,s.name,s.module,BundleType(fieldsx))
            }
            case (s:DefMemory) => {
               mdt(s.name) = s.data_type
               if (is_ground(s.data_type)) s else {
                  val es = create_exps(s.name,s.data_type)
                  val stmts = es.map{ e => {
                     DefMemory(s.info,lowered_name(e),tpe(e),s.depth,s.write_latency,s.read_latency,s.readers,s.writers,s.readwriters)
                  }}
                  Begin(stmts)
               }
            }
            case (s:IsInvalid) => {
               val sx = (s map (lower_types_e)).as[IsInvalid].get
               kind(sx.exp) match {
                  case (k:MemKind) => {
                     val es = lower_mem(sx.exp)
                     Begin(es.map(e => {IsInvalid(sx.info,e)}))
                  }
                  case (_) => sx
               }
            }
            case (s:Connect) => {
               val sx = (s map (lower_types_e)).as[Connect].get
               kind(sx.loc) match {
                  case (k:MemKind) => {
                     val es = lower_mem(sx.loc)
                     Begin(es.map(e => {Connect(sx.info,e,sx.exp)}))
                  }
                  case (_) => sx
               }
            }
            case (s:DefNode) => {
               val locs = create_exps(s.name,tpe(s.value))
               val n = locs.size
               val nodes = ArrayBuffer[Stmt]()
               val exps = create_exps(s.value)
               for (i <- 0 until n) {
                  val locx = locs(i)
                  val expx = exps(i)
                  nodes += DefNode(s.info,lowered_name(locx),lower_types_e(expx))
               }
               if (n == 1) nodes(0) else Begin(nodes)
            }
            case (s) => s map (lower_types) map (lower_types_e)
         }
      }
   
      val portsx = m.ports.flatMap{ p => {
         val es = create_exps(WRef(p.name,p.tpe,PortKind(),to_gender(p.direction)))
         es.map(e => { Port(p.info,lowered_name(e),to_dir(gender(e)),tpe(e)) })
      }}
      (m) match {
         case (m:ExModule) => ExModule(m.info,m.name,portsx)
         case (m:InModule) => InModule(m.info,m.name,portsx,lower_types(m.body))
      }
   }
   
   def run (c:Circuit) : Circuit = {
      val modulesx = c.modules.map(m => lower_types(m))
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
                        stmts += Connect(s.info,SubField(SubField(Ref(s.name,ut),r.name,ut),"clk",taddr),r.clk)
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
                  val ens = ArrayBuffer[String]()
                  val masks = ArrayBuffer[String]()
                  s.direction match {
                     case MReadWrite => {
                        repl(s.name) = DataRef(SubField(Ref(s.mem,ut),s.name,ut),"rdata","data","mask",true)
                        addrs += "addr"
                        ens += "en"
                        masks += "mask"
                     }
                     case MWrite => {
                        repl(s.name) = DataRef(SubField(Ref(s.mem,ut),s.name,ut),"data","data","mask",false)
                        addrs += "addr"
                        ens += "en"
                        masks += "mask"
                     }
                     case _ => {
                        repl(s.name) = DataRef(SubField(Ref(s.mem,ut),s.name,ut),"data","data","blah",false)
                        addrs += "addr"
                        ens += "en"
                     }
                  }
                  val stmts = ArrayBuffer[Stmt]()
                  for (x <- addrs ) {
                     stmts += Connect(s.info,SubField(SubField(Ref(s.mem,ut),s.name,ut),x,ut),s.exps(0))
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
