///*
//Copyright (c) 2014 - 2016 The Regents of the University of
//California (Regents). All Rights Reserved.  Redistribution and use in
//source and binary forms, with or without modification, are permitted
//provided that the following conditions are met:
//   * Redistributions of source code must retain the above
//     copyright notice, this list of conditions and the following
//     two paragraphs of disclaimer.
//   * Redistributions in binary form must reproduce the above
//     copyright notice, this list of conditions and the following
//     two paragraphs of disclaimer in the documentation and/or other materials
//     provided with the distribution.
//   * Neither the name of the Regents nor the names of its contributors
//     may be used to endorse or promote products derived from this
//     software without specific prior written permission.
//IN NO EVENT SHALL REGENTS BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
//SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS,
//ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
//REGENTS HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT
//LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
//A PARTICULAR PURPOSE. THE SOFTWARE AND ACCOMPANYING DOCUMENTATION, IF
//ANY, PROVIDED HEREUNDER IS PROVIDED "AS IS". REGENTS HAS NO OBLIGATION
//TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
//MODIFICATIONS.
//*/
//
//package firrtl.passes
//
//import com.typesafe.scalalogging.LazyLogging
//
//import firrtl._
//import firrtl.ir._
//import firrtl.Utils._
//import firrtl.Mappers._
//import firrtl.PrimOps._
//
//trait Pass extends LazyLogging {
//  def name: String
//  def run(c: Circuit): Circuit
//}
//
//// Error handling
//class PassException(message: String) extends Exception(message)
//class PassExceptions(exceptions: Seq[PassException]) extends Exception("\n" + exceptions.mkString("\n"))
//class Errors {
//  val errors = collection.mutable.ArrayBuffer[PassException]()
//  def append(pe: PassException) = errors.append(pe)
//  def trigger() = errors.size match {
//    case 0 =>
//    case 1 => throw errors.head
//    case _ =>
//      append(new PassException(s"${errors.length} errors detected!"))
//      throw new PassExceptions(errors)
//  }
//}
//
//// These should be distributed into separate files
//object ToWorkingIR extends Pass {
//<<<<<<< HEAD
//   private var mname = ""
//   def name = "Working IR"
//   def run (c:Circuit): Circuit = {
//      def toExp (e:Expression) : Expression = {
//         e map (toExp) match {
//            case e:Reference => WRef(e.name, e.tpe, NodeKind(), UNKNOWNGENDER)
//            case e:SubField => WSubField(e.expr, e.name, e.tpe, UNKNOWNGENDER)
//            case e:SubIndex => WSubIndex(e.expr, e.value, e.tpe, UNKNOWNGENDER)
//            case e:SubAccess => WSubAccess(e.expr, e.index, e.tpe, UNKNOWNGENDER)
//            case e => e
//         }
//      }
//      def toStmt (s:Statement) : Statement = {
//         s map (toExp) match {
//            case s:DefInstance => WDefInstance(s.info,s.name,s.module,UnknownType)
//            case s => s map (toStmt)
//         }
//      }
//      val modulesx = c.modules.map { m => 
//         mname = m.name
//         m match {
//            case m:Module => Module(m.info,m.name, m.ports, toStmt(m.body))
//            case m:ExtModule => m
//         }
//      }
//      Circuit(c.info,modulesx,c.main)
//   }
//}
//
//object ResolveKinds extends Pass {
//   private var mname = ""
//   def name = "Resolve Kinds"
//   def run (c:Circuit): Circuit = {
//      def resolve_kinds (m:DefModule, c:Circuit):DefModule = {
//         val kinds = LinkedHashMap[String,Kind]()
//         def resolve (body:Statement) = {
//            def resolve_expr (e:Expression):Expression = {
//               e match {
//                  case e:WRef => WRef(e.name,tpe(e),kinds(e.name),e.gender)
//                  case e => e map (resolve_expr)
//               }
//            }
//            def resolve_stmt (s:Statement):Statement = s map (resolve_stmt) map (resolve_expr)
//            resolve_stmt(body)
//         }
//   
//         def find (m:DefModule) = {
//            def find_stmt (s:Statement):Statement = {
//               s match {
//                  case s:DefWire => kinds(s.name) = WireKind()
//                  case s:DefNode => kinds(s.name) = NodeKind()
//                  case s:DefRegister => kinds(s.name) = RegKind()
//                  case s:WDefInstance => kinds(s.name) = InstanceKind()
//                  case s:DefMemory => kinds(s.name) = MemKind(s.readers ++ s.writers ++ s.readwriters)
//                  case s => false
//               }
//               s map (find_stmt)
//            }
//            m.ports.foreach { p => kinds(p.name) = PortKind() }
//            m match {
//               case m:Module => find_stmt(m.body)
//               case m:ExtModule => false
//            }
//         }
//       
//         mname = m.name
//         find(m)   
//         m match {
//            case m:Module => {
//               val bodyx = resolve(m.body)
//               Module(m.info,m.name,m.ports,bodyx)
//            }
//            case m:ExtModule => ExtModule(m.info,m.name,m.ports)
//         }
//      }
//      val modulesx = c.modules.map(m => resolve_kinds(m,c))
//      Circuit(c.info,modulesx,c.main)
//   }
//}
//
//object InferTypes extends Pass {
//   private var mname = ""
//   def name = "Infer Types"
//   def set_type (s:Statement, t:Type) : Statement = {
//      s match {
//         case s:DefWire => DefWire(s.info,s.name,t)
//         case s:DefRegister => DefRegister(s.info,s.name,t,s.clock,s.reset,s.init)
//         case s:DefMemory => DefMemory(s.info,s.name,t,s.depth,s.writeLatency,s.readLatency,s.readers,s.writers,s.readwriters)
//         case s:DefNode => s
//      }
//   }
//   def remove_unknowns_w (w:Width)(implicit namespace: Namespace):Width = {
//      w match {
//         case UnknownWidth => VarWidth(namespace.newName("w"))
//         case w => w
//      }
//   }
//   def remove_unknowns (t:Type)(implicit n: Namespace): Type = mapr(remove_unknowns_w _,t)
//   def run (c:Circuit): Circuit = {
//      val module_types = LinkedHashMap[String,Type]()
//      implicit val wnamespace = Namespace()
//      def infer_types (m:DefModule) : DefModule = {
//         val types = LinkedHashMap[String,Type]()
//         def infer_types_e (e:Expression) : Expression = {
//            e map (infer_types_e) match {
//               case e:ValidIf => ValidIf(e.cond,e.value,tpe(e.value))
//               case e:WRef => WRef(e.name, types(e.name),e.kind,e.gender)
//               case e:WSubField => WSubField(e.exp,e.name,field_type(tpe(e.exp),e.name),e.gender)
//               case e:WSubIndex => WSubIndex(e.exp,e.value,sub_type(tpe(e.exp)),e.gender)
//               case e:WSubAccess => WSubAccess(e.exp,e.index,sub_type(tpe(e.exp)),e.gender)
//               case e:DoPrim => set_primop_type(e)
//               case e:Mux => Mux(e.cond,e.tval,e.fval,mux_type_and_widths(e.tval,e.fval))
//               case e:UIntLiteral => e
//               case e:SIntLiteral => e
//            }
//         }
//         def infer_types_s (s:Statement) : Statement = {
//            s match {
//               case s:DefRegister => {
//                  val t = remove_unknowns(get_type(s))
//                  types(s.name) = t
//                  set_type(s,t) map (infer_types_e)
//               }
//               case s:DefWire => {
//                  val sx = s map(infer_types_e)
//                  val t = remove_unknowns(get_type(sx))
//                  types(s.name) = t
//                  set_type(sx,t)
//               }
//               case s:DefNode => {
//                  val sx = s map (infer_types_e)
//                  val t = remove_unknowns(get_type(sx))
//                  types(s.name) = t
//                  set_type(sx,t)
//               }
//               case s:DefMemory => {
//                  val t = remove_unknowns(get_type(s))
//                  types(s.name) = t
//                  val dt = remove_unknowns(s.dataType)
//                  set_type(s,dt)
//               }
//               case s:WDefInstance => {
//                  types(s.name) = module_types(s.module)
//                  WDefInstance(s.info,s.name,s.module,module_types(s.module))
//               }
//               case s => s map (infer_types_s) map (infer_types_e)
//            }
//         }
// 
//         mname = m.name
//         m.ports.foreach(p => types(p.name) = p.tpe)
//         m match {
//            case m:Module => Module(m.info,m.name,m.ports,infer_types_s(m.body))
//            case m:ExtModule => m
//         }
//       }
// 
//      val modulesx = c.modules.map { 
//         m => {
//            mname = m.name
//            val portsx = m.ports.map(p => Port(p.info,p.name,p.direction,remove_unknowns(p.tpe)))
//            m match {
//               case m:Module => Module(m.info,m.name,portsx,m.body)
//               case m:ExtModule => ExtModule(m.info,m.name,portsx)
//            }
//         }
//      }
//      modulesx.foreach(m => module_types(m.name) = module_type(m))
//      Circuit(c.info,modulesx.map({m => mname = m.name; infer_types(m)}) , c.main )
//   }
//}
//
//object ResolveGenders extends Pass {
//   private var mname = ""
//   def name = "Resolve Genders"
//   def run (c:Circuit): Circuit = {
//      def resolve_e (g:Gender)(e:Expression) : Expression = {
//         e match {
//            case e:WRef => WRef(e.name,e.tpe,e.kind,g)
//            case e:WSubField => {
//               val expx = 
//                  field_flip(tpe(e.exp),e.name) match {
//                     case Default => resolve_e(g)(e.exp)
//                     case Flip => resolve_e(swap(g))(e.exp)
//                  }
//               WSubField(expx,e.name,e.tpe,g)
//            }
//            case e:WSubIndex => {
//               val expx = resolve_e(g)(e.exp)
//               WSubIndex(expx,e.value,e.tpe,g)
//            }
//            case e:WSubAccess => {
//               val expx = resolve_e(g)(e.exp)
//               val indexx = resolve_e(MALE)(e.index)
//               WSubAccess(expx,indexx,e.tpe,g)
//            }
//            case e => e map (resolve_e(g))
//         }
//      }
//            
//      def resolve_s (s:Statement) : Statement = {
//         s match {
//            case s:IsInvalid => {
//               val expx = resolve_e(FEMALE)(s.expr)
//               IsInvalid(s.info,expx)
//            }
//            case s:Connect => {
//               val locx = resolve_e(FEMALE)(s.loc)
//               val expx = resolve_e(MALE)(s.expr)
//               Connect(s.info,locx,expx)
//            }
//            case s:PartialConnect => {
//               val locx = resolve_e(FEMALE)(s.loc)
//               val expx = resolve_e(MALE)(s.expr)
//               PartialConnect(s.info,locx,expx)
//            }
//            case s => s map (resolve_e(MALE)) map (resolve_s)
//         }
//      }
//      val modulesx = c.modules.map { 
//         m => {
//            mname = m.name
//            m match {
//               case m:Module => {
//                  val bodyx = resolve_s(m.body)
//                  Module(m.info,m.name,m.ports,bodyx)
//               }
//               case m:ExtModule => m
//            }
//         }
//      }
//      Circuit(c.info,modulesx,c.main)
//   }
//}
//
//object InferWidths extends Pass {
//   def name = "Infer Widths"
//   var mname = ""
//   def solve_constraints (l:Seq[WGeq]) : LinkedHashMap[String,Width] = {
//      def unique (ls:Seq[Width]) : Seq[Width] = ls.map(w => new WrappedWidth(w)).distinct.map(_.w)
//      def make_unique (ls:Seq[WGeq]) : LinkedHashMap[String,Width] = {
//         val h = LinkedHashMap[String,Width]()
//         for (g <- ls) {
//            (g.loc) match {
//               case (w:VarWidth) => {
//                  val n = w.name
//                  if (h.contains(n)) h(n) = MaxWidth(Seq(g.exp,h(n))) else h(n) = g.exp
//               }
//               case (w) => w 
//            }
//         }
//         h 
//      }
//      def simplify (w:Width) : Width = {
//         (w map (simplify)) match {
//            case (w:MinWidth) => {
//               val v = ArrayBuffer[Width]()
//               for (wx <- w.args) {
//                  (wx) match {
//                     case (wx:MinWidth) => for (x <- wx.args) { v += x }
//                     case (wx) => v += wx } }
//               MinWidth(unique(v)) }
//            case (w:MaxWidth) => {
//               val v = ArrayBuffer[Width]()
//               for (wx <- w.args) {
//                  (wx) match {
//                     case (wx:MaxWidth) => for (x <- wx.args) { v += x }
//                     case (wx) => v += wx } }
//               MaxWidth(unique(v)) }
//            case (w:PlusWidth) => {
//               (w.arg1,w.arg2) match {
//                  case (w1:IntWidth,w2:IntWidth) => IntWidth(w1.width + w2.width)
//                  case (w1,w2) => w }}
//            case (w:MinusWidth) => {
//               (w.arg1,w.arg2) match {
//                  case (w1:IntWidth,w2:IntWidth) => IntWidth(w1.width - w2.width)
//                  case (w1,w2) => w }}
//            case (w:ExpWidth) => {
//               (w.arg1) match {
//                  case (w1:IntWidth) => IntWidth(BigInt((scala.math.pow(2,w1.width.toDouble) - 1).toLong))
//                  case (w1) => w }}
//            case (w) => w } }
//      def substitute (h:LinkedHashMap[String,Width])(w:Width) : Width = {
//         //;println-all-debug(["Substituting for [" w "]"])
//         val wx = simplify(w)
//         //;println-all-debug(["After Simplify: [" wx "]"])
//         (simplify(w) map (substitute(h))) match {
//            case (w:VarWidth) => {
//               //;("matched  println-debugvarwidth!")
//               if (h.contains(w.name)) {
//                  //;println-debug("Contained!")
//                  //;println-all-debug(["Width: " w])
//                  //;println-all-debug(["Accessed: " h[name(w)]])
//                  val t = simplify(substitute(h)(h(w.name)))
//                  //;val t = h[name(w)]
//                  //;println-all-debug(["Width after sub: " t])
//                  h(w.name) = t
//                  t
//               } else w
//            }
//            case (w) => w
//               //;println-all-debug(["not varwidth!" w])
//         }
//      }
//      def b_sub (h:LinkedHashMap[String,Width])(w:Width) : Width = {
//         (w map (b_sub(h))) match {
//            case (w:VarWidth) => if (h.contains(w.name)) h(w.name) else w
//            case (w) => w
//         }
//      }
//      def remove_cycle (n:String)(w:Width) : Width = {
//         //;println-all-debug(["Removing cycle for " n " inside " w])
//         val wx = (w map (remove_cycle(n))) match {
//            case (w:MaxWidth) => MaxWidth(w.args.filter{ w => {
//               w match {
//                  case (w:VarWidth) => !(n equals w.name)
//                  case (w) => true
//               }}})
//            case (w:MinusWidth) => {
//               w.arg1 match {
//                  case (v:VarWidth) => if (n == v.name) v else w
//                  case (v) => w }}
//            case (w) => w
//         }
//         //;println-all-debug(["After removing cycle for " n ", returning " wx])
//         wx
//      }
//      def self_rec (n:String,w:Width) : Boolean = {
//         var has = false
//         def look (w:Width) : Width = {
//            (w map (look)) match {
//               case (w:VarWidth) => if (w.name == n) has = true
//               case (w) => w }
//            w }
//         look(w)
//         has }
//          
//      //; Forward solve
//      //; Returns a solved list where each constraint undergoes:
//      //;  1) Continuous Solving (using triangular solving)
//      //;  2) Remove Cycles
//      //;  3) Move to solved if not self-recursive
//      val u = make_unique(l)
//      
//      //println("======== UNIQUE CONSTRAINTS ========")
//      //for (x <- u) { println(x) }
//      //println("====================================")
//      
//   
//      val f = LinkedHashMap[String,Width]()
//      val o = ArrayBuffer[String]()
//      for (x <- u) {
//         //println("==== SOLUTIONS TABLE ====")
//         //for (x <- f) println(x)
//         //println("=========================")
//   
//         val (n, e) = (x._1, x._2)
//         val e_sub = substitute(f)(e)
//
//         //println("Solving " + n + " => " + e)
//         //println("After Substitute: " + n + " => " + e_sub)
//         //println("==== SOLUTIONS TABLE (Post Substitute) ====")
//         //for (x <- f) println(x)
//         //println("=========================")
//
//         val ex = remove_cycle(n)(e_sub)
//
//         //println("After Remove Cycle: " + n + " => " + ex)
//         if (!self_rec(n,ex)) {
//            //println("Not rec!: " + n + " => " + ex)
//            //println("Adding [" + n + "=>" + ex + "] to Solutions Table")
//            o += n
//            f(n) = ex
//         }
//      }
//   
//      //println("Forward Solved Constraints")
//      //for (x <- f) println(x)
//   
//      //; Backwards Solve
//      val b = LinkedHashMap[String,Width]()
//      for (i <- 0 until o.size) {
//         val n = o(o.size - 1 - i)
//         /*
//         println("SOLVE BACK: [" + n + " => " + f(n) + "]")
//         println("==== SOLUTIONS TABLE ====")
//         for (x <- b) println(x)
//         println("=========================")
//         */
//         val ex = simplify(b_sub(b)(f(n)))
//         /*
//         println("BACK RETURN: [" + n + " => " + ex + "]")
//         */
//         b(n) = ex
//         /*
//         println("==== SOLUTIONS TABLE (Post backsolve) ====")
//         for (x <- b) println(x)
//         println("=========================")
//         */
//      }
//      b
//   }
//
//   def reduce_var_widths(c: Circuit, h: LinkedHashMap[String,Width]): Circuit = {
//      def evaluate(w: Width): Width = {
//         def map2(a: Option[BigInt], b: Option[BigInt], f: (BigInt,BigInt) => BigInt): Option[BigInt] =
//            for (a_num <- a; b_num <- b) yield f(a_num, b_num)
//         def reduceOptions(l: Seq[Option[BigInt]], f: (BigInt,BigInt) => BigInt): Option[BigInt] =
//            l.reduce(map2(_, _, f))
//
//         // This function shouldn't be necessary
//         // Added as protection in case a constraint accidentally uses MinWidth/MaxWidth
//         // without any actual Widths. This should be elevated to an earlier error
//         def forceNonEmpty(in: Seq[Option[BigInt]], default: Option[BigInt]): Seq[Option[BigInt]] =
//            if(in.isEmpty) Seq(default)
//            else in
//
//
//         def solve(w: Width): Option[BigInt] = w match {
//            case (w: VarWidth) =>
//               for{
//                  v <- h.get(w.name) if !v.isInstanceOf[VarWidth]
//                  result <- solve(v)
//               } yield result
//            case (w: MaxWidth) => reduceOptions(forceNonEmpty(w.args.map(solve _), Some(BigInt(0))), max)
//            case (w: MinWidth) => reduceOptions(forceNonEmpty(w.args.map(solve _), None), min)
//            case (w: PlusWidth) => map2(solve(w.arg1), solve(w.arg2), {_ + _})
//            case (w: MinusWidth) => map2(solve(w.arg1), solve(w.arg2), {_ - _})
//            case (w: ExpWidth) => map2(Some(BigInt(2)), solve(w.arg1), pow_minus_one)
//            case (w: IntWidth) => Some(w.width)
//            case (w) => println(w); error("Shouldn't be here"); None;
//         }
//
//  def toExp(e: Expression): Expression = e map (toExp) match {
//    case e: Reference => WRef(e.name, e.tpe, NodeKind, UNKNOWNGENDER)
//    case e: SubField => WSubField(e.expr, e.name, e.tpe, UNKNOWNGENDER)
//    case e: SubIndex => WSubIndex(e.expr, e.value, e.tpe, UNKNOWNGENDER)
//    case e: SubAccess => WSubAccess(e.expr, e.index, e.tpe, UNKNOWNGENDER)
//    case e => e
//  }
//
//<<<<<<< HEAD
//      def reduce_var_widths_w (w:Width) : Width = {
//         //println-all-debug(["REPLACE: " w])
//         val wx = evaluate(w)
//         //println-all-debug(["WITH: " wx])
//         wx
//      }
//      def reduce_var_widths_s (s: Statement): Statement = {
//        def onType(t: Type): Type = t map onType map reduce_var_widths_w
//        s map reduce_var_widths_s map onType
//      }
//   
//      val modulesx = c.modules.map{ m => {
//         val portsx = m.ports.map{ p => {
//            Port(p.info,p.name,p.direction,mapr(reduce_var_widths_w _,p.tpe)) }}
//         (m) match {
//            case (m:ExtModule) => ExtModule(m.info,m.name,portsx)
//            case (m:Module) =>
//              mname = m.name
//              Module(m.info,m.name,portsx,m.body map reduce_var_widths_s _) }}}
//      InferTypes.run(Circuit(c.info,modulesx,c.main))
//   }
//   
//   def run (c:Circuit): Circuit = {
//      val v = ArrayBuffer[WGeq]()
//      def constrain(t1: Type, t2: Type) : Unit = (t1, t2) match {
//         case (FixedType(w1, p1), FixedType(w2, p2)) => 
//            v += WGeq(w1,w2)
//            v += WGeq(p1,p2)
//         case (t1: GroundType, t2: GroundType) => v += WGeq(t1.width, t2.width)
//         case (t) => error("No width!")
//      }
//      def get_constraints_t (t1:Type,t2:Type,f:Orientation) : Unit = {
//         (t1,t2) match {
//            case (t1:GroundType,t2:GroundType) => constrain(t1, t2)
//            case (t1:BundleType,t2:BundleType) => {
//               (t1.fields,t2.fields).zipped.foreach{ (f1,f2) => {
//                  get_constraints_t(f1.tpe,f2.tpe,times(f1.flip,f)) }}}
//            case (t1:VectorType,t2:VectorType) => get_constraints_t(t1.tpe,t2.tpe,f) }}
//      def get_constraints_e (e:Expression) : Expression = {
//         (e map (get_constraints_e)) match {
//            case (e:Mux) => {
//               constrain(e.cond.tpe, BoolType)
//               constrain(BoolType, e.cond.tpe)
//               e }
//            case (e) => e }}
//      def get_constraints_declared_type (t: Type): Type = t match {
//         case FixedType(_, p) => 
//            v += WGeq(p,IntWidth(0))
//            t
//         case _ => t map get_constraints_declared_type
//      }
//      def get_constraints (s:Statement) : Statement = {
//         (s map (get_constraints_e) map get_constraints_declared_type) match {
//            case (s:Connect) => {
//               val n = get_size(tpe(s.loc))
//               val ce_loc = create_exps(s.loc)
//               val ce_exp = create_exps(s.expr)
//               for (i <- 0 until n) {
//                  val locx = ce_loc(i)
//                  val expx = ce_exp(i)
//                  get_flip(tpe(s.loc),i,Default) match {
//                     case Default => constrain(locx.tpe, expx.tpe)
//                     case Flip => constrain(expx.tpe,locx.tpe) }}
//               s }
//            case (s:PartialConnect) => {
//               val ls = get_valid_points(tpe(s.loc),tpe(s.expr),Default,Default)
//               for (x <- ls) {
//                  val locx = create_exps(s.loc)(x._1)
//                  val expx = create_exps(s.expr)(x._2)
//                  get_flip(tpe(s.loc),x._1,Default) match {
//                     case Default => constrain(locx.tpe,expx.tpe)
//                     case Flip => constrain(expx.tpe,locx.tpe) }}
//               s }
//            case (s:DefRegister) => {
//               constrain(s.reset.tpe,BoolType)
//               constrain(BoolType,s.reset.tpe)
//               get_constraints_t(s.tpe,tpe(s.init),Default)
//               s }
//            case (s:Conditionally) => {
//               constrain(s.pred.tpe,BoolType)
//               constrain(BoolType,s.pred.tpe)
//               s map (get_constraints) }
//            case (s) => s map (get_constraints) }}
//
//      for (m <- c.modules) {
//         (m) match {
//            case (m:Module) =>
//               mname = m.name
//               get_constraints(m.body)
//               m.ports.foreach(p => get_constraints_declared_type(p.tpe))
//            case (m) => false }}
//      //println("======== ALL CONSTRAINTS ========")
//      //for(x <- v) {println(x)}
//      //println("=================================")
//      val h = solve_constraints(v)
//      //println("======== SOLVED CONSTRAINTS ========")
//      //for(x <- h) {println(x)}
//      //println("====================================")
//      reduce_var_widths(Circuit(c.info,c.modules,c.main),h)
//   }
//}
//
//object PullMuxes extends Pass {
//   def name = "Pull Muxes"
//   def run(c: Circuit): Circuit = {
//     def pull_muxes_e(e: Expression): Expression = {
//       val ex = e map (pull_muxes_e) match {
//         case (e: WSubField) => e.exp match {
//           case (ex: Mux) => Mux(ex.cond,
//              WSubField(ex.tval, e.name, e.tpe, e.gender),
//              WSubField(ex.fval, e.name, e.tpe, e.gender), e.tpe)
//           case (ex: ValidIf) => ValidIf(ex.cond,
//              WSubField(ex.value, e.name, e.tpe, e.gender), e.tpe)
//           case (ex) => e
//         }
//         case (e: WSubIndex) => e.exp match {
//           case (ex: Mux) => Mux(ex.cond,
//              WSubIndex(ex.tval, e.value, e.tpe, e.gender),
//              WSubIndex(ex.fval, e.value, e.tpe, e.gender), e.tpe)
//           case (ex: ValidIf) => ValidIf(ex.cond,
//              WSubIndex(ex.value, e.value, e.tpe, e.gender), e.tpe)
//           case (ex) => e
//         }
//         case (e: WSubAccess) => e.exp match {
//           case (ex: Mux) => Mux(ex.cond,
//              WSubAccess(ex.tval, e.index, e.tpe, e.gender),
//              WSubAccess(ex.fval, e.index, e.tpe, e.gender), e.tpe)
//           case (ex: ValidIf) => ValidIf(ex.cond,
//              WSubAccess(ex.value, e.index, e.tpe, e.gender), e.tpe)
//           case (ex) => e
//         }
//         case (e) => e
//       }
//       ex map (pull_muxes_e)
//     }
//     def pull_muxes(s: Statement): Statement = s map (pull_muxes) map (pull_muxes_e)
//     val modulesx = c.modules.map {
//       case (m:Module) => Module(m.info, m.name, m.ports, pull_muxes(m.body))
//       case (m:ExtModule) => m
//     }
//     Circuit(c.info, modulesx, c.main)
//   }
//}
//
//object ExpandConnects extends Pass {
//  def name = "Expand Connects"
//  def run(c: Circuit): Circuit = {
//    def expand_connects(m: Module): Module = {
//      val genders = collection.mutable.LinkedHashMap[String,Gender]()
//      def expand_s(s: Statement): Statement = {
//        def set_gender(e: Expression): Expression = e map (set_gender) match {
//          case (e: WRef) => WRef(e.name, e.tpe, e.kind, genders(e.name))
//          case (e: WSubField) =>
//            val f = get_field(e.exp.tpe, e.name)
//            val genderx = times(gender(e.exp), f.flip)
//            WSubField(e.exp, e.name, e.tpe, genderx)
//          case (e: WSubIndex) => WSubIndex(e.exp, e.value, e.tpe, gender(e.exp))
//          case (e: WSubAccess) => WSubAccess(e.exp, e.index, e.tpe, gender(e.exp))
//          case (e) => e
//        }
//        s match {
//          case (s: DefWire) => genders(s.name) = BIGENDER; s
//          case (s: DefRegister) => genders(s.name) = BIGENDER; s
//          case (s: WDefInstance) => genders(s.name) = MALE; s
//          case (s: DefMemory) => genders(s.name) = MALE; s
//          case (s: DefNode) => genders(s.name) = MALE; s
//          case (s: IsInvalid) =>
//            val invalids = (create_exps(s.expr) foldLeft Seq[Statement]())(
//               (invalids,  expx) => gender(set_gender(expx)) match {
//                  case BIGENDER => invalids :+ IsInvalid(s.info, expx)
//                  case FEMALE => invalids :+ IsInvalid(s.info, expx)
//                  case _ => invalids
//               }
//            )
//            invalids.size match {
//               case 0 => EmptyStmt
//               case 1 => invalids.head
//               case _ => Block(invalids)
//            }
//          case (s: Connect) =>
//            val locs = create_exps(s.loc)
//            val exps = create_exps(s.expr)
//            Block((locs zip exps).zipWithIndex map {case ((locx, expx), i) =>
//               get_flip(s.loc.tpe, i, Default) match {
//                  case Default => Connect(s.info, locx, expx)
//                  case Flip => Connect(s.info, expx, locx)
//               }
//            })
//          case (s: PartialConnect) =>
//            val ls = get_valid_points(s.loc.tpe, s.expr.tpe, Default, Default)
//            val locs = create_exps(s.loc)
//            val exps = create_exps(s.expr)
//            Block(ls map {case (x, y) =>
//              get_flip(s.loc.tpe, x, Default) match {
//                 case Default => Connect(s.info, locs(x), exps(y))
//                 case Flip => Connect(s.info, exps(y), locs(x))
//              }
//            })
//          case (s) => s map (expand_s)
//        }
//      }
//
//      m.ports.foreach { p => genders(p.name) = to_gender(p.direction) }
//      Module(m.info, m.name, m.ports, expand_s(m.body))
//    }
//
//    val modulesx = c.modules.map {
//       case (m: ExtModule) => m
//       case (m: Module) => expand_connects(m)
//    }
//    Circuit(c.info, modulesx, c.main)
//  }
//}
//
//
//// Replace shr by amount >= arg width with 0 for UInts and MSB for SInts
//// TODO replace UInt with zero-width wire instead
//object Legalize extends Pass {
//  def name = "Legalize"
//  private def legalizeShiftRight(e: DoPrim): Expression = {
//    require(e.op == Shr)
//    val amount = e.consts.head.toInt
//    val width = bitWidth(e.args.head.tpe)
//    lazy val msb = width - 1
//    if (amount >= width) {
//      e.tpe match {
//        case UIntType(_) => zero
//        case SIntType(_) =>
//          val bits = DoPrim(Bits, e.args, Seq(msb, msb), BoolType)
//          DoPrim(AsSInt, Seq(bits), Seq.empty, SIntType(IntWidth(1)))
//        case t => error(s"Unsupported type $t for Primop Shift Right")
//      }
//    } else {
//      e
//    }
//  }
//<<<<<<< HEAD
//  def legalizeConnect(c: Connect): Statement = {
//    val t = tpe(c.loc)
//    val w = long_BANG(t)
//    if (w >= long_BANG(tpe(c.expr))) c
//    else {
//      val newType = t match {
//        case _: UIntType => UIntType(IntWidth(w))
//        case _: SIntType => SIntType(IntWidth(w))
//        case FixedType(width, point) => FixedType(width, point)
//      }
//      Connect(c.info, c.loc, DoPrim(Bits, Seq(c.expr), Seq(w-1, 0), newType))
//    }
//  }
//  def run (c: Circuit): Circuit = {
//    def legalizeE(expr: Expression): Expression = expr map legalizeE match {
//      case prim: DoPrim => prim.op match {
//        case Shr => legalizeShiftRight(prim)
//        case Pad => legalizePad(prim)
//        case Bits => legalizeBits(prim)
//        case _ => prim
//      }
//      case e => e // respect pre-order traversal
//    }
//    def legalizeS (s: Statement): Statement = {
//      val legalizedStmt = s match {
//        case c: Connect => legalizeConnect(c)
//        case _ => s
//      }
//      legalizedStmt map legalizeS map legalizeE
//    }
//    c copy (modules = (c.modules map (_ map legalizeS)))
//  }
//}
//
//object VerilogWrap extends Pass {
//  def name = "Verilog Wrap"
//  def vWrapE(e: Expression): Expression = e map vWrapE match {
//    case e: DoPrim => e.op match {
//      case Tail => e.args.head match {
//        case e0: DoPrim => e0.op match {
//          case Add => DoPrim(Addw, e0.args, Nil, e.tpe)
//          case Sub => DoPrim(Subw, e0.args, Nil, e.tpe)
//          case _ => e
//        }
//        case _ => e
//      }
//      case _ => e
//    }
//    case _ => e
//  }
//  def vWrapS(s: Statement): Statement = {
//    s map vWrapS map vWrapE match {
//      case s: Print => s copy (string = VerilogStringLitHandler.format(s.string))
//      case s => s
//    }
//  }
//
//  def run(c: Circuit): Circuit =
//    c copy (modules = (c.modules map (_ map vWrapS)))
//}
//
//object VerilogRename extends Pass {
//  def name = "Verilog Rename"
//  def verilogRenameN(n: String): String =
//    if (v_keywords(n)) "%s$".format(n) else n
//
//  def verilogRenameE(e: Expression): Expression = e match {
//    case e: WRef => e copy (name = verilogRenameN(e.name))
//    case e => e map verilogRenameE
//  }
//
//  def verilogRenameS(s: Statement): Statement =
//    s map verilogRenameS map verilogRenameE map verilogRenameN
//
//  def verilogRenameP(p: Port): Port =
//    p copy (name = verilogRenameN(p.name))
//
//  def run(c: Circuit): Circuit =
//    c copy (modules = (c.modules map (_ map verilogRenameP map verilogRenameS)))
//}
//
//
//object VerilogPrep extends Pass {
//  def name = "Verilog Prep"
//  type InstAttaches = collection.mutable.HashMap[String, Expression]
//  def run(c: Circuit): Circuit = {
//    def buildS(attaches: InstAttaches)(s: Statement): Statement = s match {
//      case Attach(_, source, exps) => 
//        exps foreach { e => attaches(e.serialize) = source }
//        s
//      case _ => s map buildS(attaches)
//    }
//    def lowerE(e: Expression): Expression = e match {
//      case _: WRef|_: WSubField if (kind(e) == InstanceKind) => 
//        WRef(LowerTypes.loweredName(e), e.tpe, kind(e), gender(e))
//      case _ => e map lowerE
//    }
//    def lowerS(attaches: InstAttaches)(s: Statement): Statement = s match {
//      case WDefInstance(info, name, module, tpe) =>
//        val exps = create_exps(WRef(name, tpe, ExpKind, MALE))
//        val wcon = WDefInstanceConnector(info, name, module, tpe, exps.map( e => e.tpe match {
//          case AnalogType(w) => attaches(e.serialize)
//          case _ => WRef(LowerTypes.loweredName(e), e.tpe, WireKind, MALE)
//        }))
//        val wires = exps.map ( e => e.tpe match {
//          case AnalogType(w) => EmptyStmt
//          case _ => DefWire(info, LowerTypes.loweredName(e), e.tpe)
//        })
//        Block(Seq(wcon) ++ wires)
//      case Attach(info, source, exps) => EmptyStmt
//      case _ => s map lowerS(attaches) map lowerE
//    }
//    def prepModule(m: DefModule): DefModule = {
//      val attaches = new InstAttaches
//      m map buildS(attaches)
//      m map lowerS(attaches)
//    }
//    c copy (modules = (c.modules map prepModule))
//  }
//}
