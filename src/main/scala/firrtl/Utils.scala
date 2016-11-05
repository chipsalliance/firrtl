// See LICENSE for license details.

package firrtl

import firrtl.ir._
import firrtl.PrimOps._
import firrtl.Mappers._
import firrtl.WrappedExpression._
import firrtl.WrappedType._
import scala.collection.mutable
import scala.collection.mutable.{StringBuilder, ArrayBuffer, LinkedHashMap, HashMap, HashSet}
import java.io.PrintWriter
import logger.LazyLogging

class FIRRTLException(str: String) extends Exception(str)

object Utils extends LazyLogging {
  def throwInternalError =
    error("Internal Error! Please file an issue at https://github.com/ucb-bar/firrtl/issues")
  private[firrtl] def time[R](name: String)(block: => R): R = {
    logger.info(s"Starting $name")
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    logger.info(s"Finished $name")
    val timeMillis = (t1 - t0) / 1000000.0
    logger.info(f"$name took $timeMillis%.1f ms\n")
    result
  }

  /** Removes all [[firrtl.ir.Empty]] statements and condenses
   * [[firrtl.ir.Block]] statements.
    */
  def squashEmpty(s: Statement): Statement = s map squashEmpty match {
    case Block(stmts) =>
      val newStmts = stmts filter (_ != EmptyStmt)
      newStmts.size match {
        case 0 => EmptyStmt
        case 1 => newStmts.head
        case _ => Block(newStmts)
      }
    case sx => sx
  }

  /** Indent the results of [[ir.FirrtlNode.serialize]] */
  def indent(str: String) = str replaceAllLiterally ("\n", "\n  ")
  def serialize(bi: BigInt): String =
    if (bi < BigInt(0)) "\"h" + bi.toString(16).substring(1) + "\""
    else "\"h" + bi.toString(16) + "\""

  implicit def toWrappedExpression (x:Expression): WrappedExpression = new WrappedExpression(x)
  def ceilLog2(x: BigInt): Int = (x-1).bitLength
  def max(a: BigInt, b: BigInt): BigInt = if (a >= b) a else b
  def min(a: BigInt, b: BigInt): BigInt = if (a >= b) b else a
  def pow_minus_one(a: BigInt, b: BigInt): BigInt = a.pow(b.toInt) - 1
  val BoolType = UIntType(IntWidth(1))
  val one  = UIntLiteral(BigInt(1), IntWidth(1))
  val zero = UIntLiteral(BigInt(0), IntWidth(1))
  def uint(i: BigInt): UIntLiteral = UIntLiteral(i, IntWidth(1 max i.bitLength))

  def create_exps(n: String, t: Type): Seq[Expression] =
    create_exps(WRef(n, t, ExpKind, UNKNOWNGENDER))
  def create_exps(e: Expression): Seq[Expression] = e match {
    case ex: Mux =>
      val e1s = create_exps(ex.tval)
      val e2s = create_exps(ex.fval)
      e1s zip e2s map {case (e1, e2) =>
        Mux(ex.cond, e1, e2, mux_type_and_widths(e1, e2))
      }
    case ex: ValidIf => create_exps(ex.value) map (e1 => ValidIf(ex.cond, e1, e1.tpe))
    case ex => ex.tpe match {
      case (_: GroundType) => Seq(ex)
      case (t: BundleType) => (t.fields foldLeft Seq[Expression]())((exps, f) =>
        exps ++ create_exps(WSubField(ex, f.name, f.tpe,times(gender(ex), f.flip))))
      case (t: VectorType) => (0 until t.size foldLeft Seq[Expression]())((exps, i) =>
        exps ++ create_exps(WSubIndex(ex, i, t.tpe,gender(ex))))
    }
  }
   def get_flip(t: Type, i: Int, f: Orientation): Orientation = {
     if (i >= get_size(t)) error("Shouldn't be here")
     t match {
       case (_: GroundType) => f
       case (tx: BundleType) =>
         val (_, flip) = tx.fields.foldLeft(i, None: Option[Orientation]) {
           case ((n, ret), x) if n < get_size(x.tpe) => ret match {
             case None => (n, Some(get_flip(x.tpe, n, times(x.flip, f))))
             case Some(_) => (n, ret)
           }
           case ((n, ret), x) => (n - get_size(x.tpe), ret)
         }
         flip.get
       case (tx: VectorType) =>
         val (_, flip) = (0 until tx.size).foldLeft(i, None: Option[Orientation]) {
           case ((n, ret), x) if n < get_size(tx.tpe) => ret match {
             case None => (n, Some(get_flip(tx.tpe, n, f)))
             case Some(_) => (n, ret)
           }
           case ((n, ret), x) => (n - get_size(tx.tpe), ret)
         }
         flip.get
     }
   }
  
   def get_point (e:Expression) : Int = e match {
     case (e: WRef) => 0
     case (e: WSubField) => e.exp.tpe match {case b: BundleType =>
       (b.fields takeWhile (_.name != e.name) foldLeft 0)(
         (point, f) => point + get_size(f.tpe))
    }
    case (e: WSubIndex) => e.value * get_size(e.tpe)
    case (e: WSubAccess) => get_point(e.exp)
  }

  /** Returns true if t, or any subtype, contains a flipped field
    *
    * @param t type [[firrtl.ir.Type]] to be checked
    * @return if t contains [[firrtl.ir.Flip]]
    */
  def hasFlip(t: Type): Boolean = t match {
    case t: BundleType =>
      (t.fields exists (_.flip == Flip)) ||
      (t.fields exists (f => hasFlip(f.tpe)))
    case t: VectorType => hasFlip(t.tpe)
    case _ => false
  }

//============== TYPES ================
//<<<<<<< HEAD
//   def mux_type (e1:Expression,e2:Expression) : Type = mux_type(tpe(e1),tpe(e2))
//   def mux_type (t1:Type,t2:Type) : Type = {
//      if (wt(t1) == wt(t2)) {
//         (t1,t2) match { 
//            case (t1:UIntType,t2:UIntType) => UIntType(UnknownWidth)
//            case (t1:SIntType,t2:SIntType) => SIntType(UnknownWidth)
//            case (t1:FixedType,t2:FixedType) => FixedType(UnknownWidth, UnknownWidth)
//            case (t1:VectorType,t2:VectorType) => VectorType(mux_type(t1.tpe,t2.tpe),t1.size)
//            case (t1:BundleType,t2:BundleType) => 
//               BundleType((t1.fields,t2.fields).zipped.map((f1,f2) => {
//                  Field(f1.name,f1.flip,mux_type(f1.tpe,f2.tpe))
//               }))
//         }
//      } else UnknownType
//   }
//   def mux_type_and_widths (e1:Expression,e2:Expression) : Type = mux_type_and_widths(tpe(e1),tpe(e2))
//   def PLUS (w1:Width,w2:Width) : Width = (w1, w2) match {
//     case (IntWidth(i), IntWidth(j)) => IntWidth(i + j)
//     case _ => PlusWidth(w1,w2)
//   }
//   def MAX (w1:Width,w2:Width) : Width = (w1, w2) match {
//     case (IntWidth(i), IntWidth(j)) => IntWidth(max(i,j))
//     case _ => MaxWidth(Seq(w1,w2))
//   }
//   def MINUS (w1:Width,w2:Width) : Width = (w1, w2) match {
//     case (IntWidth(i), IntWidth(j)) => IntWidth(i - j)
//     case _ => MinusWidth(w1,w2)
//   }
//   def POW (w1:Width) : Width = w1 match {
//     case IntWidth(i) => IntWidth(pow_minus_one(BigInt(2), i))
//     case _ => ExpWidth(w1)
//   }
//   def MIN (w1:Width,w2:Width) : Width = (w1, w2) match {
//     case (IntWidth(i), IntWidth(j)) => IntWidth(min(i,j))
//     case _ => MinWidth(Seq(w1,w2))
//   }
//   def mux_type_and_widths (t1:Type,t2:Type) : Type = {
//      def wmax (w1:Width,w2:Width) : Width = {
//         (w1,w2) match {
//            case (w1:IntWidth,w2:IntWidth) => IntWidth(w1.width.max(w2.width))
//            case (w1,w2) => MaxWidth(Seq(w1,w2))
//         }
//      }
//      val wt1 = new WrappedType(t1)
//      val wt2 = new WrappedType(t2)
//      if (wt1 == wt2) {
//         (t1,t2) match {
//            case (t1:UIntType,t2:UIntType) => UIntType(wmax(t1.width,t2.width))
//            case (t1:SIntType,t2:SIntType) => SIntType(wmax(t1.width,t2.width))
//            case (FixedType(w1, p1), FixedType(w2, p2)) =>
//              FixedType(PLUS(MAX(p1, p2),MAX(MINUS(w1, p1), MINUS(w2, p2))), MAX(p1, p2))
//            case (t1:VectorType,t2:VectorType) => VectorType(mux_type_and_widths(t1.tpe,t2.tpe),t1.size)
//            case (t1:BundleType,t2:BundleType) => BundleType((t1.fields zip t2.fields).map{case (f1, f2) => Field(f1.name,f1.flip,mux_type_and_widths(f1.tpe,f2.tpe))})
//         }
//      } else UnknownType
//   }
//   def module_type (m:DefModule) : Type = {
//      BundleType(m.ports.map(p => p.toField))
//   }
//   def sub_type (v:Type) : Type = {
//      v match {
//         case v:VectorType => v.tpe
//         case v => UnknownType
//      }
//   }
//   def field_type (v:Type,s:String) : Type = {
//      v match {
//         case v:BundleType => {
//            val ft = v.fields.find(p => p.name == s)
//            ft match {
//               case ft:Some[Field] => ft.get.tpe
//               case ft => UnknownType
//            }
//         }
//         case v => UnknownType
//      }
//   }
//=======
  def mux_type(e1: Expression, e2: Expression): Type = mux_type(e1.tpe, e2.tpe)
  def mux_type(t1: Type, t2: Type): Type = (t1, t2) match {
    case (t1: UIntType, t2: UIntType) => UIntType(UnknownWidth)
    case (t1: SIntType, t2: SIntType) => SIntType(UnknownWidth)
    case (t1: FixedType, t2: FixedType) => FixedType(UnknownWidth, UnknownWidth)
    case (t1: VectorType, t2: VectorType) => VectorType(mux_type(t1.tpe, t2.tpe), t1.size)
    case (t1: BundleType, t2: BundleType) => BundleType(t1.fields zip t2.fields map {
      case (f1, f2) => Field(f1.name, f1.flip, mux_type(f1.tpe, f2.tpe))
    })
    case _ => UnknownType
  }
  def mux_type_and_widths(e1: Expression,e2: Expression): Type =
    mux_type_and_widths(e1.tpe, e2.tpe)
  def mux_type_and_widths(t1: Type, t2: Type): Type = {
    def wmax(w1: Width, w2: Width): Width = (w1, w2) match {
      case (w1x: IntWidth, w2x: IntWidth) => IntWidth(w1x.width max w2x.width)
      case (w1x, w2x) => MaxWidth(Seq(w1x, w2x))
    }
    (t1, t2) match {
      case (t1x: UIntType, t2x: UIntType) => UIntType(wmax(t1x.width, t2x.width))
      case (t1x: SIntType, t2x: SIntType) => SIntType(wmax(t1x.width, t2x.width))
      case (FixedType(w1, p1), FixedType(w2, p2)) =>
        FixedType(PLUS(MAX(p1, p2),MAX(MINUS(w1, p1), MINUS(w2, p2))), MAX(p1, p2))
      case (t1x: VectorType, t2x: VectorType) => VectorType(
        mux_type_and_widths(t1x.tpe, t2x.tpe), t1x.size)
      case (t1x: BundleType, t2x: BundleType) => BundleType(t1x.fields zip t2x.fields map {
        case (f1, f2) => Field(f1.name, f1.flip, mux_type_and_widths(f1.tpe, f2.tpe))
      })
      case _ => UnknownType
    }
  }

  def module_type(m: DefModule): Type = BundleType(m.ports map {
    case Port(_, name, dir, tpe) => Field(name, to_flip(dir), tpe)
  })
  def sub_type(v: Type): Type = v match {
    case vx: VectorType => vx.tpe
    case vx => UnknownType
  }
  def field_type(v: Type, s: String) : Type = v match {
    case vx: BundleType => vx.fields find (_.name == s) match {
      case Some(f) => f.tpe
      case None => UnknownType
    }
    case vx => UnknownType
  }
//>>>>>>> e54fb610c6bf0a7fe5c9c0f0e0b3acbb3728cfd0
   
// =================================
  def error(str: String) = throw new FIRRTLException(str)

//// =============== EXPANSION FUNCTIONS ================
  def get_size(t: Type): Int = t match {
    case tx: BundleType => (tx.fields foldLeft 0)(
      (sum, f) => sum + get_size(f.tpe))
    case tx: VectorType => tx.size * get_size(tx.tpe)
    case tx => 1
  }

  def get_valid_points(t1: Type, t2: Type, flip1: Orientation, flip2: Orientation): Seq[(Int,Int)] = {
    //;println_all(["Inside with t1:" t1 ",t2:" t2 ",f1:" flip1 ",f2:" flip2])
    (t1, t2) match {
      case (_: UIntType, _: UIntType) => if (flip1 == flip2) Seq((0, 0)) else Nil
      case (_: SIntType, _: SIntType) => if (flip1 == flip2) Seq((0, 0)) else Nil
      case (_: FixedType, _: FixedType) => if (flip1 == flip2) Seq((0, 0)) else Nil
      case (t1x: BundleType, t2x: BundleType) =>
        def emptyMap = Map[String, (Type, Orientation, Int)]()
        val t1_fields = t1x.fields.foldLeft(emptyMap, 0) { case ((map, ilen), f1) =>
          (map + (f1.name ->(f1.tpe, f1.flip, ilen)), ilen + get_size(f1.tpe))
        }._1
        t2x.fields.foldLeft(Seq[(Int, Int)](), 0) { case ((points, jlen), f2) =>
          t1_fields get f2.name match {
            case None => (points, jlen + get_size(f2.tpe))
            case Some((f1_tpe, f1_flip, ilen)) =>
              val f1_times = times(flip1, f1_flip)
              val f2_times = times(flip2, f2.flip)
              val ls = get_valid_points(f1_tpe, f2.tpe, f1_times, f2_times)
              (points ++ (ls map { case (x, y) => (x + ilen, y + jlen) }), jlen + get_size(f2.tpe))
          }
        }._1
      case (t1x: VectorType, t2x: VectorType) =>
        val size = math.min(t1x.size, t2x.size)
        (0 until size).foldLeft(Seq[(Int, Int)](), 0, 0) { case ((points, ilen, jlen), _) =>
          val ls = get_valid_points(t1x.tpe, t2x.tpe, flip1, flip2)
          (points ++ (ls map { case (x, y) => (x + ilen, y + jlen) }),
            ilen + get_size(t1x.tpe), jlen + get_size(t2x.tpe))
        }._1
      case (ClockType, ClockType) => if (flip1 == flip2) Seq((0, 0)) else Nil
      case (AnalogType(w1), AnalogType(w2)) => Nil
      case _ => error("shouldn't be here")
    }
  }

// =========== GENDER/FLIP UTILS ============
  def swap(g: Gender) : Gender = g match {
    case UNKNOWNGENDER => UNKNOWNGENDER
    case MALE => FEMALE
    case FEMALE => MALE
    case BIGENDER => BIGENDER
  }
  def swap(d: Direction) : Direction = d match {
    case Output => Input
    case Input => Output
  }
  def swap(f: Orientation) : Orientation = f match {
    case Default => Flip
    case Flip => Default
  }
  def to_dir(g: Gender): Direction = g match {
    case MALE => Input
    case FEMALE => Output
  }
  def to_gender(d: Direction): Gender = d match {
    case Input => MALE
    case Output => FEMALE
  }
  def to_flip(d: Direction): Orientation = d match {
    case Input => Flip
    case Output => Default
  }
  def to_flip(g: Gender): Orientation = g match {
    case MALE => Flip
    case FEMALE => Default
  }

  def field_flip(v: Type, s: String): Orientation = v match {
    case vx: BundleType => vx.fields find (_.name == s) match {
      case Some(ft) => ft.flip
      case None => Default
    }
    case vx => Default
  }
  def get_field(v: Type, s: String): Field = v match {
    case vx: BundleType => vx.fields find (_.name == s) match {
      case Some(ft) => ft
      case None => error("Shouldn't be here")
    }
    case vx => error("Shouldn't be here")
  }

  def times(flip: Orientation, d: Direction): Direction = times(flip, d)
  def times(d: Direction,flip: Orientation): Direction = flip match {
    case Default => d
    case Flip => swap(d)
  }
  def times(g: Gender, d: Direction): Direction = times(d, g)
  def times(d: Direction, g: Gender): Direction = g match {
    case FEMALE => d
    case MALE => swap(d) // MALE == INPUT == REVERSE
  }

  def times(g: Gender, flip: Orientation): Gender = times(flip, g)
  def times(flip: Orientation, g: Gender): Gender = flip match {
    case Default => g
    case Flip => swap(g)
  }
  def times(f1: Orientation, f2: Orientation): Orientation = f2 match {
    case Default => f1
    case Flip => swap(f1)
  }

// =========== ACCESSORS =========
  def kind(e: Expression): Kind = e match {
    case ex: WRef => ex.kind
    case ex: WSubField => kind(ex.exp)
    case ex: WSubIndex => kind(ex.exp)
    case ex: WSubAccess => kind(ex.exp)
    case ex => ExpKind
  }
  def gender(e: Expression): Gender = e match {
    case ex: WRef => ex.gender
    case ex: WSubField => ex.gender
    case ex: WSubIndex => ex.gender
    case ex: WSubAccess => ex.gender
    case ex: DoPrim => MALE
    case ex: UIntLiteral => MALE
    case ex: SIntLiteral => MALE
    case ex: Mux => MALE
    case ex: ValidIf => MALE
    case WInvalid => MALE
    case ex => println(ex); error("Shouldn't be here")
  }
  def get_gender(s: Statement): Gender = s match {
    case sx: DefWire => BIGENDER
    case sx: DefRegister => BIGENDER
    case sx: WDefInstance => MALE
    case sx: DefNode => MALE
    case sx: DefInstance => MALE
    case sx: DefMemory => MALE
    case sx: Block => UNKNOWNGENDER
    case sx: Connect => UNKNOWNGENDER
    case sx: PartialConnect => UNKNOWNGENDER
    case sx: Stop => UNKNOWNGENDER
    case sx: Print => UNKNOWNGENDER
    case sx: IsInvalid => UNKNOWNGENDER
    case EmptyStmt => UNKNOWNGENDER
  }
  def get_gender(p: Port): Gender = if (p.direction == Input) MALE else FEMALE
  def get_info(s: Statement): Info = s match {
    case s: HasInfo => s.info
    case _ => NoInfo
  }

  /** Splits an Expression into root Ref and tail
    *
    * @example
    *   Given:   SubField(SubIndex(SubField(Ref("a", UIntType(IntWidth(32))), "b"), 2), "c")
    *   Returns: (Ref("a"), SubField(SubIndex(Ref("b"), 2), "c"))
    *   a.b[2].c -> (a, b[2].c)
    * @example
    *   Given:   SubField(SubIndex(Ref("b"), 2), "c")
    *   Returns: (Ref("b"), SubField(SubIndex(EmptyExpression, 2), "c"))
    *   b[2].c -> (b, EMPTY[2].c)
    * @note This function only supports WRef, WSubField, and WSubIndex
    */
  def splitRef(e: Expression): (WRef, Expression) = e match {
    case e: WRef => (e, EmptyExpression)
    case e: WSubIndex =>
      val (root, tail) = splitRef(e.exp)
      (root, WSubIndex(tail, e.value, e.tpe, e.gender))
    case e: WSubField =>
      val (root, tail) = splitRef(e.exp)
      tail match {
        case EmptyExpression => (root, WRef(e.name, e.tpe, root.kind, e.gender))
        case exp => (root, WSubField(tail, e.name, e.tpe, e.gender))
      }
  }

  /** Adds a root reference to some SubField/SubIndex chain */
  def mergeRef(root: WRef, body: Expression): Expression = body match {
    case e: WRef =>
      WSubField(root, e.name, e.tpe, e.gender)
    case e: WSubIndex =>
      WSubIndex(mergeRef(root, e.exp), e.value, e.tpe, e.gender)
    case e: WSubField =>
      WSubField(mergeRef(root, e.exp), e.name, e.tpe, e.gender)
    case EmptyExpression => root
  }

  case class DeclarationNotFoundException(msg: String) extends FIRRTLException(msg)

  /** Gets the root declaration of an expression
    *
    * @param m    the [[firrtl.ir.Module]] to search
    * @param expr the [[firrtl.ir.Expression]] that refers to some declaration
    * @return the [[firrtl.ir.IsDeclaration]] of `expr`
    * @throws DeclarationNotFoundException if no declaration of `expr` is found
    */
  def getDeclaration(m: Module, expr: Expression): IsDeclaration = {
    def getRootDecl(name: String)(s: Statement): Option[IsDeclaration] = s match {
      case decl: IsDeclaration => if (decl.name == name) Some(decl) else None
      case c: Conditionally =>
        val m = (getRootDecl(name)(c.conseq), getRootDecl(name)(c.alt))
        (m: @unchecked) match {
          case (Some(decl), None) => Some(decl)
          case (None, Some(decl)) => Some(decl)
          case (None, None) => None
        }
      case begin: Block =>
        val stmts = begin.stmts flatMap getRootDecl(name) // can we short circuit?
        if (stmts.nonEmpty) Some(stmts.head) else None
      case _ => None
    }
    expr match {
      case (_: WRef | _: WSubIndex | _: WSubField) =>
        val (root, tail) = splitRef(expr)
        val rootDecl = m.ports find (_.name == root.name) match {
          case Some(decl) => decl
          case None =>
            getRootDecl(root.name)(m.body) match {
              case Some(decl) => decl
              case None => throw new DeclarationNotFoundException(
                s"[module ${m.name}]  Reference ${expr.serialize} not declared!")
            }
        }
        rootDecl
      case e => error(s"getDeclaration does not support Expressions of type ${e.getClass}")
    }
  }

  val v_keywords = Set(
    "alias", "always", "always_comb", "always_ff", "always_latch",
    "and", "assert", "assign", "assume", "attribute", "automatic",

    "before", "begin", "bind", "bins", "binsof", "bit", "break",
    "buf", "bufif0", "bufif1", "byte",

    "case", "casex", "casez", "cell", "chandle", "class", "clocking",
    "cmos", "config", "const", "constraint", "context", "continue",
    "cover", "covergroup", "coverpoint", "cross",

    "deassign", "default", "defparam", "design", "disable", "dist", "do",

    "edge", "else", "end", "endattribute", "endcase", "endclass",
    "endclocking", "endconfig", "endfunction", "endgenerate",
    "endgroup", "endinterface", "endmodule", "endpackage",
    "endprimitive", "endprogram", "endproperty", "endspecify",
    "endsequence", "endtable", "endtask",
    "enum", "event", "expect", "export", "extends", "extern",

    "final", "first_match", "for", "force", "foreach", "forever",
    "fork", "forkjoin", "function",
    "generate", "genvar",
    "highz0", "highz1",
    "if", "iff", "ifnone", "ignore_bins", "illegal_bins", "import",
    "incdir", "include", "initial", "initvar", "inout", "input",
    "inside", "instance", "int", "integer", "interconnect",
    "interface", "intersect",

    "join", "join_any", "join_none", "large", "liblist", "library",
    "local", "localparam", "logic", "longint",

    "macromodule", "matches", "medium", "modport", "module",

    "nand", "negedge", "new", "nmos", "nor", "noshowcancelled",
    "not", "notif0", "notif1", "null",

    "or", "output",

    "package", "packed", "parameter", "pmos", "posedge",
    "primitive", "priority", "program", "property", "protected",
    "pull0", "pull1", "pulldown", "pullup",
    "pulsestyle_onevent", "pulsestyle_ondetect", "pure",

    "rand", "randc", "randcase", "randsequence", "rcmos",
    "real", "realtime", "ref", "reg", "release", "repeat",
    "return", "rnmos", "rpmos", "rtran", "rtranif0", "rtranif1",

    "scalared", "sequence", "shortint", "shortreal", "showcancelled",
    "signed", "small", "solve", "specify", "specparam", "static",
    "strength", "string", "strong0", "strong1", "struct", "super",
    "supply0", "supply1",

    "table", "tagged", "task", "this", "throughout", "time", "timeprecision",
    "timeunit", "tran", "tranif0", "tranif1", "tri", "tri0", "tri1", "triand",
    "trior", "trireg", "type","typedef",

    "union", "unique", "unsigned", "use",

    "var", "vectored", "virtual", "void",

    "wait", "wait_order", "wand", "weak0", "weak1", "while",
    "wildcard", "wire", "with", "within", "wor",

    "xnor", "xor",

    "SYNTHESIS",
    "PRINTF_COND",
    "VCS")
}

object MemoizedHash {
  implicit def convertTo[T](e: T): MemoizedHash[T] = new MemoizedHash(e)
  implicit def convertFrom[T](f: MemoizedHash[T]): T = f.t
}

class MemoizedHash[T](val t: T) {
  override lazy val hashCode = t.hashCode
  override def equals(that: Any) = that match {
    case x: MemoizedHash[_] => t equals x.t
    case _ => false
  }
}

/**
  * Maintains a one to many graph of each modules instantiated child module.
  * This graph can be searched for a path from a child module back to one of
  * it's parents.  If one is found a recursive loop has happened
  * The graph is a map between the name of a node to set of names of that nodes children
  */
class ModuleGraph {
  val nodes = mutable.HashMap[String, mutable.HashSet[String]]()

  /**
    * Add a child to a parent node
    * A parent node is created if it does not already exist
    *
    * @param parent module that instantiates another module
    * @param child  module instantiated by parent
    * @return a list indicating a path from child to parent, empty if no such path
    */
  def add(parent: String, child: String): List[String] = {
    val childSet = nodes.getOrElseUpdate(parent, new mutable.HashSet[String])
    childSet += child
    pathExists(child, parent, List(child, parent))
  }

  /**
    * Starting at the name of a given child explore the tree of all children in depth first manner.
    * Return the first path (a list of strings) that goes from child to parent,
    * or an empty list of no such path is found.
    *
    * @param child  starting name
    * @param parent name to find in children (recursively)
    * @param path   path being investigated as possible route
    * @return
    */
  def pathExists(child: String, parent: String, path: List[String] = Nil): List[String] = {
    nodes.get(child) match {
      case Some(children) =>
        if(children(parent)) {
          parent :: path
        }
        else {
          children.foreach { grandchild =>
            val newPath = pathExists(grandchild, parent, grandchild :: path)
            if(newPath.nonEmpty) {
              return newPath
            }
          }
          Nil
        }
      case _ => Nil
    }
  }
}
