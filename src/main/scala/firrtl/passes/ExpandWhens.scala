// See LICENSE for license details.

package firrtl.passes

import firrtl._
import firrtl.ir._
import firrtl.Utils._
import firrtl.Mappers._
import firrtl.PrimOps._
import firrtl.WrappedExpression._

import annotation.tailrec
import collection.mutable
import collection.immutable.ListSet

/** Expand Whens
*
* This pass does the following things:
* $ - Remove last connect semantics
* $ - Remove conditional blocks
* $ - Eliminate concept of scoping
* $ - Consolidate attaches
*
* @note Assumes bulk connects and isInvalids have been expanded
* @note Assumes all references are declared
*/
object ExpandWhens extends Pass {
  type NodeMap = mutable.HashMap[MemoizedHash[Expression], String]
  type Netlist = mutable.LinkedHashMap[WrappedExpression, Expression]
  type Simlist = mutable.ArrayBuffer[Statement]
  // Defaults ideally would be immutable.Map but conversion from mutable.LinkedHashMap to mutable.Map is VERY slow
  type Defaults = Seq[mutable.Map[WrappedExpression, Expression]]
  // Coverage is also mutable only to avoid coversion
  type Coverage = mutable.LinkedHashSet[WrappedExpression]

  // ========== Expand When Utilz ==========
  private def getFemaleRefs(n: String, t: Type, g: Gender): Seq[Expression] = {
    def getGender(t: Type, i: Int, g: Gender): Gender = times(g, get_flip(t, i, Default))
    val exps = create_exps(WRef(n, t, ExpKind, g))
    (exps.zipWithIndex foldLeft Seq[Expression]()){
      case (expsx, (exp, j)) => exp.tpe match {
        case AnalogType(w) => expsx
        case _ => getGender(t, j, g) match {
          case (BIGENDER | FEMALE) => expsx :+ exp
          case _ => expsx
        }
      }
    }
  }
  private def expandNetlist(netlist: Netlist) =
    netlist map {
      case (k, WInvalid) => IsInvalid(NoInfo, k.e1)
      case (k, v) => Connect(NoInfo, k.e1, v)
    }
  /** Combines Attaches
    * @todo Preserve Info
    */
  private def combineAttaches(attaches: Seq[Attach]): Seq[Attach] = {
    // Helper type to add an ordering index to attached Expressions
    case class AttachAcc(exprs: Seq[Expression], idx: Int)
    // Map from every attached expression to its corresponding AttachAcc
    //   (many keys will point to same value)
    val attachMap = mutable.HashMap.empty[WrappedExpression, AttachAcc]
    for (Attach(_, exprs) <- attaches) {
      val acc = exprs.map(attachMap.get(_)).flatten match {
        case Seq() => // None of these expressions is present in the attachMap
          AttachAcc(exprs, attachMap.size)
        case accs => // At least one expression present in the attachMap
          val sorted = accs sortBy (_.idx)
          AttachAcc((sorted.map(_.exprs) :+ exprs).flatten.distinct, sorted.head.idx)
      }
      attachMap ++= acc.exprs.map(e => (we(e) -> acc))
    }
    attachMap.values.toList.distinct.map(acc => Attach(NoInfo, acc.exprs))
  }
  // Searches nested scopes of defaults for lvalue
  // defaults uses mutable Map because we are searching LinkedHashMaps and conversion to immutable is VERY slow
  @tailrec
  private def getDefault(lvalue: WrappedExpression, defaults: Defaults): Option[Expression] = {
    defaults match {
      case Nil => None
      case head :: tail => head get lvalue match {
        case Some(p) => Some(p)
        case None => getDefault(lvalue, tail)
      }
    }
  }

  private def AND(e1: Expression, e2: Expression) =
    DoPrim(And, Seq(e1, e2), Nil, BoolType)
  private def NOT(e: Expression) =
    DoPrim(Eq, Seq(e, zero), Nil, BoolType)

  // ------------ Pass -------------------
  def run(c: Circuit): Circuit = {
    def expandWhens(m: Module): (Netlist, Simlist, Seq[Attach], Statement) = {
      val namespace = Namespace(m)
      val simlist = new Simlist
      val nodes = new NodeMap
      // Seq of attaches in order
      lazy val attaches = mutable.ArrayBuffer.empty[Attach]

      def expandWhens(netlist: Netlist,
                      defaults: Defaults,
                      p: Expression,
                      coverage: Coverage)
                      (s: Statement): Statement = s match {
        case w: DefWire =>
          netlist ++= (getFemaleRefs(w.name, w.tpe, BIGENDER) map (ref => we(ref) -> WVoid))
          w
        case w: DefMemory =>
          netlist ++= (getFemaleRefs(w.name, MemPortUtils.memType(w), MALE) map (ref => we(ref) -> WVoid))
          w
        case r: DefRegister =>
          netlist ++= (getFemaleRefs(r.name, r.tpe, BIGENDER) map (ref => we(ref) -> ref))
          r
        case c: Connect =>
          coverage += c.loc
          netlist(c.loc) = c.expr
          EmptyStmt
        case c: IsInvalid =>
          coverage += c.expr
          netlist(c.expr) = WInvalid
          EmptyStmt
        case a: Attach =>
          attaches += a
          EmptyStmt
        case sx: Conditionally =>
          val conseqNetlist = new Netlist
          val altNetlist = new Netlist
          val conseqCoverage = new Coverage
          val altCoverage = new Coverage

          // Produce negated condition node explicitly
          val negStmt = DefNode(sx.info, namespace.newTemp, NOT(sx.pred))
          val negRef = WRef(negStmt.name, p.tpe, NodeKind, MALE)

          val conseqStmt = expandWhens(conseqNetlist, netlist +: defaults, AND(p, sx.pred), conseqCoverage)(sx.conseq)
          val altStmt = expandWhens(altNetlist, netlist +: defaults, AND(p, negRef), altCoverage)(sx.alt)

          val whenCoverage = conseqCoverage.intersect(altCoverage)
          coverage ++= whenCoverage

          // Process combined maps because we only want to create 1 mux for each node
          //   present in the conseq and/or alt
          // val memos = (conseqNetlist ++ altNetlist) map { case (lvalue, _) =>
          def getMemos(subNetlist: Netlist, negate: Boolean = false) = {
            val activePred = if (negate) negRef else sx.pred
            subNetlist map { case (lvalue, _) =>
              // Defaults in netlist get priority over those in defaults
              val default = netlist get lvalue match {
                case Some(v) => Some(v)
                case None => getDefault(lvalue, defaults)
              }
              val res = default match {
                case Some(defaultValue) =>
                  val trueValue = subNetlist getOrElse (lvalue, defaultValue)
                  val falseValue = defaultValue
                  val isCovered = whenCoverage contains lvalue
                    (trueValue, falseValue, isCovered) match {
                    case (WInvalid, WInvalid, _) => WInvalid
                    case (WInvalid, fv, _) => ValidIf(NOT(activePred), fv, fv.tpe)
                    case (tv, WInvalid, _) => ValidIf(activePred, tv, tv.tpe)
                    // special case to eliminate mux with WVoid at top level to indicate coverage exists
                    case (tv, WVoid, true) => ValidIf(activePred, tv, tv.tpe)
                    case (tv, fv, _) => Mux(activePred, tv, fv, mux_type_and_widths(tv, fv))
                  }
                case None =>
                  // MUST be declared in subnetlist
                  subNetlist(lvalue)
              }

              res match {
                case _: ValidIf | _: Mux | _: DoPrim => nodes get res match {
                  case Some(name) =>
                    netlist(lvalue) = WRef(name, res.tpe, NodeKind, MALE)
                    EmptyStmt
                  case None =>
                    val name = namespace.newTemp
                    nodes(res) = name
                    netlist(lvalue) = WRef(name, res.tpe, NodeKind, MALE)
                    DefNode(sx.info, name, res)
                }
                case _ =>
                  netlist(lvalue) = res
                  EmptyStmt
              }
            }
          }

          val conseqMemos = getMemos(conseqNetlist, false)
          val altMemos = getMemos(altNetlist, true)
          Block(Seq(negStmt, conseqStmt, altStmt) ++ conseqMemos ++ altMemos)
        case sx: Print =>
          simlist += (if (weq(p, one)) sx else Print(sx.info, sx.string, sx.args, sx.clk, AND(p, sx.en)))
          EmptyStmt
        case sx: Stop =>
          simlist += (if (weq(p, one)) sx else Stop(sx.info, sx.ret, sx.clk, AND(p, sx.en)))
          EmptyStmt
        case sx => sx map expandWhens(netlist, defaults, p, coverage)
      }
      val netlist = new Netlist
      // Add ports to netlist
      netlist ++= (m.ports flatMap { case Port(_, name, dir, tpe) =>
        getFemaleRefs(name, tpe, to_gender(dir)) map (ref => we(ref) -> WVoid)
      })
      val coverage = new Coverage
      val bodyx = expandWhens(netlist, Seq(netlist), one, coverage)(m.body)
      (netlist, simlist, attaches, bodyx)
    }
    val modulesx = c.modules map {
      case m: ExtModule => m
      case m: Module =>
      val (netlist, simlist, attaches, bodyx) = expandWhens(m)
      val newBody = Block(Seq(squashEmpty(bodyx)) ++ expandNetlist(netlist) ++
                              combineAttaches(attaches) ++ simlist)
      Module(m.info, m.name, m.ports, newBody)
    }
    Circuit(c.info, modulesx, c.main)
  }
}

