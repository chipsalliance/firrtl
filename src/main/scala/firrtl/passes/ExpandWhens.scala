// SPDX-License-Identifier: Apache-2.0

package firrtl.passes

import firrtl._
import firrtl.ir._
import firrtl.Utils._
import firrtl.Mappers._
import firrtl.PrimOps._
import firrtl.WrappedExpression._
import firrtl.options.Dependency
import firrtl.InfoExpr.unwrap

import annotation.tailrec
import collection.mutable

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

  override def prerequisites =
    Seq(
      Dependency(PullMuxes),
      Dependency(ReplaceAccesses),
      Dependency(ExpandConnects),
      Dependency(RemoveAccesses)
    ) ++ firrtl.stage.Forms.Resolved

  override def invalidates(a: Transform): Boolean = a match {
    case CheckInitialization | ResolveKinds | InferTypes => true
    case _                                               => false
  }

  /** Returns circuit with when and last connection semantics resolved */
  def run(c: Circuit): Circuit = {
    val modulesx = c.modules.map {
      case m: ExtModule => m
      case m: Module    => onModule(m)
    }
    Circuit(c.info, modulesx, c.main)
  }

  /** Maps an expression to a declared node name. Used to memoize predicates */
  @deprecated("This will be removed in FIRRTL 1.4.0", "FIRRTL 1.3.2")
  type NodeMap = mutable.HashMap[MemoizedHash[Expression], String]

  private type NodeLookup = mutable.HashMap[WrappedExpression, String]

  /** Maps a reference to whatever connects to it. Used to resolve last connect semantics */
  type Netlist = mutable.LinkedHashMap[WrappedExpression, Expression]

  /** Contains all simulation constructs */
  type Simlist = mutable.ArrayBuffer[Statement]

  /** List of all netlists of each declared scope, ordered from closest to farthest
    * @note Note immutable.Map because conversion from mutable.LinkedHashMap to mutable.Map is VERY slow
    */
  type Defaults = Seq[mutable.Map[WrappedExpression, Expression]]

  private def getVoid(tpe: ir.Type): WIntVoid = WIntVoid(bitWidth(tpe))
  private def doConnect(
    netlist:  Netlist,
    defaults: Defaults,
    lhs:      ir.Expression,
    rhs:      ir.Expression,
    info:     Info
  ): Unit =
    lhs match {
      case ref: ir.RefLikeExpression =>
        netlist(ref) = InfoExpr(info, rhs)
      case otherLhs =>
        // normalize nested bits expressions before check
        simplifyBits(otherLhs) match {
          case ir.DoPrim(PrimOps.Bits, Seq(ref: ir.RefLikeExpression), Seq(hi, lo), _) =>
            val refWidth = bitWidth(ref.tpe)
            val highest = refWidth - 1
            assert(hi < refWidth && lo >= 0)
            // if we are assigning the whole range, things are simple
            if (hi == highest && lo == 0) {
              netlist(ref) = InfoExpr(info, rhs)
            } else {
              // perform a read modify write
              val (prevInfo, prev) = unwrap(
                netlist.getOrElse(ref, getDefault(ref, defaults).getOrElse(WIntVoid(refWidth)))
              )
              val msb = if (hi == highest) { simplifyBits(rhs) }
              else {
                Utils.cat(simplifyBits(prev, highest, hi + 1), simplifyBits(rhs))
              }
              val full = if (lo == 0) { msb }
              else {
                Utils.cat(msb, simplifyBits(prev, lo - 1, 0))
              }
              netlist(ref) = InfoExpr(ir.MultiInfo(Seq(prevInfo, info)), full)
            }
          case other =>
            throw new PassException(s"Invalid expression at the left hand side of an assignment: ${other.serialize}")
        }
    }
  private def simplifyBits(e: ir.Expression, hi: BigInt, lo: BigInt): ir.Expression =
    simplifyBits(Utils.bits(e, hi, lo))

  /** performs special simplifications which are needed in the case of sub-word assignments to avoid false positives
    *  in the connection check
    */
  private def simplifyBits(e: ir.Expression): ir.Expression = e match {
    case ir.DoPrim(PrimOps.Bits, Seq(expr), Seq(hi, lo), _) =>
      expr match {
        // combine bits of bits
        case ir.DoPrim(PrimOps.Bits, Seq(innerExpr), Seq(_, innerLo), _) =>
          simplifyBits(innerExpr, hi + innerLo, lo + innerLo)
        // push bits into mux
        case ir.Mux(cond, tval, fval, tpe) =>
          val tru = simplifyBits(tval, hi, lo)
          val fals = simplifyBits(fval, hi, lo)
          assert(tru.tpe == fals.tpe)
          ir.Mux(cond, tru, fals, tru.tpe)
        // push bits into concat
        case ir.DoPrim(PrimOps.Cat, Seq(msb, lsb), _, _) =>
          val lsbWidth = bitWidth(lsb.tpe)
          if (lsbWidth > hi) {
            simplifyBits(lsb, hi, lo)
          } else if (lo >= lsbWidth) {
            simplifyBits(msb, hi - lsbWidth, lo - lsbWidth)
          } else {
            Utils.cat(
              simplifyBits(msb, hi - lsbWidth, 0),
              simplifyBits(lsb, lsbWidth - 1, lo)
            )
          }
        // reduce size of void
        case _: WIntVoid    => WIntVoid(hi - lo + 1)
        case _: WIntInvalid => simplifyBits(WIntInvalid(hi - lo + 1))
        case _ => e // nothing to simplify
      }
    // we replace any invalids with zero because the ValidIf construct does not work well for subword assignments
    case WIntInvalid(width) => Utils.getGroundZero(UIntType(IntWidth(width)))
    case _                  => e // nothing to simplify
  }

  /** combines assignments from two branches */
  private def combineBranches(
    pred:       Expression,
    trueValue:  Expression,
    falseValue: Expression,
    tinfo:      Info,
    finfo:      Info
  ): (Expression, Info, Info) = {
    (trueValue, falseValue) match {
      case (i: WIntInvalid, _: WIntInvalid) => (i, NoInfo, NoInfo)
      case (_: WIntInvalid, fv) => (ValidIf(NOT(pred), fv, fv.tpe), finfo, NoInfo)
      case (tv, _: WIntInvalid) => (ValidIf(pred, tv, tv.tpe), tinfo, NoInfo)
      case (tv, fv) => (Mux(pred, tv, fv, mux_type_and_widths(tv, fv)), tinfo, finfo)
    }
  }

  /** Expands a module's when statements */
  private def onModule(m: Module): Module = {
    val namespace = Namespace(m)
    val simlist = new Simlist

    // Memoizes if an expression contains any WVoids inserted in this pass
    val memoizedVoid = new mutable.HashSet[WrappedExpression]

    // Does an expression contain WVoid inserted in this pass?
    def containsVoid(e: Expression): Boolean = e match {
      case _: WIntVoid => true
      case ValidIf(_, _: WIntVoid, _) => true
      case ValidIf(_, value, _) => memoizedVoid(value)
      case Mux(_, _: WIntVoid, _, _) => true
      case Mux(_, _, _: WIntVoid, _) => true
      case Mux(_, tv, fv, _) => memoizedVoid(tv) || memoizedVoid(fv)
      case _                 => false
    }

    // Memoizes the node that holds a particular expression, if any
    val nodes = new NodeLookup

    // Seq of attaches in order
    lazy val attaches = mutable.ArrayBuffer.empty[Attach]

    /* Removes connections/attaches from the statement
     * Mutates namespace, simlist, nodes, attaches
     * Mutates input netlist
     * @param netlist maps references to their values for a given immediate scope
     * @param defaults sequence of netlists of surrouding scopes, ordered closest to farthest
     * @param p predicate so far, used to update simulation constructs
     * @param s statement to expand
     */
    def expandWhens(netlist: Netlist, defaults: Defaults, p: Expression)(s: Statement): Statement = s match {
      // For each non-register declaration, update netlist with value WVoid for each sink reference
      // Return self, unchanged
      case stmt @ (_: DefNode | EmptyStmt) => stmt
      case w: DefWire =>
        netlist ++= (getSinkRefs(w.name, w.tpe, DuplexFlow).map(ref => we(ref) -> getVoid(ref.tpe)))
        w
      case w: DefMemory =>
        netlist ++= (getSinkRefs(w.name, MemPortUtils.memType(w), SourceFlow).map(ref => we(ref) -> getVoid(ref.tpe)))
        w
      case w: WDefInstance =>
        netlist ++= (getSinkRefs(w.name, w.tpe, SourceFlow).map(ref => we(ref) -> getVoid(ref.tpe)))
        w
      case r: DefRegister =>
        // Update netlist with self reference for each sink reference
        netlist ++= getSinkRefs(r.name, r.tpe, DuplexFlow).map(ref => we(ref) -> InfoExpr(r.info, ref))
        r
      // For value assignments, update netlist/attaches and return EmptyStmt
      case c: Connect =>
        doConnect(netlist, defaults, c.loc, c.expr, c.info)
        EmptyStmt
      case c: IsInvalid =>
        doConnect(netlist, defaults, c.expr, WIntInvalid(bitWidth(c.expr.tpe)), c.info)
        EmptyStmt
      case a: Attach =>
        attaches += a
        EmptyStmt
      // For simulation constructs, update simlist with predicated statement and return EmptyStmt
      case sx: Print =>
        simlist += (if (weq(p, one)) sx else sx.withEn(AND(p, sx.en)))
        EmptyStmt
      case sx: Stop =>
        simlist += (if (weq(p, one)) sx else sx.withEn(AND(p, sx.en)))
        EmptyStmt
      case sx: Verification =>
        simlist += (if (weq(p, one)) sx else sx.withEn(AND(p, sx.en)))
        EmptyStmt
      // Expand conditionally, see comments below
      case sx: Conditionally =>
        /* 1) Recurse into conseq and alt with empty netlist, updated defaults, updated predicate
         * 2) For each assigned reference (lvalue) in either conseq or alt, get merged value
         *   a) Find default value from defaults
         *   b) Create Mux, ValidIf or WInvalid, depending which (or both) conseq/alt assigned lvalue
         * 3) If a merged value has been memoized, update netlist. Otherwise, memoize then update netlist.
         * 4) Return conseq and alt declarations, followed by memoized nodes
         */
        val conseqNetlist = new Netlist
        val altNetlist = new Netlist
        val conseqStmt = expandWhens(conseqNetlist, netlist +: defaults, AND(p, sx.pred))(sx.conseq)
        val altStmt = expandWhens(altNetlist, netlist +: defaults, AND(p, NOT(sx.pred)))(sx.alt)

        // Process combined maps because we only want to create 1 mux for each node
        //   present in the conseq and/or alt
        val memos = (conseqNetlist ++ altNetlist).map {
          case (lvalue, _) =>
            // Defaults in netlist get priority over those in defaults
            val default = netlist.get(lvalue) match {
              case Some(v) => Some(v)
              case None    => getDefault(lvalue, defaults)
            }
            // info0 and info1 correspond to Mux infos, use info0 only if ValidIf
            val (res, info0, info1) = default match {
              case Some(defaultValue) =>
                val (tinfo, trueValue) = unwrap(conseqNetlist.getOrElse(lvalue, defaultValue))
                val (finfo, falseValue) = unwrap(altNetlist.getOrElse(lvalue, defaultValue))
                combineBranches(sx.pred, trueValue, falseValue, tinfo, finfo)
              case None =>
                // Since not in netlist, lvalue must be declared in EXACTLY one of conseq or alt
                (conseqNetlist.getOrElse(lvalue, altNetlist(lvalue)), NoInfo, NoInfo)
            }

            res match {
              // Don't create a node to hold mux trees with void values
              // "Idiomatic" emission of these muxes isn't a concern because they represent bad code (latches)
              case e if containsVoid(e) =>
                netlist(lvalue) = e
                memoizedVoid += e // remember that this was void
                EmptyStmt
              case _: ValidIf | _: Mux | _: DoPrim =>
                nodes.get(res) match {
                  case Some(name) =>
                    netlist(lvalue) = WRef(name, res.tpe, NodeKind, SourceFlow)
                    EmptyStmt
                  case None =>
                    val name = namespace.newTemp
                    nodes(res) = name
                    netlist(lvalue) = WRef(name, res.tpe, NodeKind, SourceFlow)
                    // Use MultiInfo constructor to preserve NoInfos
                    val info = new MultiInfo(List(sx.info, info0, info1))
                    DefNode(info, name, res)
                }
              case _ =>
                netlist(lvalue) = res
                EmptyStmt
            }
        }
        Block(Seq(conseqStmt, altStmt) ++ memos)
      case block: Block => block.map(expandWhens(netlist, defaults, p))
      case _ => throwInternalError()
    }
    val netlist = new Netlist
    // Add ports to netlist
    netlist ++= (m.ports.flatMap {
      case Port(_, name, dir, tpe) =>
        getSinkRefs(name, tpe, to_flow(dir)).map(ref => we(ref) -> getVoid(ref.tpe))
    })
    // Do traversal and construct mutable datastructures
    val bodyx = expandWhens(netlist, Seq(netlist), one)(m.body)

    val attachedAnalogs = attaches.flatMap(_.exprs.map(we)).toSet
    val newBody = Block(
      Seq(squashEmpty(bodyx)) ++ expandNetlist(netlist, attachedAnalogs) ++
        combineAttaches(attaches.toSeq) ++ simlist
    )
    Module(m.info, m.name, m.ports, newBody)
  }

  /** Returns all references to all sink leaf subcomponents of a reference */
  private def getSinkRefs(n: String, t: Type, g: Flow): Seq[Expression] = {
    val exps = create_exps(WRef(n, t, ExpKind, g))
    exps.flatMap {
      case exp =>
        exp.tpe match {
          case AnalogType(w) => None
          case _ =>
            flow(exp) match {
              case (DuplexFlow | SinkFlow) => Some(exp)
              case _                       => None
            }
        }
    }
  }

  /** Returns all connections/invalidations in the circuit
    * @note Remove IsInvalids on attached Analog-typed components
    */
  private def expandNetlist(netlist: Netlist, attached: Set[WrappedExpression]) = {
    // Remove IsInvalids on attached Analog types
    def handleInvalid(k: WrappedExpression, info: Info): Statement =
      if (attached.contains(k)) EmptyStmt else IsInvalid(info, k.e1)
    netlist.map {
      case (k, _: WIntInvalid) => handleInvalid(k, NoInfo)
      case (k, InfoExpr(info, _: WIntInvalid)) => handleInvalid(k, info)
      case (k, v) =>
        val (info, expr) = unwrap(v)
        Connect(info, k.e1, expr)
    }
  }

  /** Returns new sequence of combined Attaches
    * @todo Preserve Info
    */
  private def combineAttaches(attaches: Seq[Attach]): Seq[Attach] = {
    // Helper type to add an ordering index to attached Expressions
    case class AttachAcc(exprs: Seq[WrappedExpression], idx: Int)
    // Map from every attached expression to its corresponding AttachAcc
    //   (many keys will point to same value)
    val attachMap = mutable.LinkedHashMap.empty[WrappedExpression, AttachAcc]
    for (Attach(_, es) <- attaches) {
      val exprs = es.map(we(_))
      val acc = exprs.map(attachMap.get(_)).flatten match {
        case Seq() => // None of these expressions is present in the attachMap
          AttachAcc(exprs, attachMap.size)
        case accs => // At least one expression present in the attachMap
          val sorted = accs.sortBy(_.idx)
          AttachAcc((sorted.map(_.exprs) :+ exprs).flatten.distinct, sorted.head.idx)
      }
      attachMap ++= acc.exprs.map(_ -> acc)
    }
    attachMap.values.toList.distinct.map(acc => Attach(NoInfo, acc.exprs.map(_.e1)))
  }
  // Searches nested scopes of defaults for lvalue
  // defaults uses mutable Map because we are searching LinkedHashMaps and conversion to immutable is VERY slow
  @tailrec
  private def getDefault(lvalue: WrappedExpression, defaults: Defaults): Option[Expression] = {
    defaults match {
      case Nil => None
      case head :: tail =>
        head.get(lvalue) match {
          case Some(p) => Some(p)
          case None    => getDefault(lvalue, tail)
        }
    }
  }

  private def AND(e1: Expression, e2: Expression) = Utils.and(e1, e2)
  private def NOT(e:  Expression) = Utils.not(e)
}

class ExpandWhensAndCheck extends Transform with DependencyAPIMigration {

  override def prerequisites =
    Seq(
      Dependency(PullMuxes),
      Dependency(ReplaceAccesses),
      Dependency(ExpandConnects),
      Dependency(RemoveAccesses)
    ) ++ firrtl.stage.Forms.Deduped

  override def invalidates(a: Transform): Boolean = a match {
    case ResolveKinds | InferTypes | ResolveFlows | _: InferWidths => true
    case _ => false
  }

  override def execute(a: CircuitState): CircuitState =
    Seq(ExpandWhens, CheckInitialization).foldLeft(a) { case (acc, tx) => tx.transform(acc) }

}
