// See LICENSE for license details.

package firrtl
package transforms

import firrtl._
import firrtl.annotations._
import firrtl.ir._
import firrtl.Utils.{NodeMap => _, _}
import firrtl.Mappers._
import firrtl.PrimOps._
import firrtl.annotations.TargetToken
import firrtl.annotations.TargetToken.{Field => _, _}
import firrtl.options.Dependency
import firrtl.passes.PullMuxes

import collection.mutable
import annotation.tailrec

/** A prefix tree using TargetTokens, useful for checking if subcomponents were dontTouched
  */
sealed trait TokenTrie[T] {
  def value: Option[T]
  protected def setValue(value: T): Unit
  def children: mutable.LinkedHashMap[TargetToken, TokenTrie[T]]

  /** inserts a value into the trie
    */
  @tailrec
  def insert(tokens: Seq[TargetToken], value: T): Unit = {
    if (tokens.isEmpty) {
      setValue(value)
    } else {
      val child = children.getOrElseUpdate(tokens.head, TokenTrie.empty)
      child.insert(tokens.tail, value)
    }
  }

  /** get the value at the token path
    */
  @tailrec
  def get(tokens: Seq[TargetToken]): Option[T] = {
    if (tokens.isEmpty) {
      value
    } else if (children.contains(tokens.head)) {
      children(tokens.head).get(tokens.tail)
    } else {
      None
    }
  }

  /** get the value at the token path consisting of only one token
    */
  def getToken(token: TargetToken): Option[T] = {
    children.get(token).flatMap(_.value)
  }

  /** get the sub-trie at the token prefix
   (*/
  def getChildToken(token: TargetToken): Option[TokenTrie[T]] = {
    children.get(token)
  }

  /** apply a side-effecting function to the trie values in breadth-first order
    */
  def foreach(fn: (IndexedSeq[TargetToken], T) => Unit, parent: IndexedSeq[TargetToken] = IndexedSeq.empty): Unit = {
    value.foreach(fn(parent, _))
    children.foreach { case (token, child) =>
      child.foreach(fn, parent :+ token)
    }
  }

  /** get the value at the token path
    *
    * throws an exception if there is to value present at the path
    */
  def apply(tokens: Seq[TargetToken]): T = get(tokens).get

  /* returns true if the a value exists in the trie at the token path
   */
  def contains(tokens: Seq[TargetToken]): Boolean = get(tokens).isDefined

  /** returns true if the a value exists in the trie at the token path consisting of only one token
    */
  def containsToken(token: TargetToken): Boolean = getToken(token).isDefined

  /** returns the sub-trie with the greatest matching prefix along with the prefix
    */
  def getParent(tokens: Seq[TargetToken]): Option[(T, Seq[TargetToken])] =
    getParentImp(tokens, None)

  @tailrec
  private def getParentImp(
    tokens: Seq[TargetToken],
    default: Option[(T, Seq[TargetToken])]
  ): Option[(T, Seq[TargetToken])] = {
    val newDefault = value match {
      case v: Some[T] => v.map(_ -> tokens)
      case None => default
    }
    tokens.headOption match {
      case Some(token) => children.get(token) match {
        case Some(child) => child.getParentImp(tokens.tail, newDefault)
        case None => newDefault
      }
      case None => newDefault
    }
  }
}

object TokenTrie {
  /** creates a new empty TokenTrie
    */
  def empty[T]: TokenTrie[T] = {
    new TokenTrie[T] {
      var value: Option[T] = None
      def setValue(valuex: T): Unit = {
        value = Some(valuex)
      }
      val children = mutable.LinkedHashMap.empty[TargetToken, TokenTrie[T]]
    }
  }
}

class MidFormConstantPropagation extends BaseConstantPropagation {
  type Tokens = Seq[TargetToken]
  type NodeMap = TokenTrie[Expression]

  import BaseConstantPropagation._

  override def prerequisites =
    ((new mutable.LinkedHashSet())
       ++ firrtl.stage.Forms.MidForm
       + Dependency(firrtl.transforms.RemoveReset)
       + Dependency(firrtl.passes.RemoveValidIf)).toSeq

  override def optionalPrerequisites = Seq.empty

  override def optionalPrerequisiteOf =
    Seq( Dependency(firrtl.passes.LowerTypes),
         Dependency(firrtl.passes.memlib.VerilogMemDelays),
         Dependency[SystemVerilogEmitter],
         Dependency[VerilogEmitter] )

  override def invalidates(a: Transform): Boolean = a match {
    case firrtl.passes.Legalize | firrtl.passes.RemoveEmpty => true
    case _ => false
  }

  override val annotationClasses: Traversable[Class[_]] = Seq(classOf[DontTouchAnnotation])

  private def nodeRefIsConstProped(r: Expression, e: Expression): Boolean = e match {
    case _: UIntLiteral | _: SIntLiteral | _: WRef | _: WSubIndex | _: WSubField => true
    case _ => false
  }

  private def declIsConstProped(nodeMap: NodeMap, decl: IsDeclaration): Boolean = {
    if (nodeMap.containsToken(Ref(decl.name))) {
      nodeRefIsConstProped(WRef(decl.name), nodeMap(Seq(Ref(decl.name))))
    } else {
      false
    }
  }

  private def constPropNodeRef(r: Expression, e: Expression): Expression = {
    if (nodeRefIsConstProped(r, e)) { e } else { r }
  }

  private def constPropExpression(
    nodeMap: NodeMap,
    instMap: collection.Map[Instance, OfModule],
    constSubOutputs: Map[OfModule, Map[Tokens, Literal]])(e: Expression): Expression = {
    val old = e map constPropExpression(nodeMap, instMap, constSubOutputs)
    val propagated = old match {
      case p: DoPrim => constPropPrim(p)
      case m: Mux => constPropMux(m)
      case expr@ (_: WRef | _: WSubIndex | _: WSubField) =>
        val (ref, tokens) = toTokens(expr)
        (flow(expr), ref.kind) match {
          case (SourceFlow, _) if nodeMap.contains(tokens) =>
            constPropNodeRef(expr, nodeMap(tokens))
          case (SourceFlow, InstanceKind) if tokens.size > 1 =>
            val module = instMap(ref.name.Instance)
            // Check constSubOutputs to see if the submodule is driving a constant
            val TargetToken.Field(portName) = tokens.tail.head
            val portTokens = Ref(portName) +: tokens.tail.tail
            constSubOutputs.get(module).flatMap(_.get(portTokens)).getOrElse(expr)
          case _ => expr
        }
      case x => x
    }
    // We're done when the Expression no longer changes
    if (propagated eq old) propagated
    else constPropExpression(nodeMap, instMap, constSubOutputs)(propagated)
  }

  protected def constPropModule(
      c: CircuitTarget,
      m: Module,
      dontTouchesMap: Set[Tokens],
      instMap: collection.Map[Instance, OfModule],
      constInputs: Map[Tokens, Literal],
      constSubOutputs: Map[OfModule, Map[Tokens, Literal]],
      renames: RenameMap
    ): ConstPropedModule = {
    val dontTouches = {
      val trie = TokenTrie.empty[Unit]
      dontTouchesMap.foreach { case (tokens) =>
        trie.insert(tokens, Unit)
      }
      trie
    }

    val nodeMap = TokenTrie.empty[Expression]
    // For cases where we are trying to constprop a bad name over a good one, we swap their names
    // during the second pass
    val swapMap = mutable.HashMap.empty[String, String]

    // Keep track of any outputs we drive with a constant
    val constOutputs = mutable.HashMap.empty[Tokens, Literal]
    // Keep track of any submodule inputs we drive with a constant
    // (can have more than 1 of the same submodule)
    val constSubInputs = mutable.HashMap.empty[OfModule, mutable.HashMap[Seq[TargetToken], Seq[Literal]]]
    // AsyncReset registers don't have reset turned into a mux so we must be careful
    val asyncResetRegs = mutable.HashMap.empty[String, DefRegister]

    // Register constant propagation is intrinsically more complicated, as it is not feed-forward.
    // Therefore, we must store some memoized information about how nodes can be canditates for
    // forming part of a register const-prop "self-loop," where a register gets some combination of
    // self-assignments and assignments of the same literal value.
    val nodeRegCPEntries = new mutable.HashMap[Seq[TargetToken], RegCPEntry]

    // Copy constant mapping for constant inputs (except ones marked dontTouch!)
    constInputs.foreach {
      case (tokens, lit) if dontTouches.getParent(tokens).isEmpty =>
        nodeMap.insert(tokens, lit)
      case _ =>
    }

    // Note that on back propagation we *only* worry about swapping names and propagating references
    // to constant wires, we don't need to worry about propagating primops or muxes since we'll do
    // that on the next iteration if necessary
    def backPropExpr(expr: Expression): Expression = {
      val propagated = expr match {
        // When swapping, we swap both rhs and lhs
        case ref @ WRef(rname, _,_,_) if swapMap.contains(rname) =>
          ref.copy(name = swapMap(rname))
        // Only const prop on the rhs
        case e@ (_: WRef | _: WSubField | _: WSubIndex) =>
          val (ref, tokens) = toTokens(e)
          (ref.kind, flow(e)) match {
            case (InstanceKind, SourceFlow) =>
              val module = instMap(ref.name.Instance)
              val TargetToken.Field(portName) = tokens.tail.head
              val portTokens = Ref(portName) +: tokens.tail.tail
              // Check constSubOutputs to see if the submodule is driving a constant
              constSubOutputs.get(module).flatMap(_.get(portTokens)).getOrElse(e)
            case (_, SourceFlow) if nodeMap.contains(tokens) =>
              constPropNodeRef(e, nodeMap(tokens))
            case _ => e
          }
        case x => x map backPropExpr
      }
      propagated
    }

    def backPropStmt(stmt: Statement): Statement = stmt match {
      case reg: DefRegister if (WrappedExpression.weq(reg.init, WRef(reg))) =>
        // Self-init reset is an idiom for "no reset," and must be handled separately
        swapMap.get(reg.name)
               .map(newName => reg.copy(name = newName, init = WRef(reg).copy(name = newName)))
               .getOrElse(reg)
      case node: DefNode if declIsConstProped(nodeMap, node) =>
        renames.delete(ReferenceTarget(c.circuit, m.name, Seq.empty, node.name, Seq.empty))
        EmptyStmt
      case s => s map backPropExpr match {
        case decl: IsDeclaration if swapMap.contains(decl.name) =>
          val newName = swapMap(decl.name)
          decl match {
            case node: DefNode => node.copy(name = newName)
            case wire: DefWire => wire.copy(name = newName)
            case reg: DefRegister => reg.copy(name = newName)
            case other => throwInternalError()
          }
        case other => other map backPropStmt
      }
    }

    // When propagating a reference, check if we want to keep the name that would be deleted
    def propagateRef(lname: String, value: Expression): Unit = {
      value match {
        case WRef(rname,_,kind,_) if betterName(lname, rname) && !swapMap.contains(rname) && kind != PortKind =>
          assert(!swapMap.contains(lname)) // <- Shouldn't be possible because lname is either a
          // node declaration or the single connection to a wire or register
          swapMap(rname) = lname
        case _ =>
      }
      nodeMap.insert(Seq(Ref(lname)), value)
    }

    def constPropStmt(s: Statement): Statement = {
      val stmtx = s map constPropStmt map constPropExpression(nodeMap, instMap, constSubOutputs)
      // Record things that should be propagated
      stmtx match {
        // TODO: allow other sub-components to be propagated if dontTouch only affects a sub-component of the node
        case x: DefNode if dontTouches.getChildToken(Ref(x.name)).isEmpty => propagateRef(x.name, x.value)
        case reg: DefRegister if reg.reset.tpe == AsyncResetType =>
          asyncResetRegs(reg.name) = reg
        case c@ Connect(_, _: WRef| _: WSubField | _: WSubIndex, _) =>
          val (ref, tokens) = toTokens(c.loc)
          val dontTouched = dontTouches.contains(tokens)
          (ref.kind, c.expr) match {
            case (WireKind, lit: Literal) if !dontTouched =>
              val exprx = constPropExpression(nodeMap, instMap, constSubOutputs)(pad(lit, c.loc.tpe))
              nodeMap.insert(tokens, exprx)
            case (PortKind, lit: Literal) if !dontTouched =>
              val paddedLit = constPropExpression(nodeMap, instMap, constSubOutputs)(pad(lit, c.loc.tpe)).asInstanceOf[Literal]
              constOutputs(tokens) = paddedLit
            case (InstanceKind, lit: Literal) =>
              val paddedLit = constPropExpression(nodeMap, instMap, constSubOutputs)(pad(lit, c.loc.tpe)).asInstanceOf[Literal]
              val module = instMap(ref.name.Instance)
              val portsMap = constSubInputs.getOrElseUpdate(module, mutable.HashMap.empty)
              val portTokens = tokens.tail match {
                case TargetToken.Field(subRef) +: rest => Ref(subRef) +: rest
              }
              portsMap(portTokens) = paddedLit +: portsMap.getOrElse(portTokens, List.empty)
            // Const prop registers that are driven by a mux tree containing only instances of one constant or self-assigns
            // This requires that reset has been made explicit
            case (RegKind, rhs) if !dontTouched =>
             /* Checks if an RHS expression e of a register assignment is convertible to a constant assignment.
              * Here, this means that e must be 1) a literal, 2) a self-connect, or 3) a mux tree of
              * cases (1) and (2).  In case (3), it also recursively checks that the two mux cases can
              * be resolved: each side is allowed one candidate register and one candidate literal to
              * appear in their source trees, referring to the potential constant propagation case that
              * they could allow. If the two are compatible (no different bound sources of either of
              * the two types), they can be resolved by combining sources. Otherwise, they propagate
              * NonConstant values.  When encountering a node reference, it expands the node by to its
              * RHS assignment and recurses.
              *
              * @note Some optimization of Mux trees turn 1-bit mux operators into boolean operators. This
              * can stifle register constant propagations, which looks at drivers through value-preserving
              * Muxes and Connects only. By speculatively expanding some 1-bit Or and And operations into
              * muxes, we can obtain the best possible insight on the value of the mux with a simple peephole
              * de-optimization that does not actually appear in the output code.
              *
              * @return a RegCPEntry describing the constant prop-compatible sources driving this expression
              */

              val unbound = RegCPEntry(UnboundConstant, UnboundConstant)
              val selfBound = RegCPEntry(BoundConstant(tokens), UnboundConstant)

              def zero = passes.RemoveValidIf.getGroundZero(c.loc.tpe)
              def regConstant(e: Expression, baseCase: RegCPEntry): RegCPEntry = {
                regConstantImp(e, baseCase)
              }
              def regConstantImp(e: Expression, baseCase: RegCPEntry): RegCPEntry = e match {
                case lit: Literal => baseCase.resolve(RegCPEntry(UnboundConstant, BoundConstant(lit)))
                case _: WRef | _: WSubField | _: WSubIndex =>
                  val (ref, tokens) = toTokens(e)
                  ref.kind match {
                    case RegKind =>
                      baseCase.resolve(RegCPEntry(BoundConstant(tokens), UnboundConstant))
                    case NodeKind if nodeMap.contains(tokens) =>
                      val cached = nodeRegCPEntries.getOrElseUpdate(tokens, { regConstant(nodeMap(tokens), unbound) })
                      baseCase.resolve(cached)
                    case _ =>
                      RegCPEntry(NonConstant, NonConstant)
                  }
                case Mux(_, tval, fval, _) =>
                  regConstant(tval, baseCase).resolve(regConstant(fval, baseCase))
                case DoPrim(Or, Seq(a, b), _, BoolType) =>
                  val aSel = regConstant(Mux(a, one, b, BoolType), baseCase)
                  if (!aSel.nonConstant) aSel else regConstant(Mux(b, one, a, BoolType), baseCase)
                case DoPrim(And, Seq(a, b), _, BoolType) =>
                  val aSel = regConstant(Mux(a, b, zero, BoolType), baseCase)
                  if (!aSel.nonConstant) aSel else regConstant(Mux(b, a, zero, BoolType), baseCase)
                case _ =>
                  RegCPEntry(NonConstant, NonConstant)
              }

              // Updates nodeMap after analyzing the returned value from regConstant
              def updateNodeMapIfConstant(e: Expression): Unit = regConstant(e, selfBound) match {
                case RegCPEntry(UnboundConstant,  UnboundConstant)    => nodeMap.insert(tokens, padCPExp(zero))
                case RegCPEntry(BoundConstant(_), UnboundConstant)    => nodeMap.insert(tokens, padCPExp(zero))
                case RegCPEntry(UnboundConstant,  BoundConstant(lit)) => nodeMap.insert(tokens, padCPExp(lit))
                case RegCPEntry(BoundConstant(_), BoundConstant(lit)) => nodeMap.insert(tokens, padCPExp(lit))
                case _ =>
              }

              def padCPExp(e: Expression) = constPropExpression(nodeMap, instMap, constSubOutputs)(pad(e, c.loc.tpe))

              asyncResetRegs.get(ref.name) match {
                // Normal Register
                case None => updateNodeMapIfConstant(rhs)
                // Async Register
                case Some(reg: DefRegister) => updateNodeMapIfConstant(Mux(reg.reset, reg.init, rhs))
              }
            case _ =>
          }
        case _ =>
      }

      // Actually transform some statements
      stmtx match {
        // Propagate connections to references
        case c@ Connect(info, lhs, _: WRef | _: WSubField | _: WSubIndex) =>
          val (ref, subRef) = splitRef(c.expr)
          ref match {
            case WRef(rname, _, NodeKind, _) if dontTouches.getChildToken(Ref(rname)).isEmpty =>
              val nodeValue = nodeMap(Seq(Ref(rname)))
              val merged = mergeRef(nodeValue, subRef)
              // may need to pull mux if an inner node ref was replaced with a mux
              val newExpr = nodeValue match {
                case _: Mux => PullMuxes.pull_muxes_e(merged)
                case _ => merged
              }
              c.copy(expr = newExpr)
            case e => c
          }
        case Attach(info, exprs) if exprs.exists(kind(_) == PortKind) =>
          Attach(info, exprs.filterNot(kind(_) == WireKind))
        case other => other
      }
    }

    val modx = m.copy(body = squashEmpty(backPropStmt(constPropStmt(m.body))))
    ConstPropedModule(modx, constOutputs.toMap, constSubInputs.mapValues(_.toMap).toMap)
  }

  // iterating multiple times eats up the runtime saved by propagating nodes
  def allowMultipleModuleIterations = false

  def referenceToTokens(ref: ReferenceTarget): Tokens = ref match {
    case Target(_, Some(m), tokens) => tokens
  }
}
