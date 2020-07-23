// See LICENSE for license details.

package firrtl.analyses

import firrtl.annotations.TargetToken._
import firrtl.annotations._
import firrtl.ir._
import firrtl.passes.MemPortUtils
import firrtl.{DuplexFlow, ExpKind, Flow, InstanceKind, Kind, MemKind, PortKind, RegKind, SinkFlow, SourceFlow, UnknownFlow, Utils, WInvalid, WireKind}

import scala.collection.mutable

object IRLookup {
  def apply(circuit: Circuit): IRLookup = ConnectionGraph(circuit).irLookup
}

/** Handy lookup for obtaining AST information about a given Target
  *
  * @param declarations Maps references (not subreferences) to declarations
  * @param modules      Maps module targets to modules
  */
class IRLookup private[analyses](private val declarations: Map[ModuleTarget, Map[ReferenceTarget, FirrtlNode]],
                                 private val modules: Map[ModuleTarget, DefModule]) {

  private val flowCache = mutable.HashMap[ReferenceTarget, Flow]()
  private val kindCache = mutable.HashMap[ReferenceTarget, Kind]()
  private val tpeCache = mutable.HashMap[ReferenceTarget, Type]()
  private val exprCache = mutable.HashMap[(ReferenceTarget, Flow), Expression]()
  private val refCache = mutable.HashMap[ModuleTarget, mutable.LinkedHashMap[Kind, mutable.ArrayBuffer[ReferenceTarget]]]()

  /** Returns the target converted to its local reference
    * E.g. Given ~Top|MyModule/inst:Other>foo.bar, returns ~Top|Other>foo
    *
    * @param t
    * @return
    */
  def asLocalRef(t: ReferenceTarget): ReferenceTarget = t.pathlessTarget.copy(component = Nil)

  def flow(t: ReferenceTarget): Flow = flowCache.getOrElseUpdate(t.pathlessTarget, Utils.flow(expr(t.pathlessTarget)))

  def kind(t: ReferenceTarget): Kind = kindCache.getOrElseUpdate(t.pathlessTarget, Utils.kind(expr(t.pathlessTarget)))

  def tpe(t: ReferenceTarget): Type = tpeCache.getOrElseUpdate(t.pathlessTarget, expr(t.pathlessTarget).tpe)

  /** get expression of the target.
    * It can return None for many reasons, including
    *  - declaration is missing
    *  - flow is wrong
    *  - component is wrong
    *
    * @param t    [[ReferenceTarget]] to be queried.
    * @param flow flow of the target
    * @return Some(e) if expression exists, None if it does not
    */
  def getExpr(t: ReferenceTarget, flow: Flow): Option[Expression] = {
    val pathless = t.pathlessTarget

    inCache(pathless, flow) match {
      case e@Some(_) => return e
      case None =>
        val mt = pathless.moduleTarget
        val emt = t.encapsulatingModuleTarget
        if (declarations.contains(emt) && declarations(emt).contains(asLocalRef(t))) {
          declarations(emt)(asLocalRef(t)) match {
            case e: Expression =>
              require(e.tpe.isInstanceOf[GroundType])
              exprCache.getOrElseUpdate((pathless, Utils.flow(e)), e)
            case d: IsDeclaration => d match {
              case n: DefNode =>
                updateExpr(mt, Reference(n.name, n.value.tpe, ExpKind, SourceFlow))
              case p: Port =>
                updateExpr(mt, Reference(p.name, p.tpe, PortKind, Utils.get_flow(p)))
              case w: DefInstance =>
                updateExpr(mt, Reference(w.name, w.tpe, InstanceKind, SourceFlow))
              case w: DefWire =>
                updateExpr(mt, Reference(w.name, w.tpe, WireKind, SourceFlow))
                updateExpr(mt, Reference(w.name, w.tpe, WireKind, SinkFlow))
                updateExpr(mt, Reference(w.name, w.tpe, WireKind, DuplexFlow))
              case r: DefRegister if pathless.tokens.last == Clock =>
                exprCache((pathless, SourceFlow)) = r.clock
              case r: DefRegister if pathless.tokens.isDefinedAt(1) && pathless.tokens(1) == Init =>
                exprCache((pathless, SourceFlow)) = r.init
                updateExpr(pathless, r.init)
              case r: DefRegister if pathless.tokens.last == Reset =>
                exprCache((pathless, SourceFlow)) = r.reset
              case r: DefRegister =>
                updateExpr(mt, Reference(r.name, r.tpe, RegKind, SourceFlow))
                updateExpr(mt, Reference(r.name, r.tpe, RegKind, SinkFlow))
                updateExpr(mt, Reference(r.name, r.tpe, RegKind, DuplexFlow))
              case m: DefMemory =>
                updateExpr(mt, Reference(m.name, MemPortUtils.memType(m), MemKind, SourceFlow))
              case other =>
                sys.error(s"Cannot call expr with: $t, given declaration $other")
            }
            case _: IsInvalid =>
              exprCache((pathless, SourceFlow)) = WInvalid
          }
        }
    }

    inCache(pathless, flow)
  }

  /**
    * @param t    [[ReferenceTarget]] to be queried.
    * @param flow flow of the target
    * @return expression of `t`
    */
  def expr(t: ReferenceTarget, flow: Flow = UnknownFlow): Expression = {
    require(contains(t), s"Cannot find\n${t.prettyPrint()}\nin circuit!")
    getExpr(t, flow) match {
      case Some(e) => e
      case None =>
        require(getExpr(t.pathlessTarget, UnknownFlow).isEmpty, s"Illegal flow $flow with target $t")
        sys.error("")
    }
  }

  /** find all kind in a [[ModuleTarget]]
    * @param moduleTarget [[ModuleTarget]] to be queried.
    * @param kind [[Kind]] to be find.
    * @return all [[ReferenceTarget]] in this node. */
  def kindFinder(moduleTarget: ModuleTarget, kind: Kind): Seq[ReferenceTarget] = {
    def updateRefs(kind: Kind, rt: ReferenceTarget): Unit = refCache
      .getOrElseUpdate(rt.moduleTarget, mutable.LinkedHashMap.empty[Kind, mutable.ArrayBuffer[ReferenceTarget]])
      .getOrElseUpdate(kind, mutable.ArrayBuffer.empty[ReferenceTarget]) += rt

    require(contains(moduleTarget), s"Cannot find\n${moduleTarget.prettyPrint()}\nin circuit!")
    if (refCache.contains(moduleTarget) && refCache(moduleTarget).contains(kind)) refCache(moduleTarget)(kind)
    else {
      declarations(moduleTarget).foreach {
        case (rt, _: DefRegister) => updateRefs(RegKind, rt)
        case (rt, _: DefWire) => updateRefs(WireKind, rt)
        case (rt, _: DefNode) => updateRefs(ExpKind, rt)
        case (rt, _: DefMemory) => updateRefs(MemKind, rt)
        case (rt, _: DefInstance) => updateRefs(InstanceKind, rt)
        case (rt, _: Port) => updateRefs(PortKind, rt)
        case _ =>
      }
      refCache.get(moduleTarget).map(_.getOrElse(kind, Seq.empty[ReferenceTarget])).getOrElse(Seq.empty[ReferenceTarget])
    }
  }

  /**
    * @param t
    * @return the statement containing the declaration of the target
    */
  def declaration(t: ReferenceTarget): FirrtlNode = {
    require(contains(t), s"Cannot find\n${t.prettyPrint()}\nin circuit!")
    declarations(t.encapsulatingModuleTarget)(asLocalRef(t))
  }

  /** Returns the references to the module's ports
    *
    * @param mt
    */
  def ports(mt: ModuleTarget): Seq[ReferenceTarget] = {
    require(contains(mt), s"Cannot find\n${mt.prettyPrint()}\nin circuit!")
    modules(mt).ports.map { p => mt.ref(p.name) }
  }

  /** Returns a target to each sub-component, including intermediate subcomponents
    * E.g.
    * Given:
    * A ReferenceTarget of ~Top|Module>ref and a type of {foo: {bar: UInt}}
    * Return:
    * Seq(~Top|Module>ref, ~Top|Module>ref.foo, ~Top|Module>ref.foo.bar)
    *
    * @param r
    * @return
    */
  def allTargets(r: ReferenceTarget): Seq[ReferenceTarget] = r.allSubTargets(tpe(r))

  /** Returns a target to each sub-component, excluding intermediate subcomponents
    * E.g.
    * Given:
    * A ReferenceTarget of ~Top|Module>ref and a type of {foo: {bar: UInt}}
    * Return:
    * Seq(~Top|Module>ref.foo.bar)
    *
    * @param r
    * @return
    */
  def leafTargets(r: ReferenceTarget): Seq[ReferenceTarget] = r.leafSubTargets(tpe(r))

  /** Returns target and type of each module port
    *
    * @param m
    * @param module
    * @return Returns ((inputs, outputs))
    */
  def moduleLeafPortTargets(m: ModuleTarget,
                            module: DefModule
                           ): (Seq[(ReferenceTarget, Type)], Seq[(ReferenceTarget, Type)]) = {
    module.ports.flatMap {
      case Port(_, name, Output, tpe) => Utils.create_exps(Reference(name, tpe, PortKind, SourceFlow))
      case Port(_, name, Input, tpe) => Utils.create_exps(Reference(name, tpe, PortKind, SinkFlow))
    }.foldLeft((Vector.empty[(ReferenceTarget, Type)], Vector.empty[(ReferenceTarget, Type)])) {
      case ((inputs, outputs), e) if Utils.flow(e) == SourceFlow =>
        (inputs, outputs :+ (ConnectionGraph.asTarget(m, new TokenTagger())(e), e.tpe))
      case ((inputs, outputs), e) =>
        (inputs :+ (ConnectionGraph.asTarget(m, new TokenTagger())(e), e.tpe), outputs)
    }
  }

  /**
    * @param t [[ReferenceTarget]] to be queried.
    * @return whether a ReferenceTarget is contained in this IRLookup
    */
  def contains(t: ReferenceTarget): Boolean = validPath(t.pathTarget) &&
    declarations.contains(t.encapsulatingModuleTarget) &&
    declarations(t.encapsulatingModuleTarget).contains(asLocalRef(t)) &&
    getExpr(t, UnknownFlow).nonEmpty

  /** Returns whether a ModuleTarget or InstanceTarget is contained in this IRLookup
    *
    * @param mt
    * @return
    */
  def contains(mt: IsModule): Boolean = validPath(mt)

  /** Returns whether a given IsModule is valid, given the circuit's module/instance hierarchy
    *
    * @param t
    * @return
    */
  def validPath(t: IsModule): Boolean = {
    t match {
      case m: ModuleTarget => declarations.contains(m)
      case i: InstanceTarget =>
        val all = i.pathAsTargets :+ i.encapsulatingModuleTarget.instOf(i.instance, i.ofModule)
        all.map { x =>
          declarations.contains(x.moduleTarget) && declarations(x.moduleTarget).contains(x.asReference) &&
            (declarations(x.moduleTarget)(x.asReference) match {
              case DefInstance(_, _, of, _) if of == x.ofModule => validPath(x.ofModuleTarget)
              case _ => false
            })
        }.reduce(_ && _)
    }
  }

  /** Updates expression cache with expression
    *
    * @param mt
    * @param ref
    */
  private def updateExpr(mt: ModuleTarget, ref: Expression): Unit = {
    val refs = Utils.expandRef(ref)
    refs.foreach { e =>
      val target = ConnectionGraph.asTarget(mt, new TokenTagger())(e)
      exprCache((target, Utils.flow(e))) = e
    }
  }

  /** Updates expression cache with expression
    *
    * @param gt
    * @param e
    */
  private def updateExpr(gt: ReferenceTarget, e: Expression): Unit = {
    val g = Utils.flow(e)
    e.tpe match {
      case _: GroundType =>
        exprCache((gt, g)) = e
      case VectorType(t, size) =>
        exprCache((gt, g)) = e
        (0 until size).foreach { i => updateExpr(gt.index(i), SubIndex(e, i, t, g)) }
      case BundleType(fields) =>
        exprCache((gt, g)) = e
        fields.foreach { f => updateExpr(gt.field(f.name), SubField(e, f.name, f.tpe, Utils.times(g, f.flip))) }
      case other => sys.error(s"Error! Unexpected type $other")
    }
  }

  /** Optionally returns the expression corresponding to the target if contained in the expression cache
    *
    * @param pathless
    * @param flow
    * @return
    */
  private def inCache(pathless: ReferenceTarget, flow: Flow): Option[Expression] = {
    (flow,
      exprCache.contains((pathless, SourceFlow)),
      exprCache.contains((pathless, SinkFlow)),
      exprCache.contains(pathless, DuplexFlow)
    ) match {
      case (SourceFlow, true, _, _) => Some(exprCache((pathless, flow)))
      case (SinkFlow, _, true, _) => Some(exprCache((pathless, flow)))
      case (DuplexFlow, _, _, true) => Some(exprCache((pathless, DuplexFlow)))
      case (UnknownFlow, _, _, true) => Some(exprCache((pathless, DuplexFlow)))
      case (UnknownFlow, true, false, false) => Some(exprCache((pathless, SourceFlow)))
      case (UnknownFlow, false, true, false) => Some(exprCache((pathless, SinkFlow)))
      case other => None
    }
  }
}


/** Used for obtaining a tag for a given label
  *
  * E.g. Used for constructing unnamed tokens
  */
class TokenTagger {
  private val counterMap = mutable.HashMap[String, Int]()

  def getTag(label: String): Int = {
    val tag = counterMap.getOrElse(label, 0)
    counterMap(label) = tag + 1
    tag
  }

  def getRef(label: String): String = {
    "@" + label + "#" + getTag(label)
  }
}

object TokenTagger {
  val literalRegex = "@([-]?[0-9]+)#[0-9]+".r
}
