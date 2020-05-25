// See LICENSE for license details.

package firrtl.analyses

import firrtl.annotations.TargetToken._
import firrtl.annotations._
import firrtl.ir._
import firrtl.passes.MemPortUtils
import firrtl.{DuplexFlow, ExpKind, SinkFlow, Flow, InstanceKind, Kind, SourceFlow, MemKind, PortKind, RegKind, UnknownFlow, Utils, WDefInstance, WInvalid, WRef, WSubField, WSubIndex, WireKind}

import scala.collection.mutable

object IRLookup {
  def apply(circuit: Circuit): IRLookup = ConnectionGraph(circuit).irLookup
}

/** Handy lookup for obtaining AST information about a given Target
  * @param declarations Maps references (not subreferences) to declarations
  * @param modules Maps module targets to modules
  */
class IRLookup private[analyses] ( private val declarations: mutable.LinkedHashMap[ModuleTarget, mutable.LinkedHashMap[ReferenceTarget, FirrtlNode]],
                                   private val modules: Map[ModuleTarget, DefModule]) {

  private val flowCache = mutable.HashMap[ReferenceTarget, Flow]()
  private val kindCache = mutable.HashMap[ReferenceTarget, Kind]()
  private val tpeCache = mutable.HashMap[ReferenceTarget, Type]()
  private val exprCache = mutable.HashMap[(ReferenceTarget, Flow), Expression]()

  private val refCache = mutable.HashMap[ModuleTarget, mutable.LinkedHashMap[Kind, mutable.ArrayBuffer[ReferenceTarget]]]()



  /** Returns the target converted to its local reference
    * E.g. Given ~Top|MyModule/inst:Other>foo.bar, returns ~Top|Other>foo
    * @param t
    * @return
    */
  def asLocalRef(t: ReferenceTarget): ReferenceTarget = t.pathlessTarget.copy(component = Nil)

  /** Returns the flow of t
    * @param t
    * @return
    */
  def flow(t: ReferenceTarget): Flow = {
    val pathless = t.pathlessTarget
    if(flowCache.contains(pathless)) return flowCache(pathless)
    val flow = Utils.flow(expr(pathless))
    flowCache(pathless) = flow
    flow
  }

  /** Returns the kind of t
    * @param t
    * @return
    */
  def kind(t: ReferenceTarget): Kind = {
    val pathless = t.pathlessTarget
    if(kindCache.contains(pathless)) return kindCache(pathless)

    val kind = Utils.kind(expr(pathless))
    kindCache(pathless) = kind
    kind
  }

  /** Returns the type of t
    * @param t
    * @return
    */
  def tpe(t: ReferenceTarget): Type = {
    val pathless = t.pathlessTarget
    if(tpeCache.contains(pathless)) return tpeCache(pathless)

    val tpe = expr(pathless).tpe
    tpeCache(pathless) = tpe
    tpe
  }

  /** Returns Some(e) if expression exists, None if it does not
    *
    * It can return None for many reasons, including
    *  - declaration is missing
    *  - flow is wrong
    *  - component is wrong
    *
    * @param t
    * @param flow
    * @return
    */
  def getExpr(t: ReferenceTarget, flow: Flow): Option[Expression] = {
    val pathless = t.pathlessTarget

    inCache(pathless, flow) match {
      case Some(e) => e
      case None =>
        val mt = pathless.moduleTarget
        val emt = t.encapsulatingModuleTarget
        if(declarations.contains(emt) && declarations(emt).contains(asLocalRef(t))){
          declarations(emt)(asLocalRef(t)) match {
            case e: Expression =>
              require(e.tpe.isInstanceOf[GroundType])
              exprCache.getOrElseUpdate((pathless, Utils.flow(e)), e)
            case d: IsDeclaration => d match {
              case n: DefNode =>
                updateExpr(mt, WRef(n.name, n.value.tpe, ExpKind, SourceFlow))
              case p: Port =>
                updateExpr(mt, WRef(p.name, p.tpe, PortKind, Utils.get_flow(p)))
              case w: WDefInstance =>
                updateExpr(mt, WRef(w.name, w.tpe, InstanceKind, SourceFlow))
              case w: DefWire =>
                updateExpr(mt, WRef(w.name, w.tpe, WireKind, SourceFlow))
                updateExpr(mt, WRef(w.name, w.tpe, WireKind, SinkFlow))
                updateExpr(mt, WRef(w.name, w.tpe, WireKind, DuplexFlow))
              case r: DefRegister if pathless.tokens.last == Clock =>
                exprCache((pathless, SourceFlow)) = r.clock
              case r: DefRegister if pathless.tokens.isDefinedAt(1) && pathless.tokens(1) == Init =>
                exprCache((pathless, SourceFlow)) = r.init
                updateExpr(pathless, r.init)
              case r: DefRegister if pathless.tokens.last == Reset =>
                exprCache((pathless, SourceFlow)) = r.reset
              case r: DefRegister =>
                updateExpr(mt, WRef(r.name, r.tpe, RegKind, SourceFlow))
                updateExpr(mt, WRef(r.name, r.tpe, RegKind, SinkFlow))
                updateExpr(mt, WRef(r.name, r.tpe, RegKind, DuplexFlow))
              case m: DefMemory =>
                updateExpr(mt, WRef(m.name, MemPortUtils.memType(m), MemKind, SourceFlow))
              case other =>
                sys.error(s"Cannot call expr with: $t, given declaration $other")
            }
            case x: IsInvalid =>
              exprCache((pathless, SourceFlow)) = WInvalid
          }
        } else None
    }

    inCache(pathless, flow)
  }

  /** Returns an expression corresponding to the target
    * @param t
    * @param flow
    * @return
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

  private def updateRefs(kind: Kind, rt: ReferenceTarget): Unit = {
    val kindMap = refCache.getOrElseUpdate(rt.moduleTarget, mutable.LinkedHashMap.empty[Kind, mutable.ArrayBuffer[ReferenceTarget]])
    kindMap.getOrElseUpdate(kind, mutable.ArrayBuffer.empty[ReferenceTarget]) += rt
  }

  def references(moduleTarget: ModuleTarget, kind: Kind): Seq[ReferenceTarget] = {
    require(contains(moduleTarget), s"Cannot find\n${moduleTarget.prettyPrint()}\nin circuit!")
    if(refCache.contains(moduleTarget) && refCache(moduleTarget).contains(kind)) refCache(moduleTarget)(kind)
    else {
      modules(moduleTarget)
      declarations(moduleTarget).foreach {
        case (rt, ir: DefRegister) => updateRefs(RegKind, rt)
        case (rt, ir: DefWire) => updateRefs(WireKind, rt)
        case (rt, ir: DefNode) => updateRefs(ExpKind, rt)
        case (rt, ir: DefMemory) => updateRefs(MemKind, rt)
        case (rt, ir: DefInstance) => updateRefs(InstanceKind, rt)
        case (rt, ir: WDefInstance) => updateRefs(InstanceKind, rt)
        case (rt, ir: Port) => updateRefs(PortKind, rt)
        case _ =>
      }
      refCache.get(moduleTarget).map(_.getOrElse(kind, Seq.empty[ReferenceTarget])).getOrElse(Seq.empty[ReferenceTarget])
    }
  }

  /** Returns the statement containing the declaration of the target
    * @param t
    * @return
    */
  def declaration(t: ReferenceTarget): FirrtlNode = {
    require(contains(t), s"Cannot find\n${t.prettyPrint()}\nin circuit!")
    declarations(t.encapsulatingModuleTarget)(asLocalRef(t))
  }

  /** Returns the references to the module's ports
    * @param mt
    */
  def ports(mt: ModuleTarget): Seq[ReferenceTarget] = {
    require(contains(mt), s"Cannot find\n${mt.prettyPrint()}\nin circuit!")
    modules(mt).ports.map { p => mt.ref(p.name) }
  }

  /** Returns a target to each sub-component, including intermediate subcomponents
    * E.g.
    *   Given:
    *     A ReferenceTarget of ~Top|Module>ref and a type of {foo: {bar: UInt}}
    *   Return:
    *     Seq(~Top|Module>ref, ~Top|Module>ref.foo, ~Top|Module>ref.foo.bar)
    * @param r
    * @return
    */
  def allTargets(r: ReferenceTarget): Seq[ReferenceTarget] = r.allSubTargets(tpe(r))

  /** Returns a target to each sub-component, excluding intermediate subcomponents
    * E.g.
    *   Given:
    *     A ReferenceTarget of ~Top|Module>ref and a type of {foo: {bar: UInt}}
    *   Return:
    *     Seq(~Top|Module>ref.foo.bar)
    * @param r
    * @return
    */
  def leafTargets(r: ReferenceTarget): Seq[ReferenceTarget] = r.leafSubTargets(tpe(r))

  /** Returns target and type of each module port
    * @param m
    * @param module
    * @return Returns ((inputs, outputs))
    */
  def moduleLeafPortTargets(m: ModuleTarget,
                            module: DefModule
                           ): (Seq[(ReferenceTarget, Type)], Seq[(ReferenceTarget, Type)]) = {
    module.ports.flatMap {
      case Port(_, name, Output, tpe) => Utils.create_exps(WRef(name, tpe, PortKind, SourceFlow))
      case Port(_, name, Input, tpe) => Utils.create_exps(WRef(name, tpe, PortKind, SinkFlow))
    }.foldLeft((Vector.empty[(ReferenceTarget, Type)], Vector.empty[(ReferenceTarget, Type)])) {
      case ((inputs, outputs), e) if Utils.flow(e) == SourceFlow =>
        (inputs, outputs :+ (ConnectionGraph.asTarget(m, new TokenTagger())(e) e.tpe))
      case ((inputs, outputs), e) =>
        (inputs :+ (ConnectionGraph.asTarget(m, new TokenTagger())(e) e.tpe), outputs)
    }
  }

  /** Returns whether a ReferenceTarget is contained in this IRLookup
    * @param t
    * @return
    */
  def contains(t: ReferenceTarget): Boolean = {
    validPath(t.pathTarget) &&
      declarations.contains(t.encapsulatingModuleTarget) &&
      declarations(t.encapsulatingModuleTarget).contains(asLocalRef(t)) &&
      getExpr(t, UnknownFlow).nonEmpty
  }

  /** Returns whether a ModuleTarget or InstanceTarget is contained in this IRLookup
    * @param mt
    * @return
    */
  def contains(mt: IsModule): Boolean = validPath(mt)

  /** Returns whether a given IsModule is valid, given the circuit's module/instance hierarchy
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
              case DefInstance(_, _, of) if of == x.ofModule => validPath(x.ofModuleTarget)
              case WDefInstance(_, _, of, _) if of == x.ofModule => validPath(x.ofModuleTarget)
              case _ => false
            })
        }.reduce(_ && _)
    }
  }

  /** Updates expression cache with expression
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
        (0 until size).foreach { i => updateExpr(gt.index(i), WSubIndex(e, i, t, g)) }
      case BundleType(fields) =>
        exprCache((gt, g)) = e
        fields.foreach { f => updateExpr(gt.field(f.name), WSubField(e, f.name, f.tpe, Utils.times(g, f.flip))) }
      case other => sys.error(s"Error! Unexpected type $other")
    }
  }

  /** Optionally returns the expression corresponding to the target if contained in the expression cache
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
      case (SourceFlow,          true,  _,     _)     => Some(exprCache((pathless, flow)))
      case (SinkFlow,        _,     true,  _)     => Some(exprCache((pathless, flow)))
      case (DuplexFlow,      _,     _,     true)  => Some(exprCache((pathless, DuplexFlow)))
      case (UnknownFlow, _,     _,     true)  => Some(exprCache((pathless, DuplexFlow)))
      case (UnknownFlow, true,  false, false) => Some(exprCache((pathless, SourceFlow)))
      case (UnknownFlow, false, true,  false) => Some(exprCache((pathless, SinkFlow)))
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
