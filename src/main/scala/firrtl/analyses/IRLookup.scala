// See LICENSE for license details.

package firrtl.analyses

import firrtl.annotations.TargetToken._
import firrtl.annotations._
import firrtl.ir._
import firrtl.passes.MemPortUtils
import firrtl.{BIGENDER, ExpKind, FEMALE, Gender, InstanceKind, Kind, MALE, MemKind, PortKind, RegKind, UNKNOWNGENDER, Utils, WDefInstance, WRef, WSubField, WSubIndex, WireKind}

import scala.collection.mutable

object IRLookup {
  def apply(circuit: Circuit): IRLookup = CircuitGraph(circuit).irLookup
}

/** Handy lookup for obtaining AST information about a given Target
  * @param declarations Maps references (not subreferences) to declarations
  * @param modules Maps module targets to modules
  */
class IRLookup private[analyses] ( private val declarations: collection.Map[ReferenceTarget, FirrtlNode],
                                   private val modules: collection.Map[ModuleTarget, DefModule]) {

  private val genderCache = mutable.HashMap[ReferenceTarget, Gender]()
  private val kindCache = mutable.HashMap[ReferenceTarget, Kind]()
  private val tpeCache = mutable.HashMap[ReferenceTarget, Type]()
  private val exprCache = mutable.HashMap[(ReferenceTarget, Gender), Expression]()

  /** Returns the target converted to its local reference
    * E.g. Given ~Top|MyModule/inst:Other>foo.bar, returns ~Top|Other>foo
    * @param t
    * @return
    */
  def asLocalRef(t: ReferenceTarget): ReferenceTarget = t.pathlessTarget.copy(component = Nil)

  /** Returns the gender of t
    * @param t
    * @return
    */
  def gender(t: ReferenceTarget): Gender = {
    val pathless = t.pathlessTarget
    require(t.moduleOpt.nonEmpty)
    if(genderCache.contains(pathless)) return genderCache(pathless)
    val gender = Utils.gender(expr(pathless))
    genderCache(pathless) = gender
    gender
  }

  /** Returns the kind of t
    * @param t
    * @return
    */
  def kind(t: ReferenceTarget): Kind = {
    val pathless = t.pathlessTarget
    require(t.moduleOpt.nonEmpty)
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
    require(t.moduleOpt.nonEmpty)
    if(tpeCache.contains(pathless)) return tpeCache(pathless)

    val tpe = expr(pathless).tpe
    tpeCache(pathless) = tpe
    tpe
  }

  /** Returns an expression corresponding to the target
    * @param t
    * @param gender
    * @return
    */
  def expr(t: ReferenceTarget, gender: Gender = UNKNOWNGENDER): Expression = {
    val pathless = t.pathlessTarget
    require(t.moduleOpt.nonEmpty && t.circuitOpt.nonEmpty)

    inCache(pathless, gender) match {
      case Some(e) => e
      case None =>
        val mt = CircuitTarget(pathless.circuitOpt.get).module(pathless.moduleOpt.get)
        declarations(asLocalRef(t)) match {
          case e: Expression =>
            require(e.tpe.isInstanceOf[GroundType])
            exprCache.getOrElseUpdate((pathless, Utils.gender(e)), e)
          case d: IsDeclaration => d match {
            case n: DefNode =>
              updateExpr(mt, WRef(n.name, n.value.tpe, ExpKind, MALE))
            case p: Port =>
              updateExpr(mt, WRef(p.name, p.tpe, PortKind, Utils.get_gender(p)))
            case w: WDefInstance =>
              updateExpr(mt, WRef(w.name, w.tpe, InstanceKind, MALE))
            case w: DefWire =>
              updateExpr(mt, WRef(w.name, w.tpe, WireKind, MALE))
              updateExpr(mt, WRef(w.name, w.tpe, WireKind, FEMALE))
              updateExpr(mt, WRef(w.name, w.tpe, WireKind, BIGENDER))
            case r: DefRegister if pathless.tokens.last == Clock =>
              exprCache((pathless, MALE)) = r.clock
            case r: DefRegister if pathless.tokens.isDefinedAt(1) && pathless.tokens(1) == Init =>
              exprCache((pathless, MALE)) = r.init
              updateExpr(pathless, r.init)
            case r: DefRegister if pathless.tokens.last == Reset =>
              exprCache((pathless, MALE)) = r.reset
            case r: DefRegister =>
              updateExpr(mt, WRef(r.name, r.tpe, RegKind, MALE))
              updateExpr(mt, WRef(r.name, r.tpe, RegKind, FEMALE))
              updateExpr(mt, WRef(r.name, r.tpe, RegKind, BIGENDER))
            case m: DefMemory =>
              updateExpr(mt, WRef(m.name, MemPortUtils.memType(m), MemKind, MALE))
            case other =>
              sys.error(s"Cannot call expr with: $t, given declaration $other")
          }
        }
    }

    inCache(pathless, gender) match {
      case Some(e) => e
      case None => sys.error(s"Illegal gender $gender with target $t")
    }
  }

  /** Returns the statement containing the declaration of the target
    * @param t
    * @return
    */
  def declaration(t: ReferenceTarget): FirrtlNode = declarations(asLocalRef(t))

  /** Returns the references to the module's ports
    * @param mt
    */
  def ports(mt: ModuleTarget): Seq[ReferenceTarget] = {
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
      case Port(_, name, Output, tpe) => Utils.create_exps(WRef(name, tpe, PortKind, MALE))
      case Port(_, name, Input, tpe) => Utils.create_exps(WRef(name, tpe, PortKind, FEMALE))
    }.foldLeft((Vector.empty[(ReferenceTarget, Type)], Vector.empty[(ReferenceTarget, Type)])) {
      case ((inputs, outputs), e) if Utils.gender(e) == MALE =>
        (inputs, outputs :+ (CircuitGraph.asTarget(m, new TokenTagger())(e).asInstanceOf[ReferenceTarget], e.tpe))
      case ((inputs, outputs), e) =>
        (inputs :+ (CircuitGraph.asTarget(m, new TokenTagger())(e).asInstanceOf[ReferenceTarget], e.tpe), outputs)
    }
  }

  /** Returns whether a target is contained in this IRLookup
    * @param t
    * @return
    */
  def contains(t: ReferenceTarget): Boolean = declarations.contains(asLocalRef(t))

  /** Updates expression cache with expression
    * @param mt
    * @param ref
    */
  private def updateExpr(mt: ModuleTarget, ref: Expression): Unit = {
    val refs = Utils.expandRef(ref)
    refs.foreach { e =>
      val target = CircuitGraph.asTarget(mt, new TokenTagger())(e)
      exprCache((target, Utils.gender(e))) = e
    }
  }

  private def updateExpr(gt: ReferenceTarget, e: Expression): Unit = {
    val g = Utils.gender(e)
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
    * @param gender
    * @return
    */
  private def inCache(pathless: ReferenceTarget, gender: Gender): Option[Expression] = {
    (gender,
      exprCache.contains((pathless, MALE)),
      exprCache.contains((pathless, FEMALE)),
      exprCache.contains(pathless, BIGENDER)
    ) match {
      case (MALE,          true,  _,     _)     => Some(exprCache((pathless, gender)))
      case (FEMALE,        _,     true,  _)     => Some(exprCache((pathless, gender)))
      case (BIGENDER,      _,     _,     true)  => Some(exprCache((pathless, BIGENDER)))
      case (UNKNOWNGENDER, _,     _,     true)  => Some(exprCache((pathless, BIGENDER)))
      case (UNKNOWNGENDER, true,  false, false) => Some(exprCache((pathless, MALE)))
      case (UNKNOWNGENDER, false, true,  false) => Some(exprCache((pathless, FEMALE)))
      case other => None
    }
  }
}
