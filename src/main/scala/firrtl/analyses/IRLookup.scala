// See LICENSE for license details.

package firrtl.analyses

import firrtl.annotations.TargetToken._
import firrtl.annotations._
import firrtl.ir._
import firrtl.{BIGENDER, FEMALE, Gender, MALE, UNKNOWNGENDER}
import firrtl.{ExpKind, InstanceKind, Kind, PortKind, RegKind, Utils, WDefInstance, WRef, WireKind}

import scala.collection.mutable

/** Handy lookup for obtaining AST information about a given Target
  * @param declarations Maps references (not subreferences) to declarations
  */
class IRLookup(private val declarations: collection.Map[Target, FirrtlNode]) {

  private val genderCache = mutable.HashMap[Target, Gender]()
  private val kindCache = mutable.HashMap[Target, Kind]()
  private val tpeCache = mutable.HashMap[Target, Type]()
  private val exprCache = mutable.HashMap[(Target, Gender), Expression]()

  /** Returns the target converted to its local reference
    * E.g. Given ~Top|MyModule/inst:Other>foo.bar, returns ~Top|Other>foo
    * @param t
    * @return
    */
  def asLocalRef(t: Target): Target = Target.getReferenceTarget(Target.getPathlessTarget(t))

  /** Returns the gender of t
    * @param t
    * @return
    */
  def gender(t: Target): Gender = {
    val pathless = asLocalRef(t)
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
  def kind(t: Target): Kind = {
    val pathless = asLocalRef(t)
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
  def tpe(t: Target): Type = {
    val pathless = asLocalRef(t)
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
  def expr(t: Target, gender: Gender = UNKNOWNGENDER): Expression = {
    val pathless = asLocalRef(t)
    require(t.moduleOpt.nonEmpty)

    inCache(pathless, gender) match {
      case Some(e) => e
      case None =>
        val mt = pathless.circuitTarget.module(pathless.moduleOpt.get)
        declarations(pathless) match {
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
            case r: DefRegister if pathless.tokens.last == Init =>
              //TODO(azidar): expand init field in case register is aggregate type?
              exprCache((pathless, MALE)) = r.init
            case r: DefRegister if pathless.tokens.last == Reset =>
              exprCache((pathless, MALE)) = r.reset
            case r: DefRegister =>
              updateExpr(mt, WRef(r.name, r.tpe, RegKind, MALE))
              updateExpr(mt, WRef(r.name, r.tpe, RegKind, FEMALE))
              updateExpr(mt, WRef(r.name, r.tpe, RegKind, BIGENDER))
            case m: DefMemory => sys.error("Will support memories soon!")
            case other => sys.error(s"Cannot call expr with: $other")
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
  def declaration(t: Target): FirrtlNode = declarations(asLocalRef(t))

  /** Updates expression cache with expression
    * @param mt
    * @param ref
    */
  private def updateExpr(mt: ModuleTarget, ref: Expression): Unit = {
    Utils.expandRef(ref).foreach { e =>
      val target = CircuitGraph.asTarget(mt, new TokenTagger())(e)
      exprCache((target, Utils.gender(e))) = e
    }
  }

  /** Optionally returns the expression corresponding to the target if contained in the expression cache
    * @param pathless
    * @param gender
    * @return
    */
  private def inCache(pathless: Target, gender: Gender): Option[Expression] = {
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
