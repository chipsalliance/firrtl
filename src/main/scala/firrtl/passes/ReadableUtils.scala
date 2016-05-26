package firrtl.passes

import firrtl.{Expression, WRef, UIntType, SIntType, WireKind, NodeKind}

object ReadableUtils {
  /**
   * Matches names of the following form:__<base>_<hash>_<tag>[_id]
   *   <base> - a named signal in the component's assigned expression
   *   <hash> - is the first 5 digits of the MD5 hash of the serialized assigned expression
   *   <tag>  - represents the general kind of assigned expression (register, mux, add, etc.)
   *   <id>   - optional, guarantees the name uniqueness
   */
  val FirrtlTempPattern = "^__(.+)_([0-9a-f]{5})_([A-Za-z]+)(?:_([0-9]+))?$".r
  /**
   * Matches names of the following form: T[_<id>]
   *   <id>   - optional, guarantees the name uniqueness
   */
  val ChiselTempPattern = "^T(?:_([0-9]+))?$".r
  def isChiselTemp(n: String): Boolean = {
    n match {
      case ChiselTempPattern(_*) => true
      case _ => false
    }
  }
  def isFIRRTLTemp(n: String): Boolean = {
    n match {
      case FirrtlTempPattern(_*) => true
      case _ => false
    }
  }
  /**
   * Returns a name if e is a WRef. Otherwise, error.
   */
  def getName(e: Expression) = e match { case e:WRef => e.name }

  /**
   * Returns true if e is a candidate.
   *
   * A candidate expression is one that is a wire/node,
   * is UIntType/SIntType, and has a Firrtl-generated
   * or Chisel-generated name
   */
  def isCandidate(e: Expression): Boolean = e match {
    case WRef(name, tpe, kind, gender) =>
      (tpe match {
        //TODO(izraelevitz): make work for aggregate types
      case (_: UIntType|_: SIntType) => true
      case _ => false
    }) && (kind match {
      case (_: WireKind|_: NodeKind) => true
      case _ => false
    }) && (isFIRRTLTemp(name) || isChiselTemp(name))
      case _ => false
  }
}
