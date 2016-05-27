/*
Copyright (c) 2014 - 2016 The Regents of the University of
California (Regents). All Rights Reserved.  Redistribution and use in
source and binary forms, with or without modification, are permitted
provided that the following conditions are met:
   * Redistributions of source code must retain the above
     copyright notice, this list of conditions and the following
     two paragraphs of disclaimer.
   * Redistributions in binary form must reproduce the above
     copyright notice, this list of conditions and the following
     two paragraphs of disclaimer in the documentation and/or other materials
     provided with the distribution.
   * Neither the name of the Regents nor the names of its contributors
     may be used to endorse or promote products derived from this
     software without specific prior written permission.
IN NO EVENT SHALL REGENTS BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS,
ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
REGENTS HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE. THE SOFTWARE AND ACCOMPANYING DOCUMENTATION, IF
ANY, PROVIDED HEREUNDER IS PROVIDED "AS IS". REGENTS HAS NO OBLIGATION
TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
MODIFICATIONS.
*/
package firrtl
package passes

import firrtl.Utils.tpe
import scala.collection.mutable
import ReadableUtils._
import Mappers.{ExpMap, StmtMap, ModuleMap}


/**
 * Inlines references that are all of the following:
 *   - Chisel or Firrtl generated
 *   - Wire or node
 *   - Ground type (UIntType or SIntType)
 *   - Read from at least once and assigned to once
 *   - Do not reference additional hardware (i.e. duplication
 *      from inlining is free)
 */
object InlineSAMR extends Pass {
  def name = "Inline SAMR"

  /**
   * Returns true if the expression will not generate
   * additional hardware
   */
  private def noLogic(e: Expression): Boolean = {
    var noLogic = true

    /**
     * Recursively walks expression to find expressions that create hardware
     */
    def recur(e: Expression): Expression = {
      (e map recur) match {
        case (_: Mux|_: ValidIf) => noLogic = false
        case e: DoPrim => if (!Seq(AS_UINT_OP, AS_SINT_OP, AS_CLOCK_OP,
          CONVERT_OP, CONCAT_OP).contains(e.op)) noLogic = false
        case _ => {}
      }; e
    }
    recur(e)
    noLogic
  }

  /**
   * Returns new Module with references that are assigned once, don't generate
   * hardware, have a generated name, are a ground type, and are either a wire
   * or a node.
   */
  private def onModule(m: Module): Module = {
    val candidateValue = mutable.HashMap[String, Expression]()
    val candidateWrites = mutable.HashMap[String, Int]()
    def maybeAddAssign(e: Expression, value: Expression): Unit =
      if (isCandidate(e) && noLogic(value)) {
        candidateValue(getName(e)) = value
        candidateWrites(getName(e)) = candidateWrites.getOrElse(getName(e), 0) + 1
      }

    /**
     * Returns true if candidate expression can be legally inlined
     */
    def replaceCandidate(e: Expression): Boolean = e match {
      case WRef(name, tpe, kind, gender) =>
        candidateValue.contains(name) &&
        (candidateWrites.getOrElse(name, 0) == 1) &&
        (gender == MALE) //If a WRef is MALE, it is being referenced
      case _ => false
    }

    /**
     * Returns true if assigning to candidate expression
     */
    def assignedCandidate(e: Expression): Boolean = e match {
      case WRef(name, tpe, kind, gender) =>
        candidateValue.contains(name) &&
        (candidateWrites.getOrElse(name, 0) == 1) &&
        (gender == FEMALE) //If a WRef is FEMALE, it is being assigned to
      case _ => false
    }

    /**
     * Recursive. Builds candidateValue/candidateWrites
     */
    def buildStmt(s: Stmt): Stmt = {
      s match {
        case s: Connect => maybeAddAssign(s.loc, s.exp)
        case s: DefNode => maybeAddAssign(WRef(s.name, tpe(s.value), NodeKind(), MALE), s.value)
        //TODO(izraelevitz): Add case for IsInvalid as an optimization
        case s => s map buildStmt
      }; s
    }

    /**
     * Recursive. Inlines accepted candidates.
     */
    def onExp(e: Expression): Expression =
      if (replaceCandidate(e)) candidateValue(getName(e))
      else (e map onExp)

      /**
       * Recursive. Inlines accepted candidates.
       */
      def onStmt(s: Stmt): Stmt = s match {
        case s: Connect =>
          if (assignedCandidate(s.loc)) Empty()
          else (s map onExp)
        case s: DefNode =>
          if (replaceCandidate(WRef(s.name, tpe(s.value), NodeKind(), MALE))) Empty()
          else (s map onExp)
        case s: DefWire =>
          if (replaceCandidate(WRef(s.name, s.tpe, WireKind(), MALE))) Empty()
          else (s map onExp)
        case s => (s map onExp) map onStmt
      }

      m map {s: Stmt => onStmt(buildStmt(s))}
  }

  def run(c: Circuit): Circuit = Circuit(c.info, c.modules.map(onModule _), c.main)
}

