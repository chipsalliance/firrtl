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

import util.Random

import firrtl.Mappers.{ExpMap, StmtMap, ModuleMap}
import firrtl.Annotations.{
  Named,
  CircuitName,
  TransID,
  Sticky,
  Loose,
  Annotation,
  AnnotationMap
}

/**
 * Determines the behavior of the replaced ValidIf when it's predicate is
 * false.
 */
sealed trait ValidIfBehavior

/**
 * When a ValidIf's predicate is false, return a random value.
 */
case class RBehavior(randomGenerator: Random) extends ValidIfBehavior

/**
 * When a ValidIf's predicate is false, return the same expression if the
 * predicate was true
 */
case object DBehavior extends ValidIfBehavior

/**
 * Passed through compiler, consumed by ReplaceValidIf
 */
case class ReplaceValidIfAnnotation(behavior: ValidIfBehavior, target: Named, tID: TransID) extends Annotation with Sticky with Loose {
  def duplicate(n: Named) = this.copy(target=n)
}


/**
 * Replaces all instances of ValidIf with an expression determined by the
 * ValidIfBehavior contained in ReplaceValidIfAnnotation.
 */
class ReplaceValidIf(tID: TransID) extends Transform {
  def name = "Replace ValidIfs"

  /**
   * Extract behavior from AnnotationMap. Error if incorrect annotation, and do
   * the default behavior if no annotation exists. Calls run to return the
   * transformed circuit.
   */
  def execute (circuit: Circuit, annotations: AnnotationMap): TransformResult = {
    val behavior = annotations.get(tID) match {
      case None => DBehavior
      case Some(map) =>
        map.get(CircuitName(circuit.main)) match {
          case Some(ReplaceValidIfAnnotation(b, _, _)) => b
          case Some(_) =>
            throw new PassException(s"$name's annotation must be ReplaceValidIfAnnotation")
          case None => DBehavior
        }
      }
    TransformResult(run(circuit, behavior))
  }

  /**
   * Walks circuit and replaces all instances of ValidIf with a corresponding
   * expression determined by behavior.
   */
  def run(c: Circuit, behavior: ValidIfBehavior): Circuit = {
    def replace(e: ValidIf): Expression = behavior match {
      case DBehavior => e.value
      case RBehavior(randomGenerator) =>
        val fval = e.tpe match {
          case UIntType(IntWidth(intwidth)) =>
            UIntValue(BigInt(intwidth.toInt, randomGenerator), IntWidth(intwidth))
          case SIntType(IntWidth(intwidth)) =>
            SIntValue(BigInt(intwidth.toInt, randomGenerator), IntWidth(intwidth))
          case _ => throw new PassException(s"Cannot replace validif with type ${e.tpe}.")
        }
        Mux(e.cond, e.value, fval, e.tpe)
    }
    def onStmt(s: Stmt): Stmt = (s map onStmt) map onExp
    def onModule(m: Module): Module = m map onStmt
    def onExp(e: Expression): Expression = (e map onExp) match {
      case v: ValidIf => replace(v)
      case x => x
    }
    Circuit(c.info, c.modules.map(onModule _), c.main)
  }
}
