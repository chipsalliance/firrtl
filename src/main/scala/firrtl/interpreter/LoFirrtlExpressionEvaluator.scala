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

package firrtl.interpreter

import firrtl._

import collection.mutable

/**
  * This is the evaluation engine for the FirrtlTerp
  * it requires the previousState of the system
  *
  * @param cicuitState  the state of the system, should not be modified before all dependencies have been resolved
  */
class LoFirrtlExpressionEvaluator(
                                   startKeys: Iterable[String],
                                   dependencyGraph: DependencyGraph,
                                   cicuitState: CircuitState) {
  val toResolve = mutable.HashSet(startKeys.toSeq:_*)
  val inProcess = toResolve.empty

  def resolveWidth(primOp: PrimOp, a: ConcreteValue, b: ConcreteValue): IntWidth = {
    primOp match {
      case ADD_OP =>
        (a.width, b.width) match {
          case (a_width: IntWidth, b_width: IntWidth) => IntWidth(a_width.width.max(b_width.width))
          case _ => throw new InterpreterException(s"Can't handle width for $primOp($a,$b)")
        }
      case SUB_OP =>
        (a.width, b.width) match {
          case (a_width: IntWidth, b_width: IntWidth) => IntWidth(a_width.width.max(b_width.width))
          case _ => throw new InterpreterException(s"Can't handle width for $primOp($a,$b)")
        }
      case MUL_OP =>
        (a.width, b.width) match {
          case (a_width: IntWidth, b_width: IntWidth) => IntWidth(a_width.width * b_width.width)
          case _ => throw new InterpreterException(s"Can't handle width for $primOp($a,$b)")
        }
      case _ => throw new InterpreterException(s"Can't handle width for $primOp($a,$b)")
    }
  }

  def getValue(key: String): ConcreteValue = {
    cicuitState.getValue(key).getOrElse {
      resolveDependency(key)
    }
  }

  def evaluate(expression: Expression): ConcreteValue = {
    expression match {
      case mux: Mux =>
        if( evaluate(mux.cond).value > 0 ) {
          evaluate(mux.tval)
        }
        else {
          evaluate(mux.fval)
        }
      case WRef(name, tpe, kind, gender) => getValue(name)
      case DoPrim(op, args, const, x) =>
        op match {
          case ADD_OP =>
            (evaluate(args.head), evaluate(args.tail.head)) match {
              case (a: UIntValue, b: UIntValue) => UIntValue(a.value + b.value, resolveWidth(op, a, b))
            }
          case SUB_OP =>
            (evaluate(args.head), evaluate(args.tail.head)) match {
              case (a: UIntValue, b: UIntValue) => UIntValue(a.value + b.value, resolveWidth(op, a, b))
            }
          case MUL_OP =>
            (evaluate(args.head), evaluate(args.tail.head)) match {
              case (a: UIntValue, b: UIntValue) => UIntValue(a.value * b.value, resolveWidth(op, a, b))
            }
          case EQUAL_OP =>
            (evaluate(args.head), evaluate(args.tail.head)) match {
              case (a: UIntValue, b: UIntValue) => UIntValue(if(a.value == b.value) 1 else 0, IntWidth(1))
            }
          case GREATER_OP =>
            (evaluate(args.head), evaluate(args.tail.head)) match {
              case (a: UIntValue, b: UIntValue) => UIntValue(if(a.value > b.value) 1 else 0, IntWidth(1))
            }
          case _ =>
            throw new InterruptedException(s"PrimOP $op in $expression not yet supported")
        }
      case c: ConcreteValue => c
    }
  }

  private def resolveDependency(key: String): ConcreteValue = {
    toResolve -= key

    println(s"resolveDependency: $key")
    val value = if(cicuitState.isInput(key)) {
      cicuitState.getValue(key).get
    }
    else {
      val expression = dependencyGraph.nameToExpression(key)
      evaluate(expression)
    }
    println(s"resolveDependency: $key <= $value")

    cicuitState.setValue(key, value)
  }

  def resolveDependencies(): Unit = {
    while (toResolve.nonEmpty) {
      val key = toResolve.head
      resolveDependency(key)
    }
  }
}
