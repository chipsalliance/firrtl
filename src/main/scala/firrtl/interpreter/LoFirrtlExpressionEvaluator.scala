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

class LoFirrtlExpressionEvaluator(previousState: CircuitState, nextState: CircuitState) {
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
      case _ => throw new InterpreterException(s"Can't handle width for $primOp($a,$b)")
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
      case WRef(ref, tpe, kind, gender) =>
        previousState.inputPorts.foreach { case (port,value) =>
          if(port.name == ref) return value
        }
        previousState.registers.foreach {
          case (port: WRef, value) =>
            if(port.name == ref) return value
          case _ =>
            throw new InterpreterException(s"Could not resolve WRef $expression")
        }
        throw new InterpreterException(s"Could not resolve WRef $expression")
      case DoPrim(op, args, const, x) =>
        op match {
          case ADD_OP =>
            (evaluate(args(0)), evaluate(args(1))) match {
              case (a: UIntValue, b: UIntValue) => UIntValue(a.value + b.value, resolveWidth(op, a, b))
            }
          case SUB_OP =>
            (evaluate(args(0)), evaluate(args(1))) match {
              case (a: UIntValue, b: UIntValue) => UIntValue(a.value + b.value, resolveWidth(op, a, b))
            }
          case _ =>
            throw new InterruptedException(s"PrimOP $op in $expression not yet supported")
        }
      case c: ConcreteValue => c
    }
  }
}
