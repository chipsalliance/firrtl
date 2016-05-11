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

/**
  * Works a lot like the chisel classic tester
  *
  * @param input a firrtl program contained in a string
  */
class InterpretiveTester(input: String) {
  val interpreter = FirrtlTerp(input)

  def setVerbose(value: Boolean = true): Unit = {
    interpreter.setVerbose(value)
  }

  def poke(name: String, value: BigInt): Unit = {
    interpreter.setValueWithBigInt(name, value)
  }

  def peek(name: String): BigInt = {
    interpreter.getValue(name) match {
      case ConcreteUInt(value, _) => value
      case ConcreteSInt(value, _) => value
      case _ => throw new InterpreterException(s"Error:peek($name) value not found")
      }
  }

  def expect(name: String, expectedValue: BigInt): Unit = {
    def testValue(value: BigInt): Unit = {
      if (value != expectedValue) {
        if(! interpreter.verbose) interpreter.reEvaluate(name)
        throw new InterpreterException (s"Error:expect($name, $expectedValue) got $value")
      }
    }
    interpreter.getValue(name) match {
      case ConcreteUInt (value, _) => testValue(value)
      case ConcreteSInt(value, _)  => testValue(value)
      case _ =>
        throw new InterpreterException(s"Error:expect($name, $expectedValue) value not found")
    }
  }

  def step(n: Int = 1): Unit = {
    for(_ <- 0 until n) {
      interpreter.cycle()
    }
  }
}
