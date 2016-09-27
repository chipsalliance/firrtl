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

package firrtlTests

import java.io._
import firrtl.Parser
import firrtl.ir._
import firrtl.Mappers._
import scala.collection.mutable

class FoldSpec extends FirrtlFlatSpec {

  /** Example using Map */
  def getDeclarationsMap(m: Module): Map[String, IsDeclaration] = {
    val decls = mutable.Map.empty[String, IsDeclaration]
    def onStmt(stmt: Statement): Statement = {
      stmt map onStmt match {
        case decl: IsDeclaration => decls += (decl.name -> decl)
        case _ => // do nothing
      }
      stmt // because map
    }
    m.ports foreach (p => decls += (p.name -> p))
    onStmt(m.body)
    decls.toMap // -> immutable
  }

  /** Get a Map of name to node for all Declarations */
  def getDeclarationsFold(m: Module): Map[String, IsDeclaration] = {
    def rec(node: FirrtlNode, acc: Map[String, IsDeclaration]): Map[String, IsDeclaration] = {
      node.foldLeft(acc) {
        case (map, n) =>
          val m = n match {
            case decl: IsDeclaration => map + (decl.name -> decl)
            case _ => map
          }
          rec(n, m)
      }
    }
    rec(m, Map.empty)
  }

  behavior of "FirrtlNode.fold"

  it should "support a simple test case" in {
    val circuit = 
     s"""circuit Test:
        |  module Test :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    output out : UInt<32>
        |   
        |    reg counter : UInt<32>, clk with :
        |      reset => (reset, UInt<32>(0))
        |    counter <= tail(add(counter, UInt<32>(1)), 1)
      """.stripMargin

    val c = Parser.parse(circuit.split("\n").toIterator)

    val module = c.modules.head.asInstanceOf[Module]
    val decls = getDeclarationsFold(module)
    val declsMap = getDeclarationsMap(module)
    assert(decls === declsMap)
    decls foreach {
      case ("clock", Port(_, "clock", Input, ClockType)) =>
      case ("reset", Port(_, "reset", Input, UIntType(IntWidth(w)))) if w == 1 =>
      case ("out", Port(_, "out", Output, UIntType(IntWidth(w)))) if w == 32 =>
      case ("counter", DefRegister(_, "counter", UIntType(IntWidth(w)), _,_,_)) if w == 32 =>
      case (name, decl) => throw new Exception(s"Unexpected declaration $name -> $decl")
    }
  }

}
