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

import scala.collection.mutable

import firrtl.Utils.{BoolType, AND, OR, NOT, one, zero, toWrappedExpression}
import firrtl.Mappers.{ExpMap, StmtMap, ModuleMap}

/** Operates on High Firrtl. Returns High Firrtl.
  *
  */
class Coverage(clockName: String, resetName: String) extends Transform {
   def name = "Coverage"
   def execute(circuit: Circuit, annotations: Seq[CircuitAnnotation]) =
     TransformResult(run(circuit))
   def run(c: Circuit): Circuit = {
      var whenID = 0
      def uniqueNumber: Int = {
         val n = whenID
         whenID = whenID + 1
         n
      }
      def coverage(m: Module): Module = {
         case class Data(name: String, guard: Expression, info: Info, id: Int)
         val namespace = Namespace(m)
         val refSet = mutable.Set[Data]()
         def getRef(guard: Expression, info: Info) = {
            val id = uniqueNumber
            val name = namespace.newName(s"trigger__$id")
            refSet += Data(name, guard, info, id)
            WRef(name, BoolType(), RegKind(), FEMALE)
         }
         def onStmt(guard: Expression)(s: Stmt): Stmt = {
            s match {
               case Conditionally(info, pred, con, alt) =>
                  val cGuard = AND(guard, pred)
                  val aGuard = AND(guard, NOT(pred))
                  val conx = Begin(Seq(con map onStmt(cGuard), Connect(info, getRef(cGuard, info), one)))
                  val altx = Begin(Seq(alt map onStmt(aGuard), Connect(info, getRef(aGuard, info), one)))
                  Conditionally(info, pred, conx, altx)
               case s => s map onStmt(guard)
            }
         }
         def fixStmt(s: Stmt): Stmt = {
            val sx = onStmt(one)(s)
            val stmts = mutable.ArrayBuffer[Stmt]()
            val clk = WRef(clockName, ClockType(), ExpKind(), MALE)
            val reset = WRef(resetName, BoolType(), ExpKind(), MALE)
            val start = WRef(namespace.newName("start__"), BoolType(), RegKind(), FEMALE)
            stmts += DefRegister(NoInfo, start.name, BoolType(), clk, reset, zero)
            stmts += Connect(NoInfo, start, one)
            val second = WRef(namespace.newName("delayStart__"), BoolType(), RegKind(), FEMALE)
            stmts += DefRegister(NoInfo, second.name, BoolType(), clk, reset, zero)
            stmts += Connect(NoInfo, second, start)
            for (data <- refSet) {
               val id = data.id
               val regTrigger = WRef(data.name, BoolType(), ExpKind(), MALE)
               val regDelayTrigger = WRef(namespace.newName(s"delayTrigger__$id"), BoolType(), RegKind(), FEMALE)
               val flag = WRef(namespace.newName(s"flag__$id"), BoolType(), ExpKind(), UNKNOWNGENDER)
               stmts += DefRegister(data.info, regTrigger.name, BoolType(), clk, reset, zero)
               stmts += DefRegister(data.info, regDelayTrigger.name, BoolType(), clk, reset, zero)
               stmts += Connect(data.info, regDelayTrigger, regTrigger)
               stmts += DefWire(data.info, flag.name, BoolType())
               stmts += Connect(data.info, flag, AND(regTrigger, NOT(regDelayTrigger)))
               val msgOnReset = FIRRTLStringLitHandler.unescape(s"#coverage #info $id: when ${data.guard.serialize}, ${data.info}, ${m.name}.\\n")
               stmts += Print(data.info, msgOnReset, Seq.empty, clk, AND(NOT(reset), AND(NOT(second), start)))
               val msgOnWhen = FIRRTLStringLitHandler.unescape(s"#coverage #trigger $id!\\n")
               stmts += Print(data.info, msgOnWhen, Seq.empty, clk, flag)
            }
            Begin(stmts.toSeq ++ Seq(sx))
         }
         m map fixStmt
      }
      Circuit(c.info, c.modules map coverage, c.main)
   }
}

