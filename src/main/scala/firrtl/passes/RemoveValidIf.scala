package firrtl
package passes
import firrtl.Mappers._
import firrtl.ir._

// Removes ValidIf as an optimization
object RemoveValidIf extends Pass {
   def name = "Remove ValidIfs"
   // Recursive. Removes ValidIf's
   private def onExp(e: Expression): Expression = {
      e map onExp match {
         case ValidIf(cond, value, tpe) => value
         case x => x
      }
   }
   // Recursive.
   private def onStmt(s: Statement): Statement = s map onStmt map onExp

   private def onModule(m: DefModule): DefModule = m map onStmt

   def run(c: Circuit): Circuit = c map onModule
}
