package firrtl
package passes

import firrtl.Mappers.{StmtMap, ExpMap, ModuleMap}
import firrtl.Utils.tpe
import scala.collection.mutable
import ReadableUtils.{getName, isCandidate}

/**
 * InlineSASR (SASR = single assignment, single read) inlines references that
 * are all of the following:
 *   - Chisel or Firrtl generated
 *   - Wire or node
 *   - Ground type (UIntType or SIntType)
 *   - Read from once and assigned to once
 */
object InlineSASR extends Pass {
   /**
    * Inline SASR = Inline Single Assignment Single Read
    */
   def name = "Inline SASR"
   /**
    * Returns new Module with references that are assigned once and read from once
    * that are legal candidates
    */
   private def onModule(m: Module): Module = {
      // Hashes the value of candidates for inlining
      val candidateValue = mutable.HashMap[String, Expression]()

      // Counts the number of reads per candidate
      val candidateReads = mutable.HashMap[String, Int]()

      // Counts the number of writes per candidate
      val candidateWrites = mutable.HashMap[String, Int]()

      /**
       * Adds an expression if it is a legal candidate, increments
       * candidateWrites
       */
      def addExpression(e: Expression, value: Expression): Unit =
         if (isCandidate(e)) {
            val name = getName(e)
            candidateValue(name) = value
            candidateWrites(name) = candidateWrites.getOrElse(name, 0) + 1
         }

      /**
       * Recursive. Builds candidateValue/candidateReads/candidateWrites
       */
      def buildExp(e: Expression): Expression = {
         (e map buildExp) match {
            case WRef(name, tpe, kind, gender) if ((gender == MALE) && candidateValue.contains(name)) =>
               candidateReads(name) = candidateReads.getOrElse(name, 0) + 1
               e
            case _ => e
         }
      }

      /**
       * Recursive. Builds candidateValue/candidateReads/candidateWrites
       */
      def buildStmt(s: Stmt): Stmt = (s map buildExp) match {
         case s: Connect =>
            addExpression(s.loc, s.exp)
            s
         case s: DefNode =>
            addExpression(WRef(s.name, tpe(s.value), NodeKind(), MALE), s.value)
            s
         case s => s map buildStmt
      }

      /**
       * Returns true if candidate expression can be legally inlined
       */
      def acceptCandidate(e: Expression): Boolean = e match {
         case WRef(name, tpe, kind, gender) =>
            candidateValue.contains(name) &&
              (candidateReads.getOrElse(name, 0) == 1) &&
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
              (candidateReads.getOrElse(name, 0) == 1) &&
              (candidateWrites.getOrElse(name, 0) == 1) &&
              (gender == FEMALE) //If a WRef is FEMALE, it is being assigned to
         case _ => false
      }

      /**
       * Recursive. Inlines accepted candidates.
       */
      def onExp(e: Expression): Expression = {
         // Recursive. Returns a deeply inlined expression.
         def getValue(e: Expression): Expression =
            if (acceptCandidate(e)) {
               val newValue = candidateValue(getName(e))
               getValue(newValue)
            } else e
         getValue(e) map onExp
      }

      /**
       * Recursive. Inlines accepted candidates.
       */
      def onStmt(s: Stmt): Stmt = {
         s match {
            // Note: DO NOT call onExp on e.exp if acceptCandidate(e.loc) is
            //       true, could result in an infinite loop
            case s: Connect =>
               if (assignedCandidate(s.loc)) Empty()
               else (s map onExp)
            case s: DefNode =>
               if (acceptCandidate(WRef(s.name, tpe(s.value), NodeKind(), MALE))) Empty()
               else (s map onExp)
            case s: DefWire =>
               if (acceptCandidate(WRef(s.name, s.tpe, WireKind(), MALE))) Empty()
               else (s map onExp)
            case s => (s map onExp) map onStmt
         }
      }

      m match {
         case m: InModule =>
            buildStmt(m.body)
            InModule(m.info, m.name, m.ports, onStmt(m.body))
         case m: ExModule => m
      }
   }
   def run(c: Circuit): Circuit = Circuit(c.info, c.modules.map(onModule _), c.main)
}
