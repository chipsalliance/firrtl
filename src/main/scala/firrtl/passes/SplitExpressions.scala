// See LICENSE for license details.

package firrtl
package passes

import firrtl.{SystemVerilogEmitter, VerilogEmitter}
import firrtl.ir._
import firrtl.options.PreservesAll
import firrtl.Mappers._
import firrtl.Utils.{kind, gender, get_info}

// Datastructures
import scala.collection.mutable

// Splits compound expressions into simple expressions
//  and named intermediate nodes
class SplitExpressions extends Pass with PreservesAll[Transform] {

  override val prerequisites: Seq[Class[Transform]] = firrtl.stage.Forms.LowForm ++
    Seq[Class[Transform]]( classOf[passes.RemoveValidIf],
                           classOf[firrtl.passes.memlib.VerilogMemDelays] )

  override val dependents: Seq[Class[Transform]] = Seq(classOf[SystemVerilogEmitter], classOf[VerilogEmitter])

   private def onModule(m: Module): Module = {
      val namespace = Namespace(m)
      def onStmt(s: Statement): Statement = {
        val v = mutable.ArrayBuffer[Statement]()
        // Splits current expression if needed
        // Adds named temporaries to v
        def split(e: Expression): Expression = e match {
          case e: DoPrim =>
            val name = namespace.newTemp
            v += DefNode(get_info(s), name, e)
            WRef(name, e.tpe, kind(e), gender(e))
          case e: Mux =>
            val name = namespace.newTemp
            v += DefNode(get_info(s), name, e)
            WRef(name, e.tpe, kind(e), gender(e))
          case e: ValidIf =>
            val name = namespace.newTemp
            v += DefNode(get_info(s), name, e)
            WRef(name, e.tpe, kind(e), gender(e))
          case _ => e
        }

        // Recursive. Splits compound nodes
        def onExp(e: Expression): Expression =
          e map onExp match {
            case ex: DoPrim => ex map split
            case ex => ex
         }

        s map onExp match {
           case x: Block => x map onStmt
           case EmptyStmt => EmptyStmt
           case x =>
             v += x
             v.size match {
               case 1 => v.head
               case _ => Block(v.toSeq)
             }
        }
      }
      Module(m.info, m.name, m.ports, onStmt(m.body))
   }
   def run(c: Circuit): Circuit = {
     val modulesx = c.modules map {
       case m: Module => onModule(m)
       case m: ExtModule => m
     }
     Circuit(c.info, modulesx, c.main)
   }
}

object SplitExpressions extends Pass with DeprecatedPassObject {

  override protected lazy val underlying = new SplitExpressions

}
