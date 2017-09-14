// See LICENSE for license details.

package firrtl.passes
package wiring

import firrtl._
import firrtl.ir._
import firrtl.Utils._
import firrtl.Mappers._
import scala.collection.mutable
import firrtl.annotations._
import firrtl.annotations.AnnotationUtils._
import firrtl.analyses.InstanceGraph
import WiringUtils._

case class WiringException(msg: String) extends PassException(msg)

case class WiringInfo(source: String, comp: String, sinks: Set[String], pin: String, top: String)

class Wiring(wiSeq: Seq[WiringInfo]) extends Pass {
  def run(c: Circuit): Circuit = {
    wiSeq.foldLeft(c) { (circuit, wi) => wire(circuit, wi) }
  }

  /** Add pins to modules and wires a signal to them, under the scope of a specified top module
    * Description:
    *   Adds a pin to each sink module
    *   Punches ports up from the source signal to the specified top module
    *   Punches ports down to each sink module
    *   Wires the source up and down, connecting to all sink modules
    * Restrictions:
    *   - Can only have one source module instance under scope of the specified top
    *   - All instances of each sink module must be under the scope of the specified top
    * Notes:
    *   - No module uniquification occurs (due to imposed restrictions)
    */
  def wire(c: Circuit, wi: WiringInfo): Circuit = {
    // Split out WiringInfo
    val source = wi.source
    val sinks = wi.sinks
    val compName = wi.comp
    val pin = wi.pin

    val iGraph = new InstanceGraph(c)

    // Create valid port names for wiring that have no name conflicts
    val portNames = c.modules.foldLeft(Map.empty[String, String]) { (map, m) =>
      map + (m.name -> {
        val ns = Namespace(m)
        if(sinks.contains(m.name)) ns.newName(pin)
        else ns.newName(tokenize(compName) filterNot ("[]." contains _) mkString "_")
      })
    }

    // Obtain the source component type
    val sourceComponentType = getType(c, source, compName)

    // 'meta' is a map of Modules to their pending modifications (Metadata)
    val meta = new mutable.HashMap[String, Metadata].withDefaultValue(Metadata())
    sinksToSources(sinks, source, wi.top, iGraph).map { case (sink, source) =>
      val lca = iGraph.lowestCommonAncestor(sink, source)

      // [Sink, ..., LCA] metadata
      sink.drop(lca.size - 1).sliding(2).toList.reverse.map {
        case Seq(WDefInstance(_,_,pm,_), WDefInstance(_,ci,cm,_)) =>
          val to = s"$ci.${portNames(cm)}"
          val from = s"${portNames(pm)}"
          meta(pm) = meta(pm).copy(
            addPort = Some((portNames(pm), DecWire)),
            cons    = (meta(pm).cons :+ (to, from)).distinct
          )
          meta(cm) = meta(cm).copy(
            addPort = Some((portNames(cm), DecInput))
          )
      }

      // Source metadata
      source.last match { case WDefInstance(_, name, module, _) =>
        val to = s"${portNames(module)}"
        val from = compName
        meta(module) = meta(module).copy(
          cons = Seq((to, from))
        )
      }

      // [Source, ..., LCA] metadata
      source.drop(lca.size - 1).sliding(2).toList.reverse.map {
        case Seq(WDefInstance(_,_,pm,_), WDefInstance(_,ci,cm,_)) => {
          val to = s"${portNames(pm)}"
          val from = s"$ci.${portNames(cm)}"
          meta(pm) = meta(pm).copy(
            cons    = (meta(pm).cons :+ (to, from)).distinct
          )
          meta(cm) = meta(cm).copy(
            addPort = Some((portNames(cm), DecOutput))
          )
        }
        case Seq(WDefInstance(_,_,pm,_)) => {
          val WDefInstance(_,ci,cm,_) = sink.drop(lca.size).head
          val to = s"$ci.${portNames(cm)}"
          val from = s"${portNames(pm)}"
          meta(pm) = meta(pm).copy(
            cons    = (meta(pm).cons :+ (to, from)).distinct
          )
        }
      }
    }

    // Return new circuit with correct wiring
    val cx = c.copy(modules = c.modules map onModule(meta.toMap, sourceComponentType))

    // Replace inserted IR nodes with WIR nodes
    ToWorkingIR.run(cx)
  }

  /** Return new module with correct wiring
    */
  private def onModule(map: Map[String, Metadata], t: Type)(m: DefModule) = {
    map.get(m.name) match {
      case None => m
      case Some(l) =>
        val stmts = mutable.ArrayBuffer[Statement]()
        val ports = mutable.ArrayBuffer[Port]()
        l.addPort match {
          case None =>
          case Some((s, dt)) => dt match {
            case DecInput => ports += Port(NoInfo, s, Input, t)
            case DecOutput => ports += Port(NoInfo, s, Output, t)
            case DecWire =>
              stmts += DefWire(NoInfo, s, t)
          }
        }
        stmts ++= (l.cons map { case ((l, r)) =>
          Connect(NoInfo, toExp(l), toExp(r))
        })
        def onStmt(s: Statement): Statement = Block(Seq(s) ++ stmts)
        m match {
          case Module(i, n, ps, s) => Module(i, n, ps ++ ports, Block(Seq(s) ++ stmts))
          case ExtModule(i, n, ps, dn, p) => ExtModule(i, n, ps ++ ports, dn, p)
        }
    }
  }

  /** Returns the type of the component specified
    */
  private def getType(c: Circuit, module: String, comp: String) = {
    def getRoot(e: Expression): String = e match {
      case r: Reference => r.name
      case i: SubIndex => getRoot(i.expr)
      case a: SubAccess => getRoot(a.expr)
      case f: SubField => getRoot(f.expr)
    }
    val eComp = toExp(comp)
    val root = getRoot(eComp)
    var tpe: Option[Type] = None
    def getType(s: Statement): Statement = s match {
      case DefRegister(_, n, t, _, _, _) if n == root =>
        tpe = Some(t)
        s
      case DefWire(_, n, t) if n == root =>
        tpe = Some(t)
        s
      case WDefInstance(_, n, m, t) if n == root =>
        tpe = Some(t)
        s
      case DefNode(_, n, e) if n == root =>
        tpe = Some(e.tpe)
        s
      case sx: DefMemory if sx.name == root =>
        tpe = Some(MemPortUtils.memType(sx))
        sx
      case sx => sx map getType
    }
    val m = c.modules find (_.name == module) getOrElse error(s"Must have a module named $module")
    tpe = m.ports find (_.name == root) map (_.tpe)
    m match {
      case Module(i, n, ps, b) => getType(b)
      case e: ExtModule =>
    }
    tpe match {
      case None => error(s"Didn't find $comp in $module!")
      case Some(t) =>
        def setType(e: Expression): Expression = e map setType match {
          case ex: Reference => ex.copy(tpe = t)
          case ex: SubField => ex.copy(tpe = field_type(ex.expr.tpe, ex.name))
          case ex: SubIndex => ex.copy(tpe = sub_type(ex.expr.tpe))
          case ex: SubAccess => ex.copy(tpe = sub_type(ex.expr.tpe))
        }
        setType(eComp).tpe
    }
  }
}
