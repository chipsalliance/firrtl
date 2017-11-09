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

case class WiringInfo(source: Named, sinks: Seq[Named], pin: String)

class Wiring(wiSeq: Seq[WiringInfo]) extends Pass {
  def run(c: Circuit): Circuit = {
    wiSeq.foldLeft(c) { (circuit, wi) => wire(circuit, wi) }
  }

  /** Wires Sources to Sinks
    *
    * Sinks are wired to their closest source through their lowest
    * common ancestor (LCA). Verbosely, this modifies the circuit in
    * the following ways:
    *   - Adds a pin to each sink module
    *   - Punches ports up from source signals to the LCA of each sink
    *   - Punches ports down from LCAs to each sink module
    *   - Wires sources up to LCA, sinks down from LCA, and across each LCA
    *
    * @param c The circuit to modify
    * @param wi A WiringInfo object containing source, sink, component, and pin information
    * @return The modified circuit
    *
    * @throws WiringException if a sink is equidistant to two sources
    */
  def wire(c: Circuit, wi: WiringInfo): Circuit = {
    // Split out wiring info
    val (ComponentName(compName, ModuleName(source,_)), sinks, pin) = (wi.source, wi.sinks, wi.pin)
    val iGraph = new InstanceGraph(c)

    // Create valid port names for wiring that have no name conflicts
    val portNames = c.modules.foldLeft(Map.empty[String, String]) { (map, m) =>
      map + (m.name -> {
        val ns = Namespace(m)
        if(sinks.filter(namedName(_) == m.name).nonEmpty) ns.newName(pin)
        else ns.newName(tokenize(compName) filterNot ("[]." contains _) mkString "_")
      })
    }

    // Obtain the source component type
    val sourceComponentType = getType(c, source, compName)
    val sinkComponents = sinks
      .foldLeft(Map.empty[String, String]) { (map, m) =>
        map ++ { m match {
          case ComponentName(c, ModuleName(m, _)) => Map(m -> c)
          case _ => Map()
        }}}

    // Determine "ownership" of sources to sinks via minimum distance
    val owners = sinksToSources(sinks, source, iGraph)

    // Determine port and pending modifications (Metadata) for all
    // sink--source ownership pairs
    val meta = new mutable.HashMap[String, Metadata].withDefaultValue(Metadata())
    owners.map { case (sink, source) =>
      val lca = iGraph.lowestCommonAncestor(sink, source)

      // Compute metadata along Sink to LCA paths
      sink.drop(lca.size - 1).sliding(2).toList.reverse.map {
        case Seq(WDefInstance(_,_,pm,_), WDefInstance(_,ci,cm,_)) =>
          val to = s"$ci.${portNames(cm)}"
          val from = s"${portNames(pm)}"
          meta(pm) = meta(pm).copy(
            addPort = Some((portNames(pm), DecWire)),
            cons = (meta(pm).cons :+ (to, from)).distinct
          )
          meta(cm) = meta(cm).copy(
            addPort = Some((portNames(cm), DecInput))
          )
        case Seq(WDefInstance(_,_,pm,_)) =>
          val WDefInstance(_,ci,cm,_) = source.drop(lca.size).head
          val to = s"${portNames(pm)}"
          val from = s"$ci.${portNames(cm)}"
          meta(pm) = meta(pm).copy(
            addPort = Some((portNames(pm), DecWire)),
            cons = (meta(pm).cons :+ (to, from)).distinct
          )
      }

      // Compute metadata for the Sink
      sink.last match { case WDefInstance(_, name, module, _) =>
        if (sinkComponents.contains(module)) {
          val to = sinkComponents(module)
          val from = s"${portNames(module)}"
          meta(module) = meta(module).copy(
            cons = Seq((to, from))
          )
        }
      }

      // Compute metadata for the Source
      source.last match { case WDefInstance(_, name, module, _) =>
        val to = s"${portNames(module)}"
        val from = compName
        meta(module) = meta(module).copy(
          cons = Seq((to, from))
        )
      }

      // Compute metadata along Source to LCA path
      source.drop(lca.size - 1).sliding(2).toList.reverse.map {
        case Seq(WDefInstance(_,_,pm,_), WDefInstance(_,ci,cm,_)) => {
          val to = s"${portNames(pm)}"
          val from = s"$ci.${portNames(cm)}"
          meta(pm) = meta(pm).copy(
            cons = (meta(pm).cons :+ (to, from)).distinct
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
            cons = (meta(pm).cons :+ (to, from)).distinct
          )
        }
      }
    }

    // Return new circuit with correct wiring by applying all metadata
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
        val defines = mutable.ArrayBuffer[Statement]()
        val connects = mutable.ArrayBuffer[Statement]()
        val ports = mutable.ArrayBuffer[Port]()
        l.addPort match {
          case None =>
          case Some((s, dt)) => dt match {
            case DecInput  => ports += Port(NoInfo, s, Input, t)
            case DecOutput => ports += Port(NoInfo, s, Output, t)
            case DecWire   => defines += DefWire(NoInfo, s, t)
          }
        }
        connects ++= (l.cons map { case ((l, r)) =>
          Connect(NoInfo, toExp(l), toExp(r))
        })
        m match {
          case Module(i, n, ps, s) => Module(i, n, ps ++ ports,
            Block(defines ++ Seq(s) ++ connects))
          case ExtModule(i, n, ps, dn, p) => ExtModule(i, n, ps ++ ports, dn, p)
        }
    }
  }
}
