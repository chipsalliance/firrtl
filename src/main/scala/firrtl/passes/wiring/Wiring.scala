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
    val updates = analyze(c, wiSeq)

    updates.foldLeft(c){ case (circuit, (u, t)) => wire(circuit, u, t) }
  }

  /** Converts multiple units of wiring information to module modifications
    *
    * @param c a [[Circuit]]
    * @param wiSeq a sequence of wiring information
    */
  private def analyze(c: Circuit, wiSeq: Seq[WiringInfo]):
      Seq[(Map[String, Modifications], Type)] = {
    val zipped: Seq[(String, String, Seq[Named], String)] = wiSeq
      .map( wi => (wi.source, wi.sinks, wi.pin) match {
             case (ComponentName(compName, ModuleName(source,_)), sinks, pin) =>
               (compName, source, sinks, pin)
           })

    val portNames = mutable.Seq.fill(zipped.size)(Map[String, String]())
    c.modules.foreach{ m =>
      val ns = Namespace(m)
      zipped.zipWithIndex.foreach{ case ((c, so, si, p), i) =>
        portNames(i) = portNames(i) +
        (m.name -> {
           if (si.exists(moduleName(_) == m.name)) ns.newName(p)
           else ns.newName(tokenize(c) filterNot ("[]." contains _) mkString "_")
         })
      }
    }

    val (timeIGraph, iGraph) = firrtl.Utils.time { new InstanceGraph(c) }
    logger.info(s"[info]     time(iGraph): $timeIGraph")

    val (timeMillis, x) = firrtl.Utils.time {
      zipped.zip(portNames).map{ case((comp, so, si, _), pn) =>
        computeModifications(c, iGraph, comp, so, si, pn) } }
    logger.info(s"[info]   time(computeModifications): $timeMillis")
    x
  }

  /** Converts a single unit of wiring information to module modifications
    *
    * @param c the circuit that will be modified
    * @param iGraph an InstanceGraph representation of the circuit
    * @param compName the name of a component
    * @param source the name of the source component
    * @param sinks a list of sink components/modules that the source
    * should be connected to
    * @param portNames a mapping of module names to new ports/wires
    * that should be generated if needed
    *
    * @return a tuple of a map of module names to modifications and
    * the determined type of the component
    */
  private def computeModifications(c: Circuit, iGraph: InstanceGraph,
                                   compName: String, source: String,
                                   sinks: Seq[Named],
                                   portNames: Map[String, String]):
      (Map[String, Modifications], Type) = {

    // Obtain the source component type
    val sourceComponentType = getType(c, source, compName)
    val sinkComponents = sinks
      .collect { case ComponentName(c, ModuleName(m, _)) => m -> c }.toMap

    // Determine "ownership" of sources to sinks via minimum distance
    val owners = sinksToSources(sinks, source, iGraph)

    // Determine port and pending modifications for all sink--source
    // ownership pairs
    val meta = new mutable.HashMap[String, Modifications]
      .withDefaultValue(Modifications())
    owners.foreach { case (sink, source) =>
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
    (meta.toMap, sourceComponentType)
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
  def wire(c: Circuit, meta: Map[String, Modifications], tpe: Type): Circuit = {
    val cx = c.copy(modules = c.modules map onModule(meta, tpe))

    // Replace inserted IR nodes with WIR nodes
    ToWorkingIR.run(cx)
  }

  /** Return new module with correct wiring
    */
  private def onModule(map: Map[String, Modifications], t: Type)(m: DefModule) = {
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
