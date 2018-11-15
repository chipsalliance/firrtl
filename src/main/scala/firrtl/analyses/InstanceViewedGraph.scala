// See LICENSE for license details.

package firrtl.analyses

import firrtl.annotations.{GenericTarget, ReferenceTarget, Target}
import firrtl.annotations.TargetToken.{Instance, OfModule}
import firrtl.graph.{DiGraph, DiGraphLike}

import scala.collection.mutable

class InstanceViewedGraph(digraph: DiGraph[Target]) extends DiGraphLike[Target] {
  override val edges = digraph.getEdgeMap.asInstanceOf[mutable.LinkedHashMap[Target, mutable.LinkedHashSet[Target]]]
  override val prev = new mutable.LinkedHashMap[Target, Target]()

  override def getEdges(v: Target): collection.Set[Target] = {
    val genT = v.toGenericTarget
    val circuitOpt = genT.circuitOpt
    val pathTokens = genT.pathTokens
    val (parentModule, astModule) = pathTokens match {
      case Seq() => (None, genT.moduleOpt.get)
      case Seq(i, OfModule(o)) => (genT.moduleOpt, o)
      case seq if seq.size > 2 && seq.size % 2 == 0 =>
        val reversed = seq.reverse
        (Some(reversed(2).value), reversed.head.value)
    }
    val pathlessEdges = super.getEdges(Target.getPathlessTarget(v))
    pathlessEdges.map { t =>
      val genE = t.toGenericTarget
      genE match {
        // In same instance
        case GenericTarget(`circuitOpt`, Some(`astModule`), tokens) =>
          GenericTarget(circuitOpt, genT.moduleOpt, pathTokens ++ tokens).tryToComplete

        // In parent instance
        case  GenericTarget(`circuitOpt`, `parentModule`, tokens) =>
          GenericTarget(circuitOpt, genT.moduleOpt, pathTokens.dropRight(2) ++ tokens).tryToComplete

        // In child instance
        case  GenericTarget(`circuitOpt`, Some(childModule), tokens) =>
          val inst = v.complete.asInstanceOf[ReferenceTarget]
          val newPath = pathTokens ++ Seq(Instance(inst.ref), OfModule(childModule))
          GenericTarget(circuitOpt, genT.moduleOpt, newPath ++ tokens).tryToComplete
      }
    }
  }
}

