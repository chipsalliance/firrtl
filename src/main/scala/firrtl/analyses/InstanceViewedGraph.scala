// See LICENSE for license details.

package firrtl.analyses

import firrtl.annotations.{GenericTarget, ReferenceTarget, Target}
import firrtl.annotations.TargetToken.{Instance, OfModule, Ref, Field}
import firrtl.graph.{DiGraph, DiGraphLike}

import scala.collection.mutable

class InstanceViewedGraph(digraph: DiGraph[Target]) extends DiGraphLike[Target] {
  override val edges = digraph.getEdgeMap.asInstanceOf[mutable.LinkedHashMap[Target, mutable.LinkedHashSet[Target]]]

  override def getEdges(v: Target, prevOpt: Option[collection.Map[Target, Target]] = None): collection.Set[Target] = {
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
    pathlessEdges.flatMap { t =>
      val genE = t.toGenericTarget
      genE match {
        // In same instance
        case GenericTarget(`circuitOpt`, Some(`astModule`), tokens) =>
          Seq(GenericTarget(circuitOpt, genT.moduleOpt, pathTokens ++ tokens).tryToComplete)

        // In parent instance
        case GenericTarget(`circuitOpt`, `parentModule`, tokens) =>
          Seq(GenericTarget(circuitOpt, genT.moduleOpt, pathTokens.dropRight(2) ++ tokens).tryToComplete)

        case GenericTarget(`circuitOpt`, Some(otherModule), tokens) =>
          (genT.tokens, tokens) match {
            // In parent but instantiates root module
            case (Ref(modPort) +: modRest, Ref(inst) +: Field(instPort) +: instRest) if modPort == instPort && modRest == instRest =>  Nil

            // In child instance
            case (Ref(inst) +: Field(instPort) +: instRest, Ref(modPort) +: modRest,) if modPort == instPort && modRest == instRest =>
              val inst = v.complete.asInstanceOf[ReferenceTarget]
              val newPath = pathTokens ++ Seq(Instance(inst.ref), OfModule(otherModule))
              Seq(GenericTarget(circuitOpt, genT.moduleOpt, newPath ++ tokens).tryToComplete)
          }
      }
    }
  }
}

