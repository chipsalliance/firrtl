// See LICENSE for license details.

package firrtl

import scala.collection._
import scala.io.Source
import java.io.{File, FileNotFoundException}

import firrtl.passes.wiring.WiringUtils


// Quick and dirty main to change the top of a circuit and delete all unused modules
object ReTop extends App {
  // Filters modules based on which was are in a particular subgraph
  def filterModules(toCheck: Set[String], cMap: Map[String, Set[String]], found: Set[String]): Set[String] = {
    if (toCheck.isEmpty) found
    else {
      val children = cMap(toCheck.head)
      val newMods = children -- found
      filterModules(toCheck.tail ++ newMods, cMap, found + toCheck.head)
    }
  }

  val newTop = args(0)
  val input = args(1)
  val output = args(2)

  val circuit = Parser.parse(Source.fromFile(input).getLines())

  val childMap = WiringUtils.getChildrenMap(circuit).toMap.map {
    case (parent, children) => parent -> children.map(_._2).toSet
  }
  val modsToKeep = filterModules(Set(newTop), childMap, Set.empty)

  val newCircuit = circuit.copy(main = newTop, modules = circuit.modules.filter(m => modsToKeep(m.name)))

  val outputFile = new java.io.PrintWriter(output)
  outputFile.write(newCircuit.serialize)
  outputFile.close()
}
