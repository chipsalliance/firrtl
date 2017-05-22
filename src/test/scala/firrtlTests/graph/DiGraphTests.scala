package firrtlTests.graph

import firrtl.util.DiGraph
import firrtlTests._

class DiGraphTests extends FirrtlFlatSpec {

  val acyclicGraph = DiGraph(Map(
    "a" -> Set("b","c"),
    "b" -> Set("d"),
    "c" -> Set("d"),
    "d" -> Set("e"),
    "e" -> Set.empty[String]))

  val cyclicGraph = DiGraph(Map(
    "a" -> Set("b","c"),
    "b" -> Set("d"),
    "c" -> Set("d"),
    "d" -> Set("a")))


  acyclicGraph.findSCCs.filter(_.length > 1) shouldBe empty

  cyclicGraph.findSCCs.filter(_.length > 1) should not be empty

  acyclicGraph.path("a","e") should not be empty

  an [acyclicGraph.PathNotFoundException] should be thrownBy acyclicGraph.path("e","a")

  acyclicGraph.linearize.head should equal ("a")

  a [cyclicGraph.CyclicException] should be thrownBy cyclicGraph.linearize

}
