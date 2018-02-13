package firrtlTests.graph

import java.io._
import org.scalatest._
import org.scalatest.prop._
import org.scalatest.Matchers._
import collection.mutable.{LinkedHashMap, LinkedHashSet}
import firrtl.graph._
import firrtlTests._

class DiGraphTests extends FirrtlFlatSpec {

  val acyclicGraph = DiGraph(LinkedHashMap(
    "a" -> LinkedHashSet("b","c"),
    "b" -> LinkedHashSet("d"),
    "c" -> LinkedHashSet("d"),
    "d" -> LinkedHashSet("e"),
    "e" -> Set.empty[String]))

  val reversedAcyclicGraph = DiGraph(LinkedHashMap(
    "a" -> Set.empty[String],
    "b" -> LinkedHashSet("a"),
    "c" -> LinkedHashSet("a"),
    "d" -> LinkedHashSet("b", "c"),
    "e" -> LinkedHashSet("d")))

  val cyclicGraph = DiGraph(LinkedHashMap(
    "a" -> LinkedHashSet("b","c"),
    "b" -> LinkedHashSet("d"),
    "c" -> LinkedHashSet("d"),
    "d" -> LinkedHashSet("a")))

  val tupleGraph = DiGraph(LinkedHashMap(
    ("a", 0) -> LinkedHashSet(("b", 2)),
    ("a", 1) -> LinkedHashSet(("c", 3)),
    ("b", 2) -> Set.empty[(String, Int)],
    ("c", 3) -> Set.empty[(String, Int)]
  ))

  val degenerateGraph = DiGraph(LinkedHashMap("a" -> Set.empty[String]))

  acyclicGraph.findSCCs.filter(_.length > 1) shouldBe empty

  cyclicGraph.findSCCs.filter(_.length > 1) should not be empty

  acyclicGraph.path("a","e") should not be empty

  an [PathNotFoundException] should be thrownBy acyclicGraph.path("e","a")

  acyclicGraph.linearize.head should equal ("a")

  a [CyclicException] should be thrownBy cyclicGraph.linearize

  acyclicGraph.reverse.getEdgeMap should equal (reversedAcyclicGraph.getEdgeMap)

  degenerateGraph.getEdgeMap should equal (degenerateGraph.reverse.getEdgeMap)

  "transformNodes" should "combine vertices that collide, not drop them" in {
    tupleGraph.transformNodes(_._1).getEdgeMap should contain ("a" -> Set("b", "c"))
  }

  def equalOrder[T](x: DiGraph[T], y: DiGraph[T]): Boolean = x
    .getEdgeMap
    .zip(y.getEdgeMap)
    .map{ case (k, v) => k == v }
    .reduce(_ && _)

  "Graph summation" should "be order-wise equivalent to original" in {
    val first = acyclicGraph.subgraph(LinkedHashSet("a", "b", "c"))
    val second = acyclicGraph.subgraph(LinkedHashSet("b", "c", "d", "e"))

    equalOrder(first + second, acyclicGraph) shouldBe true
  }

  it should "be idempotent" in {
    val first = acyclicGraph.subgraph(LinkedHashSet("a", "b", "c"))
    val second = acyclicGraph.subgraph(LinkedHashSet("b", "c", "d", "e"))

    equalOrder(first + second + second + second, acyclicGraph) shouldBe true
  }

}
