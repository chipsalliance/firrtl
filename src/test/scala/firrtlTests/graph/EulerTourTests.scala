package firrtlTests.graph

import firrtl.graph._
import firrtlTests._

class EulerTourTests extends FirrtlFlatSpec {

  val top = "0a"
  val first_layer = Seq("1a", "1b", "1c")
  val second_layer = Seq("2a", "2b", "2c")
  val third_layer = Seq("3a", "3b", "3c")

  val graph = DiGraph((
    Map("a" -> first_layer.toSet) ++
      first_layer.map{ case x => Map(x -> second_layer.toSet) }.flatten.toMap ++
      second_layer.map{ case x => Map(x -> third_layer.toSet) }.flatten.toMap
  ))
  val instances = graph.pathsInDAG("a").values.flatten
  val tour = EulerTour(graph, "a")

  "Equivalency of Berkman--Vishkin and naive range minimum queries" should "work" in {
    instances.toSeq.combinations(2).toList.map { case Seq(a, b) =>
      tour.rmqNaive(a, b) should be (tour.rmqBV(a, b))
    }
  }

  "Range Minimum Query of self" should "work" in {
    instances.toSeq.map { case a =>
      tour.rmqNaive(a, a) should be (a)
      tour.rmqBV(a, a) should be (a)
    }
  }
}
