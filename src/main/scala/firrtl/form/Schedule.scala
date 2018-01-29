package firrtl.form

import firrtl.{CircuitState, CircuitForm, Transform}

import scala.collection.mutable

case class Schedule(input: CircuitForm, output: CircuitForm, transforms: Iterable[Transform]) {
  /*
  val featureMap = transforms.foldLeft(Map[CircuitForm, mutable.Set[Transform]]()) { (map, xform) =>
    xform.input.elements.foldLeft(map) { (map, f) =>
      map + (f -> map.getOrElse(f, mutable.Set.empty[Transform]).+(xform))
    }
  }
  */
  def order(): Seq[Transform] = {
    val graph = new firrtl.graph.MutableDiGraph[CircuitForm]()
    graph.addVertex(input)

    val stateQueue = mutable.ArrayBuffer[CircuitForm]()
    val transformMap = mutable.HashMap[(CircuitForm, CircuitForm), Transform]()

    var nTransforms = 0
    var currForm = input

    while(!currForm.contains(output) && nTransforms < 100) {
      transforms.foreach { t =>
        val outputForm = currForm.update(t.outputForm)

        if(currForm.contains(t.inputForm) && !graph.contains(outputForm)) {
          graph.addVertex(outputForm)
          graph.addEdge(currForm, outputForm)
          transformMap((currForm, outputForm)) = t
          stateQueue += outputForm
        }
      }
      nTransforms += 1
      if(stateQueue.nonEmpty) {
        currForm = stateQueue.remove(0)
      } else {
        nTransforms = 1000
      }
    }

    if(nTransforms < 100) {
      val path = graph.path(input, currForm)
      //println(path.mkString("\n"))
      val (lastForm, xforms) = path.tail.foldLeft((path.head, Seq.empty[Transform])) {
        case ((prevForm, xforms), nextForm) =>
          (nextForm, xforms :+ transformMap((prevForm, nextForm)))
      }
      //println(s"From $input to $output")
      //println(path.map(_.description).mkString("\n\n"))
      //println(xforms)
      //println()
      xforms
    } else Nil
  }

}

