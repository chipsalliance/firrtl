// See LICENSE for license details.

package firrtl.benchmark

import firrtl.CircuitState
import firrtl.analyses.NodeCount
import firrtl.benchmark.util._
import firrtl.options.Dependency
import firrtl.passes.CheckChirrtl
import firrtl.stage.{Forms, TransformManager}

object NodeCountBenchmark extends App {
  val inputFile = args(0)
  val input = filenameToCircuit(inputFile)
  val initialCount = NodeCount(input).unique

  if(args.length < 2) {
    println(s"Node count directly after parsing $inputFile: $initialCount")
  } else{
    val form = args(1)
    val targets = form match {
      case "CheckChirrtl" => Forms.WorkingIR
      case "WorkingIR" => Forms.WorkingIR
      case "Checks" => Forms.Checks
      case "Resolved" => Forms.Resolved
      case other => throw new RuntimeException(s"Unknown Pass $other")
    }
    val reducedTarget = if(args.length < 3) { targets } else {
      targets.take(args(2).toInt)
    }
    val manager = new TransformManager(reducedTarget)
    val after = manager.transform(CircuitState(input, Seq())).circuit
    val count = NodeCount(after).unique

    println("Ran the following transforms after parsing:")
    manager.flattenedTransformOrder.foreach(t => println(t.name))

    val delta = count - initialCount
    val percentDelta = (delta * 100) / initialCount
    println(s"Node count after transforms: $count vs $initialCount after parsing (+$percentDelta%).")
  }

}
