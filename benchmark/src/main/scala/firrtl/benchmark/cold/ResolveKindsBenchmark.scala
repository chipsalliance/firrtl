
package firrtl
package benchmark
package cold

import passes.ResolveKinds
import stage.TransformManager
import options.Dependency
import Utils.time

import firrtl.benchmark.util._

object ResolveKindsBenchmark extends App {
  val inputFile = args(0)

  val input = filenameToCircuit(inputFile)
  val state = CircuitState(input, ChirrtlForm)
  val prereqs = ResolveKinds.prerequisites
  val manager = new TransformManager(prereqs)
  val preState = manager.execute(state)

  val (t, res) = time(ResolveKinds.run(preState.circuit))
  println(f"Took $t%.1f ms")
}
