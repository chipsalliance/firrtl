
package firrtl
package benchmark
package hot

import java.io.File
import java.util.concurrent.TimeUnit

import firrtl.benchmark.util._
import firrtl.options.Dependency
import firrtl.util.BackendCompilationUtilities
import firrtl.stage.TransformManager

import org.openjdk.jmh.annotations._

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class ConstantPropagationBenchmark extends BackendCompilationUtilities {
  val benchmarkDir = createTestDirectory("benchmark_constant_prop")
  val sourceFile = new File("../large.mid.fir")

  val input = filenameToCircuit(sourceFile.toString)
  val state = CircuitState(input, MidForm)
  val transform = new firrtl.transforms.ConstantPropagation
  // val manager = new TransformManager(transform.prerequisites)
  // val preState = manager.execute(state)
  val preState = state
  println("DONE")

  @Benchmark
  def executeTransform() = {
    transform.execute(preState)
  }
}

/*
[info] Benchmark                                                     Mode  Cnt   Score   Error  Units
[info] ConstantPropagationBenchmark.executeTransform                 avgt    5  66.506 ± 2.583  ms/op
[info] ConstantPropagationBenchmark.executeTransform:async-profiler  avgt          NaN            N/A


[info] Benchmark                                                     Mode  Cnt   Score    Error  Units
[info] ConstantPropagationBenchmark.executeTransform                 avgt    5  78.328 ± 25.751  ms/op
[info] ConstantPropagationBenchmark.executeTransform:async-profiler  avgt          NaN             N/A

FIRRTL_MAX_HEAP ?= 20G
FIRRTL_MAX_STACK ?= 8M
FIRRTL_MAIN ?= firrtl.Driver
FIRRTL ?= $(JAVA) -Xmx$(FIRRTL_MAX_HEAP) -Xss$(FIRRTL_MAX_STACK) -cp $(federation_jar) $(FIRRTL_MAIN)
*/
