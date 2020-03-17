package firrtl
package benchmark
package hot

import firrtl.ir.Circuit
import firrtl.passes.ResolveKinds
import firrtl.stage.TransformManager

import firrtl.benchmark.util._

// Must not be in default package
import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Mode, OutputTimeUnit, Scope, State}

/* Default settings for benchmarks in this class */
@OutputTimeUnit(TimeUnit.SECONDS)
@BenchmarkMode(Array(Mode.Throughput))
@State(Scope.Benchmark)
class ResolveKindsJMH {

  private val before: Circuit = {
  //def prepBefore: Unit = {
    val inputFile = "Rob.fir"
    val input = filenameToCircuit(inputFile)
    val state = CircuitState(input, ChirrtlForm)
    val prereqs = ResolveKinds.prerequisites
    val manager = new TransformManager(prereqs)
    manager.execute(state).circuit
  }

  private val after: Circuit = ResolveKinds.run(before)

  @Benchmark
  def freshResolveKinds: Unit = {
    ResolveKinds.run(before)
  }

  @Benchmark
  def staleResolveKinds: Unit = {
    ResolveKinds.run(after)
  }

}
