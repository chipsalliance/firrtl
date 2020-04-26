// See LICENSE for license details.

package firrtl.stage.transforms

import firrtl.{CircuitState, Transform, Utils}
import firrtl.options.Dependency
import firrtl.stage.Forms

class CheckTransforms(val underlying: Transform) extends Transform with WrappedTransform {

  val checks = Forms.Checks ++ Seq(
    Dependency(firrtl.passes.ResolveKinds),
    Dependency(firrtl.passes.InferTypes),
    Dependency(firrtl.passes.Uniquify),
    Dependency(firrtl.passes.ResolveFlows),
    Dependency[firrtl.passes.InferBinaryPoints],
    Dependency[firrtl.passes.TrimIntervals],
    Dependency[firrtl.passes.InferWidths],
    Dependency[firrtl.transforms.InferResets] )

  lazy val checksSet = checks.toSet

  override def execute(state: CircuitState): CircuitState = {

    println(s"Running transform: ${underlying.name}")

    val statex = underlying.transform(state)

    val statedState = statex.annotations.collectFirst {
      case TransformHistoryAnnotation(_, state) => state
    }.getOrElse(Utils.throwInternalError("You need to run CheckTransforms with TrackTransforms enabled"))

    // println("  Stated State:")
    // println(statedState.mkString("    - ", "\n    - ", ""))

    val (targets, currentState) = statedState.map(Dependency.fromTransform).partition(checksSet.contains(_))

    // println("  The following transforms should NOT have been invalidated:")
    // println(targets.map(_.getObject.name).mkString("    - ", "\n    - ", ""))

    val checkingCompiler = new Compiler(
      targets = targets.toSeq,
      currentState = currentState.toSeq)

    try {
      checkingCompiler.flattenedTransformOrder.foldLeft(statex) {
        case (acc, tx) =>
          val accx = tx.transform(acc)
          if (acc.circuit != accx.circuit && !underlying.invalidates(tx)) {
            println(s"    Circuit mismatch in ${tx.name}!")
          }
          accx
      }
    }
    // catch {
    //   case t: Throwable => println(s"    Caught exception ${t}. Will try to continue...")
    // }

    statex
  }

}

object CheckTransforms {

  def apply(a: Transform): CheckTransforms = new CheckTransforms(a)

}
