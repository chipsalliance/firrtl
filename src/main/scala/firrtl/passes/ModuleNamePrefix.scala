package firrtl.passes

import firrtl._
import firrtl.options.{Dependency, RegisteredTransform, ShellOption}
import firrtl.transforms.DedupModules
import firrtl.annotations.NoTargetAnnotation
import firrtl.stage.RunFirrtlTransformAnnotation

////////////////
// Pass
////////////////

/** Specifies a global prefix for all module names. */
case class ModulePrefix(prefix: String = "abcdefg") extends NoTargetAnnotation


class PrefixModulesPass extends Transform with DependencyAPIMigration with RegisteredTransform {
  // we run after deduplication to save some work
  override def prerequisites = Seq(Dependency[DedupModules])

  // we do not invalidate the results of any prior passes
  override def invalidates(a: Transform) = false

  val options = Seq(
    new ShellOption[String](
      longOption = "module-name-prefix",
      toAnnotationSeq = (a: String) =>
        Seq(ModulePrefix("abcdefg"), RunFirrtlTransformAnnotation(new PrefixModulesPass)),
      helpText = "Global prefix to every modules",
      shortOption = Some("prefix"),
      helpValueName = Some("<prefix>")
    )
  )

  override protected def execute(state: CircuitState): CircuitState = {
    val prefixes = state.annotations.collect{ case a: ModulePrefix => a.prefix }.distinct
    prefixes match {
      case Seq() =>
        logger.info("[PrefixModulesPass] No ModulePrefix annotation found.")
        state
      case Seq("") => state
      case Seq(prefix) =>
        val c = state.circuit.mapModule(onModule(_, prefix))
        state.copy(circuit = c.copy(main = prefix + c.main))
      case other =>
        throw new PassException(s"[PrefixModulesPass] found more than one prefix annotation: $other")
    }
  }

  private def onModule(m: ir.DefModule, prefix: String): ir.DefModule = m match {
    case e : ir.ExtModule => e.copy(name = prefix + e.name)
    case mod: ir.Module =>
      val name = prefix + mod.name
      val body = onStmt(mod.body, prefix)
      mod.copy(name=name, body=body)
  }

  private def onStmt(s: ir.Statement, prefix: String): ir.Statement = s match {
    case i : ir.DefInstance => i.copy(module = prefix + i.module)
    case other => other.mapStmt(onStmt(_, prefix))
  }
}