// See LICENSE for license details.

package firrtl.transforms

import firrtl._

import firrtl.analyses.InstanceGraph
import firrtl.annotations.{CircuitTarget, ComponentName, IsMember, ModuleTarget, ReferenceTarget, Target}
import firrtl.ir
import firrtl.passes.{PassException, Uniquify}
import firrtl.Utils.v_keywords
import firrtl.Mappers._
import firrtl.options.{Dependency, PreservesAll}

import scala.collection.mutable

/** Transform that removes collisions with reserved keywords
  * @param keywords a set of reserved words
  * @define implicitRename @param renames the [[RenameMap]] to query when renaming
  * @define implicitNamespace @param ns an encolosing [[Namespace]] with which new names must not conflict
  * @define implicitScope @param scope the enclosing scope of this name. If [[None]], then this is a [[Circuit]] name
  */
class RemoveKeywordCollisions(keywords: Set[String]) extends Transform {
  val inputForm: CircuitForm = UnknownForm
  val outputForm: CircuitForm = UnknownForm
  private type ModuleType = mutable.HashMap[String, ir.Type]
  private val inlineDelim = "_"

  /** Generate a new name, by appending underscores, that will not conflict with the existing namespace
    * @param n a name
    * @param ns a [[Namespace]]
    * @return a conflict-free name
    * @note prefix uniqueness is not respected
    */
  private def safeName(n: String, ns: Namespace): String =
    Uniquify.findValidPrefix(n + inlineDelim, Seq(""), ns.cloneUnderlying ++ keywords)

  /** Modify a name to not conflict with a Verilog keywords while respecting existing renames and a namespace
    * @param n the name to rename
    * @param renames the [[RenameMap]] to query when renaming
    * $implicitRename
    * $implicitNamespace
    * $implicitScope
    * @return a name without keyword conflicts
    */
  private def onName(n: String)(implicit renames: RenameMap, ns: Namespace, scope: Option[ Target]): String = {

    // Convert a [[String]] into [[ Target]] based on the provided scope.
    def wrap(name: String, scope: Option[ Target]):  Target = {
        scope match {
          case None                       => CircuitTarget(name)
          case Some(cir: CircuitTarget)   => ModuleTarget(cir.name, name)
          case Some(mod: ModuleTarget)    => ComponentName(name, mod).toTarget
          case Some(ref: ReferenceTarget) => ReferenceTarget(ref.circuit, ref.module, ref.path, s"${ref.name}.$name", ref.component)
        }
    }

    val named = wrap(n, scope)
//    // FIXME - Should we have a higher common type?
//    named match {
//      case c: CircuitTarget =>
//    }
    // If this has already been renamed use that name. If it conflicts with a keyword, determine a new, safe name and
    // update the renames. Otherwise, leave it alone.
    val namedx: Seq[ Target] = (named match {
      case c: CircuitTarget => renames.get(c)
      case m: ModuleTarget => renames.get(m)
      case r: ReferenceTarget => renames.get(r)
      case _ => renames.get(named.toTarget)
    }) match {
      case Some(x) => x
      case None if keywords(n) =>
        val sn = wrap(safeName(n, ns), scope)
        renames.rename(named, sn)
        Seq(sn)
      case _ => Seq(wrap(n, scope))
    }

    namedx match {
//      case Seq(ComponentName(n, _)) => n
//      case Seq(ModuleName(n, _))    => n
//      case Seq(CircuitName(n))      => n
      case Seq(ReferenceTarget(_, _, _, n, _)) => n
      case Seq(ModuleTarget(_, n))    => n
      case Seq(CircuitTarget(n))      => n
      case x => throw new PassException(
        s"Verilog renaming shouldn't result in multiple renames, but found '$named -> $namedx'")
    }
  }

  /** Rename the fields of a [[Type]] to match the ports of an instance
    * @param t the type to rename
    * $implicitRename
    * $implicitNamespace
    * $implicitScope
    * @return a [[Type]] with updated names
    * @note This is not intended for fixing arbitrary types, only [[BundleType]] in instance [[WRef]]s
    */
  private def onType(t: ir.Type)
                    (implicit renames: RenameMap,
                     ns: Namespace,
                     scope: Option[ModuleTarget]): ir.Type = t match {
    case b: ir.BundleType => b.copy(fields = b.fields.map(f => f.copy(name = onName(f.name))))
    case _                 => t
  }

  /** Rename an [[Expression]] to respect existing renames and avoid keyword collisions
    * @param e the [[Expression]] to rename
    * $implicitRename
    * $implicitNamespace
    * $implicitScope
    * @return an [[Expression]] without keyword conflicts
    */
  private def onExpression(e: ir.Expression)
                          (implicit renames: RenameMap,
                           ns: Namespace,
                           scope: Option[ModuleTarget],
                           iToM: mutable.Map[ReferenceTarget, ModuleTarget],
                           modType: ModuleType): ir.Expression = e match {
    case wsf@ WSubField(wr@ WRef(name, _, InstanceKind, _), port, _, _) =>
//      val subInst = ComponentName(name, scope.get)
//      val subModule = iToM(subInst)
//      val subPort = ComponentName(port, subModule)
      val subInst = ComponentName(name, scope.get).toTarget
      val subModule = iToM(subInst)
      val subPort = ComponentName(port, subModule).toTarget

      //        name = renames.get(subInst).orElse(Some(Seq(subInst))).get.head.name,
      val subInstTarget = renames.get(subInst).orElse(Some(Seq(subInst))).get.head
      val wrx = subInstTarget match {
        case r: ReferenceTarget =>
          wr.copy(
          name = r.ref,
          tpe = modType(subModule.name))
        case _ => throw new PassException(
          s"Verilog renaming attempt to rename a non-reference target '${wr} -> $subInstTarget'")
      }

      //        name = renames.get(subPort).orElse(Some(Seq(subPort))).get.head.name)
      val subPortTarget = renames.get(subPort).orElse(Some(Seq(subPort))).get.head
      subPortTarget match {
        case r: ReferenceTarget =>
          wsf.copy(
            expr = wrx,
            name = r.ref)
        case _ => throw new PassException(
          s"Verilog renaming attempt to rename a non-reference port '${wsf} -> $subPortTarget'")
      }
    case wr: WRef => wr.copy(name=onName(wr.name))
    case ex       => ex.map(onExpression)
  }

  /** Rename a [[Statement]] to respect existing renames and avoid keyword collisions
    * $implicitRename
    * $implicitNamespace
    * $implicitScope
    * @return a [[Statement]] without keyword conflicts
    */
  private def onStatement(s: ir.Statement)
                         (implicit renames: RenameMap,
                          ns: Namespace,
                          scope: Option[ModuleTarget],
                          iToM: mutable.Map[ReferenceTarget, ModuleTarget],
                          modType: ModuleType): ir.Statement = s match {
    case wdi: WDefInstance =>
//      scope match {
//        case Some(t: Target) =>
          val subModule = ModuleTarget(scope.get.circuit, wdi.module)
          val modulex = renames.get(subModule).orElse(Some(Seq(subModule))).get.head.module
          val wdix = wdi.copy(module = modulex,
            name = onName(wdi.name),
            tpe = onType(wdi.tpe)(renames, ns, Some(ModuleTarget(scope.get.circuit, modulex))))
          iToM(ComponentName(wdi.name, scope.get).toTarget) = ModuleTarget(scope.get.circuit, wdix.module)
          wdix
//        case Some(n: Named) =>
//          val subModule = ModuleName(wdi.module, scope.get.circuit)
//          val modulex = renames.get(subModule).orElse(Some(Seq(subModule))).get.head.name
//          val wdix = wdi.copy(module = modulex,
//            name = onName(wdi.name),
//            tpe = onType(wdi.tpe)(renames, ns, Some(ModuleName(modulex, scope.get.circuit))))
//          iToM(ComponentName(wdi.name, scope.get)) = ModuleName(wdix.module, scope.get.circuit)
//          wdix
//      }
    case _ => s
        .map(onStatement)
        .map(onExpression)
        .map(onName)
  }

  /** Rename a [[Port]] to avoid keyword collisions
    * $implicitRename
    * $implicitNamespace
    * $implicitScope
    * @return a [[Port]] without keyword conflicts
    */
  private def onPort(p: ir.Port)(implicit renames: RenameMap, ns: Namespace, scope: Option[ModuleTarget]): ir.Port =
    p.copy(name = onName(p.name))

  /** Rename a [[DefModule]] and it's internals (ports and statements) to fix keyword collisions and update instance
    * references to respect previous renames
    * @param renames a [[RenameMap]]
    * @param circuit the enclosing [[CircuitName]]
    * @return a [[DefModule]] without keyword conflicts
    */
  private def onModule(renames: RenameMap,
                       circuit:  CircuitTarget,
                       modType: ModuleType)
                      (m: ir.DefModule): ir.DefModule = {
    implicit val moduleNamespace: Namespace = Namespace(m)
//    implicit val scope: Option[ModuleTarget] = Some(ModuleName(m.name, circuit))
    implicit val scope: Option[ModuleTarget] = Some(ModuleTarget(circuit.name, m.name))
    implicit val r: RenameMap = renames
    implicit val mType: ModuleType = modType

    // Store local renames of refs to instances to their renamed modules. This is needed when renaming port connections
    // on subfields where only the local instance name is available.
    implicit val iToM: mutable.Map[ReferenceTarget, ModuleTarget] = mutable.Map.empty

    val mx = m
      .map(onPort)
      .map(onStatement)
      .map(onName(_: String)(renames, moduleNamespace, Some(circuit)))

    // Must happen after renaming the name and ports of the module itself
    mType += (mx.name -> onType(Utils.module_type(mx)))
    mx
  }

  /** Fix any Verilog keyword collisions in a [[firrtl.ir Circuit]]
    * @param c a [[firrtl.ir Circuit]] with possible name collisions
    * @param renames a [[RenameMap]] to update. If you don't want to propagate renames, this can be ignored.
    * @return a [[firrtl.ir Circuit]] without keyword conflicts
    */
  def run(c: ir.Circuit, renames: RenameMap = RenameMap()): ir.Circuit = {
    implicit val circuitNamespace: Namespace = Namespace(c)
    implicit val scope: Option[ CircuitTarget] = Some(CircuitTarget(c.main))
    val modType: ModuleType = new ModuleType()

    // Rename all modules from leafs to root in one pass while updating a shared rename map. Going from leafs to roots
    // ensures that the rename map is safe for parents to blindly consult.
    val modulesx: Map[IsMember, Seq[ir.DefModule]] = new InstanceGraph(c).moduleOrder.reverse
      .map(onModule(renames, scope.get, modType))
//      .groupBy(m => ModuleName(m.name, scope.get))
      .groupBy(m => ModuleTarget(scope.get.circuit, m.name))

    // Reorder the renamed modules into the original circuit order.
    val modulesxx: Seq[ir.DefModule] = c.modules.flatMap{ orig =>
//      val named = ModuleName(orig.name, scope.get)
//      modulesx(renames.get(named).orElse(Some(Seq(named))).get.head)
      val named = ModuleTarget(scope.get.circuit, orig.name)
      modulesx(renames.get(named).orElse(Some(Seq(named))).get.head)
    }

    // Rename the circuit if the top module was renamed
//    val mainx = renames.get(ModuleName(c.main, CircuitName(c.main))) match {
//      case Some(Seq(ModuleName(m, _))) =>
//        renames.rename(CircuitName(c.main), CircuitName(m))
//        m
//      case x@ Some(_) => throw new PassException(
//        s"Verilog renaming shouldn't result in multiple renames, but found '${c.main} -> $x'")
//      case None =>
//        c.main
//    }
    val mainx = renames.get(ModuleTarget(c.main, c.main)) match {
      case Some(Seq(ModuleTarget(_, m))) =>
        renames.record(CircuitTarget(c.main), CircuitTarget(m))
        m
      case x@ Some(_) => throw new PassException(
        s"Verilog renaming shouldn't result in multiple renames, but found '${c.main} -> $x'")
      case None =>
        c.main
    }

    // Apply all updates
    c.copy(modules = modulesxx, main = mainx)
  }

  /** Fix any Verilog keyword name collisions in a [[CircuitState]] while propagating renames
    * @param state the [[CircuitState]] with possible name collisions
    * @return a [[CircuitState]] without name collisions
    */
  def execute(state: CircuitState): CircuitState = {
    val renames = RenameMap()
    renames.setCircuit(state.circuit.main)
    state.copy(circuit = run(state.circuit, renames), renames = Some(renames))
  }
}

/** Transform that removes collisions with Verilog keywords */
class VerilogRename extends RemoveKeywordCollisions(v_keywords) with PreservesAll[Transform] {

  override val prerequisites = firrtl.stage.Forms.LowFormMinimumOptimized ++
    Seq( Dependency[BlackBoxSourceHelper],
         Dependency[FixAddingNegativeLiterals],
         Dependency[ReplaceTruncatingArithmetic],
         Dependency[InlineBitExtractionsTransform],
         Dependency[InlineCastsTransform],
         Dependency[LegalizeClocksTransform],
         Dependency[FlattenRegUpdate],
         Dependency(passes.VerilogModulusCleanup) )

  override val optionalPrerequisites = firrtl.stage.Forms.LowFormOptimized

  override val dependents = Seq.empty

}
