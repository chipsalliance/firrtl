// See LICENSE for license details.

package firrtl.passes

import firrtl.annotations.{CircuitTarget, ModuleTarget, ReferenceTarget}
import firrtl.{CircuitForm, CircuitState, DuplexFlow,
  InstanceKind, Kind, MemKind, NodeKind, RegKind, RenameMap, SinkFlow, SourceFlow,
  SymbolTable, Transform, UnknownForm, Utils, WireKind}
import firrtl.ir._
import firrtl.options.Dependency
import firrtl.stage.TransformManager.TransformDependency

import scala.collection.mutable

/** Flattens Bundles and Vecs.
  * - all SubAccess nodes need to be removed before running this pass.
  * - Combines the following legacy passes:
  *   - Uniquify, LowerTypes + some of the ExpandConnect functionality (modulo PartialConnects)
  * - Some implicit bundle types remain, but with a limited depth:
  *   - the type of a memory is still a bundle with depth 2 (mem -> port -> field)
  *   - the type of a module instance is still a bundle with depth 1 (instance -> port)
  */
object NewLowerTypes extends Transform {
  override def inputForm: CircuitForm = UnknownForm
  override def outputForm: CircuitForm = UnknownForm

  override def prerequisites: Seq[TransformDependency] = Seq(
    Dependency(CheckFlows),     // we assume that the flows are correct
    Dependency(RemoveAccesses), // we require all SubAccess nodes to have been removed
    Dependency(CheckTypes),     // we require all types to be correct
    Dependency(ExpandConnects)  // we require all PartialConnect nodes to have been expanded
  )
  override def optionalPrerequisiteOf: Seq[TransformDependency]  = Seq.empty
  override def invalidates(a: Transform): Boolean = false

  private type LoweringInfo = Seq[(Field, Seq[String])]

  override def execute(state: CircuitState): CircuitState = {
    implicit val renameMap: RenameMap = RenameMap()
    renameMap.setCircuit(state.circuit.main)

    // we first lower ports since the port lowering info will be needed at every DefInstance
    val c = CircuitTarget(state.circuit.main)
    val loweredPortsAndInfo = state.circuit.modules.map(lowerPorts(c, _))
    implicit val moduleLoweringInfo: Map[String, LoweringInfo] =
      loweredPortsAndInfo.map{ case (m, i) => m.name -> i }.toMap

    // TODO: we could get a potential speedup by only calculating the module ports/type lowering once per module
    //       instead of calculating it for every module and instance.
    val result = state.circuit.mapModule(onModule(c, _))

    state.copy(circuit = result, renames = Some(renameMap))
  }

  def lowerPorts(c: CircuitTarget, m: DefModule)(implicit renameMap: RenameMap): (DefModule, LoweringInfo) = {
    renameMap.setModule(m.name)
    val ref = c.module(m.name)
    val namespace = mutable.HashSet[String]() ++ m.ports.map(_.name)
    // ports are like fields with an additional Info field
    val portsAndReferences = m.ports.flatMap { p => DestructTypes.destruct(ref, p, namespace, renameMap) }
    val mWithRenamedPorts = m match {
      case x: Module => x.copy(ports = portsAndReferences.map(_._1))
      case x: ExtModule => x.copy(ports = portsAndReferences.map(_._1))
    }
    val loweringInfo = portsAndReferences.map{ case (p, r) => Field(p.name, Utils.to_flip(p.direction), p.tpe) -> r }
    (mWithRenamedPorts, loweringInfo)
  }

  def onModule(c: CircuitTarget, m: DefModule)
              (implicit renameMap: RenameMap, moduleLoweringInfo: Map[String, LoweringInfo]): DefModule = m match {
    case x: ExtModule => x
    case mod: Module =>
      renameMap.setModule(mod.name)
      val ref = c.module(mod.name)
      // scan modules to find all references
      val scan = SymbolTable.scanModule(new LoweringSymbolTable, mod)
      // replace all declarations and references with the destructed types
      implicit val symbols: DestructTable = new DestructTable(scan, moduleLoweringInfo, renameMap, ref)
      mod.copy(body = Block(onStatement(mod.body)))
  }

  //scalastyle:off cyclomatic.complexity
  def onStatement(s: Statement)(implicit symbols: DestructTable): Seq[Statement] = s match {
    // declarations
    case DefWire(info, name, _) =>
      symbols.destruct(name).map(r => DefWire(info, r.name, r.tpe))
    case DefRegister(info, name, _, clock, reset, init) =>
      val loweredClock = onExpressionOne(clock)
      val loweredReset = onExpressionOne(reset)
      val inits = onExpression(init)
      val refs = symbols.destruct(name)
      refs.zip(inits).map{ case (r, i) => DefRegister(info, r.name, r.tpe, loweredClock, loweredReset, i)}
    case DefMemory(info, name, _, depth, wLatency, rLatency, rs, ws, rws, readUnderWrite) =>
      // TODO: can the name of readers/writers change?
      val refs = symbols.destruct(name)
      refs.zip(symbols.getDataTypes(name)).map { case (r, dataTpe) =>
        DefMemory(info, r.name, dataTpe, depth, wLatency, rLatency, rs, ws, rws, readUnderWrite)
      }
    case DefInstance(info, name, module, _) =>
      val List(ref) = symbols.destruct(name)
      List(DefInstance(info, ref.name, module, ref.tpe))
    case DefNode(info, name, value) =>
      val refs = symbols.destruct(name)
      refs.zip(onExpression(value)).map{ case(r, v) => DefNode(info, r.name, v) }
    // connections
    case Connect(info, loc, expr) =>
      val refs = symbols.getReferences(loc.asInstanceOf[ExpressionWithFlow], lhs = true)
      refs.zip(onExpression(expr)).map{ case (r,e) => r.flow match {
        case SinkFlow => Connect(info, r, e)
        case SourceFlow => Connect(info, e, r)
      }}
    case p : PartialConnect =>
      // TODO: maybe we can handle partial connects
      throw new RuntimeException(s"NewLowerTypes expects PartialConnects to be resolved! $p")
    case IsInvalid(info, expr) =>
      val refs = symbols.getReferences(expr.asInstanceOf[ExpressionWithFlow], lhs = true)
        .filter(r => r.flow == DuplexFlow || r.flow == SinkFlow)
      refs.map(r => IsInvalid(info, r))
    // blocks
    case Block(stmts) => stmts.flatMap(onStatement)
    case Conditionally(info, pred, conseq, alt) =>
      val loweredPred = onExpressionOne(pred)
      List(Conditionally(info, loweredPred, Block(onStatement(conseq)), Block(onStatement(alt))))
    case EmptyStmt => List()
    // others
    case other => List(other.mapExpr(onExpressionOne))
  }
  //scalastyle:on cyclomatic.complexity

  // very similar to Utils.create_exps
  def onExpression(e: Expression)(implicit symbols: DestructTable): Seq[Expression] = e match {
    case Mux(cond, tval, fval, _) =>
      val loweredCond = onExpressionOne(cond)
      onExpression(tval).zip(onExpression(fval)).map { case(t, f) =>
        Mux(loweredCond, t, f, Utils.mux_type_and_widths(t, f))
      }
    case ValidIf(cond, value, _) =>
      val loweredCond = onExpressionOne(cond)
      onExpression(value).map(v => ValidIf(loweredCond, v, v.tpe))
    case r : ExpressionWithFlow => symbols.getReferences(r, lhs = false)
    case other => assert(other.tpe.isInstanceOf[GroundType]) ; List(other)
  }

  /** ensures that the result is a single expression */
  def onExpressionOne(e: Expression)(implicit symbols: DestructTable): Expression = {
    val es = onExpression(e)
    assert(es.length == 1)
    es.head
  }
}

// used for first scan of the module to discover all declarations
private class LoweringSymbolTable extends SymbolTable {
  def declare(name: String, tpe: Type, kind: Kind): Unit = symbols.append((name, kind, tpe, None))
  def declareInstance(name: String, module: String): Unit =
    symbols.append((name, InstanceKind, UnknownType, Some(module)))
  private val symbols = mutable.ArrayBuffer[(String, Kind, Type, Option[String])]()
  def getSymbols: Seq[(String, Kind, Type, Option[String])] = symbols
}

// generates the destructed types
private class DestructTable(symbols: LoweringSymbolTable, modules: Map[String, Seq[(Field, Seq[String])]],
                            renameMap: RenameMap, m: ModuleTarget) {
  private val namespace = mutable.HashSet[String]() ++ symbols.getSymbols.map(_._1)
  private val kinds = symbols.getSymbols.map(s => s._1 -> s._2).toMap
  // serialized old access string to new ground type fields
  private val refToFields = mutable.HashMap[String, Seq[Field]]()


  // TODO: compute flow from declaration similar to how it is done in ExpandConnects
  def destruct(name: String, tpe: Type = UnknownType): Seq[Field] = kinds(name) match {
    case WireKind | RegKind | NodeKind =>
      assert(tpe != UnknownType)
      val fieldsAndRefs = DestructTypes.destruct(m, Field(name, Default, tpe), namespace, renameMap)
      refToFields ++= fieldsAndRefs.flatMap{ case (f, refs) => refs.map(r => r -> f) }
                                   .groupBy(_._1).mapValues(_.map(_._2))
      fieldsAndRefs.map(_._1)
    case MemKind =>
      assert(tpe != UnknownType)
      throw new NotImplementedError("TODO")
    case InstanceKind =>
      assert(tpe == UnknownType)
      // the ports are already lowered, so the only change would be a rename of the instance



    ???
  }

  def getReferences(expr: ExpressionWithFlow, lhs: Boolean): List[ExpressionWithFlow] = expr match {
    case SubAccess(expr, index, tpe, flow) => ???
    case SubIndex(expr, value, tpe, flow) => ???
    case Reference(name, tpe, kind, flow) => ???
    case SubField(expr, name, tpe, flow) => ???
  }
  def getDataTypes(name: String): List[Type] = ???
}

//scalastyle:off

/** Calculate new type layouts and names. */
private object DestructTypes {
  type Namespace = mutable.HashSet[String]

  /** Does the following with a reference:
    * - rename reference and any bundle fields to avoid name collisions after destruction
    * - updates rename map with new targets
    * - generates all ground type fields
    * - generates a list of all old reference name that now refer to the particular ground type field
    * - updates namespace with all possibly conflicting names
    */
  def destruct(parent: ModuleTarget, ref: Field, namespace: Namespace, renameMap: RenameMap):
    Seq[(Field, Seq[String])] = {
    // ensure that the field name is part of the namespace
    namespace.add(ref.name)
    // field renames (uniquify) are computed bottom up
    val (rename, _) = uniquify(ref, namespace)

    // the reference renames are computed top down since they do need the full path
    val res = destruct(parent, ref, rename)(renameMap)

    // convert references to strings relative to the module
    res.map{ case(c,r) => c -> r.map(_.serialize.dropWhile(_ != '>').tail) }
  }

  /** convenience overload that handles the conversion from/to Port */
  def destruct(parent: ModuleTarget, ref: Port, namespace: Namespace, renameMap: RenameMap):
    Seq[(Port, Seq[String])] = {
    destruct(parent, Field(ref.name, Utils.to_flip(ref.direction), ref.tpe), namespace, renameMap)
      .map{ case(f, l) => (Port(ref.info, f.name, Utils.to_dir(f.flip), f.tpe), l) }
  }

  private def destruct(m: ModuleTarget, field: Field, rename: Option[RenameNode])
                      (implicit renameMap: RenameMap): Seq[(Field, Seq[ReferenceTarget])] =
    destruct(m, prefix = "", oldParent = None, oldField = field, isVecField = false, rename = rename)

  private def destruct(m: ModuleTarget, prefix: String,
                       oldParent: Option[ReferenceTarget], oldField: Field,
                       isVecField: Boolean, rename: Option[RenameNode])
                      (implicit renameMap: RenameMap): Seq[(Field, Seq[ReferenceTarget])] = {
    val newName = rename.map(_.name).getOrElse(oldField.name)
    val newPrefix = prefix + newName + LowerTypes.delim
    val oldRef = oldParent match {
      case Some(p) => if(isVecField) { p.index(oldField.name.toInt) } else { p.field(oldField.name) }
      case None => m.ref(oldField.name)
    }
    val ref = m.ref(prefix + newName)

    oldField.tpe match {
      case _ : GroundType =>
        val isRenamed = prefix != "" || newName != oldField.name
        if(isRenamed) { renameMap.record(oldRef, ref) }
        List((oldField.copy(name = prefix + newName), List(oldRef)))
      case _ : BundleType | _ : VectorType =>
        val isVecField = oldField.tpe.isInstanceOf[VectorType]
        val fields = oldField.tpe match {
          case v : VectorType => vecToBundle(v).fields
          case BundleType(fields) => fields
        }
        val fieldsWithCorrectOrientation = fields.map(f => f.copy(flip = Utils.times(f.flip, oldField.flip)))
        val children = fieldsWithCorrectOrientation.flatMap { f =>
          destruct(m, newPrefix, Some(oldRef), f, isVecField, rename.flatMap(_.children.get(f.name)))
        }
        // the bundle/vec reference refers to all children
        val childRefs = children.map{ case (c, _) => m.ref(c.name) }
        renameMap.record(oldRef, childRefs)
        children.map{ case(c, r) => (c, r :+ oldRef) }
    }
  }

  private case class RenameNode(name: String, children: Map[String, RenameNode])

  /** Implements the core functionality of the old Uniquify pass: rename bundle fields and top-level references
    * where necessary in order to avoid name clashes when lowering aggregate type with the `_` delimiter.
    * We don't actually do the rename here but just calculate a rename tree. */
  private def uniquify(ref: Field, namespace: Namespace): (Option[RenameNode], Seq[String]) = ref.tpe match {
    case BundleType(fields) =>
      // we rename bottom-up
      val localNamespace = new Namespace() ++ fields.map(_.name)
      val renamedFields = fields.map(f => uniquify(f, localNamespace))

      // Need leading _ for findValidPrefix, it doesn't add _ for checks
      val renamedFieldNames = renamedFields.flatMap(_._2)
      val suffixNames: Seq[String] = renamedFieldNames.map(f => LowerTypes.delim + f)
      val prefix = Uniquify.findValidPrefix(ref.name, suffixNames, namespace)
      // We added f.name in previous map, delete if we change it
      val renamed = prefix != ref.name
      if (renamed) {
        namespace -= ref.name
        namespace += prefix
      }
      val suffixes = renamedFieldNames.map(f => prefix + LowerTypes.delim + f)

      val anyChildRenamed = renamedFields.exists(_._1.isDefined)
      val rename = if(renamed || anyChildRenamed){
        val children = renamedFields.map(_._1).zip(fields).collect{ case (Some(r), f) => f.name -> r }.toMap
        Some(RenameNode(prefix, children))
      } else { None }

      (rename, suffixes :+ prefix)
    case v : VectorType=>
      // if Vecs are to be lowered, we can just treat them like a bundle
      uniquify(ref.copy(tpe = vecToBundle(v)), namespace)
    case _ : GroundType => (None, List(ref.name))
  }

  private def vecToBundle(v: VectorType): BundleType = {
    BundleType(( 0 until v.size).map(i => Field(i.toString, Default, v.tpe)))
  }
}