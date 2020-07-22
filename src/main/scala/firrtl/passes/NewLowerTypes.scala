// See LICENSE for license details.

package firrtl.passes

import firrtl.annotations.{CircuitTarget, ModuleTarget, ReferenceTarget}
import firrtl.{CircuitForm, CircuitState, InstanceKind, Kind, MemKind, PortKind, RenameMap, SymbolTable, Transform, UnknownForm, Utils}
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
  override def invalidates(a: Transform): Boolean = a match {
    case ResolveFlows => true // we generate UnknownFlow for now (could be fixed)
    case _ => false
  }

  override def execute(state: CircuitState): CircuitState = {
    // we first lower ports since the port lowering info will be needed at every DefInstance
    val portRenameMap = RenameMap()
    val c = CircuitTarget(state.circuit.main)
    val loweredPorts = state.circuit.modules.map(lowerPorts(c, _, portRenameMap))

    val resultAndRenames = loweredPorts.map{case (m, refs) => onModule(c, m, refs)}
    val result = state.circuit.copy(modules = resultAndRenames.map(_._1))
    // TODO: chain rename maps in correct order!
    val moduleRenames = resultAndRenames.collect{ case(m,Some(r)) => m.name -> r }
    state.copy(circuit = result, renames = None)
  }

  def lowerPorts(c: CircuitTarget, m: DefModule, renameMap: RenameMap): (DefModule, Seq[(String, Reference)]) = {
    renameMap.setModule(m.name)
    val ref = c.module(m.name)
    val namespace = mutable.HashSet[String]() ++ m.ports.map(_.name)
    // ports are like fields with an additional Info field
    val portsAndRefs = m.ports.flatMap { p => DestructTypes.destruct(ref, p, namespace, renameMap) }
    val newM = m match {
      case x: Module => x.copy(ports = portsAndRefs.map(_._1))
      case x: ExtModule => x.copy(ports = portsAndRefs.map(_._1))
    }
    val refs = portsAndRefs.map{ case (port, name) => name -> Reference(port.name, port.tpe, PortKind) }
    (newM, refs)
  }

  def onModule(c: CircuitTarget, m: DefModule, portRefs: Seq[(String, Reference)]): (DefModule, Option[RenameMap]) =
    m match {
    case x: ExtModule => (x, None)
    case mod: Module =>
      val renameMap = RenameMap()
      val ref = c.module(mod.name)
      // scan modules to find all references
      val scan = SymbolTable.scanModule(new LoweringSymbolTable, mod)
      // replace all declarations and references with the destructed types
      implicit val symbols: DestructTable = new DestructTable(scan, renameMap, ref, portRefs)
      (mod.copy(body = Block(onStatement(mod.body))), Some(renameMap))
  }

  def onStatement(s: Statement)(implicit symbols: DestructTable): Statement = s match {
    // declarations
    case d : DefWire =>
      Block(symbols.lower(d.name, d.tpe).map{case (name, tpe) => d.copy(name=name, tpe=tpe) })
    case d @ DefRegister(info, _, _, clock, reset, _) =>
      // clock and reset are always of ground type
      val loweredClock = onExpression(clock)
      val loweredReset = onExpression(reset)
      val inits = Utils.create_exps(d.init)
      Block(
        symbols.lower(d.name, d.tpe).zip(inits).map { case ((name, tpe), init) =>
          DefRegister(info, name, tpe, loweredClock, loweredReset, init)
      })
    case d : DefNode =>
      val values = Utils.create_exps(d.value)
      Block(
        symbols.lower(d.name, d.value.tpe).zip(values).map{ case((name, tpe), value) =>
          assert(tpe == value.tpe)
          DefNode(d.info, name, value)
      })
    case d : DefMemory => Block(symbols.lower(d))
    case d : DefInstance => symbols.lower(d)
    // connections
    case Connect(info, loc, expr) =>
      if(!expr.tpe.isInstanceOf[GroundType]) {
        throw new RuntimeException(s"NewLowerTypes expects Connects to have been expanded! ${expr.tpe.serialize}")
      }
      Connect(info, onExpression(loc), onExpression(expr))
    case p : PartialConnect =>
      throw new RuntimeException(s"NewLowerTypes expects PartialConnects to be resolved! $p")
    case IsInvalid(info, expr) =>
      if(!expr.tpe.isInstanceOf[GroundType]) {
        throw new RuntimeException(s"NewLowerTypes expects IsInvalids to have been expanded! ${expr.tpe.serialize}")
      }
      IsInvalid(info, onExpression(expr))
    // others
    case other => other.mapExpr(onExpression).mapStmt(onStatement)
  }

  /** Replaces all Reference, SubIndex and SubField nodes with the updated references */
  def onExpression(e: Expression)(implicit symbols: DestructTable): Expression = e match {
    case r: ExpressionWithFlow => symbols.getReference(r)
    case other => other.mapExpr(onExpression)
  }
}

// used for first scan of the module to discover the global namespace
private class LoweringSymbolTable extends SymbolTable {
  def declare(name: String, tpe: Type, kind: Kind): Unit = symbols.append(name)
  def declareInstance(name: String, module: String): Unit = symbols.append(name)
  private val symbols = mutable.ArrayBuffer[String]()
  def getSymbolNames: Seq[String] = symbols
}

// generates the destructed types
private class DestructTable(table: LoweringSymbolTable, renameMap: RenameMap, m: ModuleTarget,
                            portRefs: Seq[(String, Reference)]) {
  private val namespace = mutable.HashSet[String]() ++ table.getSymbolNames
  // serialized old access string to new ground type reference
  private val nameToExpr = mutable.HashMap[String, ExpressionWithFlow]() ++ portRefs

  def lower(mem: DefMemory): Seq[DefMemory] = {
    val (mems, refs) = DestructTypes.destructMemory(m, mem, namespace, renameMap)
    nameToExpr ++= refs.map { case (r, name) => name -> r }
    mems
  }
  def lower(inst: DefInstance): DefInstance = {
    val (newInst, refs) = DestructTypes.destructInstance(m, inst, namespace, renameMap)
    nameToExpr ++= refs.map { case (r, name) => name -> r }
    newInst
  }
  /** used to lower nodes, registers and wires */
  def lower(name: String, tpe: Type): Seq[(String, Type)] = {
    val fieldsAndRefs = DestructTypes.destruct(m, Field(name, Default, tpe), namespace, renameMap)
    nameToExpr ++= fieldsAndRefs.map{ case (f, ref) => ref -> Reference(f.name, f.tpe) }
    fieldsAndRefs.map { case (f, _) => (f.name, f.tpe) }
  }

  def getReference(expr: ExpressionWithFlow): ExpressionWithFlow = nameToExpr(serialize(expr))

  // We could just use FirrtlNode.serialize here, but we want to make sure there are not SubAccess nodes left.
  private def serialize(expr: ExpressionWithFlow): String = expr match {
    case Reference(name, _, _, _) => name
    case SubField(expr, name, _, _) => serialize(expr.asInstanceOf[ExpressionWithFlow]) + "." + name
    case SubIndex(expr, index, _, _) => serialize(expr.asInstanceOf[ExpressionWithFlow]) + "[" + index.toString + "]"
    case a : SubAccess =>
      throw new RuntimeException(s"NewLowerTypes expects all SubAccesses to have been expanded! ${a.serialize}")
  }
}

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
  def destruct(m: ModuleTarget, ref: Field, namespace: Namespace, renameMap: RenameMap):
    Seq[(Field, String)] = ref.tpe match {
    case _: GroundType => // early exit for ground types
      Seq((ref, ref.name))
    case _ =>
      // ensure that the field name is part of the namespace
      namespace.add(ref.name)
      // field renames (uniquify) are computed bottom up
      val (rename, _) = uniquify(ref, namespace)

      // the reference renames are computed top down since they do need the full path
      val res = destruct(m, ref, rename)
      recordRenames(res, renameMap, ModuleParentRef(m))

      res.map{ case (c,r) => c -> extractGroundTypeRefString(r) }
  }

  /** convenience overload that handles the conversion from/to Port */
  def destruct(m: ModuleTarget, ref: Port, namespace: Namespace, renameMap: RenameMap):
    Seq[(Port, String)] = {
    destruct(m, Field(ref.name, Utils.to_flip(ref.direction), ref.tpe), namespace, renameMap)
      .map{ case(f, l) => (Port(ref.info, f.name, Utils.to_dir(f.flip), f.tpe), l) }
  }

  /** instances are special because they remain a 1-deep bundle
    * @note this relies on the ports of the module having been properly renamed.
    * @return The potentially renamed instance with newly flattened type.
    *         Note that the list of fields is only of the child fields, and needs a SubField node
    *         instead of a flat Reference when turning them into access expressions.
    */
  def destructInstance(m: ModuleTarget, instance: DefInstance, namespace: Namespace, renameMap: RenameMap):
  (DefInstance, Seq[(SubField, String)]) = {
    namespace.add(instance.name)
    val (rename, _) = uniquify(Field(instance.name, Default, instance.tpe), namespace)
    val newName = rename.map(_.name).getOrElse(instance.name)

    // only destruct the sub-fields (aka ports)
    val oldParent = RefParentRef(m.ref(instance.name))
    val children = instance.tpe.asInstanceOf[BundleType].fields.flatMap { f =>
      val childRename = rename.flatMap(_.children.get(f.name))
      destruct("", oldParent, f, isVecField = false, rename = childRename)
    }

    // rename all references to the instance if necessary
    if(newName != instance.name) {
      renameMap.record(m.instOf(instance.name, instance.module), m.instOf(newName, instance.module))
    }
    // The ports do not need to be explicitly renamed here. They are renamed when the module ports are lowered.

    val newInstance = instance.copy(name = newName, tpe = BundleType(children.map(_._1)))
    val instanceRef = Reference(newName, newInstance.tpe, InstanceKind)
    val refs = children.map{ case(c,r) => SubField(instanceRef, c.name, c.tpe) -> extractGroundTypeRefString(r) }

    (newInstance, refs)
  }

  private val BoolType = UIntType(IntWidth(1))

  /** memories are special because they end up a 2-deep bundle */
  def destructMemory(m: ModuleTarget, mem: DefMemory, namespace: Namespace, renameMap: RenameMap):
  (Seq[DefMemory], Seq[(SubField, String)]) = {
    // See if any read/write ports need to be renamed. This can happen, e.g., with two ports named `r` and `r_data`.
    // While the renaming isn't necessary for LowerTypes as the port bundles are not lowered in this pass, it will
    // be needed for Verilog emission later on.
    val oldDummyField = Field("dummy", Default, MemPortUtils.memType(mem.copy(dataType = BoolType)))
    val (rawPortRenames, _) = uniquify(oldDummyField, new Namespace())
    val portRenames: Map[String, String] = rawPortRenames.map(_.children.mapValues(_.name)).getOrElse(Map())
    val memRenamedPorts = mem.copy(
      readers = mem.readers.map(n => portRenames.getOrElse(n, n)),
      writers = mem.writers.map(n => portRenames.getOrElse(n, n)),
      readwriters = mem.readwriters.map(n => portRenames.getOrElse(n, n))
    )

    // Uniquify the lowered memory names: When memories get split up into ground types, the access order is changes.
    // E.g. `mem.r.data.x` becomes `mem_x.r.data`.
    // This is why we need to create the new bundle structure before we can resolve any name clashes.
    val bundle = memBundle(mem)
    namespace.add(mem.name)
    val (dataTypeRenames, _) = uniquify(bundle, namespace)
    val res = destruct(m, Field(mem.name, Default, mem.dataType), dataTypeRenames)

    // Renames are now of the form `mem.a.b` --> `mem_a_b`.
    // We want to turn them into `mem.r.data.a.b` --> `mem_a_b.r.data`, etc. (for all readers, writers and for all ports)
    val oldMemRef = m.ref(mem.name)

    val newMemAndSubFields = res.map { case (field, refs) =>
      val newMem = memRenamedPorts.copy(name = field.name, dataType = field.tpe)
      val newMemRef = m.ref(field.name)
      val memWasRenamed = field.name != mem.name // false iff the dataType was a GroundType
      if(memWasRenamed) { renameMap.record(oldMemRef, newMemRef) }

      val newMemReference = Reference(field.name, MemPortUtils.memType(newMem), MemKind)
      val refSuffixes = refs.map(_.component).filterNot(_.isEmpty)

      val subFields = oldDummyField.tpe.asInstanceOf[BundleType].fields.flatMap { port =>
        val oldPortRef = oldMemRef.field(port.name)
        val newPortName = portRenames.getOrElse(port.name, port.name)
        val newPortRef = newMemRef.field(newPortName)
        val portWasRenamed = newPortName != port.name
        if(portWasRenamed) { renameMap.record(oldPortRef, newPortRef) }

        val newPortType = newMemReference.tpe.asInstanceOf[BundleType].fields.find(_.name == newPortName).get.tpe
        val newPortAccess = SubField(newMemReference, newPortName, newPortType)

        port.tpe.asInstanceOf[BundleType].fields.map { portField =>
          val oldPortFieldBaseRef = oldPortRef.field(portField.name)
          // there might have been multiple different fields which now alias to the same lowered field.
          val oldFieldRefs = refSuffixes.map(s => oldPortFieldBaseRef.copy(component = oldPortFieldBaseRef.component ++ s))
          val newPortFieldRef = newPortRef.field(portField.name)
          val newPortFieldAccess = SubField(newPortAccess, portField.name, field.tpe)

          // record renames only for the data field which is the only port field of non-ground type
          if(memWasRenamed && portField.name == "data") {
            oldFieldRefs.foreach { o => renameMap.record(o, newPortFieldRef) }
          }

          val oldFieldStringRef = extractGroundTypeRefString(oldFieldRefs)
          (newPortFieldAccess, oldFieldStringRef)
        }
      }
      (newMem, subFields)
    }

    (newMemAndSubFields.map(_._1), newMemAndSubFields.flatMap(_._2))
  }

  private def memBundle(mem: DefMemory): Field = mem.dataType match {
    case _: GroundType => Field(mem.name, Default, mem.dataType)
    case _: BundleType | _: VectorType =>
      val subMems = getFields(mem.dataType).map(f => mem.copy(name = f.name, dataType = f.tpe))
      val fields = subMems.map(memBundle)
      Field(mem.name, Default, BundleType(fields))
  }

  private def recordRenames(fieldToRefs: Seq[(Field, Seq[ReferenceTarget])], renameMap: RenameMap, parent: ParentRef):
  Unit = {
    // TODO: if we group by ReferenceTarget, we could reduce the number of calls to `record`. Is it worth it?
    fieldToRefs.foreach { case(field, refs) =>
      val fieldRef = parent.ref(field.name)
      refs.foreach{ r => renameMap.record(r, fieldRef) }
    }
  }

  private def extractGroundTypeRefString(refs: Seq[ReferenceTarget]): String = {
    // Since we depend on ExpandConnects any reference we encounter will be of ground type
    // and thus the one with the longest access path.
    refs.reduceLeft((x,y) => if (x.component.length > y.component.length) x else y)
    // convert references to strings relative to the module
      .serialize.dropWhile(_ != '>').tail
  }

  private def destruct(m: ModuleTarget, field: Field, rename: Option[RenameNode]): Seq[(Field, Seq[ReferenceTarget])] =
    destruct(prefix = "", oldParent = ModuleParentRef(m), oldField = field, isVecField = false, rename = rename)


  private def destruct(prefix: String, oldParent: ParentRef, oldField: Field,
                       isVecField: Boolean, rename: Option[RenameNode]): Seq[(Field, Seq[ReferenceTarget])] = {
    val newName = rename.map(_.name).getOrElse(oldField.name)
    val oldRef = oldParent.ref(oldField.name, isVecField)

    oldField.tpe match {
      case _ : GroundType => List((oldField.copy(name = prefix + newName), List(oldRef)))
      case _ : BundleType | _ : VectorType =>
        val newPrefix = prefix + newName + LowerTypes.delim
        val isVecField = oldField.tpe.isInstanceOf[VectorType]
        val fields = getFields(oldField.tpe)
        val fieldsWithCorrectOrientation = fields.map(f => f.copy(flip = Utils.times(f.flip, oldField.flip)))
        val children = fieldsWithCorrectOrientation.flatMap { f =>
          destruct(newPrefix, RefParentRef(oldRef), f, isVecField, rename.flatMap(_.children.get(f.name)))
        }
        // the bundle/vec reference refers to all children
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

  private def getFields(tpe: Type): Seq[Field] = tpe match {
    case BundleType(fields) => fields
    case v : VectorType => vecToBundle(v).fields
  }

  private def vecToBundle(v: VectorType): BundleType = {
    BundleType(( 0 until v.size).map(i => Field(i.toString, Default, v.tpe)))
  }

  private trait ParentRef { def ref(name: String, asVecField: Boolean = false): ReferenceTarget }
  private case class ModuleParentRef(m: ModuleTarget) extends ParentRef {
    override def ref(name: String, asVecField: Boolean): ReferenceTarget = m.ref(name)
  }
  private case class RefParentRef(r: ReferenceTarget) extends ParentRef {
    override def ref(name: String, asVecField: Boolean): ReferenceTarget =
      if(asVecField) { r.index(name.toInt) } else { r.field(name) }
  }
}