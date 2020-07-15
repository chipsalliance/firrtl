// See LICENSE for license details.

package firrtl.passes

import firrtl.annotations.{IsMember, ModuleName, ModuleTarget, ReferenceTarget}
import firrtl.{DuplexFlow, GlobalSymbolTable, Kind, RenameMap, SinkFlow, SourceFlow, StandardSymbolTable, SymbolTable, Utils}
import firrtl.ir._

import scala.collection.mutable

case class LowerTypesOptions(lowerBundles: Boolean, lowerVecs: Boolean, onlyUniquify: Boolean)
object LowerTypesOptions {
  val Default = LowerTypesOptions(lowerBundles = true, lowerVecs = true, onlyUniquify = false)
}


/** Flattens Bundles and Vecs.
  * - all SubAccess nodes need to be removed before running this pass.
  * - Combines the following legacy passes:
  *   - Uniquify, ExpandConnect, LowerTypes
  * - Some implicit bundle types remain, but with a limited depth:
  *   - the type of a memory is still a bundle with depth 2 (mem -> port -> field)
  *   - the type of a module instance is still a bundle with depth 1 (instance -> port)
  */
private class NewLowerTypes(c: Circuit, opt: LowerTypesOptions = LowerTypesOptions.Default) {
  import NewLowerTypes._
  val renames = RenameMap()
  renames.setCircuit(c.main)
  private val global = GlobalSymbolTable.scanModuleTypes(c, new GlobalLoweringTable(renames))

  def onModule(m: Module): Module = {
    val symbols = SymbolTable.scanModule(new LoweringTable(global, lowerVecs = opt.lowerVecs), m)

    // TODO
    m
  }


}

private object NewLowerTypes {
  // very similar to Utils.create_exps
  def onExpression(e: Expression)(implicit symbols: LoweringTable): Seq[Expression] = e match {
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
  def onExpressionOne(e: Expression)(implicit symbols: LoweringTable): Expression = {
    val es = onExpression(e)
    assert(es.length == 1)
    es.head
  }

  //scalastyle:off cyclomatic.complexity
  def onStatement(s: Statement)(implicit symbols: LoweringTable): Seq[Statement] = s match {
    // declarations
    case DefWire(info, name, _) =>
      symbols.getReferences(name).map(r => DefWire(info, r.name, r.tpe))
    case DefRegister(info, name, _, clock, reset, init) =>
      val loweredClock = onExpressionOne(clock)
      val loweredReset = onExpressionOne(reset)
      val inits = onExpression(init)
      val refs = symbols.getReferences(name)
      refs.zip(inits).map{ case (r, i) => DefRegister(info, r.name, r.tpe, loweredClock, loweredReset, i)}
    case DefMemory(info, name, _, depth, wLatency, rLatency, rs, ws, rws, readUnderWrite) =>
      // TODO: can the name of readers/writers change?
      val refs = symbols.getReferences(name)
      refs.zip(symbols.getDataTypes(name)).map { case (r, dataTpe) =>
        DefMemory(info, r.name, dataTpe, depth, wLatency, rLatency, rs, ws, rws, readUnderWrite)
      }
    case DefInstance(info, name, module, _) =>
      val List(ref) = symbols.getReferences(name)
      List(DefInstance(info, ref.name, module, ref.tpe))
    case DefNode(info, name, value) =>
      val refs = symbols.getReferences(name)
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
}

private class GlobalLoweringTable(val renames: RenameMap) extends GlobalSymbolTable {
  override def declare(name: String, tpe: BundleType): Unit = {

  }

  override def moduleType(name: String): Option[BundleType] = ???
}

private class LoweringTable(global: GlobalLoweringTable, lowerVecs: Boolean) extends StandardSymbolTable(global) {
  // TODO: compute flow from declaration similar to how it is done in ExpandConnects
  def getReferences(name: String): List[Reference] = ???

  def getReferences(expr: ExpressionWithFlow, lhs: Boolean): List[ExpressionWithFlow] = expr match {
    case SubAccess(expr, index, tpe, flow) =>
      assert(!lowerVecs, "If you want to lower vecs, please run RemoveAccesses first!")
      ???
    case SubIndex(expr, value, tpe, flow) => ???
    case Reference(name, tpe, kind, flow) => ???
    case SubField(expr, name, tpe, flow) => ???
  }
  def getDataTypes(name: String): List[Type] = ???
}

//scalastyle:off

/** Calculate new type layouts and names.
  */
private class DestructTypes(opts: LowerTypesOptions) {
  assert(opts.lowerBundles && opts.lowerVecs, "for now we can only lower bundles and vecs together!")
  assert(!opts.onlyUniquify, "only uniquify is not supported at the moment!")
  type Namespace = mutable.HashSet[String]

  /** Does the following with a reference:
    * - rename reference and any bundle fields to avoid name collisions after destruction
    * - updates rename map with new targets
    * - generates all ground type fields
    */
  def destruct(parent: ModuleTarget, ref: Field, namespace: Namespace, renameMap: RenameMap): Seq[Field] = {
    // ensure that the field name is part of the namespace
    namespace.add(ref.name)
    // field renames (uniquify) are computed bottom up
    val (rename, _) = uniquify(ref, namespace)

    // the reference renames are computed top down since they do need the full path
    destruct(parent, ref, rename)(renameMap)
  }

  private def destruct(m: ModuleTarget, field: Field, rename: Option[RenameNode])
                      (implicit renameMap: RenameMap): Seq[Field] =
    destruct(m, prefix = "", oldParent = None, oldField = field, isVecField = false, rename = rename)

  private def destruct(m: ModuleTarget, prefix: String,
                       oldParent: Option[ReferenceTarget], oldField: Field,
                       isVecField: Boolean, rename: Option[RenameNode])
                      (implicit renameMap: RenameMap): Seq[Field] = {
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
        if(isRenamed) {
          renameMap.record(oldRef, ref)
          List(oldField.copy(name = prefix + newName))
        }
        else { List(oldField) }
      case _ : BundleType | _ : VectorType =>
        val isVecField = oldField.tpe.isInstanceOf[VectorType]
        val fields = oldField.tpe match {
          case v : VectorType => vecToBundle(v).fields
          case BundleType(fields) => fields
        }
        val children = fields.map(_.copy(flip = oldField.flip)).flatMap { f =>
          destruct(m, newPrefix, Some(oldRef), f, isVecField, rename.flatMap(_.children.get(f.name)))
        }
        val childRefs = children.map(c => m.ref(c.name))
        renameMap.record(oldRef, childRefs)
        children
    }
  }

  private case class RenameNode(name: String, children: Map[String, RenameNode])

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
      if(opts.lowerVecs) {
        uniquify(ref.copy(tpe = vecToBundle(v)), namespace)
      } else {
        throw new NotImplementedError("TODO")
      }
    case _ : GroundType => (None, List(ref.name))
  }

  private def vecToBundle(v: VectorType): BundleType = {
    BundleType(( 0 until v.size).map(i => Field(i.toString, Default, v.tpe)))
  }
}