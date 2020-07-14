// See LICENSE for license details.

package firrtl.passes

import firrtl.{DuplexFlow, GlobalSymbolTable, Kind, RenameMap, SinkFlow, SourceFlow, StandardSymbolTable, SymbolTable, Utils}
import firrtl.ir._
import scala.collection.mutable

case class LowerTypesOptions(lowerBundles: Boolean, lowerVecs: Boolean, onlyUniquify: Boolean)
object LowerTypesOptions {
  val Default = LowerTypesOptions(lowerBundles = true, lowerVecs = true, onlyUniquify = false)
}


/** Flattens Bundles and Vecs.
  * - If you want to lowerVecs, you need to remove all SubAccess nodes befre running this pass.
  * - Combines the following legacy passes:
  *   - Uniquify, ExpandConnect, LowerTypes
  * - If we are only trying to get rid of bundles, we convert vecs of bundles to bundles of vecs.
  * - Only getting rid of vecs but not bundles just removes all dynamic accesses and turns a vec field into
  *   multiple bundle fields.
  * - We might also want to add an option to turn multi-dimensional vecs into 1D arrays.
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

private case class TypeDestruction()

/** Calculate new type layouts and names.
  */
private class LowerTypes(opts: LowerTypesOptions) {
  type Namespace = mutable.HashSet[String]
  def apply(ref: Reference)(implicit namespace: Namespace, renames: RenameMap): Seq[Reference] = ref.tpe match {
    case BundleType(fields) =>
      fields.foreach { f =>
        val name = ref.name + "." + f.name
        assert(!namespace.contains(name))
        namespace.add(name)
      }
      if(opts.lowerBundles) {
        fields.map(f => Reference(ref.name + "_" + f.name, f.tpe, ref.kind, Utils.times(ref.flow, f.flip))).flatMap(apply)
      } else {
        BundleType(fields.)
      }



    case VectorType(tpe, size) =>

  }


}


object UniquifyNames {
  type Namespace = mutable.HashSet[String]


//  private def uniquify(tpe: Type)(implicit namespace: Namespace): (Type, Seq[String]) = tpe match {
//    case BundleType(fields) =>
//
//  }

//  def apply(tpe: Type, namespace: Namespace)
//
//
//  def apply() = {
//    // Accepts a Type and an initial namespace
//    // Returns new Type with uniquified names
//    private def uniquifyNames(
//                               t: BundleType,
//                               namespace: collection.mutable.HashSet[String])
//                             (implicit sinfo: Info, mname: String): BundleType = {
//      def recUniquifyNames(t: Type, namespace: collection.mutable.HashSet[String]): (Type, Seq[String]) = t match {
//        case tx: BundleType =>
//          // First add everything
//          val newFieldsAndElts = tx.fields map { f =>
//            val newName = findValidPrefix(f.name, Seq(""), namespace)
//            namespace += newName
//            Field(newName, f.flip, f.tpe)
//          } map { f => f.tpe match {
//            case _: GroundType => (f, Seq[String](f.name))
//            case _ =>
//              val (tpe, eltsx) = recUniquifyNames(f.tpe, collection.mutable.HashSet())
//              // Need leading _ for findValidPrefix, it doesn't add _ for checks
//              val eltsNames: Seq[String] = eltsx map (e => "_" + e)
//              val prefix = findValidPrefix(f.name, eltsNames, namespace)
//              // We added f.name in previous map, delete if we change it
//              if (prefix != f.name) {
//                namespace -= f.name
//                namespace += prefix
//              }
//              val newElts: Seq[String] = eltsx map (e => LowerTypes.loweredName(prefix +: Seq(e)))
//              namespace ++= newElts
//              (Field(prefix, f.flip, tpe), prefix +: newElts)
//          }
//          }
//          val (newFields, elts) = newFieldsAndElts.unzip
//          (BundleType(newFields), elts.flatten)
//        case tx: VectorType =>
//          val (tpe, elts) = recUniquifyNames(tx.tpe, namespace)
//          val newElts = ((0 until tx.size) map (i => i.toString)) ++
//            ((0 until tx.size) flatMap { i =>
//              elts map (e => LowerTypes.loweredName(Seq(i.toString, e)))
//            })
//          (VectorType(tpe, tx.size), newElts)
//        case tx => (tx, Nil)
//      }
//      val (tpe, _) = recUniquifyNames(t, namespace)
//      tpe match {
//        case tx: BundleType => tx
//        case tx => throwInternalError(s"uniquifyNames: shouldn't be here - $tx")
//      }
//    }
//  }
}