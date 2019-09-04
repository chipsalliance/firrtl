// See LICENSE for license details.

package firrtl

import firrtl.ir._
import firrtl.annotations._
import firrtl.Mappers._
import firrtl.options.{Dependency, PreservesAll}

sealed trait DescriptionType
case object DocStringDescription extends DescriptionType
case object AttributeDescription extends DescriptionType

case class DescriptionAnnotation(named: Named, description: String, tpe: DescriptionType) extends Annotation {
  def update(renames: RenameMap): Seq[DescriptionAnnotation] = {
    renames.get(named) match {
      case None => Seq(this)
      case Some(seq) => seq.map(n => this.copy(named = n))
    }
  }
}

private sealed trait HasDescription {
  def descriptions: Seq[Description]
}

private sealed trait Description extends FirrtlNode

private case class DocString(string: StringLit) extends Description {
  def serialize: String = "@[" + string.serialize + "]"
}

private case class Attribute(string: StringLit) extends Description {
  def serialize: String = "@[" + string.serialize + "]"
}

private object MakeDescription {
  def apply(s: FirrtlNode, str: String, tpe: DescriptionType): Description = tpe match {
    case DocStringDescription => DocString(StringLit.unescape(str))
    case AttributeDescription => Attribute(StringLit.unescape(str))
  }
  def apply(s: FirrtlNode)(descs: Seq[(String, DescriptionType)]): Seq[Description] = {
    // combine DocStringDescriptions (multi-line comment)
    val docString = descs.collect({
      case (s: String, _: DocStringDescription.type) => s
    }).mkString("\n\n")
    val docStringDesc = if (docString.length > 0) Some(MakeDescription(s, docString, DocStringDescription)) else None
    // combine AttributeDescriptions (comma separated)
    val attr = descs.collect({
      case (s: String, _: AttributeDescription.type) => s
    }).mkString(", ")
    val attrDesc = if (attr.length > 0) Some(MakeDescription(s, attr, AttributeDescription)) else None
    Seq(docStringDesc, attrDesc).flatten
  }
}

private case class DescribedStmt(descriptions: Seq[Description], stmt: Statement) extends Statement with HasDescription {
  def serialize: String = s"${descriptions.map(_.serialize).mkString("\n")}\n${stmt.serialize}"
  def mapStmt(f: Statement => Statement): Statement = f(stmt)
  def mapExpr(f: Expression => Expression): Statement = this.copy(stmt = stmt.mapExpr(f))
  def mapType(f: Type => Type): Statement = this.copy(stmt = stmt.mapType(f))
  def mapString(f: String => String): Statement = this.copy(stmt = stmt.mapString(f))
  def mapInfo(f: Info => Info): Statement = this.copy(stmt = stmt.mapInfo(f))
  def foreachStmt(f: Statement => Unit): Unit = f(stmt)
  def foreachExpr(f: Expression => Unit): Unit = stmt.foreachExpr(f)
  def foreachType(f: Type => Unit): Unit = stmt.foreachType(f)
  def foreachString(f: String => Unit): Unit = stmt.foreachString(f)
  def foreachInfo(f: Info => Unit): Unit = stmt.foreachInfo(f)
}

private case class DescribedMod(descriptions: Seq[Description],
  portDescriptions: Map[String, Seq[Description]],
  mod: DefModule) extends DefModule with HasDescription {
  val info = mod.info
  val name = mod.name
  val ports = mod.ports
  def serialize: String = s"${descriptions.map(_.serialize).mkString("\n")}\n${mod.serialize}"
  def mapStmt(f: Statement => Statement): DefModule = this.copy(mod = mod.mapStmt(f))
  def mapPort(f: Port => Port): DefModule = this.copy(mod = mod.mapPort(f))
  def mapString(f: String => String): DefModule = this.copy(mod = mod.mapString(f))
  def mapInfo(f: Info => Info): DefModule = this.copy(mod = mod.mapInfo(f))
  def foreachStmt(f: Statement => Unit): Unit = mod.foreachStmt(f)
  def foreachPort(f: Port => Unit): Unit = mod.foreachPort(f)
  def foreachString(f: String => Unit): Unit = mod.foreachString(f)
  def foreachInfo(f: Info => Unit): Unit = mod.foreachInfo(f)
}

/** Wraps modules or statements with their respective described nodes. Descriptions come from [[DescriptionAnnotation]].
  * Describing a module or any of its ports will turn it into a `DescribedMod`. Describing a Statement will turn it into
  * a (private) `DescribedStmt`.
  *
  * @note should only be used by VerilogEmitter, described nodes will
  *       break other transforms.
  */
class AddDescriptionNodes extends Transform with DependencyAPIMigration with PreservesAll[Transform] {

  override def prerequisites = firrtl.stage.Forms.LowFormMinimumOptimized ++
    Seq( Dependency[firrtl.transforms.BlackBoxSourceHelper],
         Dependency[firrtl.transforms.FixAddingNegativeLiterals],
         Dependency[firrtl.transforms.ReplaceTruncatingArithmetic],
         Dependency[firrtl.transforms.InlineBitExtractionsTransform],
         Dependency[firrtl.transforms.PropagatePresetAnnotations],
         Dependency[firrtl.transforms.InlineCastsTransform],
         Dependency[firrtl.transforms.LegalizeClocksTransform],
         Dependency[firrtl.transforms.FlattenRegUpdate],
         Dependency(passes.VerilogModulusCleanup),
         Dependency[firrtl.transforms.VerilogRename],
         Dependency(firrtl.passes.VerilogPrep) )

  override def optionalPrerequisites = firrtl.stage.Forms.LowFormOptimized

  override def optionalPrerequisiteOf = Seq.empty

  def onStmt(compMap: Map[String, Seq[(String, DescriptionType)]])(stmt: Statement): Statement = {
    val s = stmt.map(onStmt(compMap))
    val sname = s match {
      case d: IsDeclaration => Some(d.name)
      case _ => None
    }
    val descs = sname.flatMap({ case name =>
      compMap.get(name).map { MakeDescription(s)(_) }
    })
    descs match {
      case Some(d) => DescribedStmt(d, s)
      case None => s
    }
  }

  def onModule(modMap: Map[String, Seq[(String, DescriptionType)]], compMaps: Map[String, Map[String, Seq[(String, DescriptionType)]]])
    (mod: DefModule): DefModule = {
    val compMap = compMaps.getOrElse(mod.name, Map())
    val newMod = mod.mapStmt(onStmt(compMap))
    val portDesc = mod.ports.collect {
      case p @ Port(_, name, _, _) if compMap.contains(name) =>
        name -> MakeDescription(p)(compMap(name))
    }.toMap

    val modDesc = MakeDescription(mod)(modMap.get(newMod.name).getOrElse(Seq()))

    if (portDesc.nonEmpty || modDesc.nonEmpty) {
      DescribedMod(modDesc, portDesc, newMod)
    } else {
      newMod
    }
  }

  def collectMaps(annos: Seq[Annotation]): (Map[String, Seq[(String, DescriptionType)]], Map[String, Map[String, Seq[(String, DescriptionType)]]]) = {
    val modMap = annos.collect {
      case DescriptionAnnotation(ModuleName(m, CircuitName(c)), desc, tpe) => (m, desc, tpe)
    }.groupBy(_._1).mapValues(_.map(x => (x._2, x._3)))

    val compMap = annos.collect {
      case DescriptionAnnotation(ComponentName(comp, ModuleName(mod, CircuitName(circ))), desc, tpe) =>
        (mod, comp, desc, tpe)
    }.groupBy(_._1).mapValues(_.groupBy(_._2).mapValues(_.map(x => (x._3, x._4))))

    (modMap, compMap)
  }

  def executeModule(module: DefModule, annos: Seq[Annotation]): DefModule = {
    val (modMap, compMap) = collectMaps(annos)

    onModule(modMap, compMap)(module)
  }

  override def execute(state: CircuitState): CircuitState = {
    val (modMap, compMap) = collectMaps(state.annotations)

    state.copy(circuit = state.circuit.mapModule(onModule(modMap, compMap)))
  }
}
