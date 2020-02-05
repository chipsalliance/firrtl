// See LICENSE for license details.

package firrtl

import firrtl.ir._
import firrtl.annotations._
import firrtl.Mappers._

sealed trait DescriptionAnnotation extends Annotation {
  val named: Named
  val description: String

}

case class DocStringAnnotation(named: Named, description: String) extends DescriptionAnnotation {
  def update(renames: RenameMap): Seq[DocStringAnnotation] = {
    renames.get(named) match {
      case None => Seq(this)
      case Some(seq) => seq.map(n => this.copy(named = n))
    }
  }
}
case class AttributeAnnotation(named: Named, description: String) extends DescriptionAnnotation {
  def update(renames: RenameMap): Seq[AttributeAnnotation] = {
    renames.get(named) match {
      case None => Seq(this)
      case Some(seq) => seq.map(n => this.copy(named = n))
    }
  }
}

private sealed trait HasDescription {
  def descriptions: Seq[Description]
}

sealed trait Description extends FirrtlNode

case class DocString(string: StringLit) extends Description {
  def serialize: String = "@[" + string.serialize + "]"
}

case class Attribute(string: StringLit) extends Description {
  def serialize: String = "@[" + string.serialize + "]"
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
class AddDescriptionNodes extends Transform {
  def inputForm = LowForm
  def outputForm = LowForm

  def onStmt(compMap: Map[String, Seq[Description]])(stmt: Statement): Statement = {
    val s = stmt.map(onStmt(compMap))
    val sname = s match {
      case d: IsDeclaration => Some(d.name)
      case _ => None
    }
    val descs = sname.flatMap({ case name =>
      compMap.get(name)
    })
    (descs, s) match {
      case (Some(d), DescribedStmt(prevDescs, ss)) => DescribedStmt(prevDescs ++ d, ss)
      case (Some(d), ss) => DescribedStmt(d, ss)
      case (None, _) => s
    }
  }

  def onModule(modMap: Map[String, Seq[Description]], compMaps: Map[String, Map[String, Seq[Description]]])
    (mod: DefModule): DefModule = {
    val compMap = compMaps.getOrElse(mod.name, Map())
    val newMod = mod.mapStmt(onStmt(compMap))
    val portDesc = mod.ports.collect {
      case p @ Port(_, name, _, _) if compMap.contains(name) =>
        name -> compMap(name)
    }.toMap

    val modDesc = modMap.get(newMod.name).getOrElse(Seq())

    if (portDesc.nonEmpty || modDesc.nonEmpty) {
      DescribedMod(modDesc, portDesc, newMod)
    } else {
      newMod
    }
  }

  /**
    * Merges descriptions of like types.
    *
    * Multiple DocStrings on the same object get merged together into one big multi-line comment.
    * Similarly, multiple attributes on the same object get merged into one attribute with attributes separated by
    * commas.
    * @param descs List of `Description`s that are modifying the same object
    * @return List of `Description`s with some descriptions merged
    */
  def mergeDescriptions(descs: Seq[Description]): Seq[Description] = {
    val (docs: Seq[DocString], nodocs) = descs.partition(_ match {
      case _: DocString => true
      case _ => false
    })
    val (attrs: Seq[Attribute], rest) = nodocs.partition(_ match {
      case _: Attribute => true
      case _ => false
    })

    val doc = if (docs.nonEmpty) {
      Seq(DocString(StringLit.unescape(docs.map(_.string.string).mkString("\n\n"))))
    } else {
      Seq()
    }
    val attr = if (attrs.nonEmpty) {
      Seq(Attribute(StringLit.unescape(attrs.map(_.string.string).mkString(", "))))
    } else {
      Seq()
    }

    rest ++ doc ++ attr
  }

  def collectMaps(annos: Seq[Annotation]): (Map[String, Seq[Description]], Map[String, Map[String, Seq[Description]]]) = {
    val modMap: Map[String, Seq[Description]] = annos.collect {
      case DocStringAnnotation(ModuleName(m, _), desc) => (m, DocString(StringLit.unescape(desc)))
      case AttributeAnnotation(ModuleName(m, _), desc) => (m, Attribute(StringLit.unescape(desc)))
    }.groupBy(_._1).mapValues(_.map(_._2)).mapValues(mergeDescriptions)

    val compMap = annos.collect {
      case DocStringAnnotation(ComponentName(c, ModuleName(m, _)), desc) =>
        (m, c, DocString(StringLit.unescape(desc)))
      case AttributeAnnotation(ComponentName(c, ModuleName(m, _)), desc) =>
        (m, c, Attribute(StringLit.unescape(desc)))
    }.groupBy(_._1).mapValues(_.groupBy(_._2).mapValues(_.map(_._3)).mapValues(mergeDescriptions))

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
