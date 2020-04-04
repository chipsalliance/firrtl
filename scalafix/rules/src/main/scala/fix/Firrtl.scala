package fix

import scalafix.v1._
import scala.meta._

class Firrtl extends SemanticRule("Firrtl") {

  def hasOverrides(defn: Defn.Def): Boolean = {
    defn.mods.collectFirst {
      case x@mod"override" => "override"
    }.nonEmpty
  }

  override def fix(implicit doc: SemanticDocument): Patch = {
    //println("Tree.syntax: " + doc.tree.syntax)
    //println("Tree.structure: " + doc.tree.structure)
    //println("Tree.structureLabeled: " + doc.tree.structureLabeled)
    doc.tree.collect {
      case inputForm @ Defn.Def(_, Term.Name("inputForm"), List(), List(), _, _ ) if !hasOverrides(inputForm) =>
        Patch.addLeft(inputForm, "override ")
      case outputForm @ Defn.Def(_, Term.Name("outputForm"), List(), List(), _, _ ) if !hasOverrides(outputForm) =>
        Patch.addLeft(outputForm, "override ")
    }.asPatch
  }
}
