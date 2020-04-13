package fix

import scalafix.v1._
import scala.meta._

class Firrtl extends SyntacticRule("Firrtl") {

  def hasOverrides(defn: Defn.Def): Boolean = {
    defn.mods.collectFirst {
      case x@mod"override" => "override"
    }.nonEmpty
  }
  def hasOverrides(defn: Defn.Val): Boolean = {
    defn.mods.collectFirst {
      case x@mod"override" => "override"
    }.nonEmpty
  }
  def isName(name: String): Boolean = {
    name == "inputForm" || name == "outputForm"
  }

  override def fix(implicit doc: SyntacticDocument): Patch = {
    //Note: Keep these printlns around, they are necessary to debug/develop scalafix patches
    //println("Tree.syntax: " + doc.tree.syntax)
    //println("Tree.structure: " + doc.tree.structure)
    //println("Tree.structureLabeled: " + doc.tree.structureLabeled)
    doc.tree.collect {
      case formDef @ Defn.Def(_, Term.Name(name), List(), List(), _, _ ) if !hasOverrides(formDef) && isName(name) =>
        Patch.addLeft(formDef, "override ")
      case formVal @ Defn.Val(_, List(Pat.Var(Term.Name(name))), _, rhs) if !hasOverrides(formVal) && isName(name) =>
        Patch.addLeft(formVal, "override ")
    }.asPatch
  }
}
