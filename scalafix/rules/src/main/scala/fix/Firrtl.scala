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

  override def fix(implicit doc: SyntacticDocument): Patch = {
    println("Tree.syntax: " + doc.tree.syntax)
    //println("Tree.structure: " + doc.tree.structure)
    //println("Tree.structureLabeled: " + doc.tree.structureLabeled)
    val nameMatcher = "(in|out)putForm".r
    doc.tree.collect {
      case formDef @ Defn.Def(_, Term.Name(nameMatcher), List(), List(), _, _ ) if !hasOverrides(formDef) =>
        Patch.addLeft(formDef, "override ")
      case formVal @ Defn.Val(_, List(Pat.Var(Term.Name(nameMatcher))), _, rhs) if !hasOverrides(formVal) =>
        Patch.addLeft(formVal, "override ")
    }.asPatch
  }
}
