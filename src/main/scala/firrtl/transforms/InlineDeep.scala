// See LICENSE for license details.

package firrtl
package passes

import firrtl.ir._
import firrtl.Mappers._
import firrtl.annotations._
import scala.collection.mutable

// Tags an annotation to be consumed by this pass
object InlineDeepAnnotation {
  def apply(target: Named): Annotation = Annotation(target, classOf[InlineInstancesDeep], "")

  def unapply(a: Annotation): Option[Named] = a match {
    case Annotation(named, t, _) if t == classOf[InlineInstancesDeep] => Some(named)
    case _ => None
  }
}

//  
// Takes module and instance annotations to inline the entire hierarchy of modules down from given ones
// This transformation is based on InlineInstances transformation. More concretely, it fully reuses run method and overriding execute method to collect the right set of instances to inline 
// Note: Inlining a module means inlining all its children module instances   
class InlineInstancesDeep extends InlineInstances {
   private def collectAnns(circuit: Circuit, anns: Iterable[Annotation]): (Set[ModuleName], Set[ComponentName]) =
     anns.foldLeft(Set.empty[ModuleName], Set.empty[ComponentName]) {
       case ((modNames, instNames), ann) => ann match {
         case InlineDeepAnnotation(CircuitName(c)) =>
           (circuit.modules.collect {
             case Module(_, name, _, _) if name != circuit.main => ModuleName(name, CircuitName(c))
           }.toSet, instNames)
         case InlineDeepAnnotation(ModuleName(mod, cir)) => (modNames + ModuleName(mod, cir), instNames)
         case InlineDeepAnnotation(ComponentName(com, mod)) => (modNames, instNames + ComponentName(com, mod))
         case _ => throw new PassException("Annotation must be InlineDeepAnnotation")
       }
     }

   // Duplicate the circuit such that we inline only the modules that are driven by annotations
   // Along with duplication we rewrite the original circuit to refer to the new modules that will all be inlined
   // 
   def duplicateSubCircuitsFromAnno(c : Circuit, mods:  Set[ModuleName], insts : Set[ComponentName], modsToInline : mutable.Set[ModuleName]) : Circuit = {
     val modMap = c.modules.map(m => m.name->m) toMap
     val seedMods = mutable.Map.empty[String, String]
     val newModDefs= mutable.Set.empty[DefModule]
     
     // We start with rewriting original modules that contain annotations
     def rewriteMod(parent : DefModule)(x : Statement) : Statement = x match {
         case _: Block =>
           x map rewriteMod(parent)
         case WDefInstance(info, instName, moduleName, instTpe) =>
           if (insts.contains(ComponentName(instName, ModuleName(parent.name, CircuitName("Top"))))
            || mods.contains(ModuleName(parent.name, CircuitName("Top")))) {
             val newModName = moduleName+"_DEEPINLINE"
             seedMods += moduleName -> newModName
             WDefInstance(info, instName, newModName, instTpe)
           } else {
             x
           }
         case _ =>
           //println(s"x $x")
           x
       
     }
     
     val modifMods = c.modules map {m => m map rewriteMod(m)}
     
     // we recursively rewrite modules in the hierarchy driven by seedMods (originally annotations)
     def recDupMods(mods : Map[String, String]) : Unit = {
       val modsToDup = mutable.Map.empty[String, String]

       def dupMod(x : Statement) : Statement = x match {
         case _: Block =>
           x map dupMod
         case WDefInstance(info, instName, moduleName, instTpe) =>
             val newModName = moduleName+"_DEEPINLINE"
             modsToDup += moduleName -> newModName
             WDefInstance(info, instName, newModName, instTpe)
         case _ =>
           x 
       }
       
       def dupName(name : String) : String = {
         mods(name)
       }
       val newMods = mods map {case(origName, newName) =>
         modMap(origName) map dupMod map dupName
       }
       newModDefs ++= newMods
       
       if(modsToDup.size > 0) 
         recDupMods(modsToDup.toMap)
       
       //modsToDup.toMap    
     }
     recDupMods(seedMods.toMap)
     
     modsToInline ++= newModDefs.map{m => ModuleName(m.name, CircuitName("Top"))}
     c.copy(modules = modifMods ++ newModDefs)
   }
   
   override def execute( state: CircuitState): CircuitState = {
     getMyAnnotations(state) match {
       case Nil => CircuitState(state.circuit, state.form)
       case myAnnotations =>
         val c = state.circuit
         val (modNames, instNames) = collectAnns(state.circuit, myAnnotations)
         // take incoming annotation and produce annotations for InlineInstances, i.e. traverse circuit down to find all instances to inline
         val modMap = c.modules.map(m => m.name->m) toMap
         val modsToInline = mutable.Set.empty[ModuleName]
         val newc = duplicateSubCircuitsFromAnno(state.circuit,modNames, instNames, modsToInline)
         //val instNamesToInline = goDeep(state.circuit, modMap, modNames, instNames) 
         //println(s"instNames = $instNamesToInline")
         val newCS = run(newc, modsToInline.toSet, Set.empty[ComponentName], state.annotations)
         //println(s"New CIRCUIT ${newCS.circuit.serialize}")
         newCS
     }
   }
}
