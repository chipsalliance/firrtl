// See LICENSE for license details.

package firrtl
package transforms

import firrtl.ir._
import firrtl.Mappers._
import firrtl.annotations._
import scala.collection.mutable
import firrtl.passes.{InlineInstances,PassException}

/** Tags an annotation to be consumed by this transform */
object FlattenAnnotation {
  def apply(target: Named): Annotation = Annotation(target, classOf[Flatten], "")

  def unapply(a: Annotation): Option[Named] = a match {
    case Annotation(named, t, _) if t == classOf[Flatten] => Some(named)
    case _ => None
  }
}

/**
 * Takes flatten annotations for module instances and modules and inline the entire hierarchy of modules down from the annotations.
 * This transformation instantiates and is based on the InlineInstances transformation.  
 * Note: Inlining a module means inlining all its children module instances      
 */
class Flatten extends Transform {
   def inputForm = LowForm
   def outputForm = LowForm

   val inlineTransform = new InlineInstances
   
   private def collectAnns(circuit: Circuit, anns: Iterable[Annotation]): (Set[ModuleName], Set[ComponentName]) =
     anns.foldLeft(Set.empty[ModuleName], Set.empty[ComponentName]) {
       case ((modNames, instNames), ann) => ann match {
         case FlattenAnnotation(CircuitName(c)) =>
           (circuit.modules.collect {
             case Module(_, name, _, _) if name != circuit.main => ModuleName(name, CircuitName(c))
           }.toSet, instNames)
         case FlattenAnnotation(ModuleName(mod, cir)) => (modNames + ModuleName(mod, cir), instNames)
         case FlattenAnnotation(ComponentName(com, mod)) => (modNames, instNames + ComponentName(com, mod))
         case _ => throw new PassException("Annotation must be InlineDeepAnnotation")
       }
     }

   /**
    *  Modifies the circuit by replicating the hierarchy under the annotated objects (mods and insts) and
    *  by rewriting the original circuit to refer to the new modules that will be inlined later
    */
   def duplicateSubCircuitsFromAnno(c : Circuit, mods:  Set[ModuleName], insts : Set[ComponentName], modsToInline : mutable.Set[ModuleName]) : Circuit = {
     val modMap = c.modules.map(m => m.name->m) toMap
     val seedMods = mutable.Map.empty[String, String]
     val newModDefs= mutable.Set.empty[DefModule]
     val nsp = Namespace(c)

     /** We start with rewriting original modules that contain annotations.  Populates seedMods */
     def rewriteMod(parent : DefModule)(x : Statement) : Statement = x match {
       case _: Block => x map rewriteMod(parent)
       case WDefInstance(info, instName, moduleName, instTpe) =>
         if (insts.contains(ComponentName(instName, ModuleName(parent.name, CircuitName(c.main))))
           || mods.contains(ModuleName(parent.name, CircuitName(c.main)))) {
           val newModName = nsp newName moduleName+"_TO_FLATTEN"
           seedMods += moduleName -> newModName
           WDefInstance(info, instName, newModName, instTpe)
         } else x
       case _ => x
     }
     
     val modifMods = c.modules map { m => m map rewriteMod(m) }
     
     /** Recursively rewrites modules in the hierarchy driven by seedMods (originally annotations). Populates newModDefs  */
     def recDupMods(mods : Map[String, String]) : Unit = {
       val replMods = mutable.Map.empty[String, String]

       def dupMod(x : Statement) : Statement = x match {
         case _: Block => x map dupMod
         case WDefInstance(info, instName, moduleName, instTpe) =>
             val newModName = nsp newName moduleName+"_TO_FLATTEN"
             replMods += moduleName -> newModName
             WDefInstance(info, instName, newModName, instTpe)
         case _ => x 
       }
       
       def dupName(name : String) : String = mods(name)
       val newMods = mods map { case(origName, newName) => modMap(origName) map dupMod map dupName }
       
       newModDefs ++= newMods
       
       if(replMods.size > 0) 
         recDupMods(replMods.toMap)
       
     }
     recDupMods(seedMods.toMap)

     //convert newly created modules to ModuleName for inlining next (outside this function)
     modsToInline ++= newModDefs map { m => ModuleName(m.name, CircuitName(c.main)) }
     c.copy(modules = modifMods ++ newModDefs)
   }
   
   override def execute( state: CircuitState): CircuitState = {
     getMyAnnotations(state) match {
       case Nil => CircuitState(state.circuit, state.form)
       case myAnnotations =>
         val c = state.circuit
         val (modNames, instNames) = collectAnns(state.circuit, myAnnotations)
         // take incoming annotation and produce annotations for InlineInstances, i.e. traverse circuit down to find all instances to inline
         val modsToInline = mutable.Set.empty[ModuleName]
         val newc = duplicateSubCircuitsFromAnno(state.circuit,modNames, instNames, modsToInline)
         inlineTransform.run(newc, modsToInline.toSet, Set.empty[ComponentName], state.annotations)
     }
   }
}
