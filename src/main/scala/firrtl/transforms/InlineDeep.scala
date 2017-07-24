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
         case _ => throw new PassException("Annotation must be InlineAnnotation")
       }
     }

   
   override def execute( state: CircuitState): CircuitState = {
     getMyAnnotations(state) match {
       case Nil => CircuitState(state.circuit, state.form)
       case myAnnotations =>
         val c = state.circuit
         val (modNames, instNames) = collectAnns(state.circuit, myAnnotations)
         // take incoming annotation and produce annotations for InlineInstances, i.e. traverse circuit down to find all instances to inline
         val modMap = c.modules.map(m => m.name->m) toMap
         val instNamesToInline = goDeep(state.circuit, modMap, modNames, instNames) 
         println(s"instNames = $instNamesToInline")
         run(state.circuit, Set.empty[ModuleName], instNamesToInline, state.annotations)
     }
   }

   // get instances inside given module definition   
   def collectModInsts(m : DefModule) : Map[String, String] = {
     val instModNames = mutable.ListBuffer.empty[(String, String)]
     
     def findInsts (s : Statement) : Statement = s match {
       case WDefInstance(info, instName, moduleName, instTpe) =>
         instModNames += ((instName, moduleName))
         s
       case sx => sx map findInsts
     }
     m map findInsts
     instModNames toMap
   }
   
   // Input is a pair of instanceName and its master moduleName, 
   // returning ComponentName (for instance)-> ModuleName(for master module name). 
   // Requires parent modulename    
   def asComponentNameOf(mn : ModuleName)( instmaster : (String, String)) = instmaster match {
     case (inst,master) => ComponentName(inst, mn)->ModuleName(master, CircuitName("Top"))
   }
   
   // traverse circuit starting from input annotation down and generate a collection of instances to pass to inline transform
   private def goDeep(circuit: Circuit, modMap : Map[String, DefModule], mods:  Set[ModuleName], insts : Set[ComponentName]) : Set[ComponentName] = {
     val modInsts = mods flatMap {mod => collectModInsts(modMap(mod.name)) map asComponentNameOf(mod)} toMap 
     val instsWithMaster = insts map {inst =>
       val parentModInsts = collectModInsts (modMap(inst.module.name))
       val instMasterName = parentModInsts(inst.name) 
       ComponentName(inst.name, inst.module)->ModuleName(instMasterName, CircuitName("Top"))
     } toMap 
     
     def recurCollectInstances(in : Map[ComponentName, ModuleName]) : Map[ComponentName, ModuleName] = {
       in.flatMap {case(cname, master) =>
         val modInsts = collectModInsts(modMap(master.name))
         in ++ recurCollectInstances(modInsts map asComponentNameOf(ModuleName(master.name, CircuitName("Top"))))
       }
     }
     recurCollectInstances(instsWithMaster ++ modInsts).keySet
   }
}
