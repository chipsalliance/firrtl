// See LICENSE for license details.
package firrtl.transform
package TopWiring

import firrtl._
import firrtl.ir._
import firrtl.passes._
import firrtl.annotations._
import firrtl.Mappers._
import firrtl.graph._

import java.io._
import scala.io.Source
import collection.mutable

import firrtl.passes.wiring._

/** Annotation for optional output files **/
case class TopWiringOutputFilesAnnotation(dirName: String, outputFunction: (String,Seq[((ComponentName, Type, Boolean, Seq[String],String), Int)], CircuitState) => CircuitState) extends NoTargetAnnotation

/** Annotation for indicating component to be wired, and what prefix to add to the ports that are generated */
case class TopWiringAnnotation(target: ComponentName, prefix: String) extends
    SingleTargetAnnotation[ComponentName] {
  def duplicate(n: ComponentName) = this.copy(target = n)
}


/** Punch out annotated ports out to the toplevel of the circuit.
    This also has an option to pass a function as a parmeter to generate custom output files as a result of the additional ports
  * @note This *does* work for deduped modules
  */

class TopWiringTransform extends Transform {
  def inputForm: CircuitForm = MidForm
  def outputForm: CircuitForm = MidForm

  type InstPath = Seq[String]
 
  /** Get the names of the targets that need to be wired */

  private def getSourceNames(state: CircuitState): Map[ComponentName, String] = {
    state.annotations.collect { case TopWiringAnnotation(srcname,prefix) => (srcname -> prefix) }.toMap.withDefaultValue("")
  }


  private def getSourceModNames(state: CircuitState): Seq[String] = {
    state.annotations.collect { case TopWiringAnnotation(ComponentName(_,ModuleName(srcmodname, _)),_) => srcmodname }
  }



  def getSourceTypes(sourceList: Map[ComponentName, String], sourceMap: mutable.Map[String, Seq[(ComponentName, Type, Boolean, InstPath, String)]], currentmodule: ModuleName, state: CircuitState)(s: Statement): Statement = s match {
    // If target wire, add name and size to to sourceMap
    case w: IsDeclaration =>
      if (sourceList.keys.toSeq.contains(ComponentName(w.name, currentmodule))) {
          val (isport, tpe, prefix) = w match {
            case d: DefWire => (false, d.tpe, sourceList(ComponentName(w.name,currentmodule)))
            case d: DefNode => (false, d.value.tpe, sourceList(ComponentName(w.name,currentmodule)))
            case d: DefRegister => (false, d.tpe, sourceList(ComponentName(w.name,currentmodule)))
            case d: Port => (true, d.tpe, sourceList(ComponentName(w.name,currentmodule)))
            case _ => throw new Exception(s"Cannot wire this type of declaration! ${w.serialize}")
          }
          val name = w.name
          sourceMap.get(currentmodule.name) match {
            case Some(xs:Seq[(ComponentName, Type, Boolean, InstPath, String)]) => sourceMap.update(currentmodule.name, xs :+ (ComponentName(w.name,currentmodule), tpe, isport ,Seq[String](w.name), prefix))
            case None => sourceMap(currentmodule.name) = Seq((ComponentName(w.name,currentmodule), tpe, isport ,Seq[String](w.name), prefix))
          }
          //sourceMap(currentmodule.name) +: (ComponentName(w.name,currentmodule), tpe, isport ,Seq[String](currentmodule.name))
      }
      w // Return argument unchanged (ok because DefWire has no Statement children)
    // If not, apply to all children Statement
    case _ => s map getSourceTypes(sourceList, sourceMap, currentmodule, state)
  }




  def getSourceTypesPorts(sourceList: Map[ComponentName, String], sourceMap: mutable.Map[String, Seq[(ComponentName, Type, Boolean, InstPath, String)]], currentmodule: ModuleName, state: CircuitState)(s: Port): CircuitState = s match {
    // If target port, add name and size to to sourceMap
    case w: IsDeclaration =>
      if (sourceList.keys.toSeq.contains(ComponentName(w.name, currentmodule))) {
          val (isport, tpe, prefix) = w match {
            case d: Port => (true, d.tpe, sourceList(ComponentName(w.name,currentmodule)))
            case _ => throw new Exception(s"Cannot wire this type of declaration! ${w.serialize}")
          }
          val name = w.name
          sourceMap.get(currentmodule.name) match {
            case Some(xs:Seq[(ComponentName, Type, Boolean, InstPath, String)]) => sourceMap.update(currentmodule.name, xs :+ (ComponentName(w.name,currentmodule), tpe, isport ,Seq[String](w.name), prefix))
            case None => sourceMap(currentmodule.name) = Seq((ComponentName(w.name,currentmodule), tpe, isport ,Seq[String](w.name), prefix))
          }
          //sourceMap(currentmodule.name) +: (ComponentName(w.name,currentmodule), tpe, isport ,Seq[String](currentmodule.name))
      }
      state // Return argument unchanged (ok because DefWire has no Statement children)
    // If not, apply to all children Statement
    case _ => state
  }







  /** Create a map of Module name to target wires under this module
    *
    * These paths are relative but cross module (they refer down through instance hierarchy)
    */
  private def getSourcesMap(state: CircuitState): Map[String, Seq[(ComponentName, Type, Boolean, InstPath, String)]] = {
    val sSourcesModNames = getSourceModNames(state)
    val sSourcesNames = getSourceNames(state)
     

    val cMap = WiringUtils.getChildrenMap(state.circuit).toMap
    //val digraph = DiGraph(cMap.mapValues(v => v.map(_._2).toSet))
    //val topSort = digraph.linearize.reverse
    //previous 2 line replaced by the following single line
    val topSort = (new firrtl.analyses.InstanceGraph(state.circuit)).moduleOrder.reverse

    // Map of component name to relative instance paths that result in a debug wire
    val sourcemods: mutable.Map[String, Seq[(ComponentName, Type, Boolean, InstPath, String)]] =
      mutable.Map(sSourcesModNames.map(_ -> Seq()): _*)

    state.circuit.modules.foreach { m => m map getSourceTypes(sSourcesNames, sourcemods, ModuleName(m.name, CircuitName(state.circuit.main)) , state) }
    state.circuit.modules.foreach { m => m.ports.foreach { p => Seq(p) map getSourceTypesPorts(sSourcesNames, sourcemods, ModuleName(m.name, CircuitName(state.circuit.main)) , state) }}

    for (mod <- topSort) {
      val seqChildren: Seq[(ComponentName,Type,Boolean,InstPath,String)] = cMap(mod.name).flatMap { case (inst, module) => 
        sourcemods.get(module).map( _.map { case (a,b,c,path,p) => (a,b,c, inst +: path, p)})
      }.flatten
      if (seqChildren.nonEmpty) {
        sourcemods(mod.name) = seqChildren
      }
    }

    sourcemods.toMap
  }



  /** Process a given DefModule
    *
    * For Modules that contain or are in the parent hierarchy to modules containing target wires
    * 1. Add ports for each target wire this module is parent to
    * 2. Connect these ports to ports of instances that are parents to some number of target wires
    */
  // TODO Make code more clear
  private def onModule(sources: Map[String, Seq[(ComponentName, Type, Boolean, InstPath, String)]])
                      (module: DefModule): DefModule = {
    sources.get(module.name) match {
      case Some(p) => 
        val newPorts = p.map{ case (ComponentName(cname,_), tpe, _ , path, prefix) => Port(NoInfo, prefix + path.mkString("_"), Output, tpe)}
        

        // Add connections to Module
        module match {
          case m: Module =>
            val connections: Seq[Connect] = p.map { case (ComponentName(cname,_), _, _ , path, prefix) =>
                val modRef = WRef(prefix + path.mkString("_"))
                path.size match {
                   case 1 => {
                       val leafRef = WRef(path.head.mkString("_"))
                       Connect(NoInfo, modRef, leafRef)
                   }
                   case _ =>  {
                       val instRef = WSubField(WRef(path.head), prefix + path.tail.mkString("_"))
                       Connect(NoInfo, modRef, instRef)
                  }
                }
            } 
            m.copy(ports = m.ports ++ newPorts, body = Block(Seq(m.body) ++ connections ))
          case e: ExtModule =>
            e.copy(ports = e.ports ++ newPorts)
      }
      case None => module // unchanged if no paths
    }
  }

  private def fixupCircuit(circuit: Circuit): Circuit = {
    val passes = Seq(
      InferTypes,
      ResolveKinds,
      ResolveGenders
    )
    passes.foldLeft(circuit) { case (c: Circuit, p: Pass) => p.run(c) }
  }


  def TopWiringDummyOutputFilesFunction(dir: String, mapping: Seq[((ComponentName, Type, Boolean, InstPath, String), Int)], state: CircuitState): CircuitState = {
     state  
  }

  def TopWiringTestOutputFilesFunction(dir: String, mapping: Seq[((ComponentName, Type, Boolean, InstPath, String), Int)], state: CircuitState): CircuitState = {
     val testOutputFile = new PrintWriter(new File(dir, "TopWiringOutputTest.txt" )) 
     mapping map {
          case ((_, tpe, _, path,prefix), index) => {
            val portwidth = tpe match { case GroundType(IntWidth(w)) => w }
            val portnum = index
            val portname = prefix + path.mkString("_")
            testOutputFile.append(s"new top level port $portnum : $portname, with width $portwidth \n")
          }
     }
     testOutputFile.close()
     state 
  }


  def execute(state: CircuitState): CircuitState = {

    val outputTuple: Option[(String,(String,Seq[((ComponentName, Type, Boolean, InstPath, String), Int)], CircuitState) => CircuitState)] = state.annotations.collectFirst { case TopWiringOutputFilesAnnotation(td,of) => (td, of) }
    
    // Do actual work of this transform
    val sources = getSourcesMap(state)
    val modulesx = state.circuit.modules map onModule(sources)
    val newCircuit = state.circuit.copy(modules = modulesx)
    val fixedCircuit = fixupCircuit(newCircuit)
    val mappings = sources(state.circuit.main).zipWithIndex
    //Generate output files based on the mapping.
    outputTuple match {
    case Some((dir, outputfunction)) =>
        outputfunction(dir, mappings, state)
    case None => state 
    }
    // fin.
    state.copy(circuit = fixedCircuit)
  }
}
