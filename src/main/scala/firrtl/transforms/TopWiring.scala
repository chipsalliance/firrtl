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
case class TopWiringOutputFilesAnnotation(dirname: String, outputfunction: (String,String,Seq[((ComponentName, Type, Boolean, Seq[String]), Int)], CircuitState) => CircuitState) extends NoTargetAnnotation

/** Annotation for a prefix that will be added to all the port names that are punched out to the top level **/
case class TopWiringPrefixAnnotation(prefix: String) extends NoTargetAnnotation

/** Annotation for indicating component to be wired */
case class TopWiringAnnotation(target: ComponentName) extends
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

  private def getSourceNames(state: CircuitState): Seq[ComponentName] = {
    val annos = state.annotations.collect {
      case a @ (_: TopWiringAnnotation) => a
    }
    annos match {
      case p => p.map { case TopWiringAnnotation(srcname) => srcname }
    }
  }


  private def getSourceModNames(state: CircuitState): Seq[String] = {
    val annos = state.annotations.collect {
      case a @ (_: TopWiringAnnotation) => a
    }
    annos match {
      case p => p.map { case TopWiringAnnotation(ComponentName(_,ModuleName(srcmodname, _))) => srcmodname }
    }
  }



  def getSourceTypes(sourceList: Seq[ComponentName], sourceMap: mutable.Map[String, Seq[(ComponentName, Type, Boolean, InstPath)]], currentmodule: ModuleName, state: CircuitState)(s: Statement): Statement = s match {
    // If target wire, add name and size to to sourceMap
    case w: IsDeclaration =>
      if (sourceList.contains(ComponentName(w.name, currentmodule))) {
          val (isport, tpe) = w match {
            case d: DefWire => (false, d.tpe)
            case d: DefNode => (false, d.value.tpe)
            case d: DefRegister => (false, d.tpe)
            case d: Port => (true, d.tpe)
            case _ => sys.error(s"Cannot wire this type of declaration! ${w.serialize}")
          }
          val name = w.name
          sourceMap.get(currentmodule.name) match {
            case Some(xs:Seq[(ComponentName, Type, Boolean, InstPath)]) => sourceMap.update(currentmodule.name, xs :+ (ComponentName(w.name,currentmodule), tpe, isport ,Seq[String](w.name)))
            case None => sourceMap(currentmodule.name) = Seq((ComponentName(w.name,currentmodule), tpe, isport ,Seq[String](w.name)))
          }
          //sourceMap(currentmodule.name) +: (ComponentName(w.name,currentmodule), tpe, isport ,Seq[String](currentmodule.name))
      }
      w // Return argument unchanged (ok because DefWire has no Statement children)
    // If not, apply to all children Statement
    case _ => s map getSourceTypes(sourceList, sourceMap, currentmodule, state)
  }




  def getSourceTypesPorts(sourceList: Seq[ComponentName], sourceMap: mutable.Map[String, Seq[(ComponentName, Type, Boolean, InstPath)]], currentmodule: ModuleName, state: CircuitState)(s: Port): CircuitState = s match {
    // If target port, add name and size to to sourceMap
    case w: IsDeclaration =>
      if (sourceList.contains(ComponentName(w.name, currentmodule))) {
          val (isport, tpe) = w match {
            case d: Port => (true, d.tpe)
            case _ => sys.error(s"Cannot wire this type of declaration! ${w.serialize}")
          }
          val name = w.name
          sourceMap.get(currentmodule.name) match {
            case Some(xs:Seq[(ComponentName, Type, Boolean, InstPath)]) => sourceMap.update(currentmodule.name, xs :+ (ComponentName(w.name,currentmodule), tpe, isport ,Seq[String](w.name)))
            case None => sourceMap(currentmodule.name) = Seq((ComponentName(w.name,currentmodule), tpe, isport ,Seq[String](w.name)))
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
  private def getSourcesMap(state: CircuitState): Map[String, Seq[(ComponentName, Type, Boolean, InstPath)]] = {
    val sSourcesModNames = getSourceModNames(state)
    val sSourcesNames = getSourceNames(state)
     

    val cMap = WiringUtils.getChildrenMap(state.circuit).toMap
    //val digraph = DiGraph(cMap.mapValues(v => v.map(_._2).toSet))
    //val topSort = digraph.linearize.reverse
    //previous 2 line replaced by the following single line
    val topSort = (new firrtl.analyses.InstanceGraph(state.circuit)).moduleOrder.reverse

    // Map of component name to relative instance paths that result in a debug wire
    val sourcemods: mutable.Map[String, Seq[(ComponentName, Type, Boolean, InstPath)]] =
      mutable.Map(sSourcesModNames.map(_ -> Seq()): _*)
      //mutable.Map(sSourcesModNames.map(_ -> Seq(ComponentName,UnknownType,false,Seq[String]())): _*)

    state.circuit.modules.foreach { m => m map getSourceTypes(sSourcesNames, sourcemods, ModuleName(m.name, CircuitName(state.circuit.main)) , state) }
    state.circuit.modules.foreach { m => m.ports.foreach { p => Seq(p) map getSourceTypesPorts(sSourcesNames, sourcemods, ModuleName(m.name, CircuitName(state.circuit.main)) , state) }}

    // TODO make this code more clear
    for (mod <- topSort) {
      val seqChildren: Seq[(ComponentName,Type,Boolean,InstPath)] = cMap(mod.name).flatMap { case (inst, module) => 
        sourcemods.get(module).map( _.map { case (a,b,c,path) => (a,b,c, inst +: path)})
      }.flatten
      if (seqChildren.nonEmpty) {
        sourcemods(mod.name) = seqChildren
      }
    }

    sourcemods.toMap
  }



  /** Process a given DefModule
    *
    * For Modules that contain or are in the parent hierarchy to modules containing SeqMems
    * 1. Add ports for each SeqMem this module is parent to
    * 2. Connect these ports to ports of instances that are parents to some number of SeqMems
    */
  // TODO Make code more clear
  private def onModule(prefix: String, sources: Map[String, Seq[(ComponentName, Type, Boolean, InstPath)]])
                      (module: DefModule): DefModule = {
    sources.get(module.name) match {
      case Some(p) => 
        val newPorts = p.map{ case (ComponentName(cname,_), tpe, _ , path) => Port(NoInfo, prefix + path.mkString("_"), Output, tpe)}
        

        // Add connections to Module
        module match {
          case m: Module =>
            val connections: Seq[Connect] = p.map { case (ComponentName(cname,_), _, _ , path) =>
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


  def TopWiringDummyOutputFilesFunction(prefix: String, dir: String, mapping: Seq[((ComponentName, Type, Boolean, InstPath), Int)], state: CircuitState): CircuitState = {
     state  
  }

  def TopWiringTestOutputFilesFunction(prefix: String, dir: String, mapping: Seq[((ComponentName, Type, Boolean, InstPath), Int)], state: CircuitState): CircuitState = {
     val testOutputFile = new PrintWriter(new File(dir, "TopWiringOutputTest.txt" )) 
     mapping map {
          case ((_, tpe, _, path), index) => {
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

    lazy val outputTuple: Option[(String, (String,String,Seq[((ComponentName, Type, Boolean, InstPath), Int)], CircuitState) => CircuitState)] = state.annotations.collectFirst { case TopWiringOutputFilesAnnotation(td,of) => (td, of) }
    lazy val prfix: Option[String] = state.annotations.collectFirst { case TopWiringPrefixAnnotation(pr) => pr }
    
    prfix match {
      case Some(prefix) =>
        // Do actual work of this transform
        val sources = getSourcesMap(state)
        //val prefix = s"topwiring_"
        val modulesx = state.circuit.modules map onModule(prefix,sources)
        val newCircuit = state.circuit.copy(modules = modulesx)
        val fixedCircuit = fixupCircuit(newCircuit)
        val mappings = sources(state.circuit.main).zipWithIndex
        //Generate output files based on the mapping.
        outputTuple match {
          case Some((dir, outputfunction)) =>
             outputfunction(prefix, dir, mappings, state)
          case None => state 
        }
        // fin.
        state.copy(circuit = fixedCircuit)
      case None => // Don't run pass
        throw new Exception("TopWiringAnnotation is specified so the transform should run, " +
                            "but no prefix was found, so wiring naming collisions may occur")
    }
  }
}
