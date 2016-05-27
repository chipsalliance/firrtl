/*
Copyright (c) 2014 - 2016 The Regents of the University of
California (Regents). All Rights Reserved.  Redistribution and use in
source and binary forms, with or without modification, are permitted
provided that the following conditions are met:
   * Redistributions of source code must retain the above
     copyright notice, this list of conditions and the following
     two paragraphs of disclaimer.
   * Redistributions in binary form must reproduce the above
     copyright notice, this list of conditions and the following
     two paragraphs of disclaimer in the documentation and/or other materials
     provided with the distribution.
   * Neither the name of the Regents nor the names of its contributors
     may be used to endorse or promote products derived from this
     software without specific prior written permission.
IN NO EVENT SHALL REGENTS BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS,
ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
REGENTS HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE. THE SOFTWARE AND ACCOMPANYING DOCUMENTATION, IF
ANY, PROVIDED HEREUNDER IS PROVIDED "AS IS". REGENTS HAS NO OBLIGATION
TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
MODIFICATIONS.
*/

package firrtl
package passes

import com.typesafe.scalalogging.LazyLogging
import java.nio.file.{Paths, Files}

// Datastructures
import scala.collection.mutable
import scala.collection.IndexedSeqLike
import scala.collection.IndexedSeq

import firrtl.Mappers._
import firrtl.Utils._
import firrtl.PrimOps._
import firrtl.Annotations.{AnnotationMap, Annotation, Loose, Unstable, TransID, AnnotationException, Named, CircuitName}

/** ==============================================================
  * WARNING!! Not tested and incomplete code. Use at your own risk
  * ==============================================================
  */

/**
 * Annotation consumed by HierarchyChange
 * @param target name of circuit
 * @param tID TransID of HierarchyChange
 * @param source path to source instance
 * @param dest path to destination instance
 * @param oldModRenames new names of duplicated modules in source path
 * @param newModRenames new names of duplicated modules in destination path
 * @param portName unique name of port connecting to the ports of the destination instance
 */
case class HierarchyAnnotation(
  target: Named,
  tID: TransID,
  source: InstModulePath,
  dest: InstModulePath,
  oldModRenames: Map[String, String],
  newModRenames: Map[String, String],
  portName: String
) extends Annotation with Loose with Unstable {
  def duplicate(n: Named) = this.copy(target=n)
}

/**
 * Use to reference an instance of a given module
 */
case class InstModulePair(inst: String, mod: String)

/**
 * Use to reference a given instance in a circuit
 * TODO(izraelevitz): Why use IndexedSeq and IndexedSeqLike?
 */
class InstModulePath (val path: Seq[InstModulePair]) extends IndexedSeq[InstModulePair] with IndexedSeqLike[InstModulePair, InstModulePath] {
  
  def prepend (im: InstModulePair): InstModulePath = new InstModulePath(path.+:(im))
  def length = path.length
  override def iterator = path.iterator
  def apply (idx: Int) = path(idx)
  override def toString () = {
    path.map(_.toString).reduce(_ + "/" + _)
  }
  override protected[this] def newBuilder: mutable.Builder[InstModulePair, InstModulePath] =
    new mutable.ArrayBuffer mapResult { (buf: Seq[InstModulePair]) =>
      new InstModulePath(buf)
    }
  def getIndex (x: InstModulePair): Int = getIndex(x.mod)
  def getIndex (mName: String): Int = path.indexWhere(_.mod == mName)
  def getChild(moduleName: String): Option[InstModulePair] = {
    val index = getIndex(moduleName)
    //println("For " + moduleName + " in " + path + " returning " + (index + 1))
    if (index == path.length - 1) None
    else Some(path(index + 1))
  }
  def isDirectParent (x: InstModulePair) = getIndex(x) == path.length - 2
  def isNonDirectParent (x: InstModulePair) = (getIndex(x) < path.length - 2) && (getIndex(x) != -1)
  def isNotTop (x: InstModulePair) = getIndex(x) != 0
  def isNotLeaf (x: InstModulePair) = getIndex(x) != path.length - 1
}

/**
 * Moves an instance to a difference location
 */
class HierarchyChange(transID: TransID) extends Transform {
  def name = "Heirarchy Change"

  //TODO(izraelevitz): consider MonoConnect.scala for future implementations
  class PathIsTopLevelModule(top: String, path: String) extends PassException(s" The path ${path} cannot point to the top level module ${top}.") // Paths are at least length 2
  class MissingModuleRename(module: String) extends PassException(s" The module ${module} was not given a new name.") // Every ModInstPair in Pathdiff has a rename
  class NonUniqueModuleName(name: String) extends PassException(s" The module name ${name} cannot be used as it is not unique.") // All names in <source|dest>ModRenames and modName are unique with each other and with all modules in circuit
  class IllegalPortName(portName: String, module: String) extends PassException(s" The provided port name ${portName} is illegal, as it has a conflict within the module ${module}.") // portName is legal in every module in pathdiff
  class IllegalInstanceName(instName: String, module: String) extends PassException(s" The provided instance name ${instName} is illegal, as it has a conflict within the module ${module}.") // instName is legal in final destination
  // old path points to an existing instance, and new path (minus leaf) is legal
  class NoCorrespondingInstModPair(inst: String, mod: String, path: String) extends PassException(s" The instance ${inst} of module ${mod} in ${path} cannot be found.")

  /**
   * Checks if given HierarchyAnnotation is legal
   */
  def check (c: Circuit, ann: HierarchyAnnotation) = {
    val modulesHash = mutable.HashMap[String, InModule]() // Contains all modules
    c.modules.foreach { _ match {
      case m: InModule => modulesHash(m.name) = m
    case m => {} } }
    val source = ann.source
    val dest = ann.dest
    val destModRenames = ann.newModRenames
    val sourceModRenames = new mutable.HashMap[String, String]()
    for(x <- ann.oldModRenames) { sourceModRenames += x }
    val portName = ann.portName
    val errors = mutable.ArrayBuffer[PassException]()

    // Paths are at least length 2
    if (source.length < 2) errors += new PathIsTopLevelModule(source.head.mod, source.toString)
    if (dest.length < 2) errors += new PathIsTopLevelModule(dest.head.mod, dest.toString)
    if (errors.nonEmpty) throw new PassExceptions(errors)

    // Every ModInstPair in Pathdiff has a rename
    val diffIndex = source.zipAll(dest, InstModulePair("",""), InstModulePair("","")).indexWhere{case ((o, n)) => (o != n)} // index of first difference
    sourceModRenames(source.last.mod) = dest.last.mod
    for (x <- source.slice(diffIndex, source.length)) {
      if (!sourceModRenames.contains(x.mod)) errors += new MissingModuleRename(x.mod)
    }
    for (x <- dest.slice(diffIndex, dest.length - 1)) {
      if (!destModRenames.contains(x.mod)) errors += new MissingModuleRename(x.mod)
    }
    if (errors.nonEmpty) throw new PassExceptions(errors)

    // All names in <old|new>ModRenames and modName are unique with each other and with all modules in circuit
    val allModuleNames = (sourceModRenames.values ++ destModRenames.values ++ c.modules.map(_.name)).toSeq
    val modNameDiff = allModuleNames diff allModuleNames.distinct
    for (x <- modNameDiff) {
       errors += new NonUniqueModuleName(x)
    }
    if (errors.nonEmpty) throw new PassExceptions(errors)

    // portName is legal in every module in pathdiff
    def isLegalName (m: InModule, name: String) = Namespace(m).tryName(name)
    val pathDiff = source.slice(diffIndex, source.length - 1) ++ dest.slice(diffIndex, dest.length - 1)
    for (x <- pathDiff) {
      if (!isLegalName(modulesHash(x.mod), portName)) errors += new IllegalPortName(portName, x.mod)
    }
    if (errors.nonEmpty) throw new PassExceptions(errors)

    // instName is legal in final destination
    val instName = dest(dest.length - 1).inst
    val destModuleName = dest(dest.length - 2).mod // module containing destination instance
    if (!isLegalName(modulesHash(destModuleName), instName)) errors += new IllegalInstanceName(instName, destModuleName)
    if (errors.nonEmpty) throw new PassExceptions(errors)

    // old path points to an existing instance, and new path (minus leaf) is legal
    // Function that follows a path through inst/mods and checks if path is valid
    def containsInstance (m: InModule, instName: String, modName: String) = {
      var matches = false
      def onStmt (s: Stmt): Stmt = {
        s map onStmt match {
          case s: WDefInstance => {
            if (s.name == instName && s.module == modName) matches = true ; s
          }
          case s => s
        }
      }
      onStmt(m.body)
      matches
    }
    var parentModuleName = c.main
    for (x <- source.tail) {
      if (containsInstance(modulesHash(parentModuleName), x.inst, x.mod)) parentModuleName = x.mod
      else throw new NoCorrespondingInstModPair(x.inst, x.mod, source.toString)
    }
    parentModuleName = c.main
    for (x <- dest.drop(1).dropRight(1)) {
      if (containsInstance(modulesHash(parentModuleName), x.inst, x.mod)) parentModuleName = x.mod
      else throw new NoCorrespondingInstModPair(x.inst, x.mod, dest.toString)
    }
  }

  /**
   * Executes check and run on a given circuit/annotation
   */
  def execute(circuit: Circuit, annotationMap: AnnotationMap): TransformResult = annotationMap.get(transID) match {
    case Some(map) if(map.size == 1) => map.head match {
      case (CircuitName(main), x) => x match {
        case ha: HierarchyAnnotation =>
          check(circuit, ha)
          TransformResult(run(circuit, ha))
        case _ => throw new AnnotationException("Annotation must be HierarchyAnnotation")
      }
      case _ => throw new AnnotationException("Annotation must be a CircuitName")
    }
    case _ => throw new PassException("Need exactly one circuit annotation!")
  }

  /**
   * Returns a new circuit with the instance moved
   * WARNING! Function requires proper review
   */
  def run (c: Circuit, ann: HierarchyAnnotation): Circuit = {
    val modulesHash = mutable.HashMap[String, InModule]() // Contains all modules
    c.modules.foreach { _ match {
      case m: InModule => modulesHash(m.name) = m
    case m => {} } }

    val diffIndex = ann.source.zipAll(ann.dest, InstModulePair("",""), InstModulePair("","")).indexWhere{case ((o, n)) => (o != n)} // index of first difference
    val diffTop = ann.source(diffIndex - 1).mod //Module name of the first module that needs to be modified
    val renameModuleMap = mutable.HashMap[String, String]()// Contains the oldname to newname mapping

    // 0) Add existing modules to renameModuleMap
    for (m <- c.modules) {renameModuleMap(m.name) = m.name}

    // 1) Add new name for user-selected names, and new instance name
    for (x <- ann.newModRenames) { renameModuleMap(x._1) = x._2 }
    for (x <- ann.oldModRenames) { renameModuleMap(x._1) = x._2 }
    renameModuleMap(ann.source.last.mod) = ann.dest.last.mod

    // 2) duplicate modules and insert into corresponding instances. Now, transformations to those Modules have no side effects on other instantiations
    def renameModulePath (top: String, path: InstModulePath) = {
      val moduleQueue = new mutable.Queue[InModule]()
      moduleQueue += modulesHash(top)
      while (!moduleQueue.isEmpty) {
        val m = moduleQueue.dequeue()
        //println("On " + m.name)
        var instName: String = ""
        def renameModuleRef (s: Stmt): Stmt = {
          s map renameModuleRef match {
            case s: WDefInstance => {
              if (s.name == instName) {
                moduleQueue += modulesHash(s.module)
                WDefInstance(s.info, s.name, renameModuleMap(s.module), s.tpe)
              } else s
            }
            case s => s
          }
        }
        val newBody = path.getChild(m.name) match {
          case None => m.body
          case Some(inmod) => { instName = inmod.inst; renameModuleRef(m.body) }
        }
        val moduleNewName = renameModuleMap(m.name)
        modulesHash(moduleNewName) = InModule(m.info, moduleNewName, m.ports, newBody)
      }
    }
    renameModulePath(diffTop, ann.source)
    renameModulePath(diffTop, ann.dest)

    val updatedOldPath = new InstModulePath(ann.source.map(x => InstModulePair(x.inst, renameModuleMap(x.mod))).slice(diffIndex - 1, ann.source.length))
    val updatedNewPath = new InstModulePath(ann.dest.slice(diffIndex - 1, ann.dest.length - 1).map(x => InstModulePair(x.inst, renameModuleMap(x.mod))) :+ ann.dest.last)
    val instType = module_type(modulesHash(ann.source.last.mod))
    val oldInstName = ann.source.last.inst
    val portName = ann.portName
    val newInstName = ann.dest.last.inst
    val newModName = updatedNewPath.last.mod

    var replacement: Option[Stmt] = None
    var instance: Option[Stmt] = None
    var connectFrom: Option[Expression] = None
    var connectTo: Option[Expression] = None
    var addedPort: Option[Port] = None

    def hierarchyChange (m: InModule): InModule = {
      def replace (s: Stmt): Stmt = {
        s map replace match {
          case s: WDefInstance => if (s.name == oldInstName) replacement.get else s
          case s => s
        }
      }
      def connect (s: Stmt): Stmt = Begin(Seq(s, Connect(NoInfo, connectTo.get, connectFrom.get)))
      def instantiate (s: Stmt): Stmt = Begin(Seq(instance.get, s))
      def addPort (ports: Seq[Port]): Seq[Port] = ports :+ addedPort.get

      val stmtFunctionsToApply = mutable.ArrayBuffer[Stmt => Stmt]()
      if (replacement != None) stmtFunctionsToApply += replace _
      if (instance != None) stmtFunctionsToApply += instantiate _
      if (connectFrom != None && connectTo != None) stmtFunctionsToApply += connect _

      var modifiedBody = m.body
      for (fun <- stmtFunctionsToApply) {
        modifiedBody = fun(modifiedBody)
      }

      var modifiedPorts = m.ports
      if (addedPort != None) modifiedPorts = addPort(modifiedPorts)

      InModule(m.info, m.name, modifiedPorts, modifiedBody)
    }

    for (x <- updatedOldPath ++ updatedNewPath.tail) {
      replacement = None
      instance = None
      connectFrom = None
      connectTo = None
      addedPort = None
      // - If I am direct parent of original instance, replace wire and connect to wire
      if (updatedOldPath.isDirectParent(x)) {//replace wire and connect to wire
        replacement = Some(DefWire(NoInfo, oldInstName, instType))
        connectTo = Some(WRef(oldInstName, instType, ExpKind(), UNKNOWNGENDER))
      }
      // - If difftop is a parent of you and you are a parent of old location, insert input port and connect from my added input port
      if (updatedOldPath.isNotTop(x) && updatedOldPath.isNotLeaf(x) && updatedOldPath.contains(x)) {
        addedPort = Some(Port(NoInfo, portName, INPUT, instType))
        connectFrom = Some(WRef(portName, instType, ExpKind(), UNKNOWNGENDER))
      }
      // - If I am a non-direct old parent, I connect to my old child's added port
      if (updatedOldPath.isNonDirectParent(x)) {
        connectTo = Some(WSubField(WRef(updatedOldPath.getChild(x.mod).get.inst, UnknownType(), ExpKind(), UNKNOWNGENDER), portName, instType, UNKNOWNGENDER))
      }
      // - If I am direct parent of novel instance, instantiate new instance and connect from instance
      if (updatedNewPath.isDirectParent(x)) {//replace wire and connect to wire
        instance = Some(WDefInstance(NoInfo, newInstName, newModName, instType))
        connectFrom = Some(WRef(newInstName, instType, ExpKind(), UNKNOWNGENDER))
      }
      // - If difftop is a parent of you and you are a parent of new location, insert output port and connect to my added input port
      if (updatedNewPath.isNotTop(x) && updatedNewPath.isNotLeaf(x) && updatedNewPath.contains(x)) {
        addedPort = Some(Port(NoInfo, portName, OUTPUT, instType))
        connectTo = Some(WRef(portName, instType, ExpKind(), UNKNOWNGENDER))
      }
      // - If I am a new non-direct parent, I connect from my new child's added port
      if (updatedNewPath.isNonDirectParent(x)) {
        connectFrom = Some(WSubField(WRef(updatedNewPath.getChild(x.mod).get.inst, UnknownType(), ExpKind(), UNKNOWNGENDER), portName, instType, UNKNOWNGENDER))
      }
      modulesHash(x.mod) = hierarchyChange(modulesHash(x.mod))
    }

    val modulesx = modulesHash.map(_._2).toSeq
    Circuit(c.info, modulesx, c.main)
  }
}
