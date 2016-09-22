package firrtl
import ir._
import TransformationResult._
import scala.collection.mutable
import Mappers._
import Parser.{InfoMode, IgnoreInfo, UseInfo, GenInfo, AppendInfo}

object TransformationResult {
  def mapr(c: Circuit, f:(Annotation, Name)=>Annotation): Circuit = {
    def onModule(m: DefModule): DefModule = {
      val mname = ModuleName(m.name)
      m map onPort(mname) map onStmt(mname) map {a: Annotation => f(a, mname) }
    }
    def onStmt(mname: ModuleName)(s: Statement): Statement = s match {
      case s: IsDeclaration =>
        val newAnnos = s.annos map {f(_, ComponentName(s.name, mname))}
        s match {
          case s: DefWire => s.copy(annos=newAnnos)
          case s: DefRegister => s.copy(annos=newAnnos)
          case s: DefNode => s.copy(annos=newAnnos)
          case s: DefMemory => s.copy(annos=newAnnos)
          case s: DefInstance => s.copy(annos=newAnnos)
          case s: WDefInstance => s.copy(annos=newAnnos) //TODO remove this
          case _ => error("Shouldn't be here")
        }
      case _ => s map onStmt(mname)
    }
    def onPort(mname: ModuleName)(p: Port): Port =
      p.copy(annos=p.annos map{ f(_, ComponentName(p.name, mname)) })

    // populate newNames
    c map onModule map {a: Annotation => f(a, CircuitName(c.main))}
  }
  def createTR(lines: Iterator[String], infoMode: InfoMode = UseInfo): TransformationResult =
    new TransformationResult(Map.empty, Parser.parse(lines, infoMode))
}

class TransformationResult private (history: Map[AnnotationId, Seq[AnnotationCommand]], oldCircuit: Circuit) {

  val (annotations, names) = get(oldCircuit)
  val circuit = oldCircuit
  private def get(c: Circuit): (Set[Annotation], Map[Annotation, Seq[Name]]) = {
    val annotations = mutable.HashSet[Annotation]()
    val names = mutable.HashMap[Annotation, Seq[Name]]()
    def build(anno: Annotation, name: Name): Annotation = {
      names(anno) = names.getOrElse(anno, Seq.empty) :+ name
      annotations += anno
      anno
    }
    mapr(c, build)
    (annotations.toSet, names.toMap)
  }

  val newNames = mutable.HashMap[Annotation, Seq[Name]]()
  
  def update(newCircuit: Circuit): TransformationResult = {
    // populate newNames
    val (newAnnotatons, newNames) = get(newCircuit)

    // final datastructures for return type
    val finalHistory = mutable.HashMap[AnnotationId, Seq[AnnotationCommand]]()
    def addCmd(id: AnnotationId, cmd: AnnotationCommand) {
      finalHistory(id) = finalHistory.getOrElse(id, Seq.empty) :+ cmd
    }
    val finalAnnotations = mutable.HashMap[(Name, Annotation), Annotation]()

    // new annotations
    newNames.keys.toSet diff names.keys.toSet foreach { anno =>
      addCmd(anno.id, CreateCommand)
      finalAnnotations((newNames(anno).head, anno)) = anno //its new, so it maps to itself
    }

    // deleted annotations
    names.keys.toSet diff newNames.keys.toSet foreach {anno => addCmd(anno.id, DeleteCommand) }

    // duplicated annotations
    newNames.keys foreach { anno => newNames(anno) match {
      case Seq(newName) => names.get(anno) match {
        case Some(Seq(oldName)) if oldName != newName =>  // moved to new name
          val newAnnotation = Annotation(anno.value)
          addCmd(anno.id, MoveToCommand(newName))
          addCmd(newAnnotation.id, MoveFromCommand(oldName))
          finalAnnotations((oldName, anno)) = newAnnotation //it moved, so it maps to the duplicate
        case _ =>
      }
      case seq => seq.tail foreach { dupName => // was duplicated
        val newAnnotation = Annotation(anno.value)
        addCmd(newAnnotation.id, DuplicateCommand(anno.id))
        finalAnnotations((dupName, anno)) = newAnnotation //it moved, so it maps to the duplicate
      }
    }}

    def updateAnnotations(anno: Annotation, name: Name): Annotation = finalAnnotations((name, anno))
    
    // return a new and updated TransformationResult
    new TransformationResult(history.toMap, mapr(newCircuit, updateAnnotations _))
  }
}

case class Annotation private (id: AnnotationId, value: Any)
object Annotation {
  var counter = 0 //TODO not threadsafe!!!!
  def apply(value: Any): Annotation = {
    counter += 1
    Annotation(AnnotationId(counter - 1), value, None)
  }
}

case class AnnotationId (num: Int)

abstract class Name { def name: String }
case class CircuitName(name: String) extends Name
case class ModuleName(name: String) extends Name
case class ComponentName(name: String, module: ModuleName) extends Name

sealed trait AnnotationCommand

// Can start history.
case object CreateCommand extends AnnotationCommand
// Can end history
case object DeleteCommand extends AnnotationCommand
// Can start history
case class DuplicateCommand(id: AnnotationId) extends AnnotationCommand
// Can end history
case class MoveToCommand(to: Name) extends AnnotationCommand
// Can start history
case class MoveFromCommand(from: Name) extends AnnotationCommand



// vim: set ts=4 sw=4 et:
