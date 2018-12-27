// See LICENSE for license details.

package firrtl
package transforms

import firrtl.annotations._
import firrtl.ir._
import firrtl.traversals.Foreachers._
import firrtl.passes.LowerTypes

import scala.collection.mutable

/**
  * Enumeration of the two types of readmem statements available in verilog
  */
object MemoryLoadFileType extends Enumeration {
  type FileType = Value

  val Hex:    Value = Value("h")
  val Binary: Value = Value("b")
}

/**
  * Firrtl implementation for load memory
  * @param target        memory to load
  * @param fileName      name of input file
  * @param hexOrBinary   use $readmemh or $readmemb
  */
case class LoadMemoryAnnotation(
  target: ComponentName,
  fileName: String,
  hexOrBinary: MemoryLoadFileType.FileType = MemoryLoadFileType.Hex,
  originalMemoryNameOpt: Option[String] = None
) extends SingleTargetAnnotation[Named] {

  val (prefix, suffix) = {
    fileName.split("""\.""").toList match {
      case Nil =>
        throw new Exception(s"empty filename not allowed in LoadMemoryAnnotation")
      case name :: Nil =>
        (name, "")
      case "" :: name :: Nil => // this case handles a filename that begins with dot and has no suffix
        ("." + name, "")
      case other =>
        (other.reverse.tail.reverse.mkString("."), "." + other.last)
    }
  }

  def getPrefix: String =
    prefix + originalMemoryNameOpt.map(n => target.name.drop(n.length)).getOrElse("")
  def getSuffix: String = suffix
  def getFileName: String = getPrefix + getSuffix

  def duplicate(newNamed: Named): LoadMemoryAnnotation = {
    newNamed match {
      case componentName: ComponentName =>
        this.copy(target = componentName, originalMemoryNameOpt = Some(target.name))
      case _ =>
        throw new Exception(s"Cannot annotate anything but a memory, invalid target ${newNamed.serialize}")
    }
  }
}

class LoadMemoryBundle extends Transform {
  def inputForm: CircuitForm = HighForm
  def outputForm: CircuitForm = HighForm

  /** Create a LoadMemoryAnnotation for each element in a Mem of type Bundle.
    * @param state Input Firrtl AST
    * @return A transformed Firrtl AST
    */
  def execute(state: CircuitState): CircuitState = {
    val (loadMemAnnos, otherAnnos) = state.annotations.partition(_.isInstanceOf[LoadMemoryAnnotation]);

    val memBundles = findMemBundles(state.circuit)

    val newLoadMemAnnos = loadMemAnnos.flatMap { case anno: LoadMemoryAnnotation =>
      anno.target match {
        case comp: ComponentName => {
          val tgt = comp.toTarget
          memBundles.get(tgt).map { bundle =>
            val newAnnos = mutable.Buffer[LoadMemoryAnnotation]()

            val renames = RenameMap()
            renames.setCircuit(tgt.circuit)
            renames.setModule(tgt.module)
            LowerTypes.renameExps(renames, WRef(tgt.ref, bundle, ExpKind, UNKNOWNGENDER), "").map { sub =>
              val fname = sub.replaceFirst(tgt.ref, anno.getPrefix) + anno.getSuffix
              LoadMemoryAnnotation(ComponentName(sub, comp.module), fname)
            }
          } getOrElse Seq(anno)
        }
        case _ => Seq(anno)
      }
    }

    state.copy(annotations = otherAnnos ++ newLoadMemAnnos)
  }

  private def findMemBundles(c: Circuit) = {
    val memBundles = mutable.HashMap[ReferenceTarget, BundleType]()

    def find_mems_s(module: ModuleTarget)(s: Statement): Unit = {
      s match {
        case mem: DefMemory => {
          mem.dataType match {
            case bundle: BundleType => memBundles.update(module.ref(mem.name), bundle)
            case _ => ()
          }
        }
        case _ => s foreach find_mems_s(module)
      }
    }

    def find_mems_m(circuit: CircuitTarget)(m: DefModule) {
      m foreach find_mems_s(circuit.module(m.name))
    }

    c.modules foreach find_mems_m(CircuitTarget(c.main))

    memBundles
  }
}
