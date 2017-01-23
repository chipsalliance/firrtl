// See LICENSE for license details.

package firrtl.transforms

import java.io.{File, FileNotFoundException, FileOutputStream, PrintWriter}

import firrtl._
import firrtl.annotations.{Annotation, ModuleName}


trait BlackBoxSource {
  def serialize: String
  def name: String
}

object BlackBoxSource {
  val MaxFields = 3

  def parse(s: String): Option[BlackBoxSource] = {
    s.split("\n", MaxFields).toList  match {
      case "resource" :: id ::  _ => Some(BlackBoxResource(id))
      case "inline" :: name :: text :: _ => Some(BlackBoxInline(name, text))
      case "targetDir" :: targetDir :: _ => Some(BlackBoxTargetDir(targetDir))
      case _ => throw new FIRRTLException(s"Error: Bad BlackBox annotations $s")
    }
  }
}

case class BlackBoxTargetDir(targetDir: String) extends BlackBoxSource {
  def serialize: String = s"targetDir\n$targetDir"
  def name: String = targetDir
}

case class BlackBoxResource(resourceId: String) extends BlackBoxSource {
  def serialize: String = s"resource\n$resourceId"
  def name: String = resourceId.split("/").last
}

case class BlackBoxInline(name: String, text: String) extends BlackBoxSource {
  def serialize: String = s"inline\n$name\n$text"
}

object BlackBoxSourceAnnotation {
  def apply(targetDir: ModuleName, value: String): Annotation = {
    assert(BlackBoxSource.parse(value).isDefined)
    Annotation(targetDir, classOf[DedupModules], value)
  }

  def unapply(a: Annotation): Option[(ModuleName, BlackBoxSource)] = a match {
    case Annotation(ModuleName(n, c), _, text) => Some((ModuleName(n, c), BlackBoxSource.parse(text).get))
    case _ => None
  }
}

class BlackBoxSourceHelper extends firrtl.Transform {
  var targetDir: File = new File(".")

  override def inputForm: CircuitForm = HighForm
  override def outputForm: CircuitForm = HighForm


  def getSources(annos: Seq[Annotation]): Seq[BlackBoxSource] = {
    annos.flatMap { anno => BlackBoxSource.parse(anno.value) }
      .flatMap {
        case BlackBoxTargetDir(dest) =>
          targetDir = new File(dest)
          if(! targetDir.exists()) { FileUtils.makeDirectory(targetDir.getAbsolutePath) }
          None
        case b: BlackBoxSource => Some(b)
        case _ => None
      }
      .sortBy(a => a.name)
      .distinct
  }

  override def execute(state: CircuitState): CircuitState = {
    getMyAnnotations(state) match {
      case Nil => state
      case myAnnotations =>
        val sources = getSources(myAnnotations)
        sources.foreach {
          case BlackBoxResource(resourceId) =>
            val name = resourceId.split("/").last
            BlackBoxSourceHelper.copyResourceToFile(resourceId, new File(targetDir, name))
          case BlackBoxInline(name, text) =>
            val outFile = new PrintWriter(new File(targetDir, name))
            outFile.write(text)
            outFile.close()
          case _ =>
        }
        state
    }
  }
}

object BlackBoxSourceHelper {
  def copyResourceToFile(name: String, file: File) {
    val in = getClass.getResourceAsStream(name)
    if (in == null) {
      throw new FileNotFoundException(s"Resource '$name'")
    }
    val out = new FileOutputStream(file)
    Iterator.continually(in.read).takeWhile(-1 != _).foreach(out.write)
    out.close()
  }

}
