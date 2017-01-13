// See LICENSE for license details.

package firrtl.blackboxes

import scala.collection.mutable

//scalastyle:off regex
/**
  * This provides the machinery to parse a verilog file into
  */
object VerilogFileParser {
  val StartModule = """^\s*module +(\w+).*$""".r
  val EndModule   = """^\s*endmodule.*$""".r

  def getModules(fileName: java.io.File): Map[String, String] = {
    val modules = mutable.HashMap[String, String]()

    var moduleText: Option[mutable.StringBuilder] = None
    var currentModuleName = ""

    io.Source.fromFile(fileName).getLines().foreach { line =>
      line match {
        case StartModule(moduleName) =>
          println(s"got $moduleName")
          currentModuleName = moduleName
          moduleText = Some(new mutable.StringBuilder())
        case EndModule() =>
          moduleText.foreach { text =>
            text.append(line)
            text.append("\n")
            text.append("\n")
          }
          moduleText.foreach { text =>
            modules(currentModuleName) = text.toString()
          }
          moduleText = None
        case _ =>
      }
      moduleText.foreach { text =>
        text.append(line)
        text.append("\n")
      }
    }
    modules.toMap
  }

  def getModules(fileName: String): Map[String, String] = {
    getModules(new java.io.File(fileName))
  }

  def main(args: Array[String]): Unit = {
    val modules = mutable.HashMap[String, String]()

    args.foreach { fileName =>
      modules ++= getModules(fileName)
    }

    for(key <- modules.keys.toList.sorted) {
      println(s"module $key ${modules(key).take(100)}")
    }
  }
}
