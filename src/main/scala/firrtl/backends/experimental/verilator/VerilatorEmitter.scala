package firrtl.backends.experimental.verilator

import firrtl.annotations.NoTargetAnnotation
import firrtl.{CircuitState, DependencyAPIMigration, Transform}
import os._

object VerilatorAnnotation {

}

case class VerilatorAnnotation(includeDirectories: Seq[String],
                               linkDirectories: Seq[String],
                               linkLibraries: Seq[String],
                               cSources: Seq[String],
                               args: Seq[String],
                              ) {
  def addIncludeDirectory(dir: Path) = copy(includeDirectories = this.includeDirectories :+ dir.toString)

  def addLinkDirectory(dir: Path) = copy(linkDirectories = this.linkDirectories :+ dir.toString)

  def addLinkLibrary(name: String) = copy(linkLibraries = this.linkLibraries :+ name)

  def addcSource(file: Path) = copy(cSources = this.cSources :+ file.toString)

  def CMakeList =
    s"""cmake_minimum_required(VERSION 3.20)
       |project(???)
       |include_directories(???)
       |link_directories(???)
       |find_package(verilator)
       |add_executable(??? ???)
       |set(CMAKE_C_COMPILER "???")
       |set(CMAKE_CXX_COMPILER "???")
       |set(CMAKE_CXX_FLAGS "$${CMAKE_CXX_FLAGS} -DVERILATOR -DTEST_HARNESS=??? -std=c++11 -include ???")
       |set(THREADS_PREFER_PTHREAD_FLAG ON)
       |find_package(Threads)
       |target_link_libraries(emulator PRIVATE $${CMAKE_THREAD_LIBS_INIT})
       |verilate(???
       |  SOURCES ???
       |  TOP_MODULE ???
       |  PREFIX V???
       |  VERILATOR_ARGS ???
       |)
       |""".stripMargin
}

case class VerilatorExecuable(file: String) extends NoTargetAnnotation

/** Consume a [[VerilatorAnnotation]], compiling FIRRTL and all blackboxes into new circuits.
  * @todo need to deal with public signals with Annotation.
  *       need to deal with cover/assert/assume Verification IRs.
  *       need to add VPI/DPI Annotation/IR
  *
  */
class VerilatorEmitter extends Transform with DependencyAPIMigration{
  /** Perform the transform, encode renaming with RenameMap, and can
    * delete annotations
    * Called by [[runTransform]].
    *
    * @param state Input Firrtl AST
    * @return A transformed Firrtl AST
    */
  override protected def execute(state: CircuitState): CircuitState = ???
}
