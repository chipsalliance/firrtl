// See license file for details

package firrtl.passes
package clocklist

import firrtl._
import firrtl.ir._
import Annotations._
import Utils.error
import java.io.{File, CharArrayWriter, PrintWriter, Writer}
import wiring.WiringUtils.{getChildrenMap, countInstances, ChildrenMap, getLineage}
import wiring.Lineage
import ClockListUtils._
import Utils._
import memlib.AnalysisUtils._
import memlib._
import Mappers._


case class ClockListAnnotation(t: String)
    extends Annotation with Loose with Unstable {
  def transform = classOf[ClockListTransform]
  val usage = """
[Optional] ClockList
  List which signal drives each clock of every descendent of specified module

Usage: 
  --list-clocks -c:<circuit>:-m:<module>:-o:<filename>
  *** Note: sub-arguments to --list-clocks should be delimited by : and not white space!
"""    

  //Parse pass options
  val passOptions = PassConfigUtil.getPassOptions(t, usage)
  val outputConfig = passOptions.getOrElse(
    OutputConfigFileName, 
    error("No output config file provided for ClockList!" + usage)
  )
  val passCircuit = passOptions.getOrElse(
    PassCircuitName, 
    error("No circuit name specified for ClockList!" + usage)
  )
  val passModule = passOptions.getOrElse(
    PassModuleName, 
    error("No module name specified for ClockList!" + usage)
  )
  passOptions.get(InputConfigFileName) match {
    case Some(x) => error("Unneeded input config file name!" + usage)
    case None =>
  }
  val target = ModuleName(passModule, CircuitName(passCircuit))
  def duplicate(n: Named) = n match {
    case ModuleName(m, CircuitName(c)) => this copy (t = t.replace(s"-c:$passCircuit", s"-c:$c").replace(s"-m:$passModule", s"-m:$m"))
    case _ => error("Cannot move ClockListAnnotation to a non-module")
  }
}

class ClockListTransform extends Transform {
  def inputForm = LowForm
  def outputForm = LowForm
  def passSeq(top: String, writer: Writer): Seq[Pass] =
    Seq(new ClockList(top, writer))
  def execute(state: CircuitState): CircuitState = getMyAnnotations(state) match {
    case Some(p) => (p.toSeq.collect { case (ModuleName(m, CircuitName(state.circuit.main)), cla) =>
      cla match {
        case x: ClockListAnnotation => (m, x.outputConfig)
        case _ => error(s"Found an unexpected annotation: $cla")
      }
    }) match {
      case Seq((top, out)) => 
        val outputFile = new PrintWriter(out)
        val newC = (new ClockList(top, outputFile)).run(state.circuit)
        outputFile.close()
        CircuitState(newC, state.form)
      case _ => error(s"Found too many (or too few) ClockListAnnotations!")
    }
    case None => CircuitState(state.circuit, state.form)
  }
}
