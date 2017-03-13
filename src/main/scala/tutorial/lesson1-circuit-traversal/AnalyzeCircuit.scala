// See LICENSE for license details.

package tutorial
package lesson1

// Compiler Infrastructure
import firrtl.{Transform, LowForm, CircuitState}
// Firrtl IR classes
import firrtl.ir.{Circuit, DefModule, Statement, Expression, Mux}
// Map functions
import firrtl.Mappers._
// Scala's mutable collections
import scala.collection.mutable

/** Ledger tracks [[Circuit]] statistics
  *
  * In this lesson, we want to count the number of muxes in each
  *  module in our design.
  *
  * This [[Ledger]] class will be passed along as we walk our
  *  circuit, and help us count each [[Mux]] we find.
  *
  * See [[lesson1.AnalyzeCircuit]]
  */
class Ledger {
  private var moduleName: Option[String] = None
  private val modules = mutable.Set[String]()
  private val moduleMuxMap = mutable.Map[String, Int]()
  def foundMux(): Unit = moduleName match {
    case None => sys.error("Module name not defined in Ledger!")
    case Some(name) => moduleMuxMap(name) = moduleMuxMap.getOrElse(name, 0) + 1
  }
  def getModuleName: String = moduleName match {
    case None => error("Module name not defined in Ledger!")
    case Some(name) => name
  }
  def setModuleName(myName: String): Unit = {
    modules += myName
    moduleName = Some(myName)
  }
  def serialize: String = {
    modules map { myName =>
      s"$myName => ${moduleMuxMap.getOrElse(myName, 0)} muxes!"
    } mkString "\n"
  }
}

/** AnalyzeCircuit Transform
  *
  * Walks [[ir.Circuit]], and records the number of muxes it finds, per module.
  *
  * While some compiler frameworks operate on graphs, we represent a Firrtl
  * circuit using a tree representation:
  *   - A Firrtl [[Circuit]] contains a sequence of [[DefModule]]s.
  *   - A [[DefModule]] contains a sequence of [[Port]]s, and maybe a [[Statement]].
  *   - A [[Statement]] can contain other [[Statement]]s, or [[Expression]]s.
  *   - A [[Expression]] can contain other [[Expression]]s.
  * 
  * To visit all Firrtl IR nodes in a circuit, we write functions that recursively
  *  walk down this tree. To record statistics, we will pass along the [[Ledger]]
  *  class and use it when we come across a [[Mux]].
  *
  * See the following links for more detailed explanations:
  * Firrtl's IR:
  *   - https://github.com/ucb-bar/firrtl/wiki/Understanding-Firrtl-Intermediate-Representation
  * Traversing a circuit:
  *   - https://github.com/ucb-bar/firrtl/wiki/traversing-a-circuit for more
  * Common Pass Idioms:
  *   - https://github.com/ucb-bar/firrtl/wiki/Common-Pass-Idioms
  */
class AnalyzeCircuit extends Transform {
  def inputForm = LowForm
  def outputForm = LowForm
  def execute(state: CircuitState): CircuitState = {
    val ledger = new Ledger()
    val circuit = state.circuit
    circuit map walkModule(ledger)
    println(ledger.serialize) // Print our ledger
    state
  }
  def walkModule(ledger: Ledger)(m: DefModule): DefModule = {
    ledger.setModuleName(m.name)
    m map walkStatement(ledger)
  }
  def walkStatement(ledger: Ledger)(s: Statement): Statement = {
    s map walkExpression(ledger)
    s map walkStatement(ledger)
  }
  def walkExpression(ledger: Ledger)(e: Expression): Expression = {
    val visited = e map walkExpression(ledger)
    visited match {
      case Mux(cond, tval, fval, tpe) =>
        ledger.foundMux
        e
      case notmux => notmux
    }
  }
}
