// See LICENSE for license details.

package firrtl
package transforms

// Firrtl IR classes
import ir._
import PrimOps._

// Map functions
import firrtl.Mappers._
// Scala's mutable collections
import scala.collection.mutable

/** GateCounter tracks [[Circuit]] statistics
  * 
  * In this lesson, we want to calculate the number of muxes, not just in 
  *  a module, but also in any instances it has of other modules, etc.
  *
  * To do this, we need to update our GateCounter class to keep track of this
  *  module instance information
  *
  * See [[lesson2.AnalyzeCircuit]]
  */
class GateCounter {
  private var moduleName: Option[String] = None
  private val modules = mutable.Set[String]()
  private val moduleOpMap = mutable.Map[String, mutable.Map[(String, BigInt), Int]]()
  private val moduleInstanceMap = mutable.Map[String, Seq[String]]()
  def getModuleName: String = moduleName match {
    case None => sys.error("Module name not defined in GateCounter!")
    case Some(name) => name
  }
  def setModuleName(myName: String): Unit = {
    modules += myName
    moduleName = Some(myName)
  }
  private def updateOpMap(name: String, width: BigInt): Unit = {
    val myName = getModuleName
    val opMap = moduleOpMap.getOrElse(myName, mutable.Map[(String, BigInt), Int]())
    opMap((name, width)) = opMap.getOrElse((name, width), 0) +
        opMap.getOrElse((name, width), 0) + 1
    moduleOpMap(myName) = opMap
  }
  def foundOp(e: Expression): Unit = e match {
    case DoPrim(op, _, _, GroundType(IntWidth(w))) => op match {
      case Cat|Bits|Head|Tail|AsFixedPoint|AsUInt|AsSInt|BPShl|BPShr|BPSet|Shl|Shlw|Shr|Pad =>
      case _ => updateOpMap(op.toString, w)
    }
    case DoPrim(op, _, _, tpe) => sys.error("Bad")
    case Mux(_, _, _, GroundType(IntWidth(w))) => updateOpMap("mux", w)
    case Mux(_, _, _, tpe) => sys.error("Bad")
  }
  // Added this function to track when a module instantiates another module
  def foundInstance(name: String): Unit = {
    val myName = getModuleName
    moduleInstanceMap(myName) = moduleInstanceMap.getOrElse(myName, Nil) :+ name
  }
  // Counts mux's in a module, and all its instances (recursively).
  private def countOp(myName: String): mutable.Map[(String, BigInt), Int] = {
    moduleOpMap.getOrElse(myName, mutable.Map[(String, BigInt), Int]())
  }
  // Display total of ops
  def serialize: String = modules map { myName =>
    val opCount = countOp(myName)
    val out = opCount map { case ((name, width), count) =>
      s"$name, $width => $count"
    } mkString "\n"
    s"$myName:\n$out"
  } mkString "\n"
}

/** AnalyzeCircuit Transform
  *
  * Walks [[ir.Circuit]], and records the number of muxes and primops it
  *  finds, per module.
  *   - TODO(izraelevitz) 
  */
class CountGates extends Transform {
  def inputForm = LowForm
  def outputForm = LowForm

  // Called by [[Compiler]] to run your pass.
  def execute(state: CircuitState): CircuitState = {
    val ledger = new GateCounter()
    val circuit = state.circuit

    // Execute the function walkModule(ledger) on all [[DefModule]] in circuit
    circuit map walkModule(ledger)

    // Print our ledger
    println(ledger.serialize)

    // Return an unchanged [[CircuitState]]
    state
  }

  // Deeply visits every [[Statement]] in m.
  def walkModule(ledger: GateCounter)(m: DefModule): DefModule = {
    // Set ledger to current module name
    ledger.setModuleName(m.name)

    // Execute the function walkStatement(ledger) on every [[Statement]] in m.
    m map walkStatement(ledger)
  }

  // Deeply visits every [[Statement]] and [[Expression]] in s.
  def walkStatement(ledger: GateCounter)(s: Statement): Statement = {
    // Map the functions walkStatement(ledger) and walkExpression(ledger)
    s map walkStatement(ledger) map walkExpression(ledger) 
  }

  // Deeply visits every [[Expression]] in e.
  def walkExpression(ledger: GateCounter)(e: Expression): Expression = {
    e map walkExpression(ledger) match {
      case mux: Mux =>
        ledger.foundOp(mux)
        mux
      case primop: DoPrim =>
        ledger.foundOp(primop)
        primop
      case other => other
    }
  }
}
