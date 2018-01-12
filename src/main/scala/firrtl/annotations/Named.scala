// See LICENSE for license details.

package firrtl
package annotations

import firrtl.ir.Expression
import AnnotationUtils.{validModuleName, validComponentName, toExp}

/**
 * Named classes associate an annotation with a component in a Firrtl circuit
 */
sealed trait Named {
  def serialize: String
}

/** Name referring to the top of the circuit */
final case object CircuitTopName extends Named {
  def name: String = "CircuitTop"
  def serialize: String = name
}

final case class CircuitName(name: String) extends Named {
  if(!validModuleName(name)) throw AnnotationException(s"Illegal circuit name: $name")
  def serialize: String = name
}

final case class ModuleName(name: String, circuit: CircuitName) extends Named {
  if(!validModuleName(name)) throw AnnotationException(s"Illegal module name: $name")
  def serialize: String = circuit.serialize + "." + name
}

final case class ComponentName(name: String, module: ModuleName) extends Named {
  if(!validComponentName(name)) throw AnnotationException(s"Illegal component name: $name")
  def expr: Expression = toExp(name)
  def serialize: String = module.serialize + "." + name
}
/** Used for indicating that a named thing has become a constant */
case class ConstName(value: BigInt) extends Named {
	def serialize = value.toString
}
object ConstName {
	private val Regex = """(-?\d+)""".r
	def canBuildFrom(str: String): Boolean = str match {
		case Regex(_) => true
		case _ => false
	}
}
