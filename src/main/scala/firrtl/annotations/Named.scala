// See LICENSE for license details.

package firrtl
package annotations

import firrtl.ir.Expression
import AnnotationUtils.{validModuleName, validComponentName, toExp}
import com.trueaccord.scalapb.TypeMapper

// How can we seal named but still provide a "LazyName" but also make it so that
// it's not onerous on transform writers to deal with LazyName
sealed trait AbstractNamed

trait NamedPB extends Product {
  def serialize: String = {
    this.productIterator.map {
      case named: NamedPB => named.serialize
      case other => other.toString
    }.reverse.mkString(".")
  }
}

/**
 * Named classes associate an annotation with a component in a Firrtl circuit
 */
sealed trait Named {
  def name: String
  def serialize: String
}
object Named {
  implicit val protoTypeMapper = TypeMapper(AnnotationUtils.toNamed)(_.serialize)
  //implicit val typeMapper = TypeMapper(Seconds.apply)(_.v)
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
