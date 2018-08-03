// See LICENSE for license details.

package firrtl
package annotations

import firrtl.ir.Expression
import AnnotationUtils.{toExp, validComponentName, validModuleName}
import SubComponent._

import scala.collection.mutable

trait SubComponent {
  def keyword: String
  def value: Any
  def is(keywords: String*): Boolean = {
    keywords.map { kw =>
      val lastClass = this.getClass
      lastClass == SubComponent.keyword2subcomponent(kw)("0").getClass()
    }.reduce(_ || _)
  }
  //override def toString = s"/$keyword@$value"
}

case object SubComponent {
  //implicit def string2int(s: String): Int = s.toInt
  case class Instance(value: String)  extends SubComponent { override def keyword: String = "inst" }
  case class OfModule(value: String)  extends SubComponent { override def keyword: String = "of" }
  case class Ref(value: String)       extends SubComponent { override def keyword: String = "ref" }
  case class Index(value: Int)        extends SubComponent { override def keyword: String = "[]" }
  case class Field(value: String)     extends SubComponent { override def keyword: String = "." }
  case class Arg(value: Int)          extends SubComponent { override def keyword: String = "arg" }
  case class Anonymous(value: String) extends SubComponent { override def keyword: String = "" }
  case class Bit(value: Int)          extends SubComponent { override def keyword: String = "bit" }
  case object Clock                   extends SubComponent { override def keyword: String = "clock"; val value = "" }
  case object Init                    extends SubComponent { override def keyword: String = "init";  val value = "" }
  case object Reset                   extends SubComponent { override def keyword: String = "reset"; val value = "" }
  val keyword2subcomponent = Map(
    "inst" -> ((value: String) => Instance(value)),
    "of" -> ((value: String) => OfModule(value)),
    "ref" -> ((value: String) => Ref(value)),
    "[]" -> ((value: String) => Index(value.toInt)),
    "." -> ((value: String) => Field(value)),
    "arg" -> ((value: String) => Arg(value.toInt)),
    "" -> ((value: String) => Anonymous(value)),
    "bit" -> ((value: String) => Bit(value.toInt)),
    "clock" -> ((value: String) => Clock),
    "init" -> ((value: String) => Init),
    "reset" -> ((value: String) => Reset)
  )
}

case class Component(circuit: Option[String],
                     encapsulatingModule: Option[String],
                     reference: Seq[SubComponent],
                     tag: Option[Int] ) extends Named {
  def serialize: String = toString
  def requireLast(default: Boolean, keywords: String*): Unit = {
    val isOne = if(reference.isEmpty) default else reference.last.is(keywords:_*)
    require(isOne, s"${reference.last} is not one of $keywords")
  }
  def ref(value: String): Component = {
    requireLast(true, "inst", "of")
    this.copy(reference = reference :+ Ref(value))
  }
  def inst(value: String): Component = {
    requireLast(true, "inst", "of")
    this.copy(reference = reference :+ Instance(value))
  }
  def of(value: String): Component = {
    requireLast(false, "inst")
    this.copy(reference = reference :+ OfModule(value))
  }
  def field(name: String): Component = this.copy(reference = reference :+ Field(name))
  def index(value: Int): Component = this.copy(reference = reference :+ Index(value))
  def bit(value: Int): Component = this.copy(reference = reference :+ Bit(value))
  def arg(index: Int): Component = {
    assert(reference.last.isInstanceOf[Anonymous])
    this.copy(reference = reference :+ Arg(index))
  }
  def clock: Component = this.copy(reference = reference :+ Clock)
  def init: Component = this.copy(reference = reference :+ Init)
  def reset: Component = this.copy(reference = reference :+ Reset)
  val circuitName: String = circuit.getOrElse(Component.emptyString)
  val moduleName: String = encapsulatingModule.getOrElse(Component.emptyString)
  //override def toString(): String = {
  //  s"$$$tag$$ ($circuitName,$moduleName) ${reference.map(_.toString).mkString("")}"
  //}
  def getRelativeComponentName: ComponentName = {
    val refs = reference.foldLeft(Seq.empty[SubComponent]){ (seq, s) =>
      s match {
        case _: OfModule => Seq(s)
        case other => seq :+ other
      }
    }
    val (mn, subs) = refs.head match {
      case OfModule(x) => (x, refs.tail)
      case _ => (moduleName, refs)
    }
    Component.convertComponent2ComponentName(Component(Some(circuitName), Some(mn), subs, None))
  }
  def getComponentName: ComponentName = {
    val refs = reference.filter {
      case _: OfModule => false
      case _ => true
    }.map(_.value)
    ComponentName(refs.mkString("."), ModuleName(moduleName, CircuitName(circuitName)))
  }
}

object Component {

  val emptyString: String = "E@"

  def tokenize(s: String): Seq[SubComponent] = if(!s.isEmpty && s.head == '/') {
    val endKeywordIndex = s.indexWhere(c => c == '@', 1)
    val keyword = s.slice(1, endKeywordIndex)
    val endValueIndex = s.indexWhere(c => c == '/', endKeywordIndex + 1) match {
      case -1 => s.length
      case i => i
    }
    val value = s.slice(endKeywordIndex + 1, endValueIndex)
    SubComponent.keyword2subcomponent(keyword)(value) +: tokenize(s.substring(endValueIndex))
  } else Nil

  implicit def string2opt(s: String): Option[String] = if(s == emptyString) None else Some(s)

  private def error(c: Component) = throw new Exception(s"Cannot convert $c into Named")
  def isOnly(seq: Seq[SubComponent], keywords:String*): Boolean = {
    seq.map(_.is(keywords:_*)).foldLeft(false)(_ || _) && keywords.nonEmpty
  }

  private[annotations] val counter = new java.util.concurrent.atomic.AtomicInteger(0)
  implicit def convertComponent2Named(c: Component): Named = {
    (c.circuit, c.encapsulatingModule, c.reference) match {
      case (_: Some[String], None, Nil) => convertComponent2CircuitName(c)
      case (_: Some[String], _: Some[String], Nil) => convertComponent2ModuleName(c)
      case (_: Some[String], _: Some[String], _: Seq[SubComponent]) => convertComponent2ComponentName(c)
      case other => error(c)
    }
  }
  implicit def convertComponent2ComponentName(c: Component): ComponentName = {
    val mn = convertComponent2ModuleName(c)
    Seq(c.reference:_*) match {
      case Seq(Ref(name)) => ComponentName(name, mn)
      case Ref(_) :: tail if isOnly(tail, ".", "[]") =>
        val name = c.reference.foldLeft(""){
          case ("", Ref(name)) => name
          case (string, Field(value)) => s"$string.$value"
          case (string, Index(value)) => s"$string[$value]"
        }
        ComponentName(name, mn)
      case _ => error(c)
    }
  }
  implicit def convertComponent2ModuleName(c: Component): ModuleName = {
    c.encapsulatingModule.map(ModuleName(_, convertComponent2CircuitName(c))).getOrElse(error(c))
  }
  implicit def convertComponent2CircuitName(c: Component): CircuitName = {
    c.circuit.map(CircuitName).getOrElse(error(c))
  }
  implicit def convertNamed2Component(n: Named): Component = n match {
    case CircuitName(x) => Component(Some(x), None, Nil, None)
    case ModuleName(m, CircuitName(c)) => Component(Some(c), Some(m), Nil, None)
    case ComponentName(name, ModuleName(m, CircuitName(c))) =>
      def toSubComps(name: String): Seq[SubComponent] = {
        val tokens = AnnotationUtils.tokenize(name)
        val subComps = mutable.ArrayBuffer[SubComponent]()
        subComps += Ref(tokens.head)
        if(tokens.tail.nonEmpty) {
          tokens.tail.zip(tokens.tail.tail).foreach {
            case (".", value: String) => subComps += Field(value)
            case ("[", value: String) => subComps += Index(value.toInt)
            case other =>
          }
        }
        subComps
      }
      Component(c, m, toSubComps(name), None)
  }


}

object Referable {
  def apply(name: String, encapsulatingModule: String): Component = {
    Component(None, Some(encapsulatingModule), Seq(Ref(name)), Some(Component.counter.incrementAndGet()))
  }
}

object Irreferable {
  def apply(value: String, encapsulatingModule: String): Component = {
    Component(None, Some(encapsulatingModule), Seq(Anonymous(value)), Some(Component.counter.incrementAndGet()))
  }
}

/**
  * Named classes associate an annotation with a component in a Firrtl circuit
  */
@deprecated("Use Component instead, will be removed in 1.3", "1.2")
sealed trait Named {
  def serialize: String
}

@deprecated("Use Component instead, will be removed in 1.3", "1.2")
final case class CircuitName(name: String) extends Named {
  if(!validModuleName(name)) throw AnnotationException(s"Illegal circuit name: $name")
  def serialize: String = name
}

@deprecated("Use Component instead, will be removed in 1.3", "1.2")
final case class ModuleName(name: String, circuit: CircuitName) extends Named {
  if(!validModuleName(name)) throw AnnotationException(s"Illegal module name: $name")
  def serialize: String = circuit.serialize + "." + name
}

@deprecated("Use Component instead, will be removed in 1.3", "1.2")
final case class ComponentName(name: String, module: ModuleName) extends Named {
  if(!validComponentName(name)) throw AnnotationException(s"Illegal component name: $name")
  def expr: Expression = toExp(name)
  def serialize: String = module.serialize + "." + name
}
