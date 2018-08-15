// See LICENSE for license details.

package firrtl
package annotations

import firrtl.ir.Expression
import AnnotationUtils.{toExp, validComponentName, validModuleName}
import SubComponent._

import scala.collection.mutable


/**
  * Refers to something in a FIRRTL [[firrtl.ir.Circuit]]. Used for Annotation targets.
  *
  * Can be in various states of completion/resolved:
  *   - Legal: [[SubComponent]]'s in reference are in an order that makes sense
  *   - Complete: circuit and module are non-empty
  *   - PathResolved: all instances/modules in a reference with a hierarchical path are explicitly referred to by name
  *   - ReferenceResolved: all ref/fields/etc. in a reference are explicitly referred to
  *   - Pathless: reference does not refer to things through an instance hierarchy
  * @param circuit
  * @param module
  * @param reference
  */
case class Component(circuit: Option[String],
                     module: Option[String],
                     reference: Seq[SubComponent]) extends Named {

  def serialize: String = {
    "(" + circuit.getOrElse("*") + "," + module.getOrElse("*") + ")/" + path.map{ case (Instance(i), OfModule(m)) => s"$i:$m/"}.mkString("") + notPath.mkString(" ")
  }

  def deepestModule: Option[String] = {
    val deepest = reference.reverseIterator.collectFirst { case OfModule(x) => x }
    if(deepest.isEmpty) module else deepest
  }

  def notPath: Seq[SubComponent] = {
    reference.dropWhile{ s => s.keyword == "of" || s.keyword == "inst" }
  }

  def path: Seq[(Instance, OfModule)] = {
    require(isLegal)
    val refPath = reference.reverse.dropWhile(_.keyword != "of").reverse
    val path = mutable.ArrayBuffer[(Instance, OfModule)]()
    refPath.grouped(2).foreach{
      case Seq(i: Instance, m: OfModule) =>
        path += ((i, m))
      case _ => throw new Exception("BAD!")
    }
    path
  }

  def growingPath: Seq[Seq[(Instance, OfModule)]] = {
    path.reverse.tails.map { p => p.reverse }.toSeq
  }

  def requireLast(default: Boolean, keywords: String*): Unit = {
    val isOne = if (reference.isEmpty) default else reference.last.is(keywords: _*)
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

  def regs: Component = this.copy(reference = reference :+ Regs)

  val circuitName: String = circuit.getOrElse(Component.emptyString)
  val moduleName: String = module.getOrElse(Component.emptyString)



  /**
    * Checks whether the component is legal (incomplete is ok)
    * @return
    */
  def isLegal: Boolean = {
    reference.headOption.forall(_.is("inst", "ref")) && {
      reference.tails.forall {
        case x :: Instance(_) :: tail if x.is("of") => true
        case x :: OfModule(_) :: tail if x.is("inst") => true
        case x :: Ref(_) :: tail if x.is("of") => true
        case x :: Field(_) :: tail if x.is("ref", "[]", ".") => true
        case x :: Index(_) :: tail if x.is("ref", "[]", ".") => true
        case Nil => true
        case _ => false
      }
    }
  }

  /**
    * Checks whether the component is legal and complete, meaning the circuit and module are nonEmpty
    * @return
    */
  def isComplete: Boolean = circuit.nonEmpty && module.nonEmpty && isLegal

  /**
    * Checks whether the component path has no selectors/matchers (e.g. refer to names explicitly)
    * @return
    */
  def isPathResolved: Boolean = true

  /**
    * Checks whether the component reference has no selectors/matchers (e.g. refer to names explicitly)
    * @return
    */
  def isReferenceResolved: Boolean = true

  /**
    * Checks whether the component reference has no instance/ofModule subcomponents
    * @return
    */
  def isPathless: Boolean = reference.collect {
    case _: Instance | _: OfModule => true
  }.isEmpty
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
    (c.circuit, c.module, c.reference) match {
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
    c.module.map(ModuleName(_, convertComponent2CircuitName(c))).getOrElse(error(c))
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
    case c: Component => c
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
