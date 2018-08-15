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
  *   - Complete: circuit and module are non-empty, and all Instance(_) are followed by OfModule(_)
  *   - Pathless: reference does not refer to things through an instance hierarchy (no Instance(_) or OfModule(_))
  * @param circuit
  * @param module
  * @param reference
  */
case class Component(circuit: Option[String],
                     module: Option[String],
                     reference: Seq[SubComponent]) extends Named {

  /**
    * Human-readable serialization
    * @return
    */
  def serialize: String = {
    "(" + circuit.getOrElse("*") + "," + module.getOrElse("*") + ")/" + path.map{ case (Instance(i), OfModule(m)) => s"$i:$m/"}.mkString("") + notPath.mkString(" ")
  }

  /**
    * Returns the module farthest down the instance hierarchy, or the top module if no instance hierarchy
    * @return
    */
  def deepestModule: Option[String] = {
    val deepest = reference.reverseIterator.collectFirst { case OfModule(x) => x }
    if(deepest.isEmpty) module else deepest
  }

  /**
    * Returns the [[SubComponent]]'s after any instance hierarchy, if one exists
    * @return
    */
  def notPath: Seq[SubComponent] = {
    reference.dropWhile{ s => s.keyword == "of" || s.keyword == "inst" }
  }

  /**
    * Returns the instance hierarchy path, if one exists
    * @return
    */
  def path: Seq[(Instance, OfModule)] = {
    require(isLegal, this)
    val refPath = reference.reverse.dropWhile(_.keyword != "of").reverse
    val path = mutable.ArrayBuffer[(Instance, OfModule)]()
    refPath.grouped(2).foreach{
      case Seq(i: Instance, m: OfModule) =>
        path += ((i, m))
      case _ => throw new Exception("BAD!")
    }
    path
  }

  /**
    * Returns a sequence of paths, each one increasing the instance hierarchy
    * @return
    */
  def growingPath: Seq[Seq[(Instance, OfModule)]] = {
    path.reverse.tails.map { p => p.reverse }.toSeq
  }

  /**
    * Requires the last [[SubComponent]] in reference to be one of the SubComponents keywords
    * @param default
    * @param keywords
    */
  def requireLast(default: Boolean, keywords: String*): Unit = {
    val isOne = if (reference.isEmpty) default else reference.last.is(keywords: _*)
    require(isOne, s"${reference.last} is not one of $keywords")
  }

  /**
    * Appends a subcomponent to reference, asserts legality
    * @param sub
    * @return
    */
  def add(sub: SubComponent): Component = {
    sub match {
      case _: Instance  => requireLast(true, "inst", "of")
      case _: OfModule  => requireLast(false, "inst")
      case _: Ref       => requireLast(true, "inst", "of")
      case _: Field     => requireLast(true, "ref", "[]", ".", "clock", "init", "reset")
      case _: Index     => requireLast(true, "ref", "[]", ".", "clock", "init", "reset")
      case Clock        => requireLast(true, "ref")
      case Init         => requireLast(true, "ref")
      case Reset        => requireLast(true, "ref")
    }
    this.copy(reference = reference :+ sub)
  }

  /**
    * Optionally tries to append sub to reference, fails return is not a legal Component
    * @param sub
    * @return
    */
  def optAdd(sub: SubComponent): Option[Component] = {
    try{
      Some(add(sub))
    } catch {
      case _: IllegalArgumentException => None
    }
  }

  /**
    * Creates a new Component, appending a ref
    * @param value
    * @return
    */
  def ref(value: String): Component = add(Ref(value))

  /**
    * Creates a new Component, appending an instance
    * @param value
    * @return
    */
  def inst(value: String): Component = add(Instance(value))

  /**
    * Creates a new Component, appending an ofModule
    * @param value
    * @return
    */
  def of(value: String): Component = add(OfModule(value))

  /**
    * Creates a new Component, appending a field
    * @param name
    * @return
    */
  def field(name: String): Component = add(Field(name))

  /**
    * Creates a new Component, appending an index
    * @param value
    * @return
    */
  def index(value: Int): Component = add(Index(value))

  /**
    * Creates a new Component, appending a clock
    * @return
    */
  def clock: Component = add(Clock)

  /**
    * Creates a new Component, appending an init
    * @return
    */
  def init: Component = add(Init)

  /**
    * Creates a new Component, appending a reset
    * @return
    */
  def reset: Component = add(Reset)

  /**
    * Checks whether the component is legal (incomplete is ok)
    * @return
    */
  def isLegal: Boolean = {
    try {
      var comp = this.copy(reference = Nil)
      for(sub <- reference) {
        comp = comp.add(sub)
      }
      true
    } catch {
      case _: IllegalArgumentException => false
    }
  }

  /**
    * Checks whether the component is legal and complete, meaning the circuit and module are nonEmpty and
    * all Instance(_) are followed by OfModule(_)
    * @return
    */
  def isComplete: Boolean = circuit.nonEmpty && module.nonEmpty && isLegal && reference.tails.forall {
    case Instance(_) :: OfModule(_) :: tail => true
    case Instance(_) :: x :: tail => false
    case x :: OfModule(_) :: tail => false
    case _ => true
  }

  /**
    * Checks whether the component reference has no instance/ofModule subcomponents
    * @return
    */
  def isPathless: Boolean = reference.forall {
    case _: Instance | _: OfModule => false
    case _ => true
  }
}

object Component {

  case class NamedException[N<:Named](c: Component, n: String) extends Exception(s"Cannot convert $c into $n")
  private def error(c: Component, n: String = "Named") = throw NamedException(c, n)

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
      case _ => error(c, "ComponentName")
    }
  }
  implicit def convertComponent2ModuleName(c: Component): ModuleName = {
    c.module.map(ModuleName(_, convertComponent2CircuitName(c))).getOrElse(error(c, "ModuleName"))
  }
  implicit def convertComponent2CircuitName(c: Component): CircuitName = {
    c.circuit.map(CircuitName).getOrElse(error(c, "CircuitName"))
  }
  implicit def convertNamed2Component(n: Named): Component = n match {
    case CircuitName(x) => Component(Some(x), None, Nil)
    case ModuleName(m, CircuitName(c)) => Component(Some(c), Some(m), Nil)
    case ComponentName(name, ModuleName(m, CircuitName(c))) => Component(Some(c), Some(m), toSubComps(name))
    case c: Component => c
  }

  private def tokenize(s: String): Seq[SubComponent] = if(!s.isEmpty && s.head == '/') {
    val endKeywordIndex = s.indexWhere(c => c == '@', 1)
    val keyword = s.slice(1, endKeywordIndex)
    val endValueIndex = s.indexWhere(c => c == '/', endKeywordIndex + 1) match {
      case -1 => s.length
      case i => i
    }
    val value = s.slice(endKeywordIndex + 1, endValueIndex)
    SubComponent.keyword2subcomponent(keyword)(value) +: tokenize(s.substring(endValueIndex))
  } else Nil

  private def toSubComps(name: String): Seq[SubComponent] = {
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

  private def isOnly(seq: Seq[SubComponent], keywords:String*): Boolean = {
    seq.map(_.is(keywords:_*)).foldLeft(false)(_ || _) && keywords.nonEmpty
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
