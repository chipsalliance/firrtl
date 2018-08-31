// See LICENSE for license details.

package firrtl
package annotations

import firrtl.ir.Expression
import AnnotationUtils.{toExp, validComponentName, validModuleName}
import TargetToken._

import scala.collection.mutable


/**
  * Refers to something in a FIRRTL [[firrtl.ir.Circuit]]. Used for Annotation targets.
  *
  * Can be in various states of completion/resolved:
  *   - Legal: [[TargetToken]]'s in reference are in an order that makes sense
  *   - Complete: circuit and module are non-empty, and all Instance(_) are followed by OfModule(_)
  *   - Pathless: reference does not refer to things through an instance hierarchy (no Instance(_) or OfModule(_))
  *
  * @param circuit
  * @param module
  * @param reference
  */
case class Target(circuit: Option[String],
                  module: Option[String],
                  reference: Seq[TargetToken]) extends Named {

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
    * Returns the [[TargetToken]]'s after any instance hierarchy, if one exists
    *
    * @return
    */
  def notPath: Seq[TargetToken] = {
    reference.dropWhile{ s => s.keyword == "of" || s.keyword == "inst" }
  }

  def justPath: Seq[TargetToken] = {
    reference.collect {
      case i: Instance => i
      case o: OfModule => o
    }
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
    * Requires the last [[TargetToken]] in reference to be one of the SubComponents keywords
    *
    * @param default
    * @param keywords
    */
  def requireLast(default: Boolean, keywords: String*): Unit = {
    val isOne = if (reference.isEmpty) default else reference.last.is(keywords: _*)
    require(isOne, s"${reference.last} is not one of $keywords")
  }

  /**
    * Appends a target token to reference, asserts legality
    * @param token
    * @return
    */
  def add(token: TargetToken): Target = {
    token match {
      case _: Instance  => requireLast(true, "inst", "of")
      case _: OfModule  => requireLast(false, "inst")
      case _: Ref       => requireLast(true, "inst", "of")
      case _: Field     => requireLast(true, "ref", "[]", ".", "clock", "init", "reset")
      case _: Index     => requireLast(true, "ref", "[]", ".", "clock", "init", "reset")
      case Clock        => requireLast(true, "ref")
      case Init         => requireLast(true, "ref")
      case Reset        => requireLast(true, "ref")
    }
    this.copy(reference = reference :+ token)
  }

  /**
    * Optionally tries to append token to reference, fails return is not a legal Target
    * @param token
    * @return
    */
  def optAdd(token: TargetToken): Option[Target] = {
    try{
      Some(add(token))
    } catch {
      case _: IllegalArgumentException => None
    }
  }

  /**
    * Returns a new component with the circuit name
    * @param value Circuit name
    * @return
    */
  def circuit(value: String): Target = this.copy(circuit = Some(value))

  /**
    * Returns a new component with the module name
    * @param value Module name
    * @return
    */
  def module(value: String): Target = this.copy(module = Some(value))

  /**
    * Creates a new Target, appending a ref
    * @param value
    * @return
    */
  def ref(value: String): Target = add(Ref(value))

  /**
    * Creates a new Target, appending an instance
    * @param value
    * @return
    */
  def inst(value: String): Target = add(Instance(value))

  /**
    * Creates a new Target, appending an instance and ofmodule
    * @param instance
    * @param of
    * @return
    */
  def instOf(instance: String, of: String): Target = inst(instance).of(of)

  /**
    * Creates a new Target, appending an ofModule
    * @param value
    * @return
    */
  def of(value: String): Target = add(OfModule(value))

  /**
    * Creates a new Target, appending a field
    * @param name
    * @return
    */
  def field(name: String): Target = add(Field(name))

  /**
    * Creates a new Target, appending an index
    * @param value
    * @return
    */
  def index(value: Int): Target = add(Index(value))

  /**
    * Creates a new Target, appending a clock
    * @return
    */
  def clock: Target = add(Clock)

  /**
    * Creates a new Target, appending an init
    * @return
    */
  def init: Target = add(Init)

  /**
    * Creates a new Target, appending a reset
    * @return
    */
  def reset: Target = add(Reset)

  /**
    * Checks whether the component is legal (incomplete is ok)
    * @return
    */
  def isLegal: Boolean = {
    try {
      var comp = this.copy(reference = Nil)
      for(token <- reference) {
        comp = comp.add(token)
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

  def isCircuitName: Boolean = circuit.nonEmpty && module.isEmpty && reference.isEmpty
  def isModuleName: Boolean = circuit.nonEmpty && module.nonEmpty && reference.isEmpty
  def isReference: Boolean = circuit.nonEmpty && module.nonEmpty && reference.nonEmpty
}

object Target {

  case class NamedException[N<:Named](c: Target, n: String) extends Exception(s"Cannot convert $c into $n")
  private def error(c: Target, n: String = "Named") = throw NamedException(c, n)

  implicit def convertTarget2Named(c: Target): Named = {
    (c.circuit, c.module, c.reference) match {
      case (_: Some[String], None, Nil) => convertTarget2CircuitName(c)
      case (_: Some[String], _: Some[String], Nil) => convertTarget2ModuleName(c)
      case (_: Some[String], _: Some[String], _: Seq[TargetToken]) => convertTarget2ComponentName(c)
      case other => error(c)
    }
  }

  implicit def convertTarget2ComponentName(c: Target): ComponentName = {
    val mn = convertTarget2ModuleName(c)
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
  implicit def convertTarget2ModuleName(c: Target): ModuleName = {
    c.module.map(ModuleName(_, convertTarget2CircuitName(c))).getOrElse(error(c, "ModuleName"))
  }
  implicit def convertTarget2CircuitName(c: Target): CircuitName = {
    c.circuit.map(CircuitName).getOrElse(error(c, "CircuitName"))
  }
  implicit def convertNamed2Target(n: Named): Target = n match {
    case CircuitName(x) => Target(Some(x), None, Nil)
    case ModuleName(m, CircuitName(c)) => Target(Some(c), Some(m), Nil)
    case ComponentName(name, ModuleName(m, CircuitName(c))) => Target(Some(c), Some(m), toTargetTokens(name))
    case c: Target => c
  }

  private def tokenize(s: String): Seq[TargetToken] = if(!s.isEmpty && s.head == '/') {
    val endKeywordIndex = s.indexWhere(c => c == '@', 1)
    val keyword = s.slice(1, endKeywordIndex)
    val endValueIndex = s.indexWhere(c => c == '/', endKeywordIndex + 1) match {
      case -1 => s.length
      case i => i
    }
    val value = s.slice(endKeywordIndex + 1, endValueIndex)
    TargetToken.keyword2targettoken(keyword)(value) +: tokenize(s.substring(endValueIndex))
  } else Nil

  private def toTargetTokens(name: String): Seq[TargetToken] = {
    val tokens = AnnotationUtils.tokenize(name)
    val subComps = mutable.ArrayBuffer[TargetToken]()
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

  private def isOnly(seq: Seq[TargetToken], keywords:String*): Boolean = {
    seq.map(_.is(keywords:_*)).foldLeft(false)(_ || _) && keywords.nonEmpty
  }
}

/**
  * Named classes associate an annotation with a component in a Firrtl circuit
  */
@deprecated("Use Target instead, will be removed in 1.3", "1.2")
sealed trait Named {
  def serialize: String
}

@deprecated("Use Target instead, will be removed in 1.3", "1.2")
final case class CircuitName(name: String) extends Named {
  if(!validModuleName(name)) throw AnnotationException(s"Illegal circuit name: $name")
  def serialize: String = name
}

@deprecated("Use Target instead, will be removed in 1.3", "1.2")
final case class ModuleName(name: String, circuit: CircuitName) extends Named {
  if(!validModuleName(name)) throw AnnotationException(s"Illegal module name: $name")
  def serialize: String = circuit.serialize + "." + name
}

@deprecated("Use Target instead, will be removed in 1.3", "1.2")
final case class ComponentName(name: String, module: ModuleName) extends Named {
  if(!validComponentName(name)) throw AnnotationException(s"Illegal component name: $name")
  def expr: Expression = toExp(name)
  def serialize: String = module.serialize + "." + name
}
