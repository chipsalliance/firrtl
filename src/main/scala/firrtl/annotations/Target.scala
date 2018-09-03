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
  *   - Complete: circuitOpt and moduleOpt are non-empty, and all Instance(_) are followed by OfModule(_)
  *   - Pathless: reference does not refer to things through an instance hierarchy (no Instance(_) or OfModule(_))
  *
  * @param circuitOpt
  * @param moduleOpt
  * @param reference
  */
case class Target(circuitOpt: Option[String],
                  moduleOpt: Option[String],
                  reference: Seq[TargetToken]) extends Named {

  /**
    * Human-readable serialization
    * @return
    */
  def serialize: String = {
    "(" + circuitOpt.getOrElse("*") + "," + moduleOpt.getOrElse("*") + ")/" + path.map{ case (Instance(i), OfModule(m)) => s"$i:$m/"}.mkString("") + notPath.mkString(" ")
  }

  /**
    * Returns the deepest module that encapsulates the reference, or if there is no reference,
    * encapsulates the final instance declaration
    * @return
    */
  def encapsulatingModule: Option[String] = {
    if(path.size == 1) moduleOpt else Some(path.dropRight(1).last._2.value)
  }

  def pathlessTarget: Target = {
    if(path.isEmpty) this else {
      this.copy(moduleOpt = Some(path.last._2.value), reference = notPath)
    }
  }

  def pathTarget: Target = {
    if(notPath.isEmpty) this else {
      this.copy(reference = reference.dropRight(notPath.size))
    }
  }

  def addAll(seq: Seq[TargetToken]): Target = this.copy(reference = reference ++ seq)

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

  def pathAsTargets: Seq[Target] = {
    val targets = mutable.ArrayBuffer[Target]()
    var m = moduleOpt.get
    reference.tails.foreach {
      case (i: Instance) :: (o: OfModule) :: tail =>
        targets += Target(circuitOpt, Some(m), Seq(i, o))
        m = o.value
      case other =>
    }
    targets
  }

  def parentReference: Target = {
    this.copy(reference = reference.dropRight(1))
  }

  def circuitTarget: Target = this.copy(moduleOpt = None, reference = Nil)
  def moduleTarget: Target = this.copy(reference = Nil)

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

  /** Appends a target token to reference, asserts legality
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

  /** Removes n number of target tokens from the right side of [[reference]]
    * @param n
    * @return
    */
  def remove(n: Int): Target = this.copy(reference = reference.dropRight(n))

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
  def circuit(value: String): Target = this.copy(circuitOpt = Some(value))

  /**
    * Returns a new component with the module name
    * @param value Module name
    * @return
    */
  def module(value: String): Target = this.copy(moduleOpt = Some(value))

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
    * Checks whether the component is legal and complete, meaning the circuitOpt and moduleOpt are nonEmpty and
    * all Instance(_) are followed by OfModule(_)
    * @return
    */
  def isComplete: Boolean = {
    isLegal && (isCircuitName || isModuleName || (isReference && reference.tails.forall {
      case Instance(_) :: OfModule(_) :: tail => true
      case Instance(_) :: x :: tail => false
      case x :: OfModule(_) :: tail => false
      case _ => true
    } ))
  }

  /**
    * Checks whether the component reference has no instance/ofModule subcomponents
    * @return
    */
  def isPathless: Boolean = reference.forall {
    case _: Instance | _: OfModule => false
    case _ => true
  }

  def levels: Seq[Target] = {
    val hierarchies = mutable.ArrayBuffer[Target]()
    if(circuitOpt.nonEmpty) {
      hierarchies += circuitTarget
      if(moduleOpt.nonEmpty) {
        hierarchies += moduleTarget
        var m = moduleOpt.get
        path.foreach {
          case (i: Instance, o: OfModule) =>
            hierarchies += Target(circuitOpt, Some(m), Seq(i, o))
            m = o.value
        }
        if(notPath.nonEmpty) hierarchies += Target(circuitOpt, Some(m), notPath)
      }
    }
    hierarchies
  }

  /** Adds another level of instance hierarchy
    *
    * Example: Given root=A and instance=b, transforms (Top, B)/c:C -> (Top, A)/b:B/c:C
    *
    * @param root
    * @param instance
    * @return
    */
  def addHierarchy(root: String, instance: String): Target = {
    this.copy(moduleOpt = Some(root), reference = Seq(Instance(instance), OfModule(moduleOpt.get)) ++ reference)
  }

  /** Removes n levels of instance hierarchy
    *
    * Example: n=1, transforms (Top, A)/b:B/c:C -> (Top, B)/c:C
    *
    * @param n
    * @return
    */
  def stripHierarchy(n: Int): Target = {
    require(path.size >= n, s"Cannot strip $n levels of hierarchy from $this")
    if(n == 0) this else {
      val newModule = path(n - 1)._2.value
      this.copy(moduleOpt = Some(newModule), reference = reference.drop(2*n))
    }
  }

  def broaden(): Target = this match {
    case CircuitTarget(_) => this
    case ModuleTarget(_, _) => circuitTarget
    case PathlessComponentTarget(_, _, _) => moduleTarget
    case InstanceTarget(_, _, _) => this.remove(2)
    case ComponentTarget(_, _, _, _) => this.remove(notPath.size)
  }

  def map(f: Target => Target): Target = this match {
    case CircuitTarget(_) => this
    case ModuleTarget(_, m) => f(circuitTarget).module(m)
    case PathlessComponentTarget(_, _, notPath) => f(moduleTarget).addAll(notPath)
    case InstanceTarget(_, _, _) => f(this.remove(2)).addAll(reference.takeRight(2))
    case ComponentTarget(_, _, _, notPath) => f(this.remove(notPath.size)).addAll(notPath)
  }

  def foreach(f: Target => Unit): Unit = if(!isCircuitName) f(broaden())

  def isCircuitName: Boolean = circuitOpt.nonEmpty && moduleOpt.isEmpty && reference.isEmpty
  def isModuleName: Boolean = circuitOpt.nonEmpty && moduleOpt.nonEmpty && reference.isEmpty
  def isReference: Boolean = circuitOpt.nonEmpty && moduleOpt.nonEmpty && reference.nonEmpty
}

object Target {

  case class NamedException[N<:Named](c: Target, n: String) extends Exception(s"Cannot convert $c into $n")
  private def error(c: Target, n: String = "Named") = throw NamedException(c, n)

  implicit def convertTarget2Named(c: Target): Named = {
    (c.circuitOpt, c.moduleOpt, c.reference) match {
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
    c.moduleOpt.map(ModuleName(_, convertTarget2CircuitName(c))).getOrElse(error(c, "ModuleName"))
  }
  implicit def convertTarget2CircuitName(c: Target): CircuitName = {
    c.circuitOpt.map(CircuitName).getOrElse(error(c, "CircuitName"))
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

object CircuitTarget {
  def unapply(t: Target): Option[String] = if(t.isCircuitName) t.circuitOpt else None
  def apply(circuit: String): Target = Target(Some(circuit), None, Nil)
}

object ModuleTarget {
  def unapply(t: Target): Option[(String, String)] = if(t.isModuleName) Some((t.circuitOpt.get, t.moduleOpt.get)) else None
  def apply(circuit: String, module: String): Target = Target(Some(circuit), Some(module), Nil)
}

object InstanceTarget {
  def unapply(t: Target): Option[(String, String, Seq[(Instance, OfModule)])] = if(t.isReference && t.notPath.isEmpty) Some((t.circuitOpt.get, t.moduleOpt.get, t.path)) else None
  def apply(circuit: String, module: String, path: Seq[(Instance, OfModule)]): Target =
    Target(Some(circuit), Some(module), path.flatMap { case (i, o) => Seq(i, o) } )
}

object PathlessComponentTarget {
  def unapply(t: Target): Option[(String, String, Seq[TargetToken])] = if(t.isReference && t.isPathless) Some((t.circuitOpt.get, t.moduleOpt.get, t.reference)) else None
  //def apply(circuit: String, module: String, references: Seq[TargetToken]): Target = Target(Some(circuit), Some(module), references)
}

object ComponentTarget {
  def unapply(t: Target): Option[(String, String, Seq[(Instance, OfModule)], Seq[TargetToken])] = if(t.isReference) Some((t.circuitOpt.get, t.moduleOpt.get, t.path, t.notPath)) else None
  //def apply(circuit: String, module: String, ): Target = Target(Some(circuit), Some(module), Nil)
}

/*
object ComponentTarget {
  def unapply(t: Target): Option[(String, String, Seq[(Instance, OfModule)], String)] =
    if(t.isReference && t.notPath.size == 1 && t.notPath.head.isInstanceOf[Ref])
      Some((t.circuitOpt.get, t.moduleOpt.get, t.path, t.notPath.head.value.toString))
    else None
  def apply(circuit: String, module: String, path: Seq[(Instance, OfModule)], ref: String): Target =
    Target(Some(circuit), Some(module), path.foldRight(Seq[TargetToken](Ref(ref))) {
      case ((i, o), tail) => i +: (o +: tail)
    } )
}




object SubComponentTarget {
  def unapply(t: Target): Option[(String, String, Seq[TargetToken])] = if(t.isReference && t.isPathless) Some((t.circuitOpt.get, t.moduleOpt.get, t.reference)) else None
  def apply(circuit: String, module: String): Target = Target(Some(circuit), Some(module), Nil)
}
*/



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
