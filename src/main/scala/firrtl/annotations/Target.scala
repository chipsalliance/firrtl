// See LICENSE for license details.

package firrtl
package annotations

import firrtl.ir.Expression
import AnnotationUtils.{toExp, validComponentName, validModuleName}
import TargetToken._
import firrtl.Utils.throwInternalError

import scala.collection.mutable

/**
  * Refers to something in a FIRRTL [[firrtl.ir.Circuit]]. Used for Annotation targets.
  *
  * Can be in various states of completion/resolved:
  *   - Legal: [[TargetToken]]'s in tokens are in an order that makes sense
  *   - Complete: circuitOpt and moduleOpt are non-empty, and all Instance(_) are followed by OfModule(_)
  *   - Pathless: tokens does not refer to things through an instance hierarchy (no Instance(_) or OfModule(_))
  */
sealed trait Target extends Named {

  def circuitOpt: Option[String]
  def moduleOpt: Option[String]
  def tokens: Seq[TargetToken]

  def modify(circuitOpt: Option[String] = circuitOpt, moduleOpt: Option[String] = moduleOpt, tokens: Seq[TargetToken] = tokens) = GenericTarget(circuitOpt, moduleOpt, tokens)

  /**
    * Human-readable serialization
    * @return
    */
  def serialize: String = {
    "(" + circuitOpt.getOrElse("???") + "," + moduleOpt.getOrElse("???") + ")/" + tokens.map {
      case Ref(r) => r
      case Instance(i) => i
      case OfModule(o) => s":$o/"
      case Field(f) => s".$f"
      case Index(v) => s"[$v]"
      case Clock => s"@clock"
      case Reset => s"@reset"
      case Init => s"@init"
    }.mkString("")
  }


  def toGenericTarget: GenericTarget = GenericTarget(circuitOpt, moduleOpt, tokens)

  def toNamed: Named = toGenericTarget.toNamed

  def getComplete: Option[CompleteTarget]

}

object Target {
  def apply(circuitOpt: Option[String], moduleOpt: Option[String], reference: Seq[TargetToken]) = GenericTarget(circuitOpt, moduleOpt, reference)
  def unapply(t: Target): Option[(Option[String], Option[String], Seq[TargetToken])] = Some((t.circuitOpt, t.moduleOpt, t.tokens))

  case class NamedException[N<:Named](c: Target, n: String) extends Exception(s"Cannot convert $c into $n")
  private def error(c: Target, n: String = "Named") = throw NamedException(c, n)

  implicit def convertCircuitTarget2CircuitName(c: CircuitTarget): CircuitName = c.toNamed
  implicit def convertModuleTarget2ModuleName(c: ModuleTarget): ModuleName = c.toNamed
  implicit def convertLocalComponent2ComponentName(c: IsComponent): ComponentName = c.toNamed
  implicit def convertTarget2Named(c: Target): Named = c.toNamed

  implicit def convertCircuitName2CircuitTarget(c: CircuitName): CircuitTarget = CircuitTarget(c.name)
  implicit def convertModuleName2ModuleTarget(c: ModuleName): ModuleTarget = ModuleTarget(c.circuit.name, c.name)
  implicit def convertComponentName2IsLocalComponent(c: ComponentName): ReferenceTarget = {
    toTargetTokens(c.name).toList match {
      //case Seq(Ref(r)) => LocalReferenceTarget(c.module.circuit.name, c.module.name, r, Nil)
      case Ref(r) :: components => ReferenceTarget(c.module.circuit.name, c.module.name, Nil, r, components)
      case other => throw new Exception(s"Cannot convert $c into [[LocalReferenceTarget]]: $other")
    }
  }
  implicit def convertNamed2Target(n: Named): CompleteTarget = n match {
    case c: CircuitName => convertCircuitName2CircuitTarget(c)
    case m: ModuleName => convertModuleName2ModuleTarget(m)
    case c: ComponentName => convertComponentName2IsLocalComponent(c)
    case c: GenericTarget => c.complete
    case c: CompleteTarget => c
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

  def isOnly(seq: Seq[TargetToken], keywords:String*): Boolean = {
    seq.map(_.is(keywords:_*)).foldLeft(false)(_ || _) && keywords.nonEmpty
  }
}

case class GenericTarget(circuitOpt: Option[String], moduleOpt: Option[String], tokens: Seq[TargetToken]) extends Target {
  override def toGenericTarget: GenericTarget = this
  override def toNamed: Named = getComplete match {
    case Some(c: IsComponent) if c.isLocal => c.toNamed
    case Some(c: ModuleTarget) => c.toNamed
    case Some(c: CircuitTarget) => c.toNamed
    case other => throwInternalError(s"Cannot convert $this to [[Named]]")
  }
  def getComplete: Option[CompleteTarget] = {
    if(isLegal) {
      this match {
        case GenericTarget(Some(c), None, Nil) => Some(CircuitTarget(c))
        case GenericTarget(Some(c), Some(m), Nil) => Some(ModuleTarget(c, m))
        case GenericTarget(Some(c), Some(m), Ref(r) :: component) => Some(ReferenceTarget(c, m, Nil, r, component))
        case GenericTarget(Some(c), Some(m), Instance(i) :: OfModule(o) :: Nil) => Some(InstanceTarget(c, m, Nil, i, o))
        case GenericTarget(Some(c), Some(m), component) =>
          val optPath = getPath
          if(optPath.nonEmpty) {
            val path = optPath.get
            val optRef = getRef
            if(optRef.nonEmpty) {
              val (r, comps) = optRef.get
              Some(ReferenceTarget(c, m, path, r, comps))
            } else {
              val (i, o) = getInstanceOf.get
              Some(InstanceTarget(c, m, path, i, o))
            }
          } else {
            val optRef = getRef
            if(optRef.nonEmpty) {
              val (r, comps) = optRef.get
              Some(ReferenceTarget(c, m, Nil, r, comps))
            } else {
              val (i, o) = getInstanceOf.get
              Some(InstanceTarget(c, m, Nil, i, o))
            }
          }
      }
    } else {
      None
    }
  }

  def getPath: Option[Seq[(Instance, OfModule)]] = if(isComplete) {
    val allInstOfs = tokens.grouped(2).collect { case Seq(i: Instance, o:OfModule) => (i, o)}.toSeq
    if(tokens.nonEmpty && tokens.last.isInstanceOf[OfModule]) Some(allInstOfs.dropRight(1)) else Some(allInstOfs)
  } else None

  def isLocal: Boolean = !(getPath.nonEmpty && getPath.get.nonEmpty)

  def getRef: Option[(String, Seq[TargetToken])] = if(isComplete) {
    val (optRef, comps) = tokens.foldLeft((None: Option[String], Vector.empty[TargetToken])) {
      case ((None, v), Ref(r)) => (Some(r), v)
      case ((r: Some[String], comps), c) => (r, comps :+ c)
      case ((r, v), other) => (None, v)
    }
    Some((optRef.get, comps))
  } else None

  def getInstanceOf: Option[(String, String)] = if(isComplete) {
    tokens.grouped(2).foldLeft(None: Option[(String, String)]) {
      case (instOf, Seq(i: Instance, o: OfModule)) => Some((i.value, o.value))
      case (instOf, _) => None
    }
  } else None

  def complete: CompleteTarget = getComplete.get

  def tryToComplete: Target = getComplete.getOrElse(this)

  /** Returns a new component with the circuit name */
  def circuit(value: String): Target = this.copy(circuitOpt = Some(value))

  /** Returns a new component with the module name */
  def module(value: String): Target = this.copy(moduleOpt = Some(value))

  def addAll(seq: Seq[TargetToken]): Target = this.copy(tokens = tokens ++ seq)

  /**
    * Requires the last [[TargetToken]] in tokens to be one of the SubComponents keywords
    *
    * @param default
    * @param keywords
    */
  def requireLast(default: Boolean, keywords: String*): Unit = {
    val isOne = if (tokens.isEmpty) default else tokens.last.is(keywords: _*)
    require(isOne, s"${tokens.last} is not one of $keywords")
  }

  /** Appends a target token to tokens, asserts legality
    * @param token
    * @return
    */
  def add(token: TargetToken): GenericTarget = {
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
    this.copy(tokens = tokens :+ token)
  }

  /** Removes n number of target tokens from the right side of [[tokens]] */
  def remove(n: Int): GenericTarget = this.copy(tokens = tokens.dropRight(n))

  /** Optionally tries to append token to tokens, fails return is not a legal Target */
  def optAdd(token: TargetToken): Option[Target] = {
    try{
      Some(add(token))
    } catch {
      case _: IllegalArgumentException => None
    }
  }

  /**
    * Checks whether the component is legal (incomplete is ok)
    * @return
    */
  def isLegal: Boolean = {
    try {
      var comp: GenericTarget = this.copy(tokens = Nil)
      for(token <- tokens) {
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
    isLegal && (isCircuitName || isModuleName || (isReference && tokens.tails.forall {
      case Instance(_) :: OfModule(_) :: tail => true
      case Instance(_) :: x :: tail => false
      case x :: OfModule(_) :: tail => false
      case _ => true
    } ))
  }


  def isCircuitName: Boolean = circuitOpt.nonEmpty && moduleOpt.isEmpty && tokens.isEmpty
  def isModuleName: Boolean = circuitOpt.nonEmpty && moduleOpt.nonEmpty && tokens.isEmpty
  def isReference: Boolean = circuitOpt.nonEmpty && moduleOpt.nonEmpty && tokens.nonEmpty
}

/** Concretely points to a FIRRTL target, no generic selectors
  * IsLegal
  */
trait CompleteTarget extends Target {
  def circuit: String
  def tokens: Seq[TargetToken]
  def circuitTarget: CircuitTarget = CircuitTarget(circuitOpt.get)
  def encapsulatingModule: Option[String]
  def getComplete: Option[CompleteTarget] = Some(this)

  /** Checks whether the component tokens has no instance/ofModule subcomponents */
  def isLocal: Boolean

  /** Adds another level of instance hierarchy
    *
    * Example: Given root=A and instance=b, transforms (Top, B)/c:C -> (Top, A)/b:B/c:C
    *
    * @param root
    * @param instance
    * @return
    */
  def addHierarchy(root: String, instance: String): IsComponent
    //this.toGenericTarget.copy(moduleOpt = Some(root), reference = Seq(Instance(instance), OfModule(moduleOpt.get)) ++ tokens)

  /*
  {
    require(path.size >= n, s"Cannot strip $n levels of hierarchy from $this")
    if(n == 0) this else {
      val newModule = path(n - 1)._2.value
      this.toGenericTarget.copy(moduleOpt = Some(newModule), reference = tokens.drop(2*n))
    }
  }
  */

  def setPathTarget(newPath: IsModule): CompleteTarget
}


/** A member of a FIRRTL Circuit (e.g. cannot point to a CircuitTarget)
  */
trait IsMember extends CompleteTarget {
  def module: String
  /**
    * Returns the instance hierarchy path, if one exists
    * @return
    */
  def path: Seq[(Instance, OfModule)]

  def justPath: Seq[TargetToken]

  def notPath: Seq[TargetToken]

  def pathlessTarget: IsMember

  def pathTarget: CompleteTarget

  // TODO is this ok?
  def moduleTarget: ModuleTarget = ModuleTarget(circuitOpt.get, moduleOpt.get)

  def targetParent: CompleteTarget

  def pathAsTargets: Seq[InstanceTarget] = {
    val targets = mutable.ArrayBuffer[InstanceTarget]()
    path.foldLeft((module, Vector.empty[InstanceTarget])) {
      case ((m, vec), (Instance(i), OfModule(o))) =>
        (o, vec :+ InstanceTarget(circuit, m, Nil, i, o))
    }._2
  }
}

trait IsModule extends IsMember {
  /** Creates a new Target, appending a ref */
  def ref(value: String): ReferenceTarget

  /** Creates a new Target, appending an instance and ofmodule */
  def instOf(instance: String, of: String): InstanceTarget

  def asPath: Seq[(Instance, OfModule)]
}

/** A component of a FIRRTL Module (e.g. cannot point to a CircuitTarget or ModuleTarget)
  */
trait IsComponent extends IsMember {
  def module: String
  //def rename(f: IsComponent => Option[Seq[IsLocalComponent]]): Option[Seq[IsLocalComponent]]
  override def toNamed: ComponentName = {
    if(isLocal){
      val mn = ModuleName(module, CircuitName(circuit))
      Seq(tokens:_*) match {
        case Seq(Ref(name)) => ComponentName(name, mn)
        case Ref(_) :: tail if Target.isOnly(tail, ".", "[]") =>
          val name = tokens.foldLeft(""){
            case ("", Ref(name)) => name
            case (string, Field(value)) => s"$string.$value"
            case (string, Index(value)) => s"$string[$value]"
          }
          ComponentName(name, mn)
        case Seq(Instance(name), OfModule(o)) => ComponentName(name, mn)
      }
    } else throw new Exception(s"Cannot convert $this to [[ComponentName]]")
  }
  /** Removes n levels of instance hierarchy
    *
    * Example: n=1, transforms (Top, A)/b:B/c:C -> (Top, B)/c:C
    *
    * @param n
    * @return
    */
  def stripHierarchy(n: Int): IsMember

  def justPath: Seq[TargetToken] = path.foldLeft(Vector.empty[TargetToken]) {
    case (vec, (i, o)) => vec ++ Seq(i, o)
  }
  def pathTarget: IsModule = {
    if(path.isEmpty) moduleTarget else {
      val (i, o) = path.last
      InstanceTarget(circuit, module, path.dropRight(1), i.value, o.value)
    }
  }

  def tokens = justPath ++ notPath

  def encapsulatingModule: Option[String] = if(path.isEmpty) moduleOpt else Some(path.last._2.value)

  def isLocal = path.isEmpty
}


/*
trait IsReference extends IsComponent {
  def ref: String

  def component: Seq[TargetToken]

  def notPath: Seq[TargetToken] = Ref(ref) +: component

  /** Creates a new Target, appending a field */
  def field(name: String): IsReference

  /* Creates a new Target, appending an index */
  def index(value: Int): IsReference

  /** Creates a new Target, appending a clock */
  def clock: IsReference

  /** Creates a new Target, appending an init */
  def init: IsReference

  /** Creates a new Target, appending a reset */
  def reset: IsReference

  def setPathTarget(newPath: IsModule): IsReference =
    ReferenceTarget(newPath.circuit, newPath.module, newPath.asPath, ref, component)
}

trait IsInstance extends IsModule with IsComponent {
  def instance: String
  def ofModule: String

  def notPath: Seq[TargetToken] = Seq(Instance(instance), OfModule(ofModule))
  def asReference: IsReference
  def ofModuleTarget: ModuleTarget = ModuleTarget(circuit, ofModule)
  def asPath: Seq[(Instance, OfModule)]
  def setPathTarget(newPath: IsModule): IsInstance =
    InstanceTarget(newPath.circuit, newPath.module, newPath.asPath, instance, ofModule)
}

/** No path - only points to things directly in AST
  */
trait IsLocal extends CompleteTarget {
  def path: Seq[(Instance, OfModule)] = Nil
  def stripHierarchy(n: Int): CompleteTarget = throwInternalError(s"Cannot strip $n levels of hierarchy from $this")
  def encapsulatingModule: Option[String] = moduleOpt
  def pathTarget: IsModule = if(moduleOpt.isEmpty) circuitTarget else moduleTarget
  def pathlessTarget: IsLocal = this
  def justPath: Seq[TargetToken] = Nil
  def pathAsTargets: Seq[InstanceTarget] = Nil
  def isLocal = true
  def growingPath: Seq[Seq[(Instance, OfModule)]] = Nil
}

/** Has a path
  */
trait NonLocalTarget extends CompleteTarget {
  def isLocal = false
  def encapsulatingModule: Option[String] = Some(path.last._2.value)
  def justPath: Seq[TargetToken] = path.foldLeft(Vector.empty[TargetToken]) {
    case (vec, (i, o)) => vec ++ Seq(i, o)
  }
  def growingPath: Seq[Seq[(Instance, OfModule)]] = {
    path.reverse.tails.map { p => p.reverse }.toSeq
  }
  def module: String
}


// Intersection between Membership and Locality
trait IsLocalMember extends IsLocal with IsMember

trait IsLocalComponent extends IsLocalMember with IsComponent {
}
*/

case class CircuitTarget(circuit: String) extends CompleteTarget {
  def circuitOpt: Option[String] = Some(circuit)
  def moduleOpt: Option[String] = None
  def tokens = Nil
  def targetParent: CompleteTarget = throwInternalError(s"Cannot call targetParent on $this")
  def isLocal = true

  override def encapsulatingModule: Option[String] = None
  override def addHierarchy(root: String, instance: String): ReferenceTarget =
    ReferenceTarget(circuit, root, Nil, instance, Nil)

  override def setPathTarget(newPath: IsModule): CircuitTarget = throwInternalError(s"Cannot call setPathTarget on $this")
  def module(m: String): ModuleTarget = ModuleTarget(circuit, m)

  override def toNamed: CircuitName = CircuitName(circuit)
}

case class ModuleTarget(circuit: String, module: String) extends IsModule {
  def circuitOpt: Option[String] = Some(circuit)
  def moduleOpt: Option[String] = Some(module)
  def tokens: Seq[TargetToken] = Nil
  def targetParent: CircuitTarget = CircuitTarget(circuit)
  def addHierarchy(root: String, instance: String): InstanceTarget = InstanceTarget(circuit, root, Nil, instance, module)
  def ref(value: String): ReferenceTarget = ReferenceTarget(circuit, module, Nil, value, Nil)
  def instOf(instance: String, of: String): InstanceTarget = InstanceTarget(circuit, module, Nil, instance, of)
  def module(m: String) = ModuleTarget(circuit, m)
  def asPath = Nil
  def path: Seq[(Instance, OfModule)] = Nil

  def justPath: Seq[TargetToken] = Nil

  def notPath: Seq[TargetToken] = Nil

  def pathlessTarget: ModuleTarget = this

  def pathTarget: ModuleTarget = this

  override def encapsulatingModule: Option[String] = Some(module)
  def isLocal = true

  def setPathTarget(newPath: IsModule): IsModule = newPath
  override def toNamed: ModuleName = ModuleName(module, CircuitName(circuit))
}

case class ReferenceTarget(circuit: String, module: String, override val path: Seq[(Instance, OfModule)], ref: String, component: Seq[TargetToken]) extends IsComponent {
  def circuitOpt: Option[String] = Some(circuit)
  def moduleOpt: Option[String] = Some(module)
  def targetParent: CompleteTarget = component match {
    case Nil =>
      if(path.isEmpty) moduleTarget else {
        val (i, o) = path.last
        InstanceTarget(circuit, module, path.dropRight(1), i.value, o.value)
      }
    case other => ReferenceTarget(circuit, module, path, ref, component.dropRight(1))
  }

  override def notPath: Seq[TargetToken] = Ref(ref) +: component
  override def addHierarchy(root: String, instance: String): ReferenceTarget =
    ReferenceTarget(circuit, root, (Instance(instance), OfModule(module)) +: path, ref, component)
  override def stripHierarchy(n: Int): ReferenceTarget = {
    require(path.size >= n, s"Cannot strip $n levels of hierarchy from $this")
    if(n == 0) this else {
      val newModule = path(n - 1)._2.value
      ReferenceTarget(circuit, newModule, path.drop(n), ref, component)
    }
  }
  def index(value: Int): ReferenceTarget = ReferenceTarget(circuit, module, path, ref, component :+ Index(value))
  def field(value: String): ReferenceTarget = ReferenceTarget(circuit, module, path, ref, component :+ Field(value))
  def init: ReferenceTarget = ReferenceTarget(circuit, module, path, ref, component :+ Init)
  def reset: ReferenceTarget = ReferenceTarget(circuit, module, path, ref, component :+ Reset)
  def clock: ReferenceTarget = ReferenceTarget(circuit, module, path, ref, component :+ Clock)
  def pathlessTarget: ReferenceTarget = ReferenceTarget(circuit, encapsulatingModule.get, Nil, ref, component)
  def setPathTarget(newPath: IsModule): ReferenceTarget =
    ReferenceTarget(newPath.circuit, newPath.module, newPath.asPath, ref, component)

}

case class InstanceTarget(circuit: String, module: String, override val path: Seq[(Instance, OfModule)], instance: String, ofModule: String) extends IsModule with IsComponent {
  def circuitOpt: Option[String] = Some(circuit)
  def moduleOpt: Option[String] = Some(module)
  def component: Seq[TargetToken] = Seq(Instance(instance), OfModule(ofModule))
  def targetParent: IsModule = {
    if(isLocal) ModuleTarget(circuit, module) else {
      val (newInstance, newOfModule) = path.last
      InstanceTarget(circuit, module, path.dropRight(1), newInstance.value, newOfModule.value)
    }
  }
  def addHierarchy(root: String, inst: String): InstanceTarget =
    InstanceTarget(circuit, root, (Instance(inst), OfModule(module)) +: path, instance, ofModule)
  def ref(value: String): ReferenceTarget = ReferenceTarget(circuit, module, asPath, value, Nil)
  def instOf(inst: String, of: String): InstanceTarget = InstanceTarget(circuit, module, asPath, inst, of)
  override def stripHierarchy(n: Int): IsModule = {
    require(path.size >= n, s"Cannot strip $n levels of hierarchy from $this")
    if(n == 0) this else {
      val newModule = path(n - 1)._2.value
      InstanceTarget(circuit, newModule, path.drop(n), instance, ofModule)
    }
  }
  def asReference: ReferenceTarget = ReferenceTarget(circuit, module, path, instance, Nil)

  def asPath: Seq[(Instance, OfModule)] = path :+ (Instance(instance), OfModule(ofModule))

  def pathlessTarget: InstanceTarget = InstanceTarget(circuit, encapsulatingModule.get, Nil, instance, ofModule)
  def notPath = Seq(Instance(instance), OfModule(ofModule))
  def setPathTarget(newPath: IsModule): InstanceTarget =
    InstanceTarget(newPath.circuit, newPath.module, newPath.asPath, instance, ofModule)
  def ofModuleTarget: ModuleTarget = ModuleTarget(circuit, ofModule)
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
  def toTarget: CircuitTarget = CircuitTarget(name)
}

@deprecated("Use Target instead, will be removed in 1.3", "1.2")
final case class ModuleName(name: String, circuit: CircuitName) extends Named {
  if(!validModuleName(name)) throw AnnotationException(s"Illegal module name: $name")
  def serialize: String = circuit.serialize + "." + name
  def toTarget: ModuleTarget = ModuleTarget(circuit.name, name)
}

@deprecated("Use Target instead, will be removed in 1.3", "1.2")
final case class ComponentName(name: String, module: ModuleName) extends Named {
  if(!validComponentName(name)) throw AnnotationException(s"Illegal component name: $name")
  def expr: Expression = toExp(name)
  def serialize: String = module.serialize + "." + name
  def toTarget: ReferenceTarget = Target.convertComponentName2IsLocalComponent(this)
}
