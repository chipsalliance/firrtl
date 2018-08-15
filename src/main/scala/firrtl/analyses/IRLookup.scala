package firrtl.analyses

import firrtl.{CircuitState, RenameMap, WDefInstance, WInvalid}
import firrtl.ir._
import firrtl.Mappers._
import firrtl.annotations.{Annotation, Component}

import scala.collection.mutable

/**
  * May be too big/heavy weight to build all these hash maps, maybe split into separate Annotations?
  * @param c
  */
class IRLookup private (c: Circuit) extends Annotation {
  override def update(renames: RenameMap): Seq[Annotation] = if(renames.hasChanges) Nil else Seq(this)

  private val instanceMap = mutable.Map[String, mutable.Map[String, WDefInstance]]()
  private val registerMap = mutable.Map[String, mutable.Map[String, DefRegister]]()
  private val memoriesMap = mutable.Map[String, mutable.Map[String, DefMemory]]()
  private val declaresMap = mutable.Map[String, mutable.Map[String, IsDeclaration]]()
  private val connectsMap = mutable.Map[String, mutable.Map[String, Expression]]()
  private val multiConSet = mutable.Map[String, mutable.Set[String]]()
  private val attachesMap = mutable.Map[String, mutable.Map[String, mutable.ArrayBuffer[Expression]]]()

  val modules: collection.Map[String, DefModule] = c.modules.map(m => m.name -> m).toMap
  val instances: collection.Map[String, collection.Map[String, WDefInstance]] = instanceMap
  val registers: collection.Map[String, collection.Map[String, DefRegister]] = registerMap
  val memories:  collection.Map[String, collection.Map[String, DefMemory]] = memoriesMap
  val declarations: collection.Map[String, collection.Map[String, IsDeclaration]] = declaresMap
  val connections: collection.Map[String, collection.Map[String, Expression]] = connectsMap
  val attaches: collection.Map[String, collection.Map[String, collection.Seq[Expression]]] = attachesMap
  val circuitName: String = c.main
  val circuitHash: Int = c.hashCode()
  val circuit: Circuit = c

  import firrtl.annotations.SubComponent._

  def resolveInstances(comp: Component): Iterable[Component] = {
    require(comp.isLegal)
    if(comp.module.isEmpty)
      modules.keys.flatMap(m => resolveInstances(comp.copy(module = Some(m))))
    else {
      comp.reference.tails.foldLeft(Seq(comp.copy(reference=Nil))) {
        case (partials, Instance(i) :: OfModule(m) :: tail) =>
          partials.flatMap { c =>
            val moduleName = c.deepestModule.get
            val instDecOpt = instanceMap(moduleName).get(i)
            if(instDecOpt.nonEmpty && instDecOpt.get.module == m) {
              Seq(c.inst(i).of(m))
            } else Nil
          }
        case (partials, OfModule(m) :: tail) => partials
        case (partials, next :: tail) => partials.map { c => c.copy(reference=c.reference :+ next) }
        case (partials, Nil) => partials
      }
    }
  }

  def getDeclaration(comp: Component): IsDeclaration = {
    require(comp.circuit.isDefined && comp.circuit.get == circuitName, s"Must query on matching circuit names!")
    require(comp.module.isDefined && modules.contains(comp.module.get), s"Circuit must contain the module: ${comp.module}")
    require(comp.isComplete, s"Component $comp must be complete before a query")


    val (finalModule, declaration) = comp.reference.tails.foldLeft((comp.module.get, None: Option[IsDeclaration])) {
      case ((moduleName, _), Instance(i) :: OfModule(m) :: tail) =>
        val instDecOpt = instanceMap(moduleName).get(i)
        require(instDecOpt.nonEmpty, s"$moduleName does not contain instance $i")
        val instMod = instDecOpt.get.module
        require(instMod == m, s"Instance $i in $moduleName instantiates $instMod, not $m")
        (m, instDecOpt)
      case ((moduleName, ret), OfModule(m) :: tail) =>
        assert(moduleName == m)
        (moduleName, ret)
      case ((moduleName, ret), Ref(i) :: tail) => (moduleName, declarations(moduleName).get(i))
      case ((moduleName, ret), _) => (moduleName, ret)
    }

    require(declaration.nonEmpty, s"$finalModule does not contain the declaration referenced by: $comp")
    declaration.get
  }

  private def populate(): Unit = {
    def onModule(m: DefModule): DefModule = {
      instanceMap(m.name) = mutable.Map.empty[String, WDefInstance]
      registerMap(m.name) = mutable.Map.empty[String, DefRegister]
      memoriesMap(m.name) = mutable.Map.empty[String, DefMemory]
      declaresMap(m.name) = mutable.Map.empty[String, IsDeclaration]
      connectsMap(m.name) = mutable.Map.empty[String, Expression]
      multiConSet(m.name) = mutable.Set.empty[String]
      attachesMap(m.name) = mutable.Map.empty[String, mutable.ArrayBuffer[Expression]]
      m map onStmt(m.name)
    }
    def connect(mname: String)(key: String, value: Expression): Unit = {
      if(multiConSet(mname).contains(key)) {
        // Do nothing
      } else if (connectsMap(mname).contains(key)) {
        connectsMap(mname).remove(key)
        multiConSet(mname) += key
      } else {
        connectsMap(mname)(key) = value
      }
    }
    def onStmt(mname: String)(s: Statement): Statement = {
      s match {
        case c: Connect        => connect(mname)(c.loc.serialize, c.expr)
        case p: PartialConnect => connect(mname)(p.loc.serialize, p.expr)
        case i: IsInvalid      => connect(mname)(i.expr.serialize, WInvalid)
        case a: Attach =>
          a.exprs.foreach { e =>
            val key = e.serialize
            val value = attachesMap(mname).getOrElseUpdate(key, mutable.ArrayBuffer.empty[Expression])
            value ++= a.exprs
          }
        case d: DefNode        =>
          connect(mname)(d.name, d.value)
          declaresMap(mname) += (d.name -> d)
        case d: DefRegister =>
          registerMap(mname) += (d.name -> d)
          declaresMap(mname) += (d.name -> d)
        case m: DefMemory =>
          memoriesMap(mname) += (m.name -> m)
          declaresMap(mname) += (m.name -> m)
        case i: DefInstance => firrtl.Utils.throwInternalError("Must be on WIR")
        case i: WDefInstance =>
          instanceMap(mname) += (i.name -> i)
          declaresMap(mname) += (i.name -> i)
        case dec: IsDeclaration =>
          declaresMap(mname) += (dec.name -> dec)
        case other =>
      }
      s map onStmt(mname)
    }
    c map onModule
  }

  populate()
}

object IRLookup {
  def apply(c: Circuit): IRLookup = new IRLookup(c)
  def apply(state: CircuitState): IRLookup = {
    state.annotations.collectFirst{ case x: IRLookup if x.circuitHash == state.circuit.hashCode => x } match {
      case Some(irL) => irL
      case None => IRLookup(state.circuit)
    }
  }
}
