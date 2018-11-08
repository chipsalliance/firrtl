// See LICENSE for license details.

package firrtl.analyses

import firrtl.annotations.TargetToken._
import firrtl.annotations._
import firrtl.graph.{DiGraph, MutableDiGraph}
import firrtl.ir._
import firrtl.Mappers._
import firrtl.{BIGENDER, ExpKind, FEMALE, Gender, InstanceKind, Kind, MALE, PortKind, RegKind, UNKNOWNGENDER, Utils, WDefInstance, WRef, WSubField, WSubIndex, WireKind}

import scala.collection.mutable

trait UnnamedToken extends CustomToken {
  override def checkWhitelistedFollower(follower: TargetToken): Boolean = false

  override def checkWhitelistedLeader(leader: TargetToken): Boolean = false

  override def canBeAfter(optT: Option[TargetToken]): Boolean = optT match {
    case None => true
    case Some(_: OfModule) => true
    case Some(other: CustomToken) => other.checkWhitelistedFollower(this)
    case _ => false
  }

  val tag: Int// = UnnamedToken.counter.getAndIncrement()
}

class Tagger {
  private val counterMap = mutable.HashMap[String, Int]()
  def getTag(label: String): Int = {
    val tag = counterMap.getOrElse(label, 0)
    counterMap(label) = tag + 1
    tag
  }
}

case class PrintToken(tag: Int) extends UnnamedToken { override val value: String = tag.toString }

case class StopToken(tag: Int) extends UnnamedToken { override val value: String = tag.toString }

case class OpToken(op: String, tag: Int) extends UnnamedToken { override val value: String = op + "$" + tag }

case class LitToken(literal: BigInt, tag: Int) extends UnnamedToken { override val value: String = Int.toString }


//trait Label
//
//case class IRLabel(ir: FirrtlNode) extends Label

object CircuitGraph {

  def toTarget(m: ModuleTarget, f: FirrtlNode, tagger: Tagger): Target = f match {
    case s: Statement => s match {
      case d: IsDeclaration => m.ref(d.name)
      case _: Stop => m.toGenericTarget.add(StopToken(tagger.getTag("stop")))
      case _: Print => m.toGenericTarget.add(PrintToken(tagger.getTag("print")))
      case _ => sys.error(s"Unsupported: $s")
    }
    case e: Expression => e match {
      case l: Literal => m.toGenericTarget.add(LitToken(l.value, tagger.getTag(l.value.toString)))
      case w: WRef => m.ref(w.name)
      case r: Reference => m.ref(r.name)
      case w: WSubIndex => toTarget(m, w.expr, tagger).asInstanceOf[ReferenceTarget].index(w.value)
      case s: SubIndex => toTarget(m, s.expr, tagger).asInstanceOf[ReferenceTarget].index(s.value)
      case w: WSubField => toTarget(m, w.expr, tagger).asInstanceOf[ReferenceTarget].field(w.name)
      case s: SubField => toTarget(m, s.expr, tagger).asInstanceOf[ReferenceTarget].field(s.name)
      case d: DoPrim => m.toGenericTarget.add(OpToken(d.op.serialize, tagger.getTag(d.op.serialize)))
      case _: Mux => m.toGenericTarget.add(OpToken("mux", tagger.getTag("mux")))
      case _: ValidIf => m.toGenericTarget.add(OpToken("validif", tagger.getTag("validif")))
      case other => sys.error(s"Unsupported: $other")
    }
    case _: Type => sys.error("Unsupported!")
    case _: Width => sys.error("Unsupported!")
  }


  /**
    * @param m
    * @param module
    * @return Returns ((inputs, outputs))
    */
  def modulePortTargets(m: ModuleTarget,
                        module: DefModule
                       ): (Seq[(ReferenceTarget, Type)], Seq[(ReferenceTarget, Type)]) = {
    module.ports.flatMap {
      case Port(_, name, Output, tpe) => Utils.create_exps(WRef(name, tpe, PortKind, MALE))
      case Port(_, name, Input, tpe) => Utils.create_exps(WRef(name, tpe, PortKind, FEMALE))
    }.foldLeft((Vector.empty[(ReferenceTarget, Type)], Vector.empty[(ReferenceTarget, Type)])) {
      case ((inputs, outputs), e) if Utils.gender(e) == MALE =>
        (inputs, outputs :+ (CircuitGraph.toTarget(m, e, new Tagger()).asInstanceOf[ReferenceTarget], e.tpe))
      case ((inputs, outputs), e) =>
        (inputs :+ (CircuitGraph.toTarget(m, e, new Tagger()).asInstanceOf[ReferenceTarget], e.tpe), outputs)
    }
  }

  def buildCircuitGraph(circuit: Circuit): (DiGraph[Target], IRLookup) = {
    val mdg = new MutableDiGraph[Target]()
    val declarations = mutable.LinkedHashMap[Target, FirrtlNode]()
    val circuitTarget = CircuitTarget(circuit.main)
    val moduleTypes = circuit.modules.map { m => m.name -> firrtl.Utils.module_type(m) }.toMap
    val moduleMap = circuit.modules.map { m => m.name -> m }.toMap
    val top = circuitTarget.module(circuit.main)

    circuit map buildModule(circuitTarget)

    def emptySet[T]: mutable.LinkedHashSet[T] = mutable.LinkedHashSet.empty[T]

    def addLabeledVertex(v: Target, f: FirrtlNode): Unit = {
      mdg.addVertex(v)
      declarations(v) = f
    }

    def buildModule(c: CircuitTarget)(module: DefModule): DefModule = {
      val m = c.module(module.name)
      addLabeledVertex(m, module)
      module map buildPort(m, module) map buildStatement(m, new Tagger())
    }

    def buildPort(m: ModuleTarget, module: DefModule)(port: Port): Port = {
      val p = m.ref(port.name)
      addLabeledVertex(p, port)
      port
    }

    def buildInstance(m: ModuleTarget, tagger: Tagger, name: String, ofModule: String, tpe: Type): Unit = {

      val instTarget = m.instOf(name, ofModule)
      val instPorts = Utils.create_exps(WRef(name, tpe, InstanceKind, FEMALE))
      val modulePorts = tpe.asInstanceOf[BundleType].fields.flatMap {
        // Module output
        case firrtl.ir.Field(name, Default, tpe) => Utils.create_exps(WRef(name, tpe, PortKind, MALE))
        // Module input
        case firrtl.ir.Field(name, Flip, tpe) => Utils.create_exps(WRef(name, tpe, PortKind, FEMALE))
      }
      assert(instPorts.size == modulePorts.size)
      val o = m.circuitTarget.module(ofModule)
      instPorts.zip(modulePorts).foreach { x =>
        val (instExp, modExp) = x
        val it = toTarget(m, instExp, tagger)
        val mt = toTarget(o, modExp, tagger)
        (Utils.gender(instExp), Utils.gender(modExp)) match {
          case (MALE, FEMALE) => mdg.addPairWithEdge(it, mt)
          case (FEMALE, MALE) => mdg.addPairWithEdge(mt, it)
          case _ => sys.error("Something went wrong...")
        }
      }
    }

    def buildRegister(m: ModuleTarget, tagger: Tagger, d: DefRegister): Unit = {
      val regTarget = m.ref(d.name)
      val clockTarget = regTarget.clock
      val resetTarget = regTarget.reset
      val initTarget = regTarget.init
      val regKidTargets = Seq(clockTarget, resetTarget, initTarget)
      val regKids = Seq(d.clock, d.reset, d.init)

      regKids.zip(regKidTargets).foreach { case (kid, target) =>
        addLabeledVertex(target, d)
        mdg.addEdge(target, regTarget)
        buildExpression(m, tagger, target)(kid)
      }
    }

    def buildStatement(m: ModuleTarget, tagger: Tagger)(stmt: Statement): Statement = {
      stmt match {
        case d: DefWire =>
          addLabeledVertex(m.ref(d.name), stmt)

        case d: DefNode =>
          val sinkTarget = m.ref(d.name)
          addLabeledVertex(sinkTarget, stmt)
          buildExpression(m, tagger, sinkTarget)(d.value)

        case c: Connect =>
          val sinkTarget = toTarget(m, c.loc, tagger)
          buildExpression(m, tagger, sinkTarget)(c.expr)

        case WDefInstance(_, name, ofModule, tpe) =>
          addLabeledVertex(m.ref(name), stmt)
          buildInstance(m, tagger, name, ofModule, tpe)

        case DefInstance(_, name, ofModule) =>
          addLabeledVertex(m.ref(name), stmt)
          buildInstance(m, tagger, name, ofModule, moduleTypes(ofModule))

        case d: DefRegister =>
          addLabeledVertex(m.ref(d.name), d)
          buildRegister(m, tagger, d)

        //TODO(azidar): Support memories
        case d: DefMemory => sys.error("To be supported soon!!")

        case s: Conditionally => sys.error("Unsupported! Only works on Middle Firrtl")

        case s: Block => s map buildStatement(m, tagger)

        case a: Attach =>
          val attachTargets = a.exprs.map{ r =>
            val at = toTarget(m, r, tagger)
            addLabeledVertex(at, r)
            at
          }
          attachTargets.combinations(2).foreach { case Seq(l, r) =>
            mdg.addEdge(l, r)
            mdg.addEdge(r, l)
          }
      }
      stmt
    }

    def buildExpression(m: ModuleTarget, tagger: Tagger, sinkTarget: Target)(expr: Expression): Expression = {
      require(expr.tpe.isInstanceOf[GroundType], "Expression must be a Ground Type. Must be on Middle FIRRTL.")
      val sourceTarget = toTarget(m, expr, tagger)
      mdg.addVertex(sourceTarget)
      mdg.addEdge(sourceTarget, sinkTarget)
      expr match {
        case _: DoPrim | _: Mux | _: ValidIf | _: Literal =>
          addLabeledVertex(sourceTarget, expr)
          expr map buildExpression(m, tagger, sourceTarget)
        case other =>
      }
      expr
    }

    (DiGraph(mdg), new IRLookup(declarations))
  }
}

/** Handy lookup
  *
  * @param declarations Maps references (not subreferences) to declarations
  */
class IRLookup(private val declarations: collection.Map[Target, FirrtlNode]) {
  private val genderCache = mutable.HashMap[Target, Gender]()
  private val kindCache = mutable.HashMap[Target, Kind]()
  private val tpeCache = mutable.HashMap[Target, Type]()
  private val exprCache = mutable.HashMap[(Target, Gender), Expression]()

  def getASTRef(t: Target): Target = getReferenceTarget(getPathlessTarget(t))

  def gender(t: Target): Gender = {
    val pathless = getASTRef(t)
    require(t.moduleOpt.nonEmpty)
    if(genderCache.contains(pathless)) return genderCache(pathless)

    val gender = Utils.gender(expr(pathless))
    genderCache(pathless) = gender
    gender
  }

  def kind(t: Target): Kind = {
    val pathless = getASTRef(t)
    require(t.moduleOpt.nonEmpty)
    if(kindCache.contains(pathless)) return kindCache(pathless)

    val kind = Utils.kind(expr(pathless))
    kindCache(pathless) = kind
    kind
  }

  def tpe(t: Target): Type = {
    val pathless = getASTRef(t)
    require(t.moduleOpt.nonEmpty)
    if(tpeCache.contains(pathless)) return tpeCache(pathless)

    val tpe = expr(pathless).tpe
    tpeCache(pathless) = tpe
    tpe
  }

  private def updateExpr(mt: ModuleTarget, ref: Expression): Unit = {
    Utils.expandRef(ref).foreach { e =>
      val target = CircuitGraph.toTarget(mt, e, new Tagger())
      exprCache((target, Utils.gender(e))) = e
    }
  }

  private def inCache(pathless: Target, gender: Gender): Option[Expression] = {
    (gender,
      exprCache.contains((pathless, MALE)),
      exprCache.contains((pathless, FEMALE)),
      exprCache.contains(pathless, BIGENDER)
    ) match {
      case (MALE, true, _, _) => Some(exprCache((pathless, gender)))
      case (FEMALE, _, true, _) => Some(exprCache((pathless, gender)))
      // For now, default to MALE if ambiguous
      case (BIGENDER, _, _, true) => Some(exprCache((pathless, BIGENDER)))
      case (UNKNOWNGENDER, _, _, true) => Some(exprCache((pathless, BIGENDER)))
      case (UNKNOWNGENDER, true, false, false) => Some(exprCache((pathless, MALE)))
      case (UNKNOWNGENDER, false, true, false) => Some(exprCache((pathless, FEMALE)))
      case other => None
    }
  }

  def expr(t: Target, gender: Gender = UNKNOWNGENDER): Expression = {
    val pathless = getASTRef(t)
    require(t.moduleOpt.nonEmpty)

    inCache(pathless, gender) match {
      case Some(e) => e
      case None =>
        val mt = pathless.circuitTarget.module(pathless.moduleOpt.get)
        declarations(pathless) match {
          case e: Expression =>
            require(e.tpe.isInstanceOf[GroundType])
            exprCache.getOrElseUpdate((pathless, Utils.gender(e)), e)
          case d: IsDeclaration => d match {
            case n: DefNode =>
              updateExpr(mt, WRef(n.name, n.value.tpe, ExpKind, MALE))
            case p: Port =>
              updateExpr(mt, WRef(p.name, p.tpe, PortKind, Utils.get_gender(p)))
            case w: WDefInstance =>
              updateExpr(mt, WRef(w.name, w.tpe, InstanceKind, MALE))
            case w: DefWire =>
              updateExpr(mt, WRef(w.name, w.tpe, WireKind, MALE))
              updateExpr(mt, WRef(w.name, w.tpe, WireKind, FEMALE))
              updateExpr(mt, WRef(w.name, w.tpe, WireKind, BIGENDER))
            case r: DefRegister if pathless.tokens.last == Clock =>
              exprCache((pathless, MALE)) = r.clock
            case r: DefRegister if pathless.tokens.last == Init =>
              //TODO(azidar): expand init field in case register is aggregate type?
              exprCache((pathless, MALE)) = r.init
            case r: DefRegister if pathless.tokens.last == Reset =>
              exprCache((pathless, MALE)) = r.reset
            case r: DefRegister =>
              updateExpr(mt, WRef(r.name, r.tpe, RegKind, MALE))
              updateExpr(mt, WRef(r.name, r.tpe, RegKind, FEMALE))
              updateExpr(mt, WRef(r.name, r.tpe, RegKind, BIGENDER))
            case m: DefMemory => sys.error("Will support memories soon!")
            case other => sys.error(s"Cannot call expr with: $other")
          }
        }
    }

    inCache(pathless, gender) match{
      case Some(e) => e
      case None => sys.error(s"Illegal gender $gender with target $t")
    }
  }




      //TODO error if UNKNOWN but exprCache is missing (compiler bug)
      //TODO error if BIGENDER but here (user bug)
  private def setBiGender(e: Expression): Expression = e match {
    case w: WRef => w.copy(gender = BIGENDER)
    case w: WSubField => w.copy(gender = BIGENDER).map(setBiGender)
    case w: WSubIndex => w.copy(gender = BIGENDER).map(setBiGender)
    case other => sys.error(s"Unexpected expression: $other")
  }

  def declaration(t: Target): FirrtlNode = declarations(getASTRef(t))

  def getPathlessTarget(t: Target): Target = {
    t.tryToComplete match {
      case c: CircuitTarget => c
      case m: IsMember => m.pathlessTarget
      case t: GenericTarget if t.isLegal =>
        val newTokens = t.tokens.dropWhile(x => x.isInstanceOf[Instance] || x.isInstanceOf[OfModule])
        GenericTarget(t.circuitOpt, t.moduleOpt, newTokens)
      case other => sys.error(s"Can't make $other pathless!")
    }
  }

  def getReferenceTarget(t: Target): Target = {
    (t.toGenericTarget match {
      case t: GenericTarget if t.isLegal =>
        val newTokens = t.tokens.reverse.dropWhile(x => !x.isInstanceOf[Ref]).reverse
        GenericTarget(t.circuitOpt, t.moduleOpt, newTokens)
      case other => sys.error(s"Can't make $other pathless!")
    }).tryToComplete

  }
}

