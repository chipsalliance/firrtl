// See LICENSE for license details.

package firrtl.analyses

import firrtl.annotations._
import firrtl.graph.{DiGraph, DiGraphLike, MutableDiGraph}
import firrtl.ir._
import firrtl.Mappers._
import firrtl.annotations.TargetToken
import firrtl.passes.MemPortUtils
import firrtl.{FEMALE, InstanceKind, MALE, PortKind, Utils, WDefInstance, WRef, WSubField, WSubIndex}

import scala.collection.mutable

class CircuitGraph protected (val circuit: Circuit, val digraph: DiGraph[Target], val irLookup: IRLookup) extends DiGraphLike[Target] {
  override val edges = digraph.getEdgeMap.asInstanceOf[mutable.LinkedHashMap[Target, mutable.LinkedHashSet[Target]]]

  override def getEdges(v: Target, prevOpt: Option[collection.Map[Target, Target]] = None): collection.Set[Target] = {
    val genT = v.toGenericTarget
    val circuitOpt = genT.circuitOpt
    val pathTokens = genT.pathTokens
    val (parentModule, astModule) = pathTokens match {
      case Seq() => (None, genT.moduleOpt.get)
      case Seq(i, TargetToken.OfModule(o)) => (genT.moduleOpt, o)
      case seq if seq.size > 2 && seq.size % 2 == 0 =>
        val reversed = seq.reverse
        (Some(reversed(2).value), reversed.head.value)
    }
    val pathlessEdges = super.getEdges(Target.getPathlessTarget(v))
    pathlessEdges.flatMap { t =>
      val genE = t.toGenericTarget
      genE match {
        // In same instance
        case GenericTarget(`circuitOpt`, Some(`astModule`), tokens) =>
          Seq(GenericTarget(circuitOpt, genT.moduleOpt, pathTokens ++ tokens).tryToComplete)

        // In parent instance
        case GenericTarget(`circuitOpt`, `parentModule`, tokens) =>
          Seq(GenericTarget(circuitOpt, genT.moduleOpt, pathTokens.dropRight(2) ++ tokens).tryToComplete)

        case GenericTarget(`circuitOpt`, Some(otherModule), tokens) =>
          (Target.getPathlessTarget(genT).tokens, tokens) match {
            // In parent but instantiates root module
            case (TargetToken.Ref(modPort) +: modRest, TargetToken.Ref(inst) +: TargetToken.Field(instPort) +: instRest) if modPort == instPort && modRest == instRest =>  Nil

            // In child instance
            case (TargetToken.Ref(inst) +: TargetToken.Field(instPort) +: instRest, TargetToken.Ref(modPort) +: modRest) if modPort == instPort && modRest == instRest =>
              val inst = v.complete.asInstanceOf[ReferenceTarget]
              val newPath = pathTokens ++ Seq(TargetToken.Instance(inst.ref), TargetToken.OfModule(otherModule))
              Seq(GenericTarget(circuitOpt, genT.moduleOpt, newPath ++ tokens).tryToComplete)
          }
      }
    }
  }
}

object CircuitGraph {

  /** Returns a [[DiGraph]] of [[Target]] and corresponding [[IRLookup]]
    *
    * Represents the directed connectivity of a FIRRTL circuit
    * @param circuit
    * @return
    */
  def apply(circuit: Circuit): CircuitGraph = buildCircuitGraph(circuit)

  /** Within a module, given an [[Expression]] inside a module, return a corresponding [[Target]]
    * @param m Target of module containing the expression
    * @param tagger Used to uniquely identify unnamed targets, e.g. primops
    * @param e
    * @return
    */
  def asTarget(m: ModuleTarget, tagger: TokenTagger)(e: Expression): Target = e match {
    case l: Literal => m.toGenericTarget.add(LitToken(l.value, tagger.getTag(l.value.toString)))
    case w: WRef => m.ref(w.name)
    case r: Reference => m.ref(r.name)
    case w: WSubIndex => asTarget(m, tagger)(w.expr).asInstanceOf[ReferenceTarget].index(w.value)
    case s: SubIndex => asTarget(m, tagger)(s.expr).asInstanceOf[ReferenceTarget].index(s.value)
    case w: WSubField => asTarget(m, tagger)(w.expr).asInstanceOf[ReferenceTarget].field(w.name)
    case s: SubField => asTarget(m, tagger)(s.expr).asInstanceOf[ReferenceTarget].field(s.name)
    case d: DoPrim => m.toGenericTarget.add(OpToken(d.op.serialize, tagger.getTag(d.op.serialize)))
    case _: Mux => m.toGenericTarget.add(OpToken("mux", tagger.getTag("mux")))
    case _: ValidIf => m.toGenericTarget.add(OpToken("validif", tagger.getTag("validif")))
    case other => sys.error(s"Unsupported: $other")
  }


  private def buildCircuitGraph(circuit: Circuit): CircuitGraph = {
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
      module map buildPort(m, module) map buildStatement(m, new TokenTagger())
    }

    def buildPort(m: ModuleTarget, module: DefModule)(port: Port): Port = {
      val p = m.ref(port.name)
      addLabeledVertex(p, port)
      port
    }

    def buildInstance(m: ModuleTarget, tagger: TokenTagger, name: String, ofModule: String, tpe: Type): Unit = {

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
        val it = asTarget(m, tagger)(instExp)
        val mt = asTarget(o, tagger)(modExp)
        (Utils.gender(instExp), Utils.gender(modExp)) match {
          case (MALE, FEMALE) => mdg.addPairWithEdge(it, mt)
          case (FEMALE, MALE) => mdg.addPairWithEdge(mt, it)
          case _ => sys.error("Something went wrong...")
        }
      }
    }

    def buildMemory(mt: ModuleTarget, d: DefMemory): Unit = {
      val readers = d.readers.toSet
      val writers = d.writers.toSet
      val readwriters = d.readwriters.toSet
      val mem = mt.ref(d.name)
      MemPortUtils.memType(d).fields.foreach {
        case Field(name, flip, tpe: BundleType) if readers.contains(name) || readwriters.contains(name) =>
          val port = mem.field(name)
          val sources = Seq(
            port.field("clk"),
            port.field("en"),
            port.field("addr")
          ) ++ (if(readwriters.contains(name)) Seq(port.field("wmode")) else Nil)

          val data = if(readers.contains(name)) port.field("data") else port.field("rdata")
          val sinks = IRLookup.leafTargets(data, d.dataType)

          sources.foreach { mdg.addVertex }
          sinks.foreach { sink =>
            mdg.addVertex(sink)
            sources.foreach { source => mdg.addEdge(source, sink) }
          }
        case other =>
      }
    }

    def buildRegister(m: ModuleTarget, tagger: TokenTagger, d: DefRegister): Unit = {
      val regTarget = m.ref(d.name)
      val clockTarget = regTarget.clock
      val resetTarget = regTarget.reset
      val initTarget = regTarget.init
      val regKidTargets = Seq(clockTarget, resetTarget, initTarget)
      val regKids = Seq(d.clock, d.reset, d.init)

      // Build clock expression
      mdg.addVertex(clockTarget)
      buildExpression(m, tagger, clockTarget)(d.clock)

      // Build reset expression
      mdg.addVertex(resetTarget)
      buildExpression(m, tagger, resetTarget)(d.reset)

      // Connect each subTarget to the corresponding init subTarget
      val allRegTargets = IRLookup.leafTargets(regTarget, d.tpe)
      val allInitTargets = IRLookup.leafTargets(initTarget, d.tpe).zip(Utils.create_exps(d.init))
      allRegTargets.zip(allInitTargets).foreach { case (r, (i, e)) =>
        mdg.addVertex(i)
        mdg.addVertex(r)
        mdg.addEdge(clockTarget, r)
        mdg.addEdge(resetTarget, r)
        mdg.addEdge(i, r)
        buildExpression(m, tagger, i)(e)
      }
    }

    def buildStatement(m: ModuleTarget, tagger: TokenTagger)(stmt: Statement): Statement = {
      stmt match {
        case d: DefWire =>
          addLabeledVertex(m.ref(d.name), stmt)

        case d: DefNode =>
          val sinkTarget = m.ref(d.name)
          addLabeledVertex(sinkTarget, stmt)
          buildExpression(m, tagger, sinkTarget)(d.value)

        case c: Connect =>
          val sinkTarget = asTarget(m, tagger)(c.loc)
          mdg.addVertex(sinkTarget)
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

        case d: DefMemory =>
          addLabeledVertex(m.ref(d.name), d)
          buildMemory(m, d)

        case s: Conditionally => sys.error("Unsupported! Only works on Middle Firrtl")

        case s: Block => s map buildStatement(m, tagger)

        case a: Attach =>
          val attachTargets = a.exprs.map{ r =>
            val at = asTarget(m, tagger)(r)
            addLabeledVertex(at, r)
            at
          }
          attachTargets.combinations(2).foreach { case Seq(l, r) =>
            mdg.addEdge(l, r)
            mdg.addEdge(r, l)
          }
        case EmptyStmt =>
      }
      stmt
    }

    def buildExpression(m: ModuleTarget, tagger: TokenTagger, sinkTarget: Target)(expr: Expression): Expression = {
      require(expr.tpe.isInstanceOf[GroundType], "Expression must be a Ground Type. Must be on Middle FIRRTL.")
      val sourceTarget = asTarget(m, tagger)(expr)
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

    new CircuitGraph(circuit, DiGraph(mdg), new IRLookup(declarations))
  }
}

