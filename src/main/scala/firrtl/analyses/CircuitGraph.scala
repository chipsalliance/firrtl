// See LICENSE for license details.

package firrtl.analyses

import firrtl.annotations._
import firrtl.graph.{DiGraph, DiGraphLike, MutableDiGraph}
import firrtl.ir._
import firrtl.Mappers._
import firrtl.annotations.TargetToken
import firrtl.annotations.TargetToken.{Instance, OfModule}
import firrtl.passes.MemPortUtils
import firrtl.{FEMALE, InstanceKind, MALE, PortKind, Utils, WDefInstance, WInvalid, WRef, WSubField, WSubIndex}

import scala.collection.mutable

class CircuitGraph protected (val circuit: Circuit, val digraph: DiGraph[ReferenceTarget], val irLookup: IRLookup) extends DiGraphLike[ReferenceTarget] {
  override val edges = digraph.getEdgeMap.asInstanceOf[mutable.LinkedHashMap[ReferenceTarget, mutable.LinkedHashSet[ReferenceTarget]]]

  val portConnectivityStack: mutable.HashMap[ReferenceTarget, List[ReferenceTarget]] =
    mutable.HashMap.empty[ReferenceTarget, List[ReferenceTarget]]

  val portShortCuts: mutable.HashMap[ReferenceTarget, mutable.HashSet[ReferenceTarget]] =
    mutable.HashMap.empty[ReferenceTarget, mutable.HashSet[ReferenceTarget]]

  def getShortCutEdges(source: ReferenceTarget, edges: collection.Set[ReferenceTarget]): collection.Set[ReferenceTarget] = {
    edges.flatMap { e =>
      portShortCuts.get(e.pathlessTarget) match {
        case None => Seq(e)
        case Some(set) => set.map{x => x.setPathTarget(e.pathTarget)}
      }
    }
  }

  override def getEdges(source: ReferenceTarget, prevOpt: Option[collection.Map[ReferenceTarget, ReferenceTarget]] = None): collection.Set[ReferenceTarget] = {
    val localSource = source.pathlessTarget
    val pathlessEdges = super.getEdges(localSource)
    val ret = pathlessEdges.flatMap {
      case e@ReferenceTarget(c, m, Seq(), r, component) =>
        source match {
          // If e is in the same instance, then they must share the same encapsulating module
          case x if x.encapsulatingModule == m =>
            val localE = e.pathlessTarget
            portConnectivityStack(localE) = portConnectivityStack.getOrElse(localSource, Nil)
            Set[ReferenceTarget](e.setPathTarget(x.pathTarget))
          // If e is in a parent instance, then source's parent of its encapsulating module must be e's module
          case it@ReferenceTarget(`c`, _, path, _, _)
            if it.path.nonEmpty &&
              it.noComponents.targetParent.asInstanceOf[InstanceTarget].encapsulatingModule == m &&
              r == it.path.last._1.value =>
            val currentStack = portConnectivityStack.getOrElse(localSource, Nil)
            val localE = e.pathlessTarget
            if(currentStack.nonEmpty && currentStack.head.module == localE.module) {
              // Exiting back to parent module
              // Update shortcut path from entrance from parent to new exit to parent
              val instancePort = currentStack.head
              val modulePort = ReferenceTarget(
                localSource.circuit,
                localSource.module,
                Nil,
                instancePort.component.head.value.toString,
                instancePort.component.tail
              )
              val destinations = portShortCuts.getOrElse(modulePort, mutable.HashSet.empty[ReferenceTarget])
              portShortCuts(modulePort) = destinations + localSource
              // Remove entrace from parent from stack
              portConnectivityStack(localE) = currentStack.tail
            } else {
              // Exiting to parent, but had unresolved trip through child, so don't update shortcut
              // TODO think about this more....
              portConnectivityStack(localE) = localSource +: currentStack
            }
            Set[ReferenceTarget](e.setPathTarget(it.targetParent.asInstanceOf[IsComponent].pathTarget))
          case _ =>
            source match {
              // If e is in a child instance, then source must be referencing an instance port
              case ReferenceTarget(_, top, path, instance, TargetToken.Field(port) +: comps) if port == r && comps == component =>
                val localE = e.pathlessTarget
                portConnectivityStack(localE) = localSource +: portConnectivityStack.getOrElse(localSource, Nil)
                val x = e.setPathTarget(source.pathTarget.instOf(instance, m))
                Set[ReferenceTarget](x)
              // If e is in a parent instance who's instantiating source's root module, then e must be referencing an instance port
              case ReferenceTarget(_, top, Seq(), port, comps) if port == component.head.value && comps == component.tail =>
                Set[ReferenceTarget]()
              case other =>
                Set[ReferenceTarget]()
            }
        }
    }
    ret
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

  def isAsClock(t: ReferenceTarget): Boolean = t.ref.length >= 9 && t.ref.substring(0, 9) == "@asClock#"

  /** Within a module, given an [[Expression]] inside a module, return a corresponding [[Target]]
    * @param m Target of module containing the expression
    * @param tagger Used to uniquely identify unnamed targets, e.g. primops
    * @param e
    * @return
    */
  def asTarget(m: ModuleTarget, tagger: TokenTagger)(e: Expression): ReferenceTarget = e match {
    case l: Literal => m.ref("@" + l.value + "#" + tagger.getTag(l.value.toString))
    case w: WRef => m.ref(w.name)
    case r: Reference => m.ref(r.name)
    case w: WSubIndex => asTarget(m, tagger)(w.expr).index(w.value)
    case s: SubIndex => asTarget(m, tagger)(s.expr).index(s.value)
    case w: WSubField => asTarget(m, tagger)(w.expr).field(w.name)
    case s: SubField => asTarget(m, tagger)(s.expr).field(s.name)
    case d: DoPrim => m.ref("@" + d.op.serialize + "#" + tagger.getTag(d.op.serialize))
    case _: Mux => m.ref("@mux" + "#" + tagger.getTag("mux"))
    case _: ValidIf => m.ref("@validif" + "#" + tagger.getTag("validif"))
    case WInvalid => m.ref("@invalid#" + tagger.getTag("invalid"))
    case other => sys.error(s"Unsupported: $other")
  }


  private def buildCircuitGraph(circuit: Circuit): CircuitGraph = {
    val mdg = new MutableDiGraph[ReferenceTarget]()
    val declarations = mutable.LinkedHashMap[ReferenceTarget, FirrtlNode]()
    val circuitTarget = CircuitTarget(circuit.main)
    val moduleTypes = circuit.modules.map { m => m.name -> firrtl.Utils.module_type(m) }.toMap
    val moduleMap = circuit.modules.map { m => circuitTarget.module(m.name) -> m }.toMap
    val top = circuitTarget.module(circuit.main)

    circuit map buildModule(circuitTarget)

    def emptySet[T]: mutable.LinkedHashSet[T] = mutable.LinkedHashSet.empty[T]

    def addLabeledVertex(v: ReferenceTarget, f: FirrtlNode): Unit = {
      mdg.addVertex(v)
      declarations(v) = f
    }

    def buildModule(c: CircuitTarget)(module: DefModule): DefModule = {
      val m = c.module(module.name)
      //addLabeledVertex(m, module)
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
          val sinks = data.leafSubTargets(d.dataType)

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
      val allRegTargets = regTarget.leafSubTargets(d.tpe)
      val allInitTargets = initTarget.leafSubTargets(d.tpe).zip(Utils.create_exps(d.init))
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
          val nodeTargets = sinkTarget.leafSubTargets(d.value.tpe)
          nodeTargets.zip(Utils.create_exps(d.value)).foreach { case (n, e) =>
            mdg.addVertex(n)
            buildExpression(m, tagger, n)(e)
          }

        case c: Connect =>
          val sinkTarget = asTarget(m, tagger)(c.loc)
          mdg.addVertex(sinkTarget)
          buildExpression(m, tagger, sinkTarget)(c.expr)

        case i: IsInvalid =>
          val sourceTarget = asTarget(m, tagger)(WInvalid)
          mdg.addVertex(sourceTarget)
          val sinkTarget = asTarget(m, tagger)(i.expr)
          sinkTarget.allSubTargets(i.expr.tpe).foreach { st =>
            mdg.addVertex(st)
            mdg.addEdge(sourceTarget, st)
          }

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
            //addLabeledVertex(at, r)
            mdg.addVertex(at)
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

    def buildExpression(m: ModuleTarget, tagger: TokenTagger, sinkTarget: ReferenceTarget)(expr: Expression): Expression = {
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

    new CircuitGraph(circuit, DiGraph(mdg), new IRLookup(declarations, moduleMap))
  }
}

