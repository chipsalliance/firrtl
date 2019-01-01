// See LICENSE for license details.

package firrtl.analyses

import firrtl.annotations._
import firrtl.graph.{DiGraph, DiGraphLike, MutableDiGraph, PathNotFoundException}
import firrtl.ir._
import firrtl.Mappers._
import firrtl.annotations.TargetToken
import firrtl.passes.MemPortUtils
import firrtl.{FEMALE, InstanceKind, MALE, PortKind, Utils, WDefInstance, WInvalid, WRef, WSubField, WSubIndex}

import scala.collection.mutable

class ConnectionGraph protected(val circuit: Circuit, val digraph: DiGraph[ReferenceTarget], val irLookup: IRLookup) extends DiGraphLike[ReferenceTarget] {
  override val edges =
    digraph.getEdgeMap.asInstanceOf[mutable.LinkedHashMap[ReferenceTarget, mutable.LinkedHashSet[ReferenceTarget]]]

  private val portConnectivityStack: mutable.HashMap[ReferenceTarget, List[ReferenceTarget]] =
    mutable.HashMap.empty[ReferenceTarget, List[ReferenceTarget]]

  private val foundShortCuts: mutable.HashMap[ReferenceTarget, mutable.HashSet[ReferenceTarget]] =
    mutable.HashMap.empty[ReferenceTarget, mutable.HashSet[ReferenceTarget]]

  private val bfsShortCuts: mutable.HashMap[ReferenceTarget, mutable.HashSet[ReferenceTarget]] =
    mutable.HashMap.empty[ReferenceTarget, mutable.HashSet[ReferenceTarget]]

  def hasShortCut(target: ReferenceTarget): Boolean = getShortCut(target).nonEmpty

  def getShortCut(target: ReferenceTarget): Option[Set[ReferenceTarget]] =
    foundShortCuts.get(target.pathlessTarget).map(set => set.map(_.setPathTarget(target.pathTarget)).toSet)

  def shortCut(target: ReferenceTarget): Set[ReferenceTarget] = getShortCut(target).get

  def reverseConnectionGraph: ConnectionGraph = new ConnectionGraph(circuit, digraph.reverse, irLookup)

  protected def tagPath(destination: ReferenceTarget,
                      prev: collection.Map[ReferenceTarget, ReferenceTarget],
                      tag: ReferenceTarget,
                      tagMap: mutable.LinkedHashMap[ReferenceTarget, mutable.HashSet[ReferenceTarget]]): Unit = {
    tagPath(destination, prev, Set(tag), tagMap)
  }

  protected def tagPath(destination: ReferenceTarget,
                      prev: collection.Map[ReferenceTarget, ReferenceTarget],
                      tags: collection.Set[ReferenceTarget],
                      tagMap: mutable.LinkedHashMap[ReferenceTarget, mutable.HashSet[ReferenceTarget]]): Unit = {
    val perModuleTags = mutable.HashMap[String, mutable.HashSet[ReferenceTarget]]()

    def updatePerModuleTags(tag: ReferenceTarget): Unit = {
      perModuleTags.getOrElseUpdate(tag.module, mutable.HashSet.empty[ReferenceTarget]) += tag
      if(tag.path.nonEmpty) {
        updatePerModuleTags(tag.stripHierarchy(1))
      }
    }

    tags.foreach { tag => updatePerModuleTags(tag) }

    val nodePath = new mutable.ArrayBuffer[ReferenceTarget]()
    nodePath += destination
    while (prev.contains(nodePath.last)) {
      val x = nodePath.last
      perModuleTags.get(x.encapsulatingModule) match {
        case Some(tags) =>
          tagMap.getOrElseUpdate(x.pathlessTarget, mutable.HashSet.empty[ReferenceTarget]) ++= tags
        case None =>
      }
      nodePath += prev(nodePath.last)
    }
    tagMap.getOrElseUpdate(nodePath.last, mutable.HashSet.empty[ReferenceTarget]) ++= tags
  }

  override def BFS(root: ReferenceTarget, blacklist: collection.Set[ReferenceTarget]): collection.Map[ReferenceTarget, ReferenceTarget] = {

    val prev = new mutable.LinkedHashMap[ReferenceTarget, ReferenceTarget]()

    val ordering = new Ordering[ReferenceTarget]{
      override def compare(x: ReferenceTarget, y: ReferenceTarget): Int = x.path.size - y.path.size
    }

    val bfsQueue = new mutable.PriorityQueue[ReferenceTarget]()(ordering)

    bfsQueue.enqueue(root)

    while (bfsQueue.nonEmpty) {
      val u = bfsQueue.dequeue
      for (v <- getEdges(u, Some(prev))) {
        if (!prev.contains(v) && !blacklist.contains(v)) {
          prev(v) = u
          bfsQueue.enqueue(v)
        }
      }
    }

    foundShortCuts ++= bfsShortCuts
    bfsShortCuts.clear()
    portConnectivityStack.clear()

    prev
  }

  override def getEdges(source: ReferenceTarget, prevOpt: Option[collection.Map[ReferenceTarget, ReferenceTarget]] = None): collection.Set[ReferenceTarget] = {
    import ConnectionGraph._

    val localSource = source.pathlessTarget

    bfsShortCuts.get(localSource) match {
      case Some(set) => set.map{x => x.setPathTarget(source.pathTarget)}
      case None =>

        val pathlessEdges = super.getEdges(localSource)

        val ret = pathlessEdges.flatMap {

          case localSink if withinSameInstance(source)(localSink) =>
            portConnectivityStack(localSink) = portConnectivityStack.getOrElse(localSource, Nil)
            Set[ReferenceTarget](localSink.setPathTarget(source.pathTarget))

          case localSink if enteringParentInstance(source)(localSink) =>
            val currentStack = portConnectivityStack.getOrElse(localSource, Nil)
            if(currentStack.nonEmpty && currentStack.head.module == localSink.module) {
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
              val destinations = bfsShortCuts.getOrElse(modulePort, mutable.HashSet.empty[ReferenceTarget])
              bfsShortCuts(modulePort) = destinations + localSource
              // Remove entrance from parent from stack
              portConnectivityStack(localSink) = currentStack.tail
            } else {
              // Exiting to parent, but had unresolved trip through child, so don't update shortcut
              portConnectivityStack(localSink) = localSource +: currentStack
            }
            Set[ReferenceTarget](localSink.setPathTarget(source.targetParent.asInstanceOf[IsComponent].pathTarget))

          case localSink if enteringChildInstance(source)(localSink) =>
            portConnectivityStack(localSink) = localSource +: portConnectivityStack.getOrElse(localSource, Nil)
            val x = localSink.setPathTarget(source.pathTarget.instOf(source.ref, localSink.module))
            Set[ReferenceTarget](x)

          case localSink if leavingRootInstance(source)(localSink) => Set[ReferenceTarget]()

          case localSink if enteringNonParentInstance(source)(localSink) => Set[ReferenceTarget]()

          case other => Utils.throwInternalError(s"BAD? $source -> $other")

        }
        ret
    }

  }

  /** Finds a path (if one exists) from one node to another, with a blacklist
    *
    * @param start the start node
    * @param end the destination node
    * @param blacklist list of nodes which break path, if encountered
    * @throws PathNotFoundException
    * @return a Seq[T] of nodes defining an arbitrary valid path
    */
  override def path(start: ReferenceTarget, end: ReferenceTarget, blacklist: collection.Set[ReferenceTarget]): Seq[ReferenceTarget] = {
    insertShortCuts(super.path(start, end, blacklist))
  }

  private def insertShortCuts(path: Seq[ReferenceTarget]): Seq[ReferenceTarget] = {
    val soFar = mutable.HashSet[ReferenceTarget]()
    if(path.size > 1) {
      path.head +: path.sliding(2).flatMap {
        case Seq(from, to) =>
          getShortCut(from) match {
            case Some(set) if set.contains(to) && soFar.contains(from) =>
              soFar += from
              Seq(from.pathTarget.ref("..."), to)
            case other =>
              soFar += from
              Seq(to)
          }
      }.toSeq
    } else path
  }

  /** Finds all paths starting at a particular node in a DAG
    *
    * WARNING: This is an exponential time algorithm (as any algorithm
    * must be for this problem), but is useful for flattening circuit
    * graph hierarchies. Each path is represented by a Seq[T] of nodes
    * in a traversable order.
    *
    * @param start the node to start at
    * @return a Map[T,Seq[Seq[T]]] where the value associated with v is the Seq of all paths from start to v
    */
  override def pathsInDAG(start: ReferenceTarget): mutable.LinkedHashMap[ReferenceTarget,Seq[Seq[ReferenceTarget]]] = {
    val linkedMap = super.pathsInDAG(start)
    linkedMap.keysIterator.foreach { key =>
      linkedMap(key) = linkedMap(key).map(insertShortCuts)
    }
    linkedMap
  }

  override def findSCCs: Seq[Seq[ReferenceTarget]] = Utils.throwInternalError("Cannot call findSCCs on ConnectionGraph")
}

object ConnectionGraph {

  /** Returns a [[DiGraph]] of [[Target]] and corresponding [[IRLookup]]
    *
    * Represents the directed connectivity of a FIRRTL circuit
    * @param circuit
    * @return
    */
  def apply(circuit: Circuit): ConnectionGraph = buildCircuitGraph(circuit)

  def isAsClock(t: ReferenceTarget): Boolean = t.ref.length >= 9 && t.ref.substring(0, 9) == "@asClock#"

  /** Within a module, given an [[Expression]] inside a module, return a corresponding [[Target]]
    * @param m Target of module containing the expression
    * @param tagger Used to uniquely identify unnamed targets, e.g. primops
    * @param e
    * @return
    */
  def asTarget(m: ModuleTarget, tagger: TokenTagger)(e: FirrtlNode): ReferenceTarget = e match {
    case l: Literal => m.ref(tagger.getRef(l.value.toString))
    case w: WRef => m.ref(w.name)
    case r: Reference => m.ref(r.name)
    case w: WSubIndex => asTarget(m, tagger)(w.expr).index(w.value)
    case s: SubIndex => asTarget(m, tagger)(s.expr).index(s.value)
    case w: WSubField => asTarget(m, tagger)(w.expr).field(w.name)
    case s: SubField => asTarget(m, tagger)(s.expr).field(s.name)
    case d: DoPrim => m.ref(tagger.getRef(d.op.serialize))
    case _: Mux => m.ref(tagger.getRef("mux"))
    case _: ValidIf => m.ref(tagger.getRef("validif"))
    case WInvalid => m.ref(tagger.getRef("invalid"))
    case _: Print => m.ref(tagger.getRef("print"))
    case _: Stop => m.ref(tagger.getRef("print"))
    case other => sys.error(s"Unsupported: $other")
  }

  def withinSameInstance(source: ReferenceTarget)(localSink: ReferenceTarget): Boolean = {
    source.encapsulatingModule == localSink.encapsulatingModule
  }

  def enteringParentInstance(source: ReferenceTarget)(localSink: ReferenceTarget): Boolean = {
    source.path.nonEmpty &&
      source.noComponents.targetParent.asInstanceOf[InstanceTarget].encapsulatingModule == localSink.module &&
      localSink.ref == source.path.last._1.value
  }

  def enteringNonParentInstance(source: ReferenceTarget)(localSink: ReferenceTarget): Boolean = {
    source.path.nonEmpty &&
      source.noComponents.targetParent.asInstanceOf[InstanceTarget].encapsulatingModule == localSink.module &&
      localSink.ref != source.path.last._1.value
  }

  def enteringChildInstance(source: ReferenceTarget)(localSink: ReferenceTarget): Boolean = source match {
    case ReferenceTarget(_, top, path, instance, TargetToken.Field(port) +: comps)
      if port == localSink.ref && comps == localSink.component => true
    case _ => false
  }

  def leavingRootInstance(source: ReferenceTarget)(localSink: ReferenceTarget): Boolean = source match {
    case ReferenceTarget(_, top, Seq(), port, comps)
      if port == localSink.component.head.value && comps == localSink.component.tail => true
    case _ => false
  }



  private def buildCircuitGraph(circuit: Circuit): ConnectionGraph = {
    val mdg = new MutableDiGraph[ReferenceTarget]()
    val declarations = mutable.LinkedHashMap[ModuleTarget, mutable.LinkedHashMap[ReferenceTarget, FirrtlNode]]()
    val circuitTarget = CircuitTarget(circuit.main)
    val moduleTypes = circuit.modules.map { m => m.name -> firrtl.Utils.module_type(m) }.toMap
    val moduleMap = circuit.modules.map { m => circuitTarget.module(m.name) -> m }.toMap
    val top = circuitTarget.module(circuit.main)

    circuit map buildModule(circuitTarget)

    def emptySet[T]: mutable.LinkedHashSet[T] = mutable.LinkedHashSet.empty[T]

    def addLabeledVertex(v: ReferenceTarget, f: FirrtlNode): Unit = {
      mdg.addVertex(v)
      declarations.getOrElseUpdate(v.moduleTarget, mutable.LinkedHashMap.empty[ReferenceTarget, FirrtlNode])(v) = f
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
          addLabeledVertex(sourceTarget, stmt)
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
        case p: Print => addLabeledVertex(asTarget(m, tagger)(p), p)
        case s: Stop => addLabeledVertex(asTarget(m, tagger)(s), s)
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

    new ConnectionGraph(circuit, DiGraph(mdg), new IRLookup(declarations, moduleMap))
  }
}

