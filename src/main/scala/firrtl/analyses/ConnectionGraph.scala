// See LICENSE for license details.

package firrtl.analyses

import firrtl.annotations._
import firrtl.graph.{DiGraph, MutableDiGraph, PathNotFoundException}
import firrtl.ir._
import firrtl.Mappers._
import firrtl.annotations.TargetToken
import firrtl.passes.MemPortUtils
import firrtl.{InstanceKind, PortKind, SinkFlow, SourceFlow, Utils, WDefInstance, WInvalid, WRef, WSubField, WSubIndex}

import scala.collection.mutable

class ConnectionGraph protected(val circuit: Circuit,
                                val digraph: DiGraph[ReferenceTarget],
                                val irLookup: IRLookup)
  extends DiGraph[ReferenceTarget](digraph.getEdgeMap.asInstanceOf[mutable.LinkedHashMap[ReferenceTarget, mutable.LinkedHashSet[ReferenceTarget]]]) {

  /** Used by BFS to map each visited node to the list of instance inputs visited thus far
    *
    * When BFS descends into a child instance, the child instance port is prepended to the list
    * When BFS ascends into a parent instance, the head of the list is removed
    * In essence, the list is a stack that you push when descending, and pop when ascending
    *
    * Because the search is BFS not DFS, we must record the state of the stack for each edge node, so
    * when that edge node is finally visited, we know the state of the stack
    *
    * For example:
    * circuit Top:
    *   module Top:
    *     input in: UInt
    *     output out: UInt
    *     inst a of A
    *     a.in <= in
    *     out <= a.out
    *   module A:
    *     input in: UInt
    *     output out: UInt
    *     inst b of B
    *     b.in <= in
    *     out <= b.out
    *   module B:
    *     input in: UInt
    *     output out: UInt
    *     out <= in
    *
    * We perform BFS starting at Top>in
    *  Node                ConnectivityStack
    *  Top>in              List()
    *  Top>a.in            List()
    *  Top/a:A>in          List(Top>a.in)
    *  Top/a:A>b.in        List(Top>a.in)
    *  Top/a:A/b:B/in      List(Top/a:A>b.in, Top>a.in)
    *  Top/a:A/b:B/out     List(Top/a:A>b.in, Top>a.in)
    *  Top/a:A>b.out       List(Top>a.in)
    *  Top/a:A>out         List(Top>a.in)
    *  Top>a.out           List()
    *  Top>out             List()
    * when we reach Top/a:A>in the stack is List
    */
  private val portConnectivityStack: mutable.HashMap[ReferenceTarget, List[ReferenceTarget]] =
    mutable.HashMap.empty[ReferenceTarget, List[ReferenceTarget]]

  /** Records connectivities found while BFS is executing, from a module's source port to sink ports of a module
    *
    * All keys and values are local references.
    *
    * A BFS search will first query this map. If the query fails, then it continues and populates the map. If the query
    *   succeeds, then the BFS shortcuts with the values provided by the query.
    *
    * Because this BFS implementation uses a priority queue which prioritizes exploring deeper instances first, a
    *   successful query during BFS will only occur after all paths which leave the module from that reference have
    *   already been searched.
    */
  private val bfsShortCuts: mutable.HashMap[ReferenceTarget, mutable.HashSet[ReferenceTarget]] =
    mutable.HashMap.empty[ReferenceTarget, mutable.HashSet[ReferenceTarget]]

  /** Records connectivities found after BFS is completed, from a module's source port to sink ports of a module
    *
    * All keys and values are local references.
    *
    * If its keys contain a reference, then the value will be complete, in that all paths from the reference out of
    *   the module will have been explored
    *
    * For example, if Top>in connects to Top>out1 and Top>out2, then foundShortCuts(Top>in) will contain
    *   Set(Top>out1, Top>out2), not Set(Top>out1) or Set(Top>out2)
    */
  private val foundShortCuts: mutable.HashMap[ReferenceTarget, mutable.HashSet[ReferenceTarget]] =
    mutable.HashMap.empty[ReferenceTarget, mutable.HashSet[ReferenceTarget]]

  /** Returns whether a previous BFS search has found a shortcut out of a module, starting from target
    * @param target
    * @return
    */
  def hasShortCut(target: ReferenceTarget): Boolean = getShortCut(target).nonEmpty

  /** Optionally returns the shortcut a previous BFS search may have found out of a module, starting from target
    * @param target
    * @return
    */
  def getShortCut(target: ReferenceTarget): Option[Set[ReferenceTarget]] =
    foundShortCuts.get(target.pathlessTarget).map(set => set.map(_.setPathTarget(target.pathTarget)).toSet)

  /** Returns the shortcut a previous BFS search may have found out of a module, starting from target
    * @param target
    * @return
    */
  def shortCut(target: ReferenceTarget): Set[ReferenceTarget] = getShortCut(target).get

  /** Returns a new, reversed connection graph where edges point from sinks to sources
    * @return
    */
  def reverseConnectionGraph: ConnectionGraph = new ConnectionGraph(circuit, digraph.reverse, irLookup)

  def getTag(node: ReferenceTarget,
             tagMap: mutable.LinkedHashMap[(String, ReferenceTarget), mutable.HashSet[ReferenceTarget]]
            ): Option[collection.Set[ReferenceTarget]] = {
    def recGetTag(root: String,
                  node: ReferenceTarget,
                  tagMap: mutable.LinkedHashMap[(String, ReferenceTarget), mutable.HashSet[ReferenceTarget]]
                 ): Option[collection.Set[ReferenceTarget]] = {
      tagMap.get(root, node) match {
        case Some(set) => Some(set)
        case None if node.path.isEmpty => None
        case None => recGetTag(root, node.stripHierarchy(1), tagMap).map(_.map(_.addHierarchy(node.module, node.path.head._1.value)))
      }
    }
    val ret = recGetTag(node.module, node, tagMap)
    ret
  }

  /** For each node in the path from the search start until the destination, update tagMap with the provided tags
    *
    * Tags should be rooted in the same module its ok to be non-local, e.g. Top/a:A>clk is ok.
    * All values in tagMap will share their key's root module
    * TagMap will also contain more local versions of the key/values pair, if they are legal (see example)
    *
    * For example, if we are tagging node Top/a:A>x with Set(Top/a:A>clk, Top>clk) :
    *   TagMap:
    *   Key         Value
    *   Top/a:A>x   Set(Top/a:A>clk, Top>clk)
    *   A>x         Set(A>clk)
    * @param destination
    * @param prev
    * @param tags
    * @param tagMap
    */
  protected def tagPath(destination: ReferenceTarget,
                        prev: collection.Map[ReferenceTarget, ReferenceTarget],
                        tags: collection.Set[ReferenceTarget],
                        tagMap: mutable.LinkedHashMap[(String, ReferenceTarget), mutable.HashSet[ReferenceTarget]]): Unit = {

    val modules = tags.map(_.module) + destination.module
    require(modules.size == 1, s"All tags ($tags) and nodes ($destination) in the path must share their root module ($modules)")

    val perModuleTags = mutable.HashMap[String, mutable.HashSet[ReferenceTarget]]()
    tags.foreach { tag => updatePerModuleTags(tag, perModuleTags) }

    val nodePath = new mutable.ArrayBuffer[ReferenceTarget]()
    nodePath += destination
    while (prev.contains(nodePath.last)) {
      setTag(nodePath.last, perModuleTags, tagMap)
      nodePath += prev(nodePath.last)
    }
    setTag(nodePath.last, perModuleTags, tagMap)
  }

  /** Update tagMap with the provided tags for node
    *
    * Tags should be rooted in the same  be non-local, e.g. Top/a:A>clk is ok.
    * All values in tagMap will share their key's root module
    * TagMap will also contain more local versions of the key/values pair, if they are legal (see example)
    *
    * For example, if we are tagging node Top/a:A>x with Set(Top/a:A>clk, Top>clk) :
    *   TagMap:
    *   Key         Value
    *   Top/a:A>x   Set(Top/a:A>clk, Top>clk)
    *   A>x         Set(A>clk)
    * @param node
    * @param tags
    * @param tagMap
    */
  protected def tagNode(node: ReferenceTarget,
                        tags: collection.Set[ReferenceTarget],
                        tagMap: mutable.LinkedHashMap[(String, ReferenceTarget), mutable.HashSet[ReferenceTarget]]): Unit = {
    //require((tags.map(_.module) ++ node.module).size == 1, "All tags and nodes in the path must share their root module")
    val perModuleTags = mutable.HashMap[String, mutable.HashSet[ReferenceTarget]]()
    tags.foreach { tag => updatePerModuleTags(tag, perModuleTags) }
    setTag(node, perModuleTags, tagMap)
  }

  /** Tags a single node
    * @param node
    * @param perModuleTags
    * @param tagMap
    */
  private def setTag(node: ReferenceTarget,
                  perModuleTags: collection.Map[String, collection.Set[ReferenceTarget]],
                  tagMap: mutable.LinkedHashMap[(String, ReferenceTarget), mutable.HashSet[ReferenceTarget]]): Unit = {
    def recSetTag(root: String,
                  node: ReferenceTarget,
                  perModuleTags: collection.Map[String, collection.Set[ReferenceTarget]],
                  tagMap: mutable.LinkedHashMap[(String, ReferenceTarget), mutable.HashSet[ReferenceTarget]]): Unit = {
      perModuleTags.get(node.module) match {
        case Some(tags) =>
          tagMap.getOrElseUpdate((root, node), mutable.HashSet.empty[ReferenceTarget]) ++= tags
        case None =>
      }
      if (node.path.nonEmpty) {
        recSetTag(root, node.stripHierarchy(1), perModuleTags, tagMap)
      }
    }
    recSetTag(node.module, node, perModuleTags, tagMap)
  }

  private def updatePerModuleTags(tag: ReferenceTarget, perModuleTags: mutable.HashMap[String, mutable.HashSet[ReferenceTarget]]): Unit = {
    perModuleTags.getOrElseUpdate(tag.module, mutable.HashSet.empty[ReferenceTarget]) += tag
    if(tag.path.nonEmpty) {
      updatePerModuleTags(tag.stripHierarchy(1), perModuleTags)
    }
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
            Set[ReferenceTarget](localSink.setPathTarget(source.noComponents.targetParent.asInstanceOf[IsComponent].pathTarget))

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
            case Some(set) if set.contains(to) && soFar.contains(from.pathlessTarget) =>
              soFar += from.pathlessTarget
              Seq(from.pathTarget.ref("..."), to)
            case other =>
              soFar += from.pathlessTarget
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

  def isInvalid(t: ReferenceTarget): Boolean = t.ref.length >= 9 && t.ref.substring(0, 9) == "@invalid#"

  def isLiteral(t: ReferenceTarget): Boolean = {
    t.ref match {
      case TokenTagger.literalRegex(value) => true
      case _ => false
    }
  }

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
    val b1 = source.path.nonEmpty
    def b2 = source.noComponents.targetParent.asInstanceOf[InstanceTarget].encapsulatingModule == localSink.module
    def b3 = localSink.ref == source.path.last._1.value
    if(localSink.module == "AXI4Buffer_1" && localSink.ref == "Queue_1") {
      println(s"$source -> $localSink")
      println(s"$b1 $b2 $b3")
    }
    b1 && b2 && b3
  }

  def enteringNonParentInstance(source: ReferenceTarget)(localSink: ReferenceTarget): Boolean = {
    source.path.nonEmpty &&
      (source.noComponents.targetParent.asInstanceOf[InstanceTarget].encapsulatingModule != localSink.module ||
      localSink.ref != source.path.last._1.value)
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
      val instPorts = Utils.create_exps(WRef(name, tpe, InstanceKind, SinkFlow))
      val modulePorts = tpe.asInstanceOf[BundleType].fields.flatMap {
        // Module output
        case firrtl.ir.Field(name, Default, tpe) => Utils.create_exps(WRef(name, tpe, PortKind, SourceFlow))
        // Module input
        case firrtl.ir.Field(name, Flip, tpe) => Utils.create_exps(WRef(name, tpe, PortKind, SinkFlow))
      }
      assert(instPorts.size == modulePorts.size)
      val o = m.circuitTarget.module(ofModule)
      instPorts.zip(modulePorts).foreach { x =>
        val (instExp, modExp) = x
        val it = asTarget(m, tagger)(instExp)
        val mt = asTarget(o, tagger)(modExp)
        (Utils.flow(instExp), Utils.flow(modExp)) match {
          case (SourceFlow, SinkFlow) => mdg.addPairWithEdge(it, mt)
          case (SinkFlow, SourceFlow) => mdg.addPairWithEdge(mt, it)
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

