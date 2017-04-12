
package firrtl.passes

import firrtl._
import firrtl.ir._
import firrtl.annotations._
import firrtl.graph._
import firrtl.analyses.InstanceGraph
import firrtl.Mappers._
import firrtl.WrappedExpression._
import firrtl.Utils.{throwInternalError, toWrappedExpression, kind}
import wiring.WiringUtils.getChildrenMap
import logger._

import collection.mutable
import java.io.{File, FileWriter}

object DeadCodeElimination extends Pass {

  /** Based on LogicNode ins CheckCombLoops, currently kind of faking it */
  private type LogicNode = WrappedExpression
  private object LogicNode {
    def apply(moduleName: String, expr: Expression): LogicNode =
      WrappedExpression(Utils.mergeRef(WRef(moduleName), expr))
    def apply(moduleName: String, name: String): LogicNode = apply(moduleName, WRef(name))
  }

  /** Expression used to represent outputs in the circuit (# is illegal in names) */
  private val circuitSink = LogicNode("#Top", "#Sink")

  /** Extract all References and SubFields from a possibly nested Expression */
  def extractRefs(expr: Expression): Seq[Expression] = {
    val refs = mutable.ArrayBuffer.empty[Expression]
    def rec(e: Expression): Expression = {
      e match {
        case ref @ (_: WRef | _: WSubField) => refs += ref
        case nested @ (_: Mux | _: DoPrim | _: ValidIf) => nested map rec
        case ignore @ (_: Literal) => // Do nothing
        case unexpected => throwInternalError
      }
      e
    }
    rec(expr)
    refs
  }

  /** Construct the dependency graph within this module */
  private def setupDepGraph(depGraph: MutableDiGraph[LogicNode],
                            instMap: collection.Map[String, String])
                           (mod: Module): Unit = {
    // Gets all dependencies and constructs LogicNodes from them
    def getDeps(expr: Expression): Seq[LogicNode] =
      extractRefs(expr).map { e =>
        if (kind(e) == InstanceKind) {
          val (inst, tail) = Utils.splitRef(e)
          LogicNode(instMap(inst.name), tail)
        } else {
          LogicNode(mod.name, e)
        }
      }

    def onStmt(stmt: Statement): Unit = stmt match {
      case DefRegister(_, name, _, clock, reset, init) =>
        val node = LogicNode(mod.name, name)
        depGraph.addVertex(node)
        Seq(clock, reset, init).flatMap(getDeps(_)).foreach(ref => depGraph.addEdge(node, ref))
      case DefNode(_, name, value) =>
        val node = LogicNode(mod.name, name)
        depGraph.addVertex(node)
        getDeps(value).foreach(ref => depGraph.addEdge(node, ref))
      case DefWire(_, name, _) =>
        depGraph.addVertex(LogicNode(mod.name, name))
      case mem: DefMemory =>
        // Treat DefMems as a node with outputs depending on the node and node depending on inputs
				// From perpsective of the module or instance, MALE expressions are inputs, FEMALE are outputs
        val memRef = WRef(mem.name, MemPortUtils.memType(mem), ExpKind, FEMALE)
				val exprs = Utils.create_exps(memRef).groupBy(Utils.gender(_))
				val sources = exprs.getOrElse(MALE, List.empty).flatMap(getDeps(_))
				val sinks = exprs.getOrElse(FEMALE, List.empty).flatMap(getDeps(_))
        val memNode = getDeps(memRef) match { case Seq(node) => node }
        depGraph.addVertex(memNode)
        sinks.foreach(sink => depGraph.addEdge(sink, memNode))
        sources.foreach(source => depGraph.addEdge(memNode, source))
      case Attach(_, exprs) => // Add edge between each expression
        exprs.flatMap(getDeps(_)).toSet.subsets(2).map(_.toList).foreach {
          case Seq(a, b) =>
            depGraph.addEdge(a, b)
            depGraph.addEdge(b, a)
        }
      case Connect(_, loc, expr) =>
        // This match enforces the low Firrtl requirement of expanded connections
        val node = getDeps(loc) match { case Seq(elt) => elt }
        getDeps(expr).foreach(ref => depGraph.addEdge(node, ref))
      // Simulation constructs are treated as top-level outputs
      case Stop(_,_, clk, en) =>
        Seq(clk, en).flatMap(getDeps(_)).foreach(ref => depGraph.addEdge(circuitSink, ref))
      case Print(_, _, args, clk, en) =>
        (args :+ clk :+ en).flatMap(getDeps(_)).foreach(ref => depGraph.addEdge(circuitSink, ref))
      case Block(stmts) => stmts.foreach(onStmt(_))
      case ignore @ (_: IsInvalid | _: WDefInstance | EmptyStmt) => // do nothing
      case other => throw new Exception(s"Unexpected Statement $other")
    }

    // Add all ports as vertices
    mod.ports.foreach { case Port(_, name, _, _: GroundType) =>
      depGraph.addVertex(LogicNode(mod.name, name))
    }
    onStmt(mod.body)
  }

  private def createDependencyGraph(instMaps: collection.Map[String, collection.Map[String, String]],
                                    c: Circuit): DiGraph[LogicNode] = {
    val depGraph = new MutableDiGraph[LogicNode]
    c.modules.foreach {
      case mod: Module => setupDepGraph(depGraph, instMaps(mod.name))(mod)
      case ext: ExtModule => ??? // TODO Just connect all inputs to all outputs
    }
    // Connect circuitSink to top-level outputs
    val topModule = c.modules.find(_.name == c.main).get
    val topOutputs = topModule.ports.filter(_.direction == Output)
    topOutputs.foreach(output => depGraph.addEdge(circuitSink, LogicNode(c.main, output.name)))

    DiGraph(depGraph)
  }

  private def deleteDeadCode(instMap: collection.Map[String, String],
                             deadNodes: Set[LogicNode],
                             moduleMap: collection.Map[String, DefModule])
                            (mod: Module): Option[Module] = {
    // Gets all dependencies and constructs LogicNodes from them
    // TODO this is a duplicate from setupDepGraph, remove by improving how we lookup
    def getDeps(expr: Expression): Seq[LogicNode] =
      extractRefs(expr).map { e =>
        if (kind(e) == InstanceKind) {
          val (inst, tail) = Utils.splitRef(e)
          LogicNode(instMap(inst.name), tail)
        } else {
          LogicNode(mod.name, e)
        }
      }

    // TODO Delete unused writers from DefMemory???
    def onStmt(stmt: Statement): Statement = stmt match {
      case inst: WDefInstance =>
        moduleMap.get(inst.module) match {
          case Some(instMod) => inst.copy(tpe = Utils.module_type(instMod))
          case None => EmptyStmt
        }
      case decl: IsDeclaration =>
        val node = LogicNode(mod.name, decl.name)
        if (deadNodes.contains(node)) EmptyStmt else decl
      case con: Connect =>
        val node = getDeps(con.loc) match { case Seq(elt) => elt }
        if (deadNodes.contains(node)) EmptyStmt else con
      case Attach(info, exprs) => // If any exprs are dead then all should be
        val exprsx = exprs.flatMap(getDeps(_)).filterNot(deadNodes.contains(_)).map(_.e1)
        if (exprsx.isEmpty) EmptyStmt else Attach(info, exprsx)
      case other => other map onStmt
    }

    val portsx = mod.ports.filterNot(p => deadNodes.contains(LogicNode(mod.name, p.name)))
    if (portsx.isEmpty) None else Some(mod.copy(ports = portsx, body = onStmt(mod.body)))
  }

  def run(c: Circuit): Circuit = {
    val moduleMap = c.modules.map(m => m.name -> m).toMap
    val iGraph = new InstanceGraph(c)
    val moduleDeps = iGraph.graph.edges.map { case (k,v) =>
      k.module -> v.map(i => i.name -> i.module).toMap
    }
    val topoSortedModules = iGraph.graph.transformNodes(_.module).linearize.reverse.map(moduleMap(_))

    //println(" ********** moduleMap ********** ")
    //println(moduleMap)
    //val instMaps = getChildrenMap(c).map { case (mod, insts) => mod -> insts.toMap }.toMap
    //val moduleDeps2 = instMaps.map { case (mod, insts) => mod -> insts.values.toSet }
    //val topoSortedModules2 = DiGraph(moduleDeps2).linearize.reverse.map(moduleMap(_))
    //println(" ********** moduleDeps ********** ")
    //println(moduleDeps)
    //println(moduleDeps2)
    //println(" ********** topoSortedModules ********** ")
    //println(topoSortedModules.map(_.name))
    //println(topoSortedModules2.map(_.name))

    //println(" ********** instMaps ********** ")
    //println(instMaps)
    //println(moduleDeps)


    val depGraph = createDependencyGraph(moduleDeps, c)

    //println(" ********** Dependency Graph ********** ")
    //val vertices = depGraph.getVertices
    //vertices.foreach { v =>
    //  val deps = depGraph.getEdges(v)
    //  deps.foreach(d => println("  " + v.e1.serialize + " -> " + d.e1.serialize))
    //}

    val liveNodes = depGraph.reachableFrom(circuitSink) + circuitSink
    //println(" ********** Live Nodes ********** ")
    //liveNodes.foreach(n => println("  " + n.e1.serialize))

    val deadNodes = depGraph.getVertices -- liveNodes
    //println(" ********** Dead Nodes ********** ")
    //deadNodes.foreach(n => println("  " + n.e1.serialize))

    // As we delete deadCode, we will delete ports from Modules and somtimes complete modules
    // themselves. We iterate over the modules in a topological order from leaves to the top. The
    // current status of the modulesxMap is used to either delete instances or update their types
    val modulesxMap = mutable.HashMap.empty[String, DefModule]
    topoSortedModules.foreach {
      case mod: Module =>
        deleteDeadCode(moduleDeps(mod.name), deadNodes, modulesxMap)(mod).foreach { m =>
          modulesxMap += m.name -> m
        }
      case ext: ExtModule =>
        modulesxMap += ext.name -> ext
    }

    // Preserve original module order
    val res = c.copy(modules = c.modules.flatMap(m => modulesxMap.get(m.name)))
    //println(" ********** New Circuit ********** ")
    //println(res.serialize)
    //throw new Exception("bail")
    res
  }
}
