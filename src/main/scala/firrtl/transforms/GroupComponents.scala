package firrtl.transforms

import firrtl._
import firrtl.Mappers._
import firrtl.ir._
import firrtl.annotations.{Annotation, ComponentName}
import firrtl.passes.{InferTypes, LowerTypes, MemPortUtils}
import firrtl.Utils.{kind, throwInternalError}
import firrtl.graph.{DiGraph, MutableDiGraph}

import scala.collection.mutable


case class GroupAnnotation(components: Seq[ComponentName], newModule: String, newInstance: String, outputSuffix: Option[String] = None, inputSuffix: Option[String] = None) extends Annotation {
  if(components.nonEmpty) {
    require(components.forall(_.module == components.head.module), "All components must be in the same module.")
    require(components.forall(!_.name.contains('.')), "No components can be a subcomponent.")
  }
  def currentModule: String = components.head.module.name

  /* Only keeps components renamed to components */
  def update(renames: RenameMap): Seq[Annotation] = {
    val newComponents = components.flatMap{c => renames.get(c).getOrElse(Seq(c))}.collect {
      case c: ComponentName => c
    }
    Seq(GroupAnnotation(newComponents, newModule, newInstance, outputSuffix, inputSuffix))
  }
}

class GroupAndDedup extends GroupComponents {
  override def execute(state: CircuitState): CircuitState = {
    val cs = super.execute(state)
    val csx = new DedupModules().execute(cs)
    println(cs.circuit.serialize)
    csx
  }
}

class GroupComponents extends firrtl.Transform {
  type MSet[T] = mutable.Set[T]

  def inputForm: CircuitForm = MidForm
  def outputForm: CircuitForm = MidForm

  override def execute(state: CircuitState): CircuitState = {
    println(state.circuit.serialize)
    val groups = state.annotations.collect {case g: GroupAnnotation => g}
    val byModule = groups.groupBy{_.currentModule}
    val mnamespace = Namespace(state.circuit)
    val newModules = state.circuit.modules.flatMap {
      case m: Module if byModule.contains(m.name) =>
        // do stuff
        groupModule(m, byModule(m.name).filter(_.components.nonEmpty), mnamespace)
      case other => Seq(other)
    }
    val cs = state.copy(circuit = state.circuit.copy(modules = newModules))
    val csx = InferTypes.execute(cs)
    println(cs.circuit.serialize)
    csx
  }

  def groupModule(m: Module, groups: Seq[GroupAnnotation], mnamespace: Namespace): Seq[Module] = {
    val namespace = Namespace(m)
    val groupRoots = groups.map(_.components.map(_.name))
    val totalSum = groupRoots.foldLeft(0){(total, set) => total + set.size}
    val union = groupRoots.foldLeft(Set.empty[String]){(all, set) => all.union(set.toSet)}

    require(groupRoots.forall{_.forall{namespace.contains}}, "All names should be in this module")
    require(totalSum == union.size, "No name can be in more than one group")
    require(groupRoots.forall(_.nonEmpty), "All groupRoots must by non-empty")



    // Name groupRoots by first element
    //val byGroup: Map[String, MSet[String]] = groupRoots.collect{case set => set.head -> mutable.Set(set.toSeq:_*)}.toMap

    val topKV = ("", mutable.Set(""))
    val annos = groups.collect{case g:GroupAnnotation => g.components.head.name -> g}.toMap
    val byGroup: Map[String, MSet[String]] = groups.collect{case GroupAnnotation(set, module, instance, _, _) => set.head.name -> mutable.Set(set.map(_.name).toSeq:_*)}.toMap + topKV
    val groupModule: Map[String, String] = groups.map(a => a.components.head.name -> mnamespace.newName(a.newModule)).toMap
    val groupInstance: Map[String, String] = groups.map(a => a.components.head.name -> namespace.newName(a.newInstance)).toMap

    // Build set of components not in set
    val notSet = byGroup.map { case (key, value) => key -> union.diff(value) }


    // Get all dependencies between components
    val (deps, drives) = getBiDirectionalComponentConnectivity(m)
    //println(deps.getEdgeMap)
    //println(drives.getEdgeMap)

    // For each node not in the set, which group can reach it
    val reachableNodes = new mutable.HashMap[String, MSet[String]]()

    // For each group, add connectivity between nodes in set
    // Populate reachableNodes with reachability, where blacklist is their notSet
    byGroup.foreach { case (head, set) =>
      set.foreach { x =>
        deps.addPairWithEdge(head, x)
      }
      deps.reachableFrom(head, notSet(head)) foreach { node =>
        reachableNodes.getOrElseUpdate(node, mutable.Set.empty[String]) += head
      }
    }

    // Add nodes who are reached by a single group, to that group
    reachableNodes.foreach { case (node, membership) =>
      if(membership.size == 1) {
        byGroup(membership.head) += node
      } else {
        byGroup("") += node
      }
    }
    //println(byGroup)

    // Per group, find edges that cross group border
    val crossings = new mutable.HashMap[String, MSet[(String, String)]]
    byGroup.foreach { case (head, set) =>
      val edges = set.flatMap { node =>
        deps.getEdges(node).collect{case you: String if !set.contains(you) => (node, you)}
      }
      crossings(head) = edges
    }

    applyGrouping(m, namespace, byGroup, crossings, groupModule, groupInstance, annos).toSeq
  }

  def applyGrouping( m: Module,
                     namespace: Namespace,
                     byGroup: Map[String, MSet[String]],
                     crossings: mutable.Map[String, MSet[(String, String)]],
                     groupModule: Map[String, String],
                     groupInstance: Map[String, String],
                     annos: Map[String, GroupAnnotation]
                   ): Set[Module] = {
    // Maps node to group
    val byNode = mutable.HashMap[String, String]()
    byGroup.foreach { case (group, nodes) =>
      nodes.foreach { node =>
        byNode(node) = group
      }
    }
    val groupNamespace = byGroup.map { case (head, set) => head -> Namespace(set.toSeq) }

    val groupStatements = mutable.HashMap[String, mutable.ArrayBuffer[Statement]]()
    val groupPorts = mutable.HashMap[String, mutable.ArrayBuffer[Port]]()
    val groupPortNames = mutable.HashMap[String, mutable.HashMap[String, String]]()
    byGroup.keys.foreach { group =>
      groupStatements(group) = new mutable.ArrayBuffer[Statement]()
      groupPorts(group) = new mutable.ArrayBuffer[Port]()
      groupPortNames(group) = new mutable.HashMap[String, String]()
    }

    def addPort(group: String, exp: Expression, d: Direction): String = {
      val source = LowerTypes.loweredName(exp)
      val portNames = groupPortNames(group)
      val suffix = d match {
        case Output => annos(group).outputSuffix.getOrElse("")
        case Input => annos(group).inputSuffix.getOrElse("")
      }
      val newName = groupNamespace(group).newName(source + suffix)
      val portName = portNames.getOrElseUpdate(source, newName)
      groupPorts(group) += Port(NoInfo, portName, d, exp.tpe)
      portName
    }

    def punchSignalOut(group: String, exp: Expression): String = {
      val portName = addPort(group, exp, Output)
      groupStatements(group) += Connect(NoInfo, WRef(portName), exp)
      portName
    }

    def inGroupFixExps(group: String, added: mutable.ArrayBuffer[Statement])(e: Expression): Expression = e match {
      case _: Literal => e
      case _: DoPrim | _: Mux | _: ValidIf => e map inGroupFixExps(group, added)
      case otherExp: Expression =>
        val wref = getWRef(otherExp)
        val source = wref.name
        byNode.get(source) match {
          // case 1: source in the same group as sink
          case Some(`group`) => otherExp //do nothing

          // case 2: source in top
          case Some("") =>
            // Add port to group's Module
            val toPort = addPort(group, otherExp, Input)

            // Add connection in Top to group's Module port
            added += Connect(NoInfo, WSubField(WRef(groupInstance(group)), toPort), otherExp)

            // Return WRef with new kind (its inside the group Module now)
            WRef(toPort, otherExp.tpe, PortKind, MALE)

          // case 3: source in different group
          case Some(otherGroup) =>
            // Add port to otherGroup's Module
            val fromPort = punchSignalOut(otherGroup, otherExp)
            val toPort = addPort(group, otherExp, Input)

            // Add connection in Top from otherGroup's port to group's port
            val groupInst = groupInstance(group)
            val otherInst = groupInstance(otherGroup)
            added += Connect(NoInfo, WSubField(WRef(groupInst), toPort), WSubField(WRef(otherInst), fromPort))

            // Return WRef with new kind (its inside the group Module now)
            WRef(toPort, otherExp.tpe, PortKind, MALE)


        }
    }

    def inTopFixExps(e: Expression): Expression = e match {
      case _: DoPrim|_: Mux|_: ValidIf => e map inTopFixExps
      case otherExp: Expression =>
        val wref = getWRef(otherExp)
        if(byNode(wref.name) != "") {
          // Get the name of source's group
          val otherGroup = byNode(wref.name)

          // Add port to otherGroup's Module
          val otherPortName = punchSignalOut(otherGroup, otherExp)

          // Return WSubField (its inside the top Module still)
          WSubField(WRef(groupInstance(otherGroup)), otherPortName)

        } else otherExp
    }

    def onStmt(s: Statement): Statement = {
      s match {
        // Sink is in a group
        case r: IsDeclaration if byNode(r.name) != "" =>
          //
          val topStmts = mutable.ArrayBuffer[Statement]()
          val group = byNode(r.name)
          groupStatements(group) += r mapExpr inGroupFixExps(group, topStmts)
          Block(topStmts)
        case c: Connect if byNode(getWRef(c.loc).name) != "" =>
          // Sink is in a group
          val topStmts = mutable.ArrayBuffer[Statement]()
          val group = byNode(getWRef(c.loc).name)
          groupStatements(group) += Connect(c.info, c.loc, inGroupFixExps(group, topStmts)(c.expr))
          Block(topStmts)
        // TODO Attach if all are in a group?
        case _: IsDeclaration|_: Connect|_: Attach =>
          // Sink is in Top
          val ret = s mapExpr inTopFixExps
          ret
        case other => other map onStmt
      }
    }


    // Build datastructures
    val newTopBody = Block(groupModule.map{case (g, m) => WDefInstance(NoInfo, groupInstance(g), m, UnknownType)}.toSeq ++ Seq(onStmt(m.body)))
    val finalTopBody = Block(Utils.squashEmpty(newTopBody).asInstanceOf[Block].stmts.distinct)

    val newModules = byGroup.keys.filter(_ != "") map { group =>
      Module(NoInfo, groupModule(group), groupPorts(group).distinct, Block(groupStatements(group).distinct))
    }
    Set(m.copy(body = finalTopBody)) ++ newModules
  }

  def getWRef(e: Expression): WRef = e match {
    case w: WRef => w
    case other =>
      var w = WRef("")
      other mapExpr { e => w = getWRef(e); e}
      w
  }

  def getBiDirectionalComponentConnectivity(m: Module): (MutableDiGraph[String], MutableDiGraph[String]) = {
    //val blacklist = m.ports.map(_.name).toSet
    val colorGraph = new MutableDiGraph[String]
    val driveGraph = new MutableDiGraph[String]
    val simNamespace = Namespace()
    val simulations = new mutable.HashMap[String, Statement]
    def onExpr(sink: WRef)(e: Expression): Expression = e match {
      case w@WRef(name, _, _, _) => //if !blacklist.contains(name) && !blacklist.contains(sink.name) =>
        //println(blacklist)
        //println(name)
        colorGraph.addPairWithEdge(sink.name, name)
        colorGraph.addPairWithEdge(name, sink.name)
        driveGraph.addPairWithEdge(name, sink.name)
        w
      case other => other map onExpr(sink)
    }
    def onStmt(stmt: Statement): Unit = stmt match {
      case w: WDefInstance =>
      case h: IsDeclaration => h map onExpr(WRef(h.name))
      case Attach(_, exprs) => // Add edge between each expression
        exprs.tail map onExpr(getWRef(exprs.head))
      case Connect(_, loc, expr) =>
        onExpr(getWRef(loc))(expr)
      case q@Stop(_,_, clk, en) =>
        val simName = simNamespace.newTemp
        simulations(simName) = q
        Seq(clk, en) map onExpr(WRef(simName))
      case q@Print(_, _, args, clk, en) =>
        val simName = simNamespace.newTemp
        simulations(simName) = q
        (args :+ clk :+ en) map onExpr(WRef(simName))
      case Block(stmts) => stmts.foreach(onStmt)
      case ignore @ (_: IsInvalid | EmptyStmt) => // do nothing
      case other => throw new Exception(s"Unexpected Statement $other")
    }

    onStmt(m.body)
    m.ports.foreach { p =>
      colorGraph.addPairWithEdge("", p.name)
      colorGraph.addPairWithEdge(p.name, "")
    }
    (colorGraph, driveGraph)
  }
}
