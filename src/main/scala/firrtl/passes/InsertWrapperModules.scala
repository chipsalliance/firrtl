package firrtl.passes
import firrtl._
import firrtl.ir._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.collection.JavaConverters._
import com.google.protobuf.TextFormat
import com.google.protos.third_party.hardware.chips.firrtl.CustomPassInput

/**
 * Automatically inserts wrapper modules into the module hierarchy around user
 * specified module instances by automatically generating wrapper module defs
 * and a new container module def that instantiates and connects the generated
 * wrapper modules
 */
object InsertWrapperModules extends Transform {
  sealed trait CommandLineArg
  case object RequestProtoFilePath extends CommandLineArg

  def execute (
    circuit: Circuit,
    annotations: Seq[CircuitAnnotation],
    commandArgs: Seq[String]
  ): TransformResult = {
    val customPassInputProtoFilePaths = CustomPassInputUtils.parseCommandlineArgs(commandArgs)
    var result = circuit
    if (!customPassInputProtoFilePaths.isEmpty) {
      // The last CustomPassInputProto in the commandline argument overrides all the other
      val lastRequestProtoFilePath = customPassInputProtoFilePaths.last
      val requestsProto = CustomPassInputUtils.fromTextFile(lastRequestProtoFilePath)
      val insertWrapperModulesRequests = requestsProto.getInsertWrapperModulesRequestsList()
      val nameToModuleMap = (circuit.modules).map((module) => (module.name, module)).toMap
      for (request <- insertWrapperModulesRequests.asScala) {
        val containerModule: Option[DefModule] =
          nameToModuleMap.get(request.getContainerModuleDefName())
        val subModuleGroups: Seq[Seq[String]] =
          for (moduleList <- request.getSubModuleGroupsList().asScala) yield {
            moduleList.getModuleNamesList().asScala
          }
        containerModule match {
          case Some(defModule) => {
            defModule match {
              case m: Module =>
                val (newWrapperMods, newContainerMod) = insertWrapperModules(m, subModuleGroups)
                result = updateCircuit(circuit, m, newWrapperMods, newContainerMod)
              case _ =>
                throw new IllegalArgumentException (
                  s"InsertWrapperModules pass encountered an invalid module"
                  + s" name from its input proto: ${request.getContainerModuleDefName()}"
                )
            }
          }
          case None => {
            throw new IllegalArgumentException (
              s"InsertWrapperModules pass encountered an invalid module"
              + s" name from its input proto: ${request.getContainerModuleDefName()}"
            )
          }
        }
      }
    }
    TransformResult(result)
  }

  private def updateCircuit(
    circuit: Circuit,
    containerModule: Module,
    newWrapperModules: Seq[Module],
    newContainerModule: Module
  ): Circuit = {
    val newModules =
      circuit.modules.filter((m) => m != containerModule) ++ newWrapperModules :+ newContainerModule
    Circuit(circuit.info, newModules, circuit.tops)
  }

  private def insertWrapperModules(
    containerMod: Module,
    subModuleGroups: Seq[Seq[String]]
  ): (Seq[Module], Module) = {
    val (treeNodeToGraphNodeMap, allGraphNodes) = createGraph(containerMod)
    val graphNodeToGroupIdxMap =
        colorGraph(containerMod, treeNodeToGraphNodeMap, subModuleGroups, allGraphNodes)
    splitModule(
      containerMod,
      treeNodeToGraphNodeMap,
      graphNodeToGroupIdxMap,
      subModuleGroups.length
    )
  }

  /**
   * Create a graph representation of a firrtl module def and return a mapping
   * from the statements within the firrtl module def to the corresponding
   * graph nodes.
   */
  private def createGraph(containerMod: Module): (
    Map[Statement, StatementGraphNode],
    Set[FirrtlGraphNode]
  ) = {
    val nameToGraphNodeMap = mutable.HashMap.empty[String, NamedGraphNode]
    val allGraphNodes = mutable.HashSet.empty[FirrtlGraphNode]
    for (port <- containerMod.ports) {
      createGraph(port, nameToGraphNodeMap, allGraphNodes)
    }
    val treeNodeToGraphNodeMap = mutable.HashMap.empty[Statement, StatementGraphNode]
    containerMod.body match {
      case Begin(stmts) => {
        for (stmt <- stmts) {
          createGraph(stmt, treeNodeToGraphNodeMap, nameToGraphNodeMap, allGraphNodes)
        }
      }
      case _ => {
        throw new IllegalArgumentException (
          s"InsertWrapperModules pass encountered an invalid module without a"
          + s"begin stmt. Module name = ${containerMod.name}"
        )
      }
    }
    val allNodesSet = allGraphNodes.toSet
    FirrtlGraphChecker.checkConnections(allNodesSet)
    FirrtlGraphChecker.checkUnreachableNodes(allNodesSet)
    (treeNodeToGraphNodeMap.toMap, allNodesSet)
  }

  private def createGraph (
    stmt: Statement,
    treeNodeToGraphNodeMap: mutable.HashMap[Statement, StatementGraphNode],
    nameToGraphNodeMap: mutable.HashMap[String, NamedGraphNode],
    allGraphNodes: mutable.HashSet[FirrtlGraphNode]
  ): Unit = {
    stmt match {
      case DefWire(info: Info, name: String, tpe: Type) => {
        val graphNode = new DefWireGraphNode(info, name, tpe)
        treeNodeToGraphNodeMap.put(stmt, graphNode)
        allGraphNodes += graphNode
        nameToGraphNodeMap.put(name, graphNode)
      }
      case DefInstance(info: Info, name: String, module: String) => {
        val graphNode = new DefInstanceGraphNode(info, name, module)
        treeNodeToGraphNodeMap.put(stmt, graphNode)
        allGraphNodes += graphNode
        nameToGraphNodeMap.put(name, graphNode)
      }
      case Connect(info: Info, loc: Expression, expr: Expression) => {
        val locGraphNode = createGraph(loc, nameToGraphNodeMap, allGraphNodes)
        val exprGraphNode = createGraph(expr, nameToGraphNodeMap, allGraphNodes)

        val graphNode = new ConnectGraphNode(info)
        graphNode.addLoc(locGraphNode)
        graphNode.addExpr(exprGraphNode)

        treeNodeToGraphNodeMap.put(stmt, graphNode)
        allGraphNodes += graphNode
      }
      case IsInvalid(info, e) => {
        val exprGraphNode = createGraph(e, nameToGraphNodeMap, allGraphNodes)

        val graphNode = new IsInvalidGraphNode(info)
        graphNode.expr = Some(exprGraphNode)
        exprGraphNode.parent = Some(graphNode)

        treeNodeToGraphNodeMap.put(stmt, graphNode)
        allGraphNodes += graphNode
      }
      case _ => {
        throw new IllegalArgumentException (
          s"InsertWrapperModules pass encountered an unexpected statement type: ${stmt}"
        )
      }
    }
  }

  private def createGraph (
    expr: Expression,
    nameToGraphNodeMap: mutable.HashMap[String, NamedGraphNode],
    allGraphNodes: mutable.HashSet[FirrtlGraphNode]
  ): ExpressionGraphNode = {
    expr match {
      case Reference(name, tpe) => {
        val graphNode = new ReferenceGraphNode(name, tpe)
        nameToGraphNodeMap(name).addReference(graphNode)
        allGraphNodes += graphNode
        graphNode
      }
      case SubField(e, name, tpe) => {
        val exprNode = createGraph(e, nameToGraphNodeMap, allGraphNodes)
        val graphNode = new SubFieldGraphNode(name, tpe)
        exprNode.addParent(graphNode)
        allGraphNodes += graphNode
        graphNode
      }
      case SubIndex(e, value, tpe) => {
        val exprNode = createGraph(e, nameToGraphNodeMap, allGraphNodes)
        val graphNode = new SubIndexGraphNode(value, tpe)
        exprNode.addParent(graphNode)
        allGraphNodes += graphNode
        graphNode
      }
    }
  }

  private def createGraph (
    port: Port,
    nameToGraphNodeMap: mutable.HashMap[String, NamedGraphNode],
    allGraphNodes: mutable.HashSet[FirrtlGraphNode]
  ): Unit = {
    val graphNode = new PortGraphNode(port.info, port.name, port.direction, port.tpe)
    nameToGraphNodeMap.put(port.name, graphNode)
    allGraphNodes += graphNode
  }

  /**
   * Based on the user defined grouping of the submodule instances, assign a
   * group number to every graph node in the container module definition. This
   * method does so by propagating out the group numbers from the submodule
   * instance graph nodes in a bfs manner.
   */
  private def colorGraph(
    containerMod: Module,
    treeNodeToGraphNodeMap: Map[Statement, StatementGraphNode],
    subModuleGroups: Seq[Seq[String]],
    allGraphNodes: Set[FirrtlGraphNode]
  ): mutable.HashMap[FirrtlGraphNode, Int] = {
    val instNameToDefInstanceMap = createInstNameToDefInstanceMap(containerMod)
    val graphNodeToGroupIdxMap = new mutable.HashMap[FirrtlGraphNode, Int]()
    val bfsQueue = new mutable.Queue[FirrtlGraphNode]()
    val visitedNodesSet = new mutable.HashSet[FirrtlGraphNode]()

    //colorGraph with bfs
    //initialize bfs queue and graphNodeToGroupIdxMap
    for (groupIdx <- 0 until subModuleGroups.length) {
      val instNames = subModuleGroups(groupIdx)
      for (instName <- instNames) {
        val treeNode = instNameToDefInstanceMap(instName)
        val graphNode = treeNodeToGraphNodeMap(treeNode)
        bfsQueue.enqueue(graphNode)
        graphNodeToGroupIdxMap.put(graphNode, groupIdx)
      }
    }
    for (graphNode <- allGraphNodes) {
      graphNode match {
        case port: PortGraphNode =>
          bfsQueue.enqueue(graphNode)
          graphNodeToGroupIdxMap.put(graphNode, -1)//containerMod nodes have groupIdx = -1
        case _ =>
      }
    }
    //run bfs coloring
    while (!bfsQueue.isEmpty) {
      val node = bfsQueue.dequeue()
      visitedNodesSet += node
      val nodeGroupIdx = graphNodeToGroupIdxMap(node)
      for (neighbor <- node.neighbors) {
        //set neighbor colors
        if (!graphNodeToGroupIdxMap.contains(neighbor)){
          graphNodeToGroupIdxMap.put(neighbor, nodeGroupIdx)
        }
        //enqueue neighbors
        if (!visitedNodesSet.contains(neighbor)){
          bfsQueue.enqueue(neighbor)
        }
      }
    }
    assert(
      allGraphNodes.subsetOf(visitedNodesSet),
      "Graph coloring failed to reach all nodes in the graph"
    )
    graphNodeToGroupIdxMap
  }

  private def createInstNameToDefInstanceMap(containerMod: Module): Map[String, DefInstance] = {
    val result = new mutable.HashMap[String, DefInstance]()
    containerMod.body match {
      case Begin(stmts) => {
        for (stmt <- stmts) {
          stmt match {
            case d: DefInstance => {
              result.put(d.name, d)
            }
            case _ =>
          }
        }
      }
      case _ => {
        throw new IllegalArgumentException (
          s"InsertWrapperModules pass encountered an invalid module without a" +
          s"begin stmt. Module name = ${containerMod.name}"
        )
      }
    }
    result.toMap
  }

  /**
   * Based on the graphNodeToGraphIdxMap, create the new wrapper module defs and
   * the new container module def.
   */
  private def splitModule(
    containerMod: Module,
    treeNodeToGraphNodeMap: Map[Statement, StatementGraphNode],
    graphNodeToGroupIdxMap: mutable.HashMap[FirrtlGraphNode, Int],
    numWrapperMods: Int
  ): (Seq[Module], Module) = {
    splitGraph(graphNodeToGroupIdxMap, numWrapperMods)
    FirrtlGraphChecker.checkConnections(graphNodeToGroupIdxMap.keySet.toSet)
    FirrtlGraphChecker.checkUnreachableNodes(graphNodeToGroupIdxMap.keySet.toSet)
    FirrtlGraphChecker.checkNodeListContainsAllNodes(graphNodeToGroupIdxMap.keySet.toSet)
    FirrtlGraphChecker.checkGraphSplit(graphNodeToGroupIdxMap)

    val newContainerModule = genContainerModFromGraph(graphNodeToGroupIdxMap, containerMod.name)
    val wrapperModules =
      for (i <- 0 until numWrapperMods) yield genWrapperModFromGraph(graphNodeToGroupIdxMap, i)

    (wrapperModules, newContainerModule)
  }

  private def genModuleDefParts(
    graphNodes: Seq[FirrtlGraphNode]
  ): (Seq[Port], Seq[DefWire], Seq[DefInstance], Seq[IsInvalid], Seq[Connect]) = {
    val ports = new mutable.ArrayBuffer[Port]()
    val defWires = new mutable.ArrayBuffer[DefWire]()
    val defInstances = new mutable.ArrayBuffer[DefInstance]()
    val invalids = new mutable.ArrayBuffer[IsInvalid]()
    val connects = new mutable.ArrayBuffer[Connect]()
    for (graphNode  <- graphNodes) {
      graphNode match {
        case defWire: DefWireGraphNode =>
          defWires += DefWire(defWire.info, defWire.name, defWire.tpe)
        case defInstance: DefInstanceGraphNode =>
          defInstances += DefInstance(defInstance.info, defInstance.name, defInstance.module)
        case connect: ConnectGraphNode =>
          connects += Connect(
            connect.info,
            genTreeNode(getGraphNode(connect.loc)),
            genTreeNode(getGraphNode(connect.expr))
          )
        case isInvalid: IsInvalidGraphNode =>
          invalids += IsInvalid(isInvalid.info, genTreeNode(getGraphNode(isInvalid.expr)))
        case port: PortGraphNode =>
          ports += Port(port.info, port.name, port.direction, port.tpe)
        case _ =>
      }
    }
    return (ports, defWires, defInstances, invalids, connects)
  }

  private def genWrapperModFromGraph(
    graphNodeToGroupIdxMap: mutable.HashMap[FirrtlGraphNode, Int],
    groupIdx: Int
  ): Module = {
    val graphNodes = for ((graphNode, i) <- graphNodeToGroupIdxMap if i == groupIdx) yield graphNode
    val (ports, defWires, defInstances, invalids, connects) = genModuleDefParts(graphNodes.toSeq)
    Module(
      NoInfo,
      genWrapperModuleName(groupIdx),
      ports,
      Begin(defWires ++ defInstances ++ invalids ++ connects)
    )
  }

  private def genContainerModFromGraph(
    graphNodeToGroupIdxMap: mutable.HashMap[FirrtlGraphNode, Int],
    containerModName: String
  ): Module = {
    val graphNodes = for ((graphNode, i) <- graphNodeToGroupIdxMap if i == -1) yield graphNode
    val (ports, defWires, defInstances, invalids, connects) = genModuleDefParts(graphNodes.toSeq)
    Module(
      NoInfo,
      containerModName,
      ports.toSeq,
      Begin(defWires ++ defInstances ++ invalids ++ connects)
    )
  }

  def genTreeNode(node: ExpressionGraphNode): Expression = {
    node match {
      case reference: ReferenceGraphNode =>
        Reference(reference.name, reference.tpe)
      case subIndex: SubIndexGraphNode =>
        val expr = genTreeNode(getGraphNode(subIndex.expr))
        SubIndex(expr, subIndex.value, subIndex.tpe)
      case subField: SubFieldGraphNode =>
        val expr = genTreeNode(getGraphNode(subField.expr))
        SubField(expr, subField.name, subField.tpe)
    }
  }
  /**
   * Based on the graphNodeToGroupidxMap, modify the graph so that the neccessary
   * port nodes are inserted between pairs of NamedGraphNodes that are connected,
   * but have differnt group numbers.
   */
  private def splitGraph(
    graphNodeToGroupIdxMap: mutable.HashMap[FirrtlGraphNode, Int],
    numWrapperMods: Int
  ): Unit = {
    val groupIdxToDefInstanceMap = new mutable.HashMap[Int, DefInstanceGraphNode]()
    for (i <- 0 until numWrapperMods) {
      val defInst = new DefInstanceGraphNode(
        NoInfo,
        genWrapperInstName(i),
        genWrapperModuleName(i)
      )
      groupIdxToDefInstanceMap.put(i, defInst)
      graphNodeToGroupIdxMap.put(
        defInst,
        -1 //indicate that the def inst belongs to the container module
      )
    }
    for ((graphNode, groupIdx) <- graphNodeToGroupIdxMap) {
      graphNode match {
        case assignable: AssignableGraphNode => {
          for (reference <- assignable.references) {
            separateExprChain(
              reference, graphNodeToGroupIdxMap, groupIdxToDefInstanceMap.toMap
            )
          }
        }
        case _ =>
      }
    }
  }

  /**
   * Given a ReferenceGraphNode that is the start of a chain of ExpressionGraphNodes,
   * cut the chain of ExpressionGraphNodes if the StatementGraphNodes on the ends
   * of the chain have different group idx.
   */
  private def separateExprChain(
    reference: ReferenceGraphNode,
    graphNodeToGroupIdxMap: mutable.HashMap[FirrtlGraphNode, Int],
    groupIdxToDefInstanceMap: Map[Int, DefInstanceGraphNode]
  ): Unit = {
    val sourceNode = getGraphNode(reference.namedNode)
    val sinkNode = findParentStmtNode(reference)
    if (isRefParentStmtConnectExprSide(reference)) {
      val connectNode = sinkNode.asInstanceOf[ConnectGraphNode]
      val exprStmtNode = sourceNode
      val locStmtNode = findNamedGraphNode(getGraphNode(connectNode.loc))
      val exprStmtGroupIdx = graphNodeToGroupIdxMap(exprStmtNode)
      val locStmtGroupIdx = graphNodeToGroupIdxMap(locStmtNode)
      if (exprStmtGroupIdx != locStmtGroupIdx) {
        graphNodeToGroupIdxMap.put(connectNode, -1)
        if (exprStmtGroupIdx > -1) {
          val newConnectExprNode = genDefInstRef(
            getGraphNode(connectNode.expr),
            groupIdxToDefInstanceMap(exprStmtGroupIdx),
            graphNodeToGroupIdxMap
          )
          val newExprStmtDataSink = genWrapperPortConnectExprRef(
            getGraphNode(connectNode.expr),
            exprStmtGroupIdx,
            graphNodeToGroupIdxMap
          )
          replaceReference(
            exprStmtNode,
            findReference(getGraphNode(connectNode.expr)),
            newExprStmtDataSink
          )
          removeRefChainFromNodeToGroupIdxMap(
            getGraphNode(connectNode.expr),
            graphNodeToGroupIdxMap
          )
          connectNode.addExpr(newConnectExprNode)
        } else {
          setRefChainGroupIdx(
            getGraphNode(connectNode.expr),
            -1,
            graphNodeToGroupIdxMap
          )
        }
        if (locStmtGroupIdx > -1) {
          val newConnectLocNode = genDefInstRef(
            getGraphNode(connectNode.loc),
            groupIdxToDefInstanceMap(locStmtGroupIdx),
            graphNodeToGroupIdxMap
          )
          val newLocStmtDataSource = genWrapperPortConnectLocRef(
            getGraphNode(connectNode.loc),
            locStmtGroupIdx,
            graphNodeToGroupIdxMap
          )
          replaceReference(
            locStmtNode,
            findReference(getGraphNode(connectNode.loc)),
            newLocStmtDataSource
          )
          removeRefChainFromNodeToGroupIdxMap(
            getGraphNode(connectNode.loc),
            graphNodeToGroupIdxMap
          )
          connectNode.addLoc(newConnectLocNode)
        } else {
          setRefChainGroupIdx(
            getGraphNode(connectNode.loc),
            -1,
            graphNodeToGroupIdxMap
          )
        }
      }
    } else if (isRefParentStmtIsInvalid(reference)) {
      val isInvalid = sinkNode.asInstanceOf[IsInvalidGraphNode]
      val sourceNodeGroupIdx = graphNodeToGroupIdxMap(sourceNode)
      graphNodeToGroupIdxMap.put(isInvalid, sourceNodeGroupIdx)
    }
  }

  private def isRefParentStmtConnectExprSide(ref: ReferenceGraphNode): Boolean = {
    val parent = findParentStmtNode(ref)
    parent match {
      case connect: ConnectGraphNode => getGraphNode(connect.expr) eq findRefChainTop(ref)
      case _ => false
    }
  }

  private def isRefParentStmtIsInvalid(ref: ReferenceGraphNode): Boolean = {
    val parent = findParentStmtNode(ref)
    parent match {
      case isInvalid: IsInvalidGraphNode =>
        true
      case _ =>
        false
    }
  }

  private def replaceReference(
    namedNode: NamedGraphNode,
    oldRef: ReferenceGraphNode,
    newRef: ReferenceGraphNode
  ): Unit = {
    val referenceIdx =
      namedNode.references.indexOf(oldRef)
    assert(
      referenceIdx >= 0,
      "oldRef expected to be found in namedNode references list"
    )
    namedNode.references(referenceIdx) = newRef
  }

  private def genDefInstRef(
    refChainHead: ExpressionGraphNode,
    defInstance: DefInstanceGraphNode,
    graphNodeToGroupIdxMap: mutable.HashMap[FirrtlGraphNode, Int]
  ): ExpressionGraphNode = {
    val portName = genPortName(refChainHead)
    val newReference = new ReferenceGraphNode(
      defInstance.name,
      BundleType(Seq(Field(portName, Default, refChainHead.tpe)))
    )
    defInstance.addReference(newReference)
    graphNodeToGroupIdxMap.put(newReference, -1)
    val newSubField = new SubFieldGraphNode(
      portName, refChainHead.tpe
    )
    newReference.addParent(newSubField)
    graphNodeToGroupIdxMap.put(newSubField, -1)
    newSubField
  }

  private def genWrapperPortConnectExprRef(
    refChainHead: ExpressionGraphNode,
    exprStmtGroupIdx: Int,
    graphNodeToGroupIdxMap: mutable.HashMap[FirrtlGraphNode, Int]
  ): ReferenceGraphNode = {
    val newConnect = new ConnectGraphNode(NoInfo)
    graphNodeToGroupIdxMap.put(newConnect, exprStmtGroupIdx)
    val newConnectExpr = copyReferenceChain(refChainHead)
    setRefChainGroupIdx(newConnectExpr, exprStmtGroupIdx, graphNodeToGroupIdxMap)
    newConnect.addExpr(newConnectExpr)
    val port = new PortGraphNode(
      info = NoInfo,
      name = genPortName(refChainHead),
      direction = Output,
      tpe = refChainHead.tpe
    )
    graphNodeToGroupIdxMap.put(port, exprStmtGroupIdx)
    val portReference = new ReferenceGraphNode(
      port.name,
      refChainHead.tpe
    )
    graphNodeToGroupIdxMap.put(portReference, exprStmtGroupIdx)
    port.addReference(portReference)
    newConnect.addLoc(portReference)

    findReference(newConnectExpr)
  }

  private def genWrapperPortConnectLocRef(
    refChainHead: ExpressionGraphNode,
    locStmtGroupIdx: Int,
    graphNodeToGroupIdxMap: mutable.HashMap[FirrtlGraphNode, Int]
  ): ReferenceGraphNode = {
    val newConnect = new ConnectGraphNode(NoInfo)
    graphNodeToGroupIdxMap.put(newConnect, locStmtGroupIdx)
    val newConnectLoc = copyReferenceChain(refChainHead)
    setRefChainGroupIdx(newConnectLoc, locStmtGroupIdx, graphNodeToGroupIdxMap)
    newConnect.addLoc(newConnectLoc)
    val port = new PortGraphNode(
      info = NoInfo,
      name = genPortName(refChainHead),
      direction = Input,
      tpe = refChainHead.tpe
    )
    graphNodeToGroupIdxMap.put(port, locStmtGroupIdx)
    val portReference = new ReferenceGraphNode(
      port.name,
      refChainHead.tpe
    )
    graphNodeToGroupIdxMap.put(portReference, locStmtGroupIdx)
    port.addReference(portReference)
    newConnect.addExpr(portReference)

    findReference(newConnectLoc)
  }

  private def genPortName(refChainHead: ExpressionGraphNode): String = {
    "io" + genPortName(refChainHead, "")
  }

  private def genPortName(refChainHead: ExpressionGraphNode, soFar: String): String = {
    refChainHead match {
      case reference: ReferenceGraphNode =>
        s"_${getGraphNode(reference.namedNode).name}${soFar}"
      case subIndex: SubIndexGraphNode =>
        s"_${subIndex.value}${soFar}"
      case subField: SubFieldGraphNode =>
        s"_${subField.name}${soFar}"
    }
  }

  private def removeRefChainFromNodeToGroupIdxMap(
    refChainHead: ExpressionGraphNode,
    graphNodeToGroupIdxMap: mutable.HashMap[FirrtlGraphNode, Int]
  ): Unit = {
    graphNodeToGroupIdxMap.remove(refChainHead)
    refChainHead match {
      case reference: ReferenceGraphNode =>
      case subIndex: SubIndexGraphNode =>
        removeRefChainFromNodeToGroupIdxMap(
          getGraphNode(subIndex.expr),
          graphNodeToGroupIdxMap
        )
      case subField: SubFieldGraphNode =>
        removeRefChainFromNodeToGroupIdxMap(
          getGraphNode(subField.expr),
          graphNodeToGroupIdxMap
        )
    }
  }

  private def findParentStmtNode(exprNode: ExpressionGraphNode): StatementGraphNode = {
    val parent = getGraphNode(exprNode.parent)
    parent match {
      case subField: SubFieldGraphNode => findParentStmtNode(subField)
      case subIndex: SubIndexGraphNode => findParentStmtNode(subIndex)
      case stmt: StatementGraphNode => stmt
      case _ =>
        throw new Exception(
          s"InsertWrapperModules.findParentStmtNode(exprNode) expects " +
          s"exprNode: ${exprNode} to have a parent of type SubFieldGraphNode," +
          s" SubIndexGraphNode, or StatementGraphNode"
        )
    }
  }

  private def findConnectGraphNode(exprNode: ExpressionGraphNode): ConnectGraphNode = {
    val parent = getGraphNode(exprNode.parent)
    parent match {
      case subField: SubFieldGraphNode => findConnectGraphNode(subField)
      case subIndex: SubIndexGraphNode => findConnectGraphNode(subIndex)
      case connect: ConnectGraphNode => connect
      case _ =>
        throw new Exception(
          s"InsertWrapperModules.findConnectGraphNode(exprNode) expects "
          + s"exprNode: ${exprNode} to have a parent of type SubFieldGraphNode,"
          + s" SubIndexGraphNode, or ConnectGraphNode"
        )
    }
  }

  private def findNamedGraphNode(exprNode: ExpressionGraphNode): NamedGraphNode = {
    exprNode match {
      case subField: SubFieldGraphNode =>
        findNamedGraphNode(
          getGraphNode(subField.expr)
        )
      case subIndex: SubIndexGraphNode =>
        findNamedGraphNode(
          getGraphNode(
            subIndex.expr
          )
        )
      case reference: ReferenceGraphNode =>
        getGraphNode(reference.namedNode)
    }
  }

  /**
   * Given a ExpressionGraphNode, copies the chain of Reference/SubIndex/SubField
   * ExpressionGraphNodes starting from @expr down through its expr pointers, ending
   * with the ReferenceGraphNode at the end of the chain and returns the top of
   * graph node of the copied chain (ie the copy of the @expr argument)
   */
  private def copyReferenceChain(expr: ExpressionGraphNode): ExpressionGraphNode = {
    expr match {
      case reference: ReferenceGraphNode =>
        val copy = new ReferenceGraphNode(reference.name, reference.tpe)
        copy.namedNode = Some(getGraphNode(reference.namedNode))
        copy
      case subField: SubFieldGraphNode =>
        val copy = new SubFieldGraphNode(subField.name, subField.tpe)
        val exprCopy = copyReferenceChain(
          getGraphNode(subField.expr)
        )
        copy.expr = Some(exprCopy)
        exprCopy.parent = Some(copy)
        copy
      case subIndex: SubIndexGraphNode =>
        val copy = new SubIndexGraphNode(subIndex.value, subIndex.tpe)
        val exprCopy = copyReferenceChain(
          getGraphNode(subIndex.expr)
        )
        copy.expr = Some(exprCopy)
        exprCopy.parent = Some(copy)
        copy
    }
  }

  /**
   * Traverses down the expr pointer in a chain of SubIndex/SubField GraphNodes
   * and returns the ReferenceGraphNode at the end of the chain
   */
  private def findReference(expr: ExpressionGraphNode): ReferenceGraphNode = {
    expr match {
      case reference: ReferenceGraphNode =>
        reference
      case subIndex: SubIndexGraphNode =>
        findReference(
          getGraphNode(subIndex.expr)
        )
      case subField: SubFieldGraphNode =>
        findReference(
          getGraphNode(subField.expr)
        )
    }
  }

  /**
   * Traverses up the parent pointer in a chain of SubIndex/SubField/Reference GraphNodes
   * and returns the ExpressionGraphNode at the top of the chain
   */
  private def findRefChainTop(expr: ExpressionGraphNode): ExpressionGraphNode = {
    val parent = getGraphNode(expr.parent)
    parent match {
      case exprParent: ExpressionGraphNode =>
        findRefChainTop(exprParent)
      case _ =>
        expr
    }
  }

  private def setRefChainGroupIdx(
    expr: ExpressionGraphNode,
    groupIdx: Int,
    graphNodeToGroupIdxMap: mutable.HashMap[FirrtlGraphNode, Int]
  ): Unit = {
    expr match {
      case reference: ReferenceGraphNode =>
        graphNodeToGroupIdxMap.put(expr, groupIdx)
      case subIndex: SubIndexGraphNode =>
        graphNodeToGroupIdxMap.put(expr, groupIdx)
        setRefChainGroupIdx(
          getGraphNode(subIndex.expr), groupIdx, graphNodeToGroupIdxMap
        )
      case subField: SubFieldGraphNode =>
        graphNodeToGroupIdxMap.put(expr, groupIdx)
        setRefChainGroupIdx(
          getGraphNode(subField.expr), groupIdx, graphNodeToGroupIdxMap
        )
    }
  }

  private def genWrapperInstName(groupIdx: Int): String = {
    s"group_${groupIdx}_wrapper_inst"
  }

  private def genWrapperModuleName(groupIdx: Int): String = {
    s"group_${groupIdx}_wrapper_mod"
  }
}
