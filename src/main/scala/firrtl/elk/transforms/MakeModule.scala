// SPDX-License-Identifier: Apache-2.0

package firrtl.elk.transforms

import firrtl.PrimOps._
import firrtl.elk.elknodes._
import firrtl.elk.{ElkFlattenLevelAnnotation, ElkTopModuleAnnotation}
import firrtl.ir._
import firrtl.options.TargetDirAnnotation
import firrtl.{BuildInfo, CircuitState, DependencyAPIMigration, Transform, WDefInstance, WRef, WSubField, WSubIndex}

import java.io.PrintWriter
import scala.collection.mutable

case class Scope(depth: Int = -1, maxDepth: Int = -1) {
  def doComponents(): Boolean = {
    val result = depth >= 0 && (maxDepth == -1 || depth < maxDepth)
    result
  }
  def doPorts(): Boolean = depth >= 0 && (maxDepth == -1 || depth <= maxDepth)

  def descend: Scope = {
    if (depth == -1) {
      Scope()
    } else {
      val newDepth = if (depth == maxDepth) -1 else depth + 1
      Scope(newDepth, maxDepth)
    }
  }

  override def toString: String = {
    val s = (depth, maxDepth) match {
      case (-1, -1) => "out"
      case (_, -1)  => "in, no depth limit"
      case (_, _)   => s"in, do ports ${doPorts()}, do components ${doComponents()}"
    }
    s"Scope($depth, $maxDepth): $s"
  }
}

class MakeModule(targetDir: String) extends Transform with DependencyAPIMigration {
  override def prerequisites = Seq.empty

  override def optionalPrerequisites = Seq.empty

  override def optionalPrerequisiteOf = Seq.empty

  override def invalidates(a: Transform) = false

  val subModulesFound: mutable.HashSet[DefModule] = new mutable.HashSet[DefModule]()

  def execute(state: CircuitState): CircuitState = {
    val nameToNode: mutable.HashMap[String, ElkNode] = new mutable.HashMap()

    val c = state.circuit

    val startModuleName = state.annotations.collectFirst {
      case ElkTopModuleAnnotation(moduleName) => moduleName
    }.getOrElse(state.circuit.main)

    val maxDepth = state.annotations.collectFirst {
      case ElkFlattenLevelAnnotation(depth) => depth
    }.getOrElse(1)

    var linesPrintedSinceFlush = 0
    var totalLinesPrinted = 0

    val printFileName = s"$targetDir/$startModuleName.graph"
    println(s"Creating elk graph file $printFileName")
    val printFile = new PrintWriter(new java.io.File(printFileName))
    def pl(s: String): Unit = {
      printFile.println(s)
      val lines = s.count(_ == '\n')
      totalLinesPrinted += lines
      linesPrintedSinceFlush += lines
      if (linesPrintedSinceFlush > 1000) {
        printFile.flush()
        linesPrintedSinceFlush = 0
      }
    }

    def findModule(moduleName: String, circuit: Circuit): DefModule = {
      circuit.modules.find(module => module.name == moduleName) match {
        case Some(module: firrtl.ir.Module) =>
          module
        case Some(externalModule: DefModule) =>
          externalModule
        case _ =>
          throw new Exception(s"Could not find top level module in $moduleName")
      }
    }

    def processModule(
      modulePrefix:   String,
      myModule:       DefModule,
      moduleNode:     ModuleNode,
      scope:          Scope = Scope(),
      subModuleDepth: Int = 0
    ): ElkNode = {

      def getFirrtlName(name: String): String = {
        if (modulePrefix.isEmpty) name else modulePrefix + "." + name
      }

      def expand(name: String): String = {
        s"${moduleNode.absoluteName}.$name"
      }

      def reducedLongLiteral(s: String): String = {
        if (s.length > 32) { s.take(16) + "..." + s.takeRight(16) }
        else { s }
      }

      def getLiteralValue(expression: Expression): Option[String] = {
        expression match {
          case UIntLiteral(x, _) => Some(reducedLongLiteral(x.toString))
          case SIntLiteral(x, _) => Some(reducedLongLiteral(x.toString))
          case _                 => None
        }
      }

      def processPrimOp(primOp: DoPrim): String = {
        def addBinOpNode(symbol: String): String = {
          val arg0ValueOpt = getLiteralValue(primOp.args.head)
          val arg1ValueOpt = getLiteralValue(primOp.args.tail.head)

          val opNode = BinaryOpNode(symbol, Some(moduleNode), arg0ValueOpt, arg1ValueOpt)
          moduleNode += opNode
          if (arg0ValueOpt.isEmpty) moduleNode.connect(opNode.in1, processExpression(primOp.args.head))
          if (arg1ValueOpt.isEmpty) moduleNode.connect(opNode.in2, processExpression(primOp.args.tail.head))
          opNode.asRhs
        }

        def addUnaryOpNode(symbol: String): String = {
          val opNode = UnaryOpNode(symbol, Some(moduleNode))
          moduleNode += opNode
          moduleNode.connect(opNode.in1, processExpression(primOp.args.head))
          opNode.asRhs
        }

        def addOneArgOneParamOpNode(symbol: String): String = {
          val opNode = OneArgOneParamOpNode(symbol, Some(moduleNode), primOp.consts.head)
          moduleNode += opNode
          moduleNode.connect(opNode.in1, processExpression(primOp.args.head))
          opNode.asRhs
        }

        primOp.op match {
          case Add => addBinOpNode("add")
          case Sub => addBinOpNode("sub")
          case Mul => addBinOpNode("mul")
          case Div => addBinOpNode("div")
          case Rem => addBinOpNode("rem")

          case Eq  => addBinOpNode("eq")
          case Neq => addBinOpNode("neq")
          case Lt  => addBinOpNode("lt")
          case Leq => addBinOpNode("lte")
          case Gt  => addBinOpNode("gt")
          case Geq => addBinOpNode("gte")

          case Pad => addOneArgOneParamOpNode("pad")

          case AsUInt => addUnaryOpNode("asUInt")
          case AsSInt => addUnaryOpNode("asSInt")

          case Shl => addOneArgOneParamOpNode("shl")
          case Shr => addOneArgOneParamOpNode("shr")

          case Dshl => addBinOpNode("dshl")
          case Dshr => addBinOpNode("dshr")

          case Cvt => addUnaryOpNode("cvt")
          case Neg => addUnaryOpNode("neg")
          case Not => addUnaryOpNode("not")

          case And => addBinOpNode("and")
          case Or  => addBinOpNode("or")
          case Xor => addBinOpNode("xor")

          case Andr => addUnaryOpNode("andr")
          case Orr  => addUnaryOpNode("orr")
          case Xorr => addUnaryOpNode("xorr")

          case Cat => addBinOpNode("cat")

          case Bits =>
            val opNode = OneArgTwoParamOpNode("bits", Some(moduleNode), primOp.consts.head, primOp.consts.tail.head)
            moduleNode += opNode
            moduleNode.connect(opNode.in1, processExpression(primOp.args.head))
            opNode.asRhs

          case Head => addOneArgOneParamOpNode("head")
          case Tail => addOneArgOneParamOpNode("tail")

          case _ =>
            "dummy"
        }
      }

      def processExpression(expression: firrtl.ir.Expression): String = {
        def resolveRef(firrtlName: String, dotName: String): String = {
          nameToNode.get(firrtlName) match {
            case Some(node) =>
              node.asRhs
            case _ => dotName
          }
        }

        val result = expression match {
          case mux: firrtl.ir.Mux =>
            val arg0ValueOpt = getLiteralValue(mux.tval)
            val arg1ValueOpt = getLiteralValue(mux.fval)

            val muxNode = MuxNode("mux", Some(moduleNode), arg0ValueOpt, arg1ValueOpt)
            moduleNode += muxNode
            moduleNode.connect(muxNode.select, processExpression(mux.cond))
            moduleNode.connect(muxNode.in1, processExpression(mux.tval))
            moduleNode.connect(muxNode.in2, processExpression(mux.fval))
            muxNode.asRhs
          case WRef(name, _, _, _) =>
            resolveRef(getFirrtlName(name), expand(name))
          case Reference(name, _, _, _) =>
            resolveRef(getFirrtlName(name), expand(name))
          case subfield: WSubField =>
            resolveRef(getFirrtlName(subfield.serialize), expand(subfield.serialize))
          case subIndex: WSubIndex =>
            resolveRef(getFirrtlName(subIndex.serialize), expand(subIndex.serialize))
          case primOp: DoPrim =>
            processPrimOp(primOp)
          case c: UIntLiteral =>
            val uInt = LiteralNode(s"lit${PrimOpNode.hash}", c.value, Some(moduleNode))
            moduleNode += uInt
            uInt.absoluteName
          case c: SIntLiteral =>
            val uInt = LiteralNode(s"lit${PrimOpNode.hash}", c.value, Some(moduleNode))
            moduleNode += uInt
            uInt.absoluteName
          case other =>
            // throw new Exception(s"renameExpression:error: unhandled expression $expression")
//            other.getClass.getName
            ""
        }
        result
      }

      def processPorts(module: DefModule): Unit = {
        def showPorts(dir: firrtl.ir.Direction): Unit = {
          module.ports.foreach {
            case port if port.direction == dir =>
              val portNode = PortNode(
                port.name,
                Some(moduleNode),
                port.direction == firrtl.ir.Input
              )
              nameToNode(getFirrtlName(port.name)) = portNode
              moduleNode += portNode
            case _ => None
          }

        }

        if (scope.doPorts()) {
          showPorts(firrtl.ir.Input)
          showPorts(firrtl.ir.Output)
        }
      }

      def processMemory(memory: DefMemory): Unit = {
        val fName = getFirrtlName(memory.name)
        val memNode = MemNode(memory.name, Some(moduleNode), fName, memory, nameToNode)
        moduleNode += memNode
      }

      def getConnectInfo(expression: Expression): String = {
        val (fName, dotName) = expression match {
          case WRef(name, _, _, _)      => (getFirrtlName(name), expand(name))
          case Reference(name, _, _, _) => (getFirrtlName(name), expand(name))
          case subfield: WSubField =>
            (getFirrtlName(subfield.serialize), expand(subfield.serialize))
          case s: WSubIndex => (getFirrtlName(s.serialize), expand(s.serialize))
          case other =>
            println(s"Found bad connect arg $other")
            ("badName", "badName")
        }
        // firrtl sink => source, elk source -> sink
        // return sink
        val lhsName = nameToNode.get(fName) match {
          case Some(regNode: RegisterNode) => regNode.in
          case Some(port: PortNode) => port.absoluteName
          case Some(memPort: MemoryPort) => memPort.absoluteName
          case _ => dotName
        }
        lhsName
      }

      def processStatement(s: Statement): Unit = {
        s match {
          case block: Block =>
            block.stmts.foreach { subStatement =>
              processStatement(subStatement)
            }
          case con: Connect if scope.doComponents() =>
            val lhsName = getConnectInfo(con.loc)
            moduleNode.connect(lhsName, processExpression(con.expr))

          case Attach(_, exprs) if scope.doComponents() =>
            exprs.toList match {
              case lhs :: tail =>
                val lhsName = getConnectInfo(lhs)
                tail.foreach { rhs =>
                  moduleNode.analogConnect(lhsName, processExpression(rhs))
                }
              case _ =>
            }

          case WDefInstance(_, instanceName, moduleName, _) if scope.doComponents() =>
            val subModule = findModule(moduleName, c)
            val newPrefix = if (modulePrefix.isEmpty) instanceName else modulePrefix + "." + instanceName
            val moduleNameParsed = moduleName.split("/").last
            val subModuleNode =
              ModuleNode(instanceName, Some(moduleNode), Some(moduleNameParsed), subModuleDepth + 1)
            moduleNode += subModuleNode

            subModulesFound += subModule

            processModule(newPrefix, subModule, subModuleNode, scope.descend, subModuleDepth + 1)

            moduleNode.subModuleNames += subModuleNode.absoluteName

          case DefNode(_, name, expression) if scope.doComponents() =>
            val fName = getFirrtlName(name)
            val nodeNode = NodeNode(name, Some(moduleNode))
            moduleNode += nodeNode
            nameToNode(fName) = nodeNode
            moduleNode.connect(expand(name), processExpression(expression))
          case DefWire(_, name, _) if scope.doComponents() =>
            val fName = getFirrtlName(name)
            val nodeNode = NodeNode(name, Some(moduleNode))
            nameToNode(fName) = nodeNode
          case reg: DefRegister if scope.doComponents() =>
            val regNode = RegisterNode(reg.name, Some(moduleNode))
            nameToNode(getFirrtlName(reg.name)) = regNode
            moduleNode += regNode
          case memory: DefMemory if scope.doComponents() =>
            processMemory(memory)
          case _ =>
          // let everything else slide
        }
      }

      def removeTempWires(): Unit = {
        for (key <- moduleNode.connections.keys) {
//          val node = moduleNode.namedNodes.getOrElse(key,null)
          moduleNode.namedNodes.get(key) match {
            case Some(_: NodeNode) => {
              // must be once
              val value = moduleNode.connections(key)
              for ((sink, source) <- moduleNode.connections) {
                if (source == key) {
                  // must be once
                  moduleNode.connections(sink) = value
                }
              }
              moduleNode.connections.remove(key)
              moduleNode.children -= moduleNode.namedNodes(key)
            }
            case _ =>
          }
        }
      }

      myModule match {
        case module: firrtl.ir.Module =>
          processPorts(myModule)
          processStatement(module.body)
          removeTempWires()
        case extModule: ExtModule =>
          processPorts(extModule)
        case a =>
          println(s"got a $a")
      }

      moduleNode
    }

    findModule(startModuleName, c) match {
      case topModule: DefModule =>
//        pl(s"// ${BuildInfo.toString}")
        pl("// for visualize the graph file, please visit https://github.com/easysoc/easysoc-diagrammer")
        pl("algorithm: layered")
        pl("hierarchyHandling: INCLUDE_CHILDREN")
        val topModuleNode = ModuleNode(startModuleName, parentOpt = None)
        processModule("", topModule, topModuleNode, Scope(0, maxDepth))
        pl(topModuleNode.render)
      case _ =>
        println(s"Could not find top module $startModuleName")
    }

    printFile.close()
    println(s"Print file closed, $totalLinesPrinted lines printed")

    state
  }
}
