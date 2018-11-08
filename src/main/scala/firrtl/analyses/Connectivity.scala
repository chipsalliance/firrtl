package firrtl.analyses

import firrtl._
import firrtl.ir._
import firrtl.Mappers._
import firrtl.PrimOps.AsClock
import firrtl.graph._
import firrtl.analyses.component._

import scala.collection.mutable

trait Label
case class IntoInstance(name: String) extends Label
case class OutOfInstance(name: String) extends Label
case object IntoRegister extends Label
case object OutOfRegister extends Label
case object Drives extends Label

class Connectivity private (val declarations: Declarations, val top: String) {
  import Connectivity._

  private val moduleMap = new mutable.HashMap[String, DefModule]()

  private val connectionsBuilder = new MutableEdgeLabeledDiGraph[Component, Label]
  lazy val connections = EdgeLabeledDiGraph(connectionsBuilder)

  private val asClocks = new mutable.HashSet[Component]()

  //module name is vertex, instance name is edge label
  private val instancesBuilder = new MutableEdgeLabeledDiGraph[String, String]()
  lazy val instances = EdgeLabeledDiGraph(instancesBuilder)

  private def connect(sink: Expression, source: Expression, encapsulatingModule: String): Unit = {
    val src = Connectivity.toComponent(source, encapsulatingModule, declarations)
    val snk = Connectivity.toComponent(sink, encapsulatingModule, declarations)
    source match {
      case DoPrim(AsClock, _, _, _) => asClocks += src
      case _ =>
    }

    (Utils.kind(sink), Utils.kind(source)) match {
      case (_, RegKind) => connectionsBuilder.addPairWithEdge(src, snk, OutOfRegister)
      case (RegKind, _) => connectionsBuilder.addPairWithEdge(src, snk, IntoRegister)
      case _ => connectionsBuilder.addPairWithEdge(src, snk, Drives)

    }
  }

  private def instance(instance: WDefInstance, encapsulatingModule: String): Unit = {
    instancesBuilder.addPairWithEdge(encapsulatingModule, instance.module, instance.name)
    Utils.create_exps(WRef(instance.name, instance.tpe, InstanceKind, MALE)).foreach { e =>
      val iport = toComponent(e, encapsulatingModule, declarations)
      val mport = toComponent(removeRef(e), instance.module, declarations)
      Utils.gender(e) match {
        case MALE => // instance port is male
          connectionsBuilder.addPairWithEdge(mport, iport, OutOfInstance(instance.name))
        case FEMALE => // instance port is female
          connectionsBuilder.addPairWithEdge(iport, mport, IntoInstance(instance.name))
        case _ => sys.error("BAD")
      }
    }
  }

  private def addModule(m: DefModule): Unit = moduleMap(m.name) = m

  def registerPaths(): Seq[Seq[(Component, Seq[Label])]] = {
    connections.getVertices.foreach{println}
    ???
  }

  def clockSources(): Seq[(Component, Seq[Label])] = {
    val sourceComponents = (connections.findSources ++ asClocks).filter{c => declarations.ir(c) match {
      case DoPrim(AsClock, _, _, _) => true
      case Port(_, _, Output, ClockType) if moduleMap(c.encapsulatingModule).isInstanceOf[ExtModule] => true
      case Port(_, _, Input, ClockType) if c.encapsulatingModule == top => true
      case _ => false
    }}
    val traversalState = new TraversalState[String, String](Nil)

    // Discover all clock sources
    val sources = sourceComponents.flatMap { src =>
      // Sequence of Paths, where each path is a sequence of vertex/labelsofar pairs
      traversalState.set(top, Nil)
      val paths: Seq[Seq[(String, Seq[String])]] = instances.paths(traversalState, src.encapsulatingModule)
      traversalState.set(top, Nil)
      paths.map(path => (src, path.last._2.map(IntoInstance)))
    }

    // Ok, now map register to clock source
    val register2source = mutable.HashMap[State, State]()
    val source2register = sources.foldLeft(Map.empty[(State), Set[(State)]]) {
      case (map, (vertex, state)) =>
        val registerTraversal = new TraversalState[Component, Label](Nil)
        registerTraversal.setUpdateState {
          case ((_, labels), (_, Drives)) => labels
          case ((_, Nil), (_, l)) => Seq(l)
          case ((_, labels), (_, l)) => (labels.last, l) match {
            case (IntoInstance(x), OutOfInstance(y)) if x == y => labels.dropRight(1)
            case _ => labels :+ l
          }
        }
        registerTraversal.setTraversalCondition {
          case (_, (_, via: Label)) => via != OutOfRegister
        }
        registerTraversal.set(vertex, state)
        val fullPaths = connections.getFullPaths(registerTraversal)
        val registers = fullPaths.map(_.last).filter{ case (c: Component, _) => declarations.ir(c.tag).isInstanceOf[DefRegister]}
        registers.foreach { register =>
          register2source(register) = (vertex, state)
        }
        map + ((vertex, state) -> registers.toSet)
    }
    source2register.foreach{case (clock, registers) => println(s"For ${serialize(clock)}:"); println(registers.map("\t" + serialize(_)).mkString("\n"))}

    // Now, find all paths from each register, ending at another register

    //case class
    //case class Reg2RegPath(from: (Component, Seq[Label]), clk: to (Component, Seq[Label]))
    type State = (Component, Seq[Label])
    val clockCrossingPaths = mutable.ArrayBuffer[(State, State, Seq[State])]() //source clock, sink clock, path
    println("Clock crossing paths")
    source2register.foreach { case (clock, registers) =>
      registers.foreach { register =>
        println(s"On $clock and $register")
        val registerPathTraverser = new TraversalState[Component, Label](Nil)
        registerPathTraverser.setUpdateState {
          case ((_, labels), (_, Drives)) => labels
          case ((_, Nil), (_, l)) => Seq(l)
          case ((_, labels), (_, l)) => l match {
            case OutOfInstance(y) =>
              println("------")
              println(labels)
              println(l)
              val x = labels.reverse.dropWhile{ _ != IntoInstance(`y`) }
              val z = x.reverse.drop(1)
              println(z)
              z
            case IntoInstance(y) => labels :+ l
            case _ => labels
          }
        }
        registerPathTraverser.setTraversalCondition {
          case ((current, state), (next, via: Label)) =>
            via != OutOfRegister || (state == register._2 && current == register._1)
        }
        registerPathTraverser.set(register._1, register._2)

        connections.BFS(registerPathTraverser)
        val sinkRegisters = registerPathTraverser.getLeafs.filter{ case (c: Component, _) => declarations.ir(c.tag).isInstanceOf[DefRegister]}
        val outsideRegisters = sinkRegisters.filter{!source2register(clock).contains(_)}
        outsideRegisters.foreach { or =>
          registerPathTraverser.set(register._1, register._2)
          //println(s"From $register to $or")
          clockCrossingPaths += ((clock, register2source(or), connections.path(registerPathTraverser, or)))
        }
      }
    }
    clockCrossingPaths.foreach { case (sourceClock, sinkClock, path) =>
        println(s"Clock crossing from domain (${serialize(sourceClock)}) to domain (${serialize(sinkClock)}) via:\n\t" + path.map("(" + serialize(_) + ")").mkString(" -> "))
    }
    ???
  }
}


object Connectivity {

  def serialize(tuple: (Component, Seq[Label])): String = serialize(tuple._1, tuple._2)
  def serialize(component: Component, labels: Seq[Label]): String = {
    labels.map { case IntoInstance(i) => i }.mkString("/") + "/" + component.reference.mkString("") + " in " + component.encapsulatingModule
  }
  def apply(state: CircuitState): Connectivity = apply(state.circuit)

  def apply(circuit: Circuit): Connectivity = {
    val declarations = Declarations(circuit)
    val connectivity = new Connectivity(declarations, circuit.main)
    circuit.modules.foreach { apply(connectivity, declarations) }
    connectivity
  }

  def apply(module: DefModule): Connectivity = {
    val declarations = Declarations(module)
    val connectivity = new Connectivity(declarations, module.name)
    apply(connectivity, declarations)(module)
    connectivity
  }

  private def apply(connectivity: Connectivity, declarations: Declarations)(module: DefModule): Connectivity = {
    val encapsulatingModule = module.name
    connectivity.addModule(module)

    def connectFrom(sink: Expression)(e: Expression): Expression = e match {
      case _: WRef | _: WSubField | _: WSubIndex =>
        connectivity.connect(sink, e, encapsulatingModule)
        e
      case other =>
        // Connect to only other references
        e map connectFrom(sink)
        // Connect to every expression
        connectivity.connect(sink, other, encapsulatingModule)
        e map connectFrom(other)
    }

    def onStmt(s: Statement): Statement = s match {
      case Connect(_, loc, expr) =>
        connectFrom(loc)(expr)
        s
      case DefNode(_, name, value) =>
        connectFrom(WRef(name, value.tpe))(value)
        s
      case IsInvalid(_, value) =>
        connectFrom(value)(WInvalid)
        s
      case w: WDefInstance =>
        connectivity.instance(w, module.name)
        w
      case r: DefRegister =>
        r map connectFrom(WRef(r.name, r.tpe))
        r
      case other => other map onStmt
    }

    module map onStmt
    connectivity
  }

  /*
  def expand(name: String, tpe: Type): Seq[Expression] = {
    val collection = new mutable.ArrayBuffer[Expression]()
    expand(collection)(WRef(name, tpe))
    collection
  }

  def expand(collection: mutable.ArrayBuffer[Expression])(e: Expression): Unit = {
    collection += e
    Utils.getKids(e) map expand(collection)
  }
  */

  /**
    * Returns a referenceable component
    * @param e referencable component
    * @param encapsulatingModule module containing e
    * @return
    */
  def toComponent(e: Expression, encapsulatingModule: String, declarations: Declarations): Component = e match {
    case w: WRef => declarations(encapsulatingModule, w.name)
    case w: WSubField => toComponent(w.expr, encapsulatingModule, declarations).field(w.name)
    case w: WSubIndex => toComponent(w.expr, encapsulatingModule, declarations).index(w.value)
    case other => declarations(other)
  }

  /**
    * Returns a new expression with the reference removed
    * a.b -> b
    *
    * @param e
    * @return
    */
  def removeRef(e: Expression): Expression = e match {
    case WSubField(expr: WRef, name, tpe, gender) => WRef(name, tpe, expr.kind, gender)
    case e: WSubField => e map removeRef
    case e: WSubIndex => e map removeRef
    case _ => sys.error("BAD")
  }
}
