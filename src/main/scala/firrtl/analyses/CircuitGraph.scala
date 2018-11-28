// See LICENSE for license details.

package firrtl.analyses

import firrtl.annotations._
import firrtl.graph.{DiGraph, MutableDiGraph}
import firrtl.ir._
import firrtl.Mappers._
import firrtl.{FEMALE, InstanceKind, MALE, PortKind, Utils, WDefInstance, WRef, WSubField, WSubIndex}

import scala.collection.mutable

object CircuitGraph {

  /** Returns a [[DiGraph]] of [[Target]] and corresponding [[IRLookup]]
    *
    * Represents the directed connectivity of a FIRRTL circuit
    * @param circuit
    * @return
    */
  def apply(circuit: Circuit): (DiGraph[Target], IRLookup) = buildCircuitGraph(circuit)

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

  /** Returns a target to each sub-component, including intermediate subcomponents
    * E.g.
    *   Given:
    *     A ReferenceTarget of ~Top|Module>ref and a type of {foo: {bar: UInt}}
    *   Return:
    *     Seq(~Top|Module>ref, ~Top|Module>ref.foo, ~Top|Module>ref.foo.bar)
    * @param r
    * @param t
    * @return
    */
  def allTargets(r: ReferenceTarget, t: Type): Seq[ReferenceTarget] = t match {
    case _: GroundType => Vector(r)
    case VectorType(tpe, size) => r +: (0 until size).flatMap { i => allTargets(r.index(i), tpe) }
    case BundleType(fields) => r +: fields.flatMap { f => allTargets(r.field(f.name), f.tpe)}
    case other => sys.error(s"Error! Unexpected type $other")
  }

  /** Returns a target to each sub-component, excluding intermediate subcomponents
    * E.g.
    *   Given:
    *     A ReferenceTarget of ~Top|Module>ref and a type of {foo: {bar: UInt}}
    *   Return:
    *     Seq(~Top|Module>ref.foo.bar)
    * @param r
    * @param t
    * @return
    */
  def leafTargets(r: ReferenceTarget, t: Type): Seq[ReferenceTarget] = t match {
    case _: GroundType => Vector(r)
    case VectorType(tpe, size) => (0 until size).flatMap { i => leafTargets(r.index(i), tpe) }
    case BundleType(fields) => fields.flatMap { f => leafTargets(r.field(f.name), f.tpe)}
    case other => sys.error(s"Error! Unexpected type $other")
  }


  /** Returns target and type of each module port
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
        (inputs, outputs :+ (CircuitGraph.asTarget(m, new TokenTagger())(e).asInstanceOf[ReferenceTarget], e.tpe))
      case ((inputs, outputs), e) =>
        (inputs :+ (CircuitGraph.asTarget(m, new TokenTagger())(e).asInstanceOf[ReferenceTarget], e.tpe), outputs)
    }
  }

  private def buildCircuitGraph(circuit: Circuit): (DiGraph[Target], IRLookup) = {
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
      val allRegTargets = leafTargets(regTarget, d.tpe)
      val allInitTargets = leafTargets(initTarget, d.tpe).zip(Utils.create_exps(d.init))
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

        //TODO(azidar): Support memories
        case d: DefMemory => sys.error("To be supported soon!!")

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

    (DiGraph(mdg), new IRLookup(declarations))
  }
}

