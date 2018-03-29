// See LICENSE for license details.

package firrtl.passes

import firrtl.PrimOps._
import firrtl.ir._
import firrtl._
import firrtl.Mappers._

/**
  * Transform that replaces zero width mems before running the ZeroWidth transform.
  * Dealing with mems is a bit tricky for that pass because the address, en, clk ports
  * of the memory are not width zero even if data is.
  *
  * This pass replaces memories with a DefWire() bundle that contains the address, en,
  * clk, and data fields implemented as zero width wires. Running the ZeroWidth
  * transform afterwards will remove these dangling references properly.
  *
  * This pass is private because it should only be run by the ZeroWidth transform
  */
private object ZeroWidthMemRemove extends Transform {
  import firrtl.passes.ZeroWidth.removeZero

  def inputForm: CircuitForm = UnknownForm
  def outputForm: CircuitForm = UnknownForm
  private def makeEmptyMemBundle(name: String): Field =
    Field(name, Flip, BundleType(Seq(
      Field("addr", Default, UIntType(IntWidth(0))),
      Field("en",   Default, UIntType(IntWidth(0))),
      Field("clk",  Default, UIntType(IntWidth(0))),
      Field("data", Flip,    UIntType(IntWidth(0)))
    )))

  private def onEmptyMemStmt(s: Statement): Statement = s match {
    case d@DefMemory(info, name, tpe, _, _, _, rs, ws, rws, _) => removeZero(tpe) match {
      case None =>
        DefWire(info, name, BundleType(
          rs.map(r => makeEmptyMemBundle(r)) ++
          ws.map(w => makeEmptyMemBundle(w)) ++
          rws.map(rw => makeEmptyMemBundle(rw))
        ))
      case Some(_) => d
    }
    case sx => sx map onEmptyMemStmt
  }
  private def onModule(m: DefModule): DefModule = {
    m match {
      case ext: ExtModule => ext
      case in: Module => in.copy(body = onEmptyMemStmt(in.body))
    }
  }
  def execute(state: CircuitState): CircuitState = {
    val c = state.circuit
    val result = c.copy(modules = c.modules map onModule)
    state.copy(circuit = result)
  }
}

object ZeroWidth extends Transform {
  def inputForm: CircuitForm = UnknownForm
  def outputForm: CircuitForm = UnknownForm
  private val ZERO = BigInt(0)
  private def getRemoved(x: IsDeclaration): Seq[String] = {
    var removedNames: Seq[String] = Seq.empty
    def onType(name: String)(t: Type): Type = {
      removedNames = Utils.create_exps(name, t) map {e => (e, e.tpe)} collect {
        case (e, GroundType(IntWidth(ZERO))) => e.serialize
      }
      t
    }
    x match {
      case s: Statement => s map onType(s.name)
      case Port(_, name, _, t) => onType(name)(t)
    }
    removedNames
  }
  private[passes] def removeZero(t: Type): Option[Type] = t match {
    case GroundType(IntWidth(ZERO)) => None
    case BundleType(fields) =>
      fields map (f => (f, removeZero(f.tpe))) collect {
        case (Field(name, flip, _), Some(t)) => Field(name, flip, t)
      } match {
        case Nil => None
        case seq => Some(BundleType(seq))
      }
    case VectorType(t, size) => removeZero(t) map (VectorType(_, size))
    case x => Some(x)
  }
  private def onExp(e: Expression): Expression = e match {
    case DoPrim(Cat, args, consts, tpe) =>
      val nonZeros = args.flatMap { x =>
        x.tpe match {
          case UIntType(IntWidth(ZERO)) => Seq.empty[Expression]
          case SIntType(IntWidth(ZERO)) => Seq.empty[Expression]
          case other => Seq(x)
        }
      }
      nonZeros match {
        case Nil => UIntLiteral(ZERO, IntWidth(BigInt(1)))
        case Seq(x) => x
        case seq => DoPrim(Cat, seq, consts, tpe) map onExp
      }
    case other => other.tpe match {
      case UIntType(IntWidth(ZERO)) => UIntLiteral(ZERO, IntWidth(BigInt(1)))
      case SIntType(IntWidth(ZERO)) => SIntLiteral(ZERO, IntWidth(BigInt(1)))
      case _ => e map onExp
    }
  }
  private def onStmt(renames: RenameMap)(s: Statement): Statement = s match {
    case d@DefWire(info, name, tpe) =>
      renames.delete(getRemoved(d))
      removeZero(tpe) match {
        case None => EmptyStmt
        case Some(t) => DefWire(info, name, t)
      }
    case d@DefRegister(info, name, tpe, clock, reset, init) =>
      renames.delete(getRemoved(d))
      removeZero(tpe) match {
        case None => EmptyStmt
        case Some(t) =>
         DefRegister(info, name, t, onExp(clock), onExp(reset), onExp(init))
      }
    case d@DefMemory(info, name, tpe, depth, wLatency, rLatency, rs, ws, rws, ruw) =>
      renames.delete(getRemoved(d))
      removeZero(tpe) match {
        case None =>
          Utils.throwInternalError(Some(s"private pass ZeroWidthMemRemove should have removed this memory: $d"))
        case Some(t) => DefMemory(info, name, t, depth, wLatency, rLatency, rs, ws, rws, ruw)
      }
    case Connect(info, loc, exp) => removeZero(loc.tpe) match {
      case None => EmptyStmt
      case Some(t) => Connect(info, loc, onExp(exp))
    }
    case IsInvalid(info, exp) => removeZero(exp.tpe) match {
      case None => EmptyStmt
      case Some(t) => IsInvalid(info, onExp(exp))
    }
    case DefNode(info, name, value) => removeZero(value.tpe) match {
      case None => EmptyStmt
      case Some(t) => DefNode(info, name, onExp(value))
    }
    case sx => sx map onStmt(renames) map onExp
  }
  private def onModule(renames: RenameMap)(m: DefModule): DefModule = {
    renames.setModule(m.name)
    // For each port, record deleted subcomponents
    m.ports.foreach{p => renames.delete(getRemoved(p))}
    val ports = m.ports map (p => (p, removeZero(p.tpe))) flatMap {
      case (Port(info, name, dir, _), Some(t)) => Seq(Port(info, name, dir, t))
      case (Port(_, name, _, _), None) =>
        renames.delete(name)
        Nil
    }
    m match {
      case ext: ExtModule => ext.copy(ports = ports)
      case in: Module => in.copy(ports = ports, body = onStmt(renames)(in.body))
    }
  }
  def execute(state: CircuitState): CircuitState = {
    // run ZeroWidthMemRemove transform first to remove zero-width memories
    // then run InferTypes to update widths for addr, en, clk, etc
    val c = InferTypes.run(ZeroWidthMemRemove.runTransform(state).circuit)
    val renames = RenameMap()
    renames.setCircuit(c.main)
    val result = c.copy(modules = c.modules map onModule(renames))
    CircuitState(result, outputForm, state.annotations, Some(renames))
  }
}
