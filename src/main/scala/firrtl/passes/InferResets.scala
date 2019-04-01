// See LICENSE for license details.

package firrtl.passes

import firrtl._
import firrtl.ir._
import firrtl.Mappers._
import firrtl.traversals.Foreachers._
import firrtl.analyses.InstanceGraph
import firrtl.graph.{DiGraph, MutableDiGraph}
import firrtl.annotations.{ReferenceTarget, TargetToken}
import firrtl.Utils.toTarget
import collection.mutable

import scala.util.{Try, Success, Failure}

/** Infers the concrete type of [[Reset]]s by their connections
  * This is a global inference because ports can be of type [[Reset]]
  */
// TODO should we error if a DefMemory is of type AsyncReset? In CheckTypes?
object InferResets extends Pass {
  private type ResetMap = Map[ReferenceTarget, (ReferenceTarget, Type)]
  private val ResetMap = Map[ReferenceTarget, (ReferenceTarget, Type)]()

  final class DifferingDriverTypesException private (msg: String) extends PassException(msg)
  object DifferingDriverTypesException {
    def apply(target: ReferenceTarget, tpes: Seq[(Type, Seq[TypeDriver])]): DifferingDriverTypesException = {
      val xs = tpes.map { case (t, ds) => s"${t.serialize} by {ds.map(_.target().serialize)}" }
      val msg = s"${target.serialize} driven with multiple types!" + xs.mkString("\n  ", "\n  ", "")
      new DifferingDriverTypesException(msg)
    }
  }
  final class MultiResetTypesException private (msg: String) extends PassException(msg)
  object MultiResetTypesException {
    def apply(): MultiResetTypesException = new MultiResetTypesException("bad")
  }

  private sealed trait ResetDriver
  private case class TargetDriver(target: ReferenceTarget) extends ResetDriver
  // We keep the target around (lazily) so that we can report errors
  private case class TypeDriver(tpe: Type, target: () => ReferenceTarget) extends ResetDriver

  // Collect all drivers for circuit elements of type ResetType
  private def analyze(c: Circuit): Map[ReferenceTarget, List[ResetDriver]] = {
    val types = mutable.Map[ReferenceTarget, List[ResetDriver]]()
    def onMod(mod: DefModule): Unit = {
      val instMap = mutable.Map[String, String]()
      // We need to convert submodule port targets into targets on the Module port itself
      def makeTarget(expr: Expression): ReferenceTarget = {
        val target = toTarget(c.main, mod.name)(expr)
        Utils.kind(expr) match {
          case InstanceKind =>
            val mod = instMap(target.ref)
            val port = target.component.head match {
              case TargetToken.Field(name) => name
            }
            //println(target)
            val res = target.copy(module = mod, ref = port, component = target.component.tail)
            //println(res)
            res
          case _ => target
        }
      }
      def onStmt(stmt: Statement): Unit = {
        stmt.foreach(onStmt)
        stmt match {
          case Connect(_, lhs, rhs) if lhs.tpe == ResetType =>
            val target = makeTarget(lhs)
            val driver = rhs.tpe match {
              case ResetType => TargetDriver(makeTarget(rhs))
              case tpe       => TypeDriver(tpe, () => makeTarget(rhs))
            }
            types(target) = driver :: types.getOrElse(target, Nil)
          case WDefInstance(_, inst, module, _) =>
            instMap += (inst -> module)
          case _ =>
        }
      }
      mod.foreach(onStmt)
    }
    c.foreach(onMod)
    types.toMap
  }

  // Determine the type driving a given ResetType
  private def resolve(map: Map[ReferenceTarget, List[ResetDriver]]): Try[Map[ReferenceTarget, Type]] = {
    val res = mutable.Map[ReferenceTarget, Type]()
    val errors = new Errors
    def rec(target: ReferenceTarget): Type = {
      val drivers = map(target)
      res.getOrElseUpdate(target, {
        val tpes = drivers.map {
          case TargetDriver(t) => TypeDriver(rec(t), () => t)
          case td: TypeDriver => td
        }.groupBy(_.tpe)
        if (tpes.keys.size != 1) {
          // Multiple types of driver!
          errors.append(DifferingDriverTypesException(target, tpes.toSeq))
        }
        tpes.keys.head
      })
    }
    for ((target, _) <- map) {
      rec(target)
    }
    Try {
      errors.trigger()
      res.toMap
    }
  }

  sealed trait TypeTree
  case class BundleTree(fields: Map[String, TypeTree]) extends TypeTree
  case class VectorTree(subType: TypeTree) extends TypeTree
  // TODO ensure is only AsyncResetType or BoolType
  case class GroundTree(tpe: Type) extends TypeTree

  object TypeTree {
    // TODO make return Try[TypeTree]
    def fromTokens(tokens: (Seq[TargetToken], Type)*): TypeTree = tokens match {
      case Seq((Seq(), tpe)) => GroundTree(tpe)
      // VectorTree
      case (TargetToken.Index(_) +: _, _) +: _ =>
        // Vectors must all have the same type, so we only process Index 0
        // If the subtype is an aggregate, there can be multiple of each index
        val ts = tokens.collect { case (TargetToken.Index(0) +: tail, tpe) => (tail, tpe) }
        VectorTree(fromTokens(ts:_*))
      // BundleTree
      case (TargetToken.Field(_) +: _, _) +: _ =>
        val fields =
          tokens.groupBy { case (TargetToken.Field(n) +: t, _) => n }
                .mapValues { ts =>
                  fromTokens(ts.map { case (_ +: t, tpe) => (t, tpe) }:_*)
                }
        BundleTree(fields)
    }
  }

  private def fixupType(tpe: Type, tree: TypeTree): Type = (tpe, tree) match {
    case (BundleType(fields), BundleTree(map)) =>
      val fieldsx =
        fields.map(f => map.get(f.name) match {
          case Some(t) => f.copy(tpe = fixupType(f.tpe, t))
          case None => f
        })
      BundleType(fieldsx)
    case (VectorType(vtpe, size), VectorTree(t)) =>
      VectorType(fixupType(vtpe, t), size)
    case (_, GroundTree(t)) => t
    case x => throw new Exception(s"Error! Unexpected pair $x")
  }

  // Assumes all ReferenceTargets are in the same module
  private def makeDeclMap(map: Map[ReferenceTarget, Type]): Map[String, TypeTree] =
    map.groupBy(_._1.ref).mapValues { ts =>
      TypeTree.fromTokens(ts.toSeq.map { case (target, tpe) => (target.component, tpe) }:_*)
    }

  private def implPort(map: Map[String, TypeTree])(port: Port): Port =
    map.get(port.name)
       .map(tree => port.copy(tpe = fixupType(port.tpe, tree)))
       .getOrElse(port)
  private def implStmt(map: Map[String, TypeTree])(stmt: Statement): Statement =
    stmt.map(implStmt(map)) match {
      case decl: IsDeclaration if map.contains(decl.name) =>
        val tree = map(decl.name)
        decl match {
          case reg: DefRegister => reg.copy(tpe = fixupType(reg.tpe, tree))
          case wire: DefWire => wire.copy(tpe = fixupType(wire.tpe, tree))
          // TODO Can this really happen?
          case mem: DefMemory => mem.copy(dataType = fixupType(mem.dataType, tree))
          case other => other
        }
      case other => other
    }

  private def implement(c: Circuit, map: Map[ReferenceTarget, Type]): Circuit = {
    val modMaps = map.groupBy(_._1.module)
    def onMod(mod: DefModule): DefModule = {
      modMaps.get(mod.name).map { tmap =>
        val map = makeDeclMap(tmap)
        //println(s"********** Implement on ${mod.name} **********")
        //println(map)
        mod.map(implPort(map)).map(implStmt(map))
      }.getOrElse(mod)
    }
    c.map(onMod)
  }

  def run(c: Circuit): Circuit = {
    val a = analyze(c)
    //println(a)
    val r = resolve(a)
    //println(r)
    val res = r.map(m => implement(c, m)).get
    //println(res.serialize)
    //throw new Exception("bail") with scala.util.control.NoStackTrace
    res
  }
}
