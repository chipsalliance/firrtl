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

  final class DifferingDriverTypesException private (msg: String) extends PassException(msg)
  object DifferingDriverTypesException {
    def apply(target: ReferenceTarget, tpes: Seq[(Type, Seq[TypeDriver])]): DifferingDriverTypesException = {
      val xs = tpes.map { case (t, ds) => s"${ds.map(_.target().serialize).mkString(", ")} of type ${t.serialize}" }
      val msg = s"${target.serialize} driven with multiple types!" + xs.mkString("\n  ", "\n  ", "")
      new DifferingDriverTypesException(msg)
    }
  }

  private sealed trait ResetDriver
  private case class TargetDriver(target: ReferenceTarget) extends ResetDriver
  // We keep the target around (lazily) so that we can report errors
  private case class TypeDriver(tpe: Type, target: () => ReferenceTarget) extends ResetDriver

  // Collect all drivers for circuit elements of type ResetType
  private def analyze(c: Circuit): Map[ReferenceTarget, List[ResetDriver]] = {
    type DriverMap = mutable.HashMap[ReferenceTarget, List[ResetDriver]]
    def onMod(mod: DefModule): DriverMap = {
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
            target.copy(module = mod, ref = port, component = target.component.tail)
          case _ => target
        }
      }
      def onStmt(map: DriverMap)(stmt: Statement): Unit = {
        stmt match {
          case Connect(_, lhs, rhs) if lhs.tpe == ResetType =>
            val target = makeTarget(lhs)
            val driver = rhs.tpe match {
              case ResetType => TargetDriver(makeTarget(rhs))
              case tpe       => TypeDriver(tpe, () => makeTarget(rhs))
            }
            map(target) = driver :: Nil
          case WDefInstance(_, inst, module, _) =>
            instMap += (inst -> module)
          case Conditionally(_, _, con, alt) =>
            val conMap = new DriverMap
            val altMap = new DriverMap
            onStmt(conMap)(con)
            onStmt(altMap)(alt)
            for (key <- conMap.keys ++ altMap.keys) {
              map(key) = (altMap.get(key).toList ++ conMap.get(key).toList).flatten
            }
          case other => other.foreach(onStmt(map))
        }
      }
      val types = new DriverMap
      mod.foreach(onStmt(types))
      types
    }
    c.modules.foldLeft(Map[ReferenceTarget, List[ResetDriver]]()) {
      case (map, mod) => map ++ onMod(mod)
    }
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

  private sealed trait TypeTree
  private case class BundleTree(fields: Map[String, TypeTree]) extends TypeTree
  private case class VectorTree(subType: TypeTree) extends TypeTree
  // TODO ensure is only AsyncResetType or BoolType
  private case class GroundTree(tpe: Type) extends TypeTree

  private object TypeTree {
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
        val declMap = makeDeclMap(tmap)
        mod.map(implPort(declMap)).map(implStmt(declMap))
      }.getOrElse(mod)
    }
    c.map(onMod)
  }

  private def fixupPasses: Seq[Pass] = Seq(
    InferTypes
  )

  def run(c: Circuit): Circuit = {
    val inferred = resolve(analyze(c))
    val result = inferred.map(m => implement(c, m)).get
    fixupPasses.foldLeft(result)((c, p) => p.run(c))
  }
}
