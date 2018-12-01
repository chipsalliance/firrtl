// See LICENSE for license details.

package firrtl
package transforms

import firrtl.annotations.{Annotation, ReferenceTarget, TargetToken}
import firrtl.ir._
import firrtl.Utils._
import firrtl.passes.{InferWidths, MemPortUtils}

case class WidthGeqConstraintAnnotation(loc: ReferenceTarget, exp: ReferenceTarget) extends Annotation {
  def update(renameMap: RenameMap): Seq[WidthGeqConstraintAnnotation] = {
    (renameMap.get(loc), renameMap.get(exp)) match {
      case (None, None) => Seq(this)
      case (Some(locs), Some(exps)) if locs.size == exps.size =>
        locs.zip(exps).map {
          case (l: ReferenceTarget, e: ReferenceTarget) => WidthGeqConstraintAnnotation(l, e)
        }.toSeq
      case other => throw new Exception("Unable to update WidthGeqConstraintAnnotation!")
    }
  }
}

object InferWidthsWithAnnos extends Transform with ResolvedAnnotationPaths {
  def inputForm: CircuitForm = UnknownForm
  def outputForm: CircuitForm = UnknownForm

  val annotationClasses = Seq(classOf[WidthGeqConstraintAnnotation])

  def execute(state: CircuitState): CircuitState = {
    val circuitName = state.circuit.main
    val typeMap = new collection.mutable.HashMap[ReferenceTarget, Type]

    def getComponentType(baseType: Type, tokens: Seq[TargetToken]): Type = {
      if (tokens.isEmpty) {
        baseType
      } else {
        val headType = tokens.head match {
          case TargetToken.Index(idx) => sub_type(baseType)
          case TargetToken.Field(field) => field_type(baseType, field)
          case _: TargetToken.Ref => baseType
        }
        getComponentType(headType, tokens.tail)
      }
    }

    def getDeclTypes(modName: String)(stmt: Statement): Statement = {
      val pairOpt = stmt match {
        case w: DefWire => Some(w.name -> w.tpe)
        case r: DefRegister => Some(r.name -> r.tpe)
        case n: DefNode => Some(n.name -> n.value.tpe)
        case i: WDefInstance => Some(i.name -> i.tpe)
        case m: DefMemory => Some(m.name -> MemPortUtils.memType(m))
        case other => None
      }
      pairOpt.foreach { case (ref, tpe) =>
        typeMap += (ReferenceTarget(circuitName, modName, Nil, ref, Nil) -> tpe)
      }
      stmt.mapStmt(getDeclTypes(modName))
    }

    state.circuit.modules.foreach { mod =>
      mod.ports.foreach { port =>
        typeMap += (ReferenceTarget(circuitName, mod.name, Nil, port.name, Nil) -> port.tpe)
      }
      mod.mapStmt(getDeclTypes(mod.name))
    }

    val extraConstraints = state.annotations.flatMap {
      case anno: WidthGeqConstraintAnnotation if anno.loc.isLocal && anno.exp.isLocal  =>
        val locBaseType = typeMap(anno.loc.copy(component = Seq.empty))
        val locType = getComponentType(locBaseType, anno.loc.component)

        val expBaseType = typeMap(anno.exp.copy(component = Seq.empty))
        val expType = getComponentType(expBaseType, anno.exp.component)

        InferWidths.get_constraints_t(locType, expType)
      case other => Seq.empty
    }

    state.copy(circuit = InferWidths.runWithExtraConstraints(state.circuit, extraConstraints))
  }
}
