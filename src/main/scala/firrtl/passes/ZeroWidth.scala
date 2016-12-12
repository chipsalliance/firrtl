// See LICENSE for license details.

package firrtl.passes

import scala.collection.mutable
import firrtl.PrimOps._
import firrtl.ir._
import firrtl._
import firrtl.Mappers._
import firrtl.Utils.throwInternalError


object ZeroWidth extends Pass {
  def name = this.getClass.getName
  private val ZERO = BigInt(0)
  private def removeZero(t: Type): Option[Type] = t match {
    case GroundType(IntWidth(ZERO)) => None
    case BundleType(fields) =>
      fields.zip(fields.map(f => removeZero(f.tpe))) collect {
        case (Field(name, flip, _), Some(t)) => Field(name, flip, t)
      } match {
        case Nil => None
        case seq => Some(BundleType(seq))
      }
    case VectorType(t, size) => removeZero(t) match {
      case None => None
      case Some(tx) => Some(VectorType(tx, size))
    }
    case x => Some(x)
  }
  private def onExp(e: Expression): Expression = removeZero(e.tpe) match {
    case None => e.tpe match {
      case UIntType(x) => UIntLiteral(ZERO, IntWidth(BigInt(1)))
      case SIntType(x) => SIntLiteral(ZERO, IntWidth(BigInt(1)))
      case _ => throwInternalError
    }
    case Some(t) => 
      def replaceType(x: Type): Type = t
      (e map replaceType) map onExp
  }
  private def onStmt(s: Statement): Statement = s map onExp match {
    case sx: IsDeclaration =>
      var removed = false
      def applyRemoveZero(t: Type): Type = removeZero(t) match {
        case None => removed = true; t
        case Some(tx) => tx
      }
      val sxx = sx map applyRemoveZero
      if(removed) EmptyStmt else sxx
    case sx => sx map onStmt
  }
  private def onModule(m: DefModule): DefModule = {
    val ports = m.ports.zip(m.ports.map(p => removeZero(p.tpe))) collect {
      case (Port(info, name, dir, _), Some(t)) => Port(info, name, dir, t)
    }
    m match {
      case ext: ExtModule => ext.copy(ports = ports)
      case in: Module => in.copy(ports = ports, body = onStmt(in.body))
    }
  }
  def run(c: Circuit): Circuit = {
    val cx = c.copy(modules = c.modules map onModule)
    ConstProp.run(InferTypes.run(cx))
  }
}
