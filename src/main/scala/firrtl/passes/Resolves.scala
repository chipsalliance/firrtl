/*
Copyright (c) 2014 - 2016 The Regents of the University of
California (Regents). All Rights Reserved.  Redistribution and use in
source and binary forms, with or without modification, are permitted
provided that the following conditions are met:
   * Redistributions of source code must retain the above
     copyright notice, this list of conditions and the following
     two paragraphs of disclaimer.
   * Redistributions in binary form must reproduce the above
     copyright notice, this list of conditions and the following
     two paragraphs of disclaimer in the documentation and/or other materials
     provided with the distribution.
   * Neither the name of the Regents nor the names of its contributors
     may be used to endorse or promote products derived from this
     software without specific prior written permission.
IN NO EVENT SHALL REGENTS BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS,
ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
REGENTS HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE. THE SOFTWARE AND ACCOMPANYING DOCUMENTATION, IF
ANY, PROVIDED HEREUNDER IS PROVIDED "AS IS". REGENTS HAS NO OBLIGATION
TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
MODIFICATIONS.
*/

package firrtl.passes

import firrtl._
import firrtl.ir._
import firrtl.Mappers._

object ResolveKinds extends Pass {
  def name = "Resolve Kinds"
  type KindMap = collection.mutable.LinkedHashMap[String, Kind]

  def findPort(kinds: KindMap)(p: Port): Port = {
    kinds(p.name) = PortKind
    p
  }

  def findStmt(kinds: KindMap)(s: Statement):Statement = {
    s match {
      case s: DefWire => kinds(s.name) = WireKind
      case s: DefNode => kinds(s.name) = NodeKind
      case s: DefRegister => kinds(s.name) = RegKind
      case s: WDefInstance => kinds(s.name) = InstanceKind
      case s: DefMemory => kinds(s.name) = MemKind
      case s =>
    } 
    s map findStmt(kinds)
  }

  def resolveExpr(kinds: KindMap)(e: Expression): Expression = e match {
    case e: WRef => e copy (kind = kinds(e.name))
    case e => e map resolveExpr(kinds)
  }

  def resolveStmt(kinds: KindMap)(s: Statement): Statement =
    s map resolveStmt(kinds) map resolveExpr(kinds)

  def resolveKinds(m: DefModule): DefModule = {
    val kinds = new KindMap
    m map findPort(kinds) map findStmt(kinds) map resolveStmt(kinds)
  }
 
  def run(c: Circuit): Circuit =
    c copy (modules = c.modules map resolveKinds)
}

object ResolveGenders extends Pass {
  def name = "Resolve Genders"
  def resolveE(g: Gender)(e: Expression): Expression = e match {
    case e: WRef => e copy (gender = g)
    case WSubField(exp, name, tpe, _) => WSubField(
      Utils.field_flip(exp.tpe, name) match {
        case Default => resolveE(g)(exp)
        case Flip => resolveE(Utils.swap(g))(exp)
      }, name, tpe, g)
    case WSubIndex(exp, value, tpe, _) =>
      WSubIndex(resolveE(g)(exp), value, tpe, g)
    case WSubAccess(exp, index, tpe, _) =>
      WSubAccess(resolveE(g)(exp), resolveE(MALE)(index), tpe, g)
    case e => e map resolveE(g)
  }
        
  def resolveS(s: Statement): Statement = s match {
    //TODO(azidar): pretty sure don't need to do anything for Attach, but not positive...
    case IsInvalid(info, expr) =>
      IsInvalid(info, resolveE(FEMALE)(expr))
    case Connect(info, loc, expr) =>
      Connect(info, resolveE(FEMALE)(loc), resolveE(MALE)(expr))
    case PartialConnect(info, loc, expr) =>
      PartialConnect(info, resolveE(FEMALE)(loc), resolveE(MALE)(expr))
    case s => s map resolveE(MALE) map resolveS
  }

  def resolveGender(m: DefModule): DefModule = m map resolveS

  def run(c: Circuit): Circuit =
    c copy (modules = c.modules map resolveGender)
}

object CInferMDir extends Pass {
  def name = "CInfer MDir"
  type MPortDirMap = collection.mutable.LinkedHashMap[String, MPortDir]

  def inferMDirE(mports: MPortDirMap, dir: MPortDir)(e: Expression): Expression = e match {
    case e: Reference =>
      mports get e.name match {
        case None =>
        case Some(p) => mports(e.name) = (p, dir) match {
          case (MInfer, MInfer) => Utils.error("Shouldn't be here")
          case (MInfer, MWrite) => MWrite
          case (MInfer, MRead) => MRead
          case (MInfer, MReadWrite) => MReadWrite
          case (MWrite, MInfer) => Utils.error("Shouldn't be here")
          case (MWrite, MWrite) => MWrite
          case (MWrite, MRead) => MReadWrite
          case (MWrite, MReadWrite) => MReadWrite
          case (MRead, MInfer) => Utils.error("Shouldn't be here")
          case (MRead, MWrite) => MReadWrite
          case (MRead, MRead) => MRead
          case (MRead, MReadWrite) => MReadWrite
          case (MReadWrite, MInfer) => Utils.error("Shouldn't be here")
          case (MReadWrite, MWrite) => MReadWrite
          case (MReadWrite, MRead) => MReadWrite
          case (MReadWrite, MReadWrite) => MReadWrite
        }
      }
      e
    case e: SubAccess =>
      inferMDirE(mports, dir)(e.expr)
      inferMDirE(mports, MRead)(e.index) // index can't be a write port
      e
    case e => e map inferMDirE(mports, dir)
  }

  def inferMDirS(mports: MPortDirMap)(s: Statement): Statement = s match { 
    case s: CDefMPort =>
       mports(s.name) = s.direction
       s map inferMDirE(mports, MRead)
    case s: Connect =>
       inferMDirE(mports, MRead)(s.expr)
       inferMDirE(mports, MWrite)(s.loc)
       s
    case s: PartialConnect =>
       inferMDirE(mports, MRead)(s.expr)
       inferMDirE(mports, MWrite)(s.loc)
       s
    case s => s map inferMDirS(mports) map inferMDirE(mports, MRead)
  }
        
  def setMDirS(mports: MPortDirMap)(s: Statement): Statement = s match { 
    case s: CDefMPort => s copy (direction = mports(s.name))
    case s => s map setMDirS(mports)
  }
  
  def inferMDir(m: DefModule): DefModule = {
    val mports = new MPortDirMap
    m map inferMDirS(mports) map setMDirS(mports)
  }
     
  def run(c: Circuit): Circuit =
    c copy (modules = c.modules map inferMDir)
}
